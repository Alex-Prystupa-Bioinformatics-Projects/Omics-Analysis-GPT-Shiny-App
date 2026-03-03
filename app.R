# ============================================================
# Libraries
# ============================================================
library(shiny)
library(bslib)
library(shinyjs)
library(dplyr)
library(tibble)
library(ggplot2)
library(stringr)
library(glue)
library(Signac)
library(httr)
library(jsonlite)
library(Seurat)
library(presto)
library(dotenv)
library(DT)

options(shiny.maxRequestSize = 5 * 1024^3)

# ============================================================
# API Keys
# ============================================================
dotenv::load_dot_env(".keys")
api_key <- Sys.getenv("API_KEY")
org_id  <- Sys.getenv("ORG_ID")

# ============================================================
# Theme & CSS
# ============================================================
app_theme <- bs_theme(
  bootswatch  = "darkly",
  primary     = "#4361ee",
  base_font   = font_google("Inter"),
  code_font   = font_google("JetBrains Mono")
)

app_css <- "
  /* ---- Chat container ---- */
  .chat-container {
    height: 420px;
    overflow-y: auto;
    padding: 10px 4px;
    display: flex;
    flex-direction: column;
    gap: 8px;
  }

  .chat-empty {
    color: #888;
    font-size: 0.85rem;
    text-align: center;
    margin-top: 40px;
  }

  /* ---- Bubbles ---- */
  .chat-bubble-row {
    display: flex;
    width: 100%;
  }

  .user-row      { justify-content: flex-end; }
  .assistant-row { justify-content: flex-start; }

  .chat-bubble {
    max-width: 85%;
    padding: 10px 14px;
    border-radius: 16px;
    font-size: 0.88rem;
    line-height: 1.5;
    word-break: break-word;
  }

  .user-bubble {
    background: #4361ee;
    color: #fff;
    border-bottom-right-radius: 4px;
  }

  .assistant-bubble {
    background: #2e3338;
    color: #e0e0e0;
    border-bottom-left-radius: 4px;
  }

  .assistant-bubble p { margin: 0; }

  /* ---- Collapsible code block ---- */
  .code-details { margin-top: 8px; }

  .code-summary {
    cursor: pointer;
    font-size: 0.78rem;
    color: #aaa;
    user-select: none;
    list-style: none;
  }

  .code-summary:hover { color: #fff; }

  .code-pre {
    background: #1a1d21;
    color: #c9d1d9;
    font-size: 0.78rem;
    padding: 10px;
    border-radius: 8px;
    overflow-x: auto;
    margin-top: 6px;
    white-space: pre-wrap;
  }

  /* ---- Typing indicator ---- */
  .typing-bubble { padding: 12px 16px; }

  .typing-indicator {
    display: flex;
    gap: 5px;
    align-items: center;
  }

  .typing-indicator span {
    width: 8px;
    height: 8px;
    background: #888;
    border-radius: 50%;
    animation: typing-bounce 1.2s infinite ease-in-out;
  }

  .typing-indicator span:nth-child(2) { animation-delay: 0.2s; }
  .typing-indicator span:nth-child(3) { animation-delay: 0.4s; }

  @keyframes typing-bounce {
    0%, 80%, 100% { transform: scale(0.7); opacity: 0.4; }
    40%           { transform: scale(1.0); opacity: 1.0; }
  }

  /* ---- Input row ---- */
  .chat-input-row {
    margin-top: 8px;
  }

  .chat-input-row textarea {
    font-size: 0.88rem;
    resize: none;
    border-radius: 10px;
  }

  /* ---- Upload area ---- */
  .upload-label {
    font-size: 0.8rem;
    color: #aaa;
    margin-bottom: 2px;
  }

  /* ---- Main panel layout ---- */
  .main-panels {
    display: flex;
    flex-direction: column;
    gap: 0;
  }

  #plot_card {
    height: 460px;
    min-height: 180px;
    overflow: hidden;
  }

  #table_card {
    min-height: 200px;
  }

  /* ---- Vertical panel resizer ---- */
  #panel-resizer {
    height: 6px;
    background: transparent;
    cursor: row-resize;
    border-radius: 3px;
    transition: background 0.15s;
    margin: 2px 0;
  }

  #panel-resizer:hover { background: #4361ee55; }

  /* ---- Sidebar horizontal resizer ---- */
  /* Handle lives on .bslib-sidebar-layout (not .sidebar), so it isn't clipped.
     left uses the same CSS var that JS sets on drag — handle auto-tracks boundary. */
  .sidebar-resize-handle {
    position: absolute;
    left: calc(var(--bslib-sidebar-width, 555px) - 4px);
    top: 0;
    bottom: 0;
    width: 8px;
    cursor: col-resize;
    background: transparent;
    z-index: 200;
    transition: background 0.15s;
  }

  .sidebar-resize-handle:hover,
  .sidebar-resize-handle.dragging { background: #4361ee55; }

  .bslib-sidebar-layout { position: relative; }

  /* ---- Plot nav controls ---- */
  .plot-nav {
    display: flex;
    align-items: center;
    gap: 6px;
  }

  .plot-nav-label {
    font-size: 0.88rem;
    min-width: 140px;
    text-align: center;
  }

  /* ---- Download bar ---- */
  .download-bar {
    display: flex;
    gap: 10px;
    flex-wrap: wrap;
    align-items: center;
  }

  .download-bar label {
    font-size: 0.82rem;
    color: #aaa;
    margin-bottom: 0;
  }

  .download-bar .form-select {
    font-size: 0.82rem;
    padding: 2px 6px;
    height: auto;
  }
"

# ============================================================
# Drag-resize JavaScript
# ============================================================
resize_js <- tags$script(HTML("
$(document).ready(function() {

  // ---- 1. Sidebar horizontal resize ----
  // Wrapped in setTimeout so bslib has time to render its layout DOM
  setTimeout(function() {
    var layout  = document.querySelector('.bslib-sidebar-layout');
    var sidebar = layout && (
      layout.querySelector('.bslib-sidebar') ||
      layout.querySelector('.sidebar')
    );

    if (sidebar) {
      // Append to layout (not sidebar) — sidebar has overflow:hidden which clips the handle
      var hHandle = $('<div class=\"sidebar-resize-handle\"></div>');
      $(layout).append(hHandle);

      var isHResizing = false, startX, startW;

      hHandle.on('mousedown', function(e) {
        isHResizing = true;
        startX = e.clientX;
        startW = $(sidebar).outerWidth();
        hHandle.addClass('dragging');
        $('body').css('user-select', 'none');
        e.preventDefault();
      });

      $(document).on('mousemove.hresize', function(e) {
        if (!isHResizing) return;
        var newW = Math.max(220, Math.min(700, startW + (e.clientX - startX)));
        // Drive grid columns directly — more reliable than CSS variable
        layout.style.gridTemplateColumns = newW + 'px 1fr';
        // Also override sidebar element width in case bslib has an inline style
        sidebar.style.width    = newW + 'px';
        sidebar.style.maxWidth = newW + 'px';
        // Keep CSS var in sync so handle position (calc) still tracks correctly
        layout.style.setProperty('--bslib-sidebar-width', newW + 'px');
      });

      $(document).on('mouseup.hresize', function() {
        if (!isHResizing) return;
        isHResizing = false;
        hHandle.removeClass('dragging');
        $('body').css('user-select', '');
      });
    }
  }, 200);

  // ---- 2. Vertical plot/table resize ----
  var vHandle   = document.getElementById('panel-resizer');
  var plotCard  = document.getElementById('plot_card');
  var tableCard = document.getElementById('table_card');

  if (vHandle && plotCard && tableCard) {
    var isVResizing = false, startY, startH;

    $(vHandle).on('mousedown', function(e) {
      if ($(plotCard).is(':hidden')) return;
      isVResizing = true;
      startY = e.clientY;
      startH = $(plotCard).outerHeight();
      $('body').css('user-select', 'none');
      e.preventDefault();
    });

    $(document).on('mousemove.vresize', function(e) {
      if (!isVResizing) return;
      var newH = Math.max(180, startH + (e.clientY - startY));
      $(plotCard).css('height', newH + 'px');
    });

    $(document).on('mouseup.vresize', function() {
      if (!isVResizing) return;
      isVResizing = false;
      $('body').css('user-select', '');
    });
  }

});
"))

# ============================================================
# UI
# ============================================================
ui <- page_sidebar(
  title  = "Omics GPT",
  theme  = app_theme,
  tags$style(HTML(app_css)),
  useShinyjs(),
  resize_js,

  # ---- Left sidebar ----
  sidebar = sidebar(
    width = 555,
    open  = TRUE,

    div(class = "upload-label", "Seurat RDS Object"),
    fileInput("rds_file", label = NULL, accept = ".rds", width = "100%"),

    hr(style = "border-color: #444; margin: 8px 0;"),

    chatUI("chat")
  ),

  # ---- Main panel ----
  div(
    class = "main-panels",

    # Plot card — hidden until first plot is generated
    shinyjs::hidden(
      card(
        id          = "plot_card",
        full_screen = TRUE,
        card_header(
          div(
            style = "display:flex; justify-content:space-between; align-items:center;",
            # Navigation controls: prev | "Title (i/n)" | next
            div(
              class = "plot-nav",
              actionButton("prev_plot", icon("chevron-left"),  class = "btn btn-sm btn-secondary"),
              div(class = "plot-nav-label", textOutput("plot_nav_label", inline = TRUE)),
              actionButton("next_plot", icon("chevron-right"), class = "btn btn-sm btn-secondary")
            ),
            # Download controls
            div(
              class = "download-bar",
              selectInput("image_height", NULL, choices = 1:20, selected = 8,  width = "70px"),
              selectInput("image_width",  NULL, choices = 1:20, selected = 12, width = "70px"),
              downloadButton("download_plot_pdf", "PDF", class = "btn-sm")
            )
          )
        ),
        card_body(padding = 0,
          plotOutput("scPlot", height = "100%")
        )
      )
    ),

    # Vertical drag handle — hidden until plot appears
    shinyjs::hidden(
      div(id = "panel-resizer")
    ),

    # Table card — always visible
    card(
      id          = "table_card",
      full_screen = TRUE,
      card_header(
        div(
          style = "display:flex; justify-content:space-between; align-items:center;",
          span("Data Table"),
          uiOutput("table_controls_ui")
        )
      ),
      card_body(
        div(style = "overflow-x:auto;", DT::DTOutput("csv_table"))
      )
    )
  )
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {

  # ---- Seurat object ----
  seu_obj <- reactive({
    req(input$rds_file)
    readRDS(input$rds_file$datapath)
  })

  # ---- State ----
  reactive_df_list <- reactiveVal(list())

  # Plot history: list of list(code, title); index: current position (1-based)
  plot_history <- reactiveVal(list())
  plot_index   <- reactiveVal(0L)

  # ============================================================
  # 1. Chat module
  # ============================================================
  chat_out <- chatServer("chat", seu_obj, api_key, org_id)

  # ============================================================
  # 2. Plot history management
  # ============================================================
  observeEvent(chat_out$plot_code(), {
    req(chat_out$plot_code())
    new_entry <- list(code = chat_out$plot_code(), title = chat_out$plot_title() %||% "")
    updated   <- append(plot_history(), list(new_entry))
    plot_history(updated)
    plot_index(length(updated))
    shinyjs::show("plot_card")
    shinyjs::show("panel-resizer")
  })

  observeEvent(input$prev_plot, {
    i <- plot_index()
    if (i > 1L) plot_index(i - 1L)
  })

  observeEvent(input$next_plot, {
    i <- plot_index()
    if (i < length(plot_history())) plot_index(i + 1L)
  })

  output$plot_nav_label <- renderText({
    hist <- plot_history()
    i    <- plot_index()
    if (length(hist) == 0 || i == 0L) return("")
    paste0(hist[[i]]$title, " (", i, "/", length(hist), ")")
  })

  output$scPlot <- renderPlot({
    hist <- plot_history()
    i    <- plot_index()
    req(length(hist) > 0, i > 0L, i <= length(hist), seu_obj())
    tryCatch({
      p <- eval_seu_gpt_query(seu_obj(), hist[[i]]$code)
      if (!is.null(p)) print(p)
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste("Plot error:\n", e$message),
                 size = 5, color = "red", hjust = 0.5) +
        theme_void()
    })
  })

  output$download_plot_pdf <- downloadHandler(
    filename = function() {
      hist <- plot_history()
      i    <- plot_index()
      if (length(hist) == 0 || i == 0L) return("plot.pdf")
      title <- hist[[i]]$title
      if (nchar(trimws(title)) == 0) return("plot.pdf")
      paste0(gsub("[^a-zA-Z0-9]+", "_", tolower(trimws(title))), ".pdf")
    },
    content = function(file) {
      hist <- plot_history()
      i    <- plot_index()
      req(length(hist) > 0, i > 0L)
      p <- eval_seu_gpt_query(seu_obj(), hist[[i]]$code)
      pdf(file, height = as.numeric(input$image_height), width = as.numeric(input$image_width))
      print(p)
      dev.off()
    }
  )

  # ============================================================
  # 3. Sheet evaluation
  # ============================================================
  observeEvent(chat_out$sheet_code(), {
    req(seu_obj())
    code     <- chat_out$sheet_code()
    eval_env <- list2env(list(seu_obj = seu_obj()), parent = globalenv())

    tryCatch(
      eval(parse(text = code), envir = eval_env),
      error = function(e) {
        showNotification(paste("Sheet code error:", e$message), type = "error", duration = 8)
      }
    )

    if (exists("generated_df_list", envir = eval_env)) {
      df_list <- get("generated_df_list", envir = eval_env)
      reactive_df_list(df_list)
      updateSelectInput(session, "selectDataTable",
                        choices  = c("MetaData", names(df_list)),
                        selected = names(df_list)[1])
    }
  })

  # ============================================================
  # 4. Table controls
  # ============================================================
  output$table_controls_ui <- renderUI({
    choices <- if (length(reactive_df_list()) > 0) c("MetaData", names(reactive_df_list())) else "MetaData"
    div(
      class = "download-bar",
      selectInput("selectDataTable", NULL, choices = choices, width = "160px"),
      downloadButton("download_csv", "CSV", class = "btn-sm")
    )
  })

  # ============================================================
  # 5. Render selected table
  # ============================================================
  output$csv_table <- DT::renderDT({
    req(seu_obj())
    DT::datatable(seu_obj()@meta.data, options = list(scrollX = TRUE, pageLength = 15))
  })

  observeEvent(input$selectDataTable, {
    selected <- input$selectDataTable
    if (selected == "MetaData") {
      output$csv_table <- DT::renderDT({
        DT::datatable(seu_obj()@meta.data, options = list(scrollX = TRUE, pageLength = 15))
      })
    } else {
      df_list <- reactive_df_list()
      if (!is.null(df_list[[selected]])) {
        output$csv_table <- DT::renderDT({
          DT::datatable(df_list[[selected]], options = list(scrollX = TRUE, pageLength = 15))
        })
      }
    }
  })

  # ============================================================
  # 6. CSV download
  # ============================================================
  output$download_csv <- downloadHandler(
    filename = function() paste0(input$selectDataTable, ".csv"),
    content  = function(file) {
      selected <- input$selectDataTable
      data_out <- if (selected == "MetaData") seu_obj()@meta.data else reactive_df_list()[[selected]]
      write.csv(data_out %||% data.frame(), file, row.names = TRUE)
    }
  )
}

# ============================================================
# Launch
# ============================================================
shinyApp(ui, server)
