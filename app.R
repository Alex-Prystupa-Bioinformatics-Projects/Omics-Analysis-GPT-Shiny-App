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
  /* ---- Sidebar content: flex column so chat module fills remaining space ---- */
  .bslib-sidebar-layout .sidebar-content {
    display: flex;
    flex-direction: column;
    height: 100%;
    box-sizing: border-box;
    overflow: hidden;
  }

  /* ---- Chat module: grows to fill leftover sidebar height ---- */
  .chat-module {
    flex: 1;
    display: flex;
    flex-direction: column;
    min-height: 0;
    overflow: hidden;
  }

  /* ---- Chat history: scrollable, expands to fill module ---- */
  .chat-container {
    flex: 1;
    min-height: 0;
    overflow-y: auto;
    padding: 10px 10px;
    display: flex;
    flex-direction: column;
    gap: 8px;
    border: 1px solid rgba(255,255,255,0.07);
    border-radius: 10px;
    background: rgba(0,0,0,0.12);
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

  /* ---- Input row: pinned to bottom of chat module ---- */
  .chat-input-row {
    flex-shrink: 0;
    padding-top: 8px;
    display: flex;
    align-items: stretch;
    gap: 6px;
  }

  .chat-input-row .shiny-input-container {
    flex: 1;
    margin-bottom: 0;
  }

  .chat-input-row textarea {
    font-size: 0.88rem;
    resize: none;
    border-radius: 10px;
    height: 100%;
  }

  /* ---- Mic button ---- */
  .mic-btn {
    flex-shrink: 0;
    width: 38px;
    height: auto;
    padding: 0;
    border-radius: 8px;
    background: #2e3338;
    border: 1px solid rgba(255,255,255,0.1);
    color: #aaa;
  }

  .mic-btn:hover { color: #fff; background: #3a3f44; }

  .mic-btn.recording {
    background: #c0392b;
    border-color: #e74c3c;
    color: #fff;
    animation: mic-pulse 1.2s infinite ease-in-out;
  }

  @keyframes mic-pulse {
    0%, 100% { box-shadow: 0 0 0 0 rgba(231, 76, 60, 0.4); }
    50%       { box-shadow: 0 0 0 6px rgba(231, 76, 60, 0); }
  }

  /* ---- Remove grey background from Shiny's renderUI wrapper inside chat ---- */
  .chat-container .shiny-html-output {
    background: transparent !important;
    border: none;
    padding: 0;
    margin: 0;
    display: contents;
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
    left: calc(var(--bslib-sidebar-width, 700px) - 4px);
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

  /* ---- Plot/table nav controls ---- */
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
  title  = "Single Cell Multiomics Analysis Dashboard",
  theme  = app_theme,
  tags$style(HTML(app_css)),
  useShinyjs(),
  resize_js,

  # ---- Left sidebar ----
  sidebar = sidebar(
    width = 700,
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
            # Navigation: prev | "Title (i/n)" | next
            div(
              class = "plot-nav",
              actionButton("prev_plot", icon("chevron-left"),  class = "btn btn-sm btn-secondary"),
              div(class = "plot-nav-label", textOutput("plot_nav_label", inline = TRUE)),
              actionButton("next_plot", icon("chevron-right"), class = "btn btn-sm btn-secondary")
            ),
            # PDF button — opens size modal before downloading
            actionButton("show_pdf_modal", tagList(icon("download"), " PDF"),
                         class = "btn btn-sm btn-secondary")
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

    # Table card — arrow navigation between MetaData and generated data frames
    card(
      id          = "table_card",
      full_screen = TRUE,
      card_header(
        div(
          style = "display:flex; justify-content:space-between; align-items:center;",
          div(
            class = "plot-nav",
            actionButton("prev_table", icon("chevron-left"),  class = "btn btn-sm btn-secondary"),
            div(class = "plot-nav-label", textOutput("table_nav_label", inline = TRUE)),
            actionButton("next_table", icon("chevron-right"), class = "btn btn-sm btn-secondary")
          ),
          downloadButton("download_csv", "CSV", class = "btn-sm")
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

  # Table navigation: choices always starts with MetaData, then any generated dfs
  table_index   <- reactiveVal(1L)
  table_choices <- reactive({
    dfs <- reactive_df_list()
    if (length(dfs) > 0) c("MetaData", names(dfs)) else "MetaData"
  })

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

  # PDF size modal — user picks dimensions before downloading
  observeEvent(input$show_pdf_modal, {
    showModal(modalDialog(
      title     = "Save Plot as PDF",
      size      = "s",
      easyClose = TRUE,
      fluidRow(
        column(6, numericInput("pdf_width",  "Width (in)",  value = 12, min = 1, max = 40, step = 1)),
        column(6, numericInput("pdf_height", "Height (in)", value = 8,  min = 1, max = 40, step = 1))
      ),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("download_plot_pdf", "Download PDF", class = "btn-primary btn-sm")
      )
    ))
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
      pdf(file,
          height = as.numeric(input$pdf_height %||% 8),
          width  = as.numeric(input$pdf_width  %||% 12))
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
      # Jump to first generated df (index 2, since MetaData is index 1)
      table_index(2L)
    }
  })

  # ============================================================
  # 4. Table navigation
  # ============================================================
  observeEvent(input$prev_table, {
    i <- table_index()
    if (i > 1L) table_index(i - 1L)
  })

  observeEvent(input$next_table, {
    i <- table_index()
    if (i < length(table_choices())) table_index(i + 1L)
  })

  output$table_nav_label <- renderText({
    choices <- table_choices()
    i       <- table_index()
    paste0(choices[i], " (", i, "/", length(choices), ")")
  })

  # ============================================================
  # 5. Render selected table
  # ============================================================
  output$csv_table <- DT::renderDT({
    choices  <- table_choices()
    i        <- table_index()
    selected <- choices[i]
    if (selected == "MetaData") {
      req(seu_obj())
      DT::datatable(seu_obj()@meta.data, options = list(scrollX = TRUE, pageLength = 15))
    } else {
      df <- reactive_df_list()[[selected]]
      req(df)
      DT::datatable(df, options = list(scrollX = TRUE, pageLength = 15))
    }
  })

  # ============================================================
  # 6. CSV download — filename derived from current table name, no spaces
  # ============================================================
  output$download_csv <- downloadHandler(
    filename = function() {
      choices <- table_choices()
      i       <- table_index()
      name    <- choices[i]
      paste0(gsub("[^a-zA-Z0-9]+", "_", tolower(trimws(name))), ".csv")
    },
    content = function(file) {
      choices  <- table_choices()
      i        <- table_index()
      selected <- choices[i]
      data_out <- if (selected == "MetaData") seu_obj()@meta.data else reactive_df_list()[[selected]]
      write.csv(data_out %||% data.frame(), file, row.names = TRUE)
    }
  )
}

# ============================================================
# Launch
# ============================================================
shinyApp(ui, server)
