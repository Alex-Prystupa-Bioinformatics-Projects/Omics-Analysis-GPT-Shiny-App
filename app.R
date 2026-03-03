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

  .user-row   { justify-content: flex-end; }
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
  .code-details {
    margin-top: 8px;
  }

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
  .typing-bubble {
    padding: 12px 16px;
  }

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
    display: flex;
    flex-direction: column;
    gap: 6px;
  }

  .chat-input-row textarea {
    font-size: 0.88rem;
    resize: none;
    border-radius: 10px;
  }

  .chat-send-btn {
    align-self: flex-end;
    border-radius: 20px;
    padding: 5px 18px;
  }

  /* ---- Upload area ---- */
  .upload-label {
    font-size: 0.8rem;
    color: #aaa;
    margin-bottom: 2px;
  }

  /* ---- Output cards ---- */
  .output-card {
    border-radius: 12px;
  }

  /* ---- Download bar ---- */
  .download-bar {
    display: flex;
    gap: 10px;
    flex-wrap: wrap;
    align-items: center;
    padding: 8px 0 4px;
  }

  .download-bar label {
    font-size: 0.82rem;
    color: #aaa;
  }
"

# ============================================================
# UI
# ============================================================
ui <- page_sidebar(
  title  = "Omics GPT",
  theme  = app_theme,
  tags$style(HTML(app_css)),
  useShinyjs(),

  # ---- Left sidebar ----
  sidebar = sidebar(
    width = 370,
    open  = TRUE,

    # RDS uploader
    div(class = "upload-label", "Seurat RDS Object"),
    fileInput(
      "rds_file",
      label  = NULL,
      accept = ".rds",
      width  = "100%"
    ),

    hr(style = "border-color: #444; margin: 8px 0;"),

    # Chat module
    chatUI("chat")
  ),

  # ---- Main panel ----

  # Plot output (hidden until a plot is generated)
  uiOutput("plot_section_ui"),

  # Table output
  card(
    class      = "output-card",
    full_screen = TRUE,
    card_header(
      div(
        style = "display:flex; justify-content:space-between; align-items:center;",
        span("Data Table"),
        uiOutput("table_controls_ui")
      )
    ),
    card_body(
      div(
        style = "overflow-x:auto;",
        DT::DTOutput("csv_table")
      )
    )
  )
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {

  # ---- Seurat object reactive ----
  seu_obj <- reactive({
    req(input$rds_file)
    readRDS(input$rds_file$datapath)
  })

  # ---- Generated data frames ----
  reactive_df_list <- reactiveVal(list())

  # ---- Latest code strings (for download re-eval) ----
  latest_plot_code  <- reactiveVal(NULL)
  latest_sheet_code <- reactiveVal(NULL)

  # ============================================================
  # 1. Chat module
  # ============================================================
  chat_out <- chatServer("chat", seu_obj, api_key, org_id)

  # ============================================================
  # 2. Plot rendering
  # ============================================================
  observeEvent(chat_out$plot_code(), {
    latest_plot_code(chat_out$plot_code())
  })

  output$plot_section_ui <- renderUI({
    req(latest_plot_code())
    card(
      class      = "output-card",
      full_screen = TRUE,
      card_header(
        div(
          style = "display:flex; justify-content:space-between; align-items:center;",
          span("Generated Plot"),
          div(
            class = "download-bar",
            column(4, selectInput("image_height", "H", choices = 1:20, selected = 8, width = "80px")),
            column(4, selectInput("image_width",  "W", choices = 1:20, selected = 12, width = "80px")),
            column(4, downloadButton("download_plot_pdf", "PDF", class = "btn-sm"))
          )
        )
      ),
      card_body(
        plotOutput("scPlot", height = "450px")
      )
    )
  })

  output$scPlot <- renderPlot({
    req(latest_plot_code(), seu_obj())
    tryCatch(
      eval_seu_gpt_query(seu_obj(), latest_plot_code()),
      error = function(e) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste("Plot error:\n", e$message),
                   size = 5, color = "red", hjust = 0.5) +
          theme_void()
      }
    )
  })

  output$download_plot_pdf <- downloadHandler(
    filename = function() "plot.pdf",
    content  = function(file) {
      p <- eval_seu_gpt_query(seu_obj(), latest_plot_code())
      pdf(file,
          height = as.numeric(input$image_height),
          width  = as.numeric(input$image_width))
      print(p)
      dev.off()
    }
  )

  # ============================================================
  # 3. Sheet evaluation
  # ============================================================
  observeEvent(chat_out$sheet_code(), {
    req(seu_obj())
    latest_sheet_code(chat_out$sheet_code())
    code      <- chat_out$sheet_code()
    eval_env  <- list2env(list(seu_obj = seu_obj()), parent = globalenv())

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
  # 4. Table controls UI (selector + download)
  # ============================================================
  output$table_controls_ui <- renderUI({
    choices <- if (length(reactive_df_list()) > 0) {
      c("MetaData", names(reactive_df_list()))
    } else {
      "MetaData"
    }
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
      data_out <- if (selected == "MetaData") {
        seu_obj()@meta.data
      } else {
        reactive_df_list()[[selected]] %||% data.frame()
      }
      write.csv(data_out, file, row.names = TRUE)
    }
  )
}

# ============================================================
# Launch
# ============================================================
shinyApp(ui, server)
