# ============================================================
# Libraries
# ============================================================
library(shiny)
library(bs4Dash)
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

options(shiny.maxRequestSize = 5 * 1024^3)

# ============================================================
# Source modules and functions
# ============================================================
source("R/gpt_functions.R")
source("R/mod_bio_chat.R")
source("R/mod_plot_code.R")
source("R/mod_sheet_code.R")

# ============================================================
# API Keys
# ============================================================
dotenv::load_dot_env(".keys")
api_key <- Sys.getenv("API_KEY")
org_id  <- Sys.getenv("ORG_ID")

# ============================================================
# UI
# ============================================================
ui <- bs4DashPage(
  title = "GPT Shiny Seurat Plotter",
  help = NULL,
  fullscreen = TRUE,
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(
    collapsed = TRUE,
    skin = "light",
    status = "primary",
    title = "GPT Plotter",
    bs4SidebarMenu(
      bs4SidebarMenuItem("SC-Genomics Dashboard", tabName = "custom_sc", icon = icon("flask"))
    )
  ),
  body = bs4DashBody(
    useShinyjs(),
    bs4TabItems(
      bs4TabItem(
        tabName = "custom_sc",
        fluidRow(

          # ---- Left column: uploader, chatbots, table toggle, save ----
          column(
            width = 5,
            bs4Card(
              title = "RDS Uploader",
              width = 12,
              collapsible = TRUE,
              fileInput("rds_file", label = h5("Upload Seurat RDS")),
              style = "padding-bottom: 0px;"
            ),
            bs4TabCard(
              title = NULL,
              id = "chat_tabs",
              width = 12,
              collapsible = TRUE,
              collapsed = FALSE,
              side = "left",
              tabPanel(title = "Bio Chat",   bioChatUI("bio_chat")),
              tabPanel(title = "Plot Code",  plotCodeUI("plot_code")),
              tabPanel(title = "Sheet Code", sheetCodeUI("sheet_code"))
            ),
            bs4Card(
              title = "Toggle Data Tables",
              width = 12,
              collapsible = TRUE,
              collapsed = FALSE,
              uiOutput("selectDataTable_UI")
            ),
            bs4Card(
              title = "Save Plot",
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              textInput("image_name", "Image Name", value = "Plot"),
              fluidRow(
                column(6, selectInput("image_height", "Image Height", choices = 1:20, selected = 8)),
                column(6, selectInput("image_width",  "Image Width",  choices = 1:20, selected = 12))
              ),
              downloadButton("download_plot_pdf", "Save Plot PDF")
            )
          ),

          # ---- Right column: table and plot outputs ----
          column(
            width = 7,
            bs4Card(
              title = "Generated Spreadsheets",
              width = 12,
              collapsible = TRUE,
              div(
                style = "overflow-x:auto; overflow-y:auto; max-height:400px;",
                DT::DTOutput("csv_table")
              )
            ),
            bs4Card(
              title = "Generated Plots",
              width = 12,
              collapsible = TRUE,
              plotOutput("scPlot", height = "400px")
            )
          )
        )
      )
    )
  ),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter()
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

  # ---- Reactive list for generated data frames ----
  reactive_df_list <- reactiveVal(list())

  # ============================================================
  # 1. Call chatbot modules
  # ============================================================
  bioChatServer("bio_chat",    seu_obj, api_key, org_id)
  plot_response  <- plotCodeServer("plot_code",   seu_obj, api_key, org_id)
  sheet_response <- sheetCodeServer("sheet_code", seu_obj, api_key, org_id)

  # ============================================================
  # 2. Render plot from plot module response
  # ============================================================
  output$scPlot <- renderPlot({
    req(plot_response())
    req(seu_obj())

    tryCatch({
      eval_seu_gpt_query(seu_obj(), plot_response())
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = paste("Plot error:\n", e$message),
                 size = 5, color = "red", hjust = 0.5) +
        theme_void()
    })
  })

  # ============================================================
  # 3. Download plot as PDF
  # ============================================================
  output$download_plot_pdf <- downloadHandler(
    filename = function() { glue("{input$image_name}.pdf") },
    content = function(file) {
      plot <- eval_seu_gpt_query(seu_obj(), plot_response())
      pdf(file, height = as.numeric(input$image_height), width = as.numeric(input$image_width))
      print(plot)
      dev.off()
    }
  )

  # ============================================================
  # 4. Data table: initialize on RDS upload
  # ============================================================
  observeEvent(seu_obj(), {
    output$selectDataTable_UI <- renderUI({
      tagList(
        selectInput("selectDataTable", "Choose Table:", choices = "MetaData", selected = "MetaData"),
        downloadButton("download_csv", "Save CSV")
      )
    })

    output$csv_table <- DT::renderDT({
      DT::datatable(seu_obj()@meta.data, options = list(scrollX = TRUE))
    })
  })

  # ============================================================
  # 5. Evaluate sheet response — update table choices with generated DFs
  # ============================================================
  observeEvent(sheet_response(), {
    req(seu_obj())

    code          <- sheet_response()
    seu_obj_local <- seu_obj()
    eval_env      <- list2env(list(seu_obj = seu_obj_local), parent = globalenv())

    tryCatch({
      eval(parse(text = code), envir = eval_env)
    }, error = function(e) {
      showNotification(paste("Sheet code error:", e$message), type = "error", duration = 8)
    })

    if (exists("generated_df_list", envir = eval_env)) {
      df_list <- get("generated_df_list", envir = eval_env)
      reactive_df_list(df_list)
      updateSelectInput(session, "selectDataTable",
                        choices  = c("MetaData", names(df_list)),
                        selected = names(df_list)[1])
    } else {
      reactive_df_list(NULL)
      updateSelectInput(session, "selectDataTable",
                        choices = "MetaData", selected = "MetaData")
    }
  })

  # ============================================================
  # 6. Render selected table
  # ============================================================
  observeEvent(input$selectDataTable, {
    selected <- input$selectDataTable

    if (selected == "MetaData") {
      output$csv_table <- DT::renderDT({
        DT::datatable(seu_obj()@meta.data, options = list(scrollX = TRUE))
      })
    } else {
      df_list <- reactive_df_list()
      if (!is.null(df_list) && !is.null(df_list[[selected]])) {
        output$csv_table <- DT::renderDT({
          DT::datatable(df_list[[selected]], options = list(scrollX = TRUE))
        })
      } else {
        output$csv_table <- DT::renderDT({ DT::datatable(data.frame()) })
      }
    }
  })

  # ============================================================
  # 7. Download selected table as CSV
  # ============================================================
  output$download_csv <- downloadHandler(
    filename = function() { paste0(input$selectDataTable, "_table.csv") },
    content = function(file) {
      selected <- input$selectDataTable

      if (selected == "MetaData") {
        data_to_save <- seu_obj()@meta.data
      } else {
        df_list <- reactive_df_list()
        if (!is.null(df_list) && !is.null(df_list[[selected]])) {
          data_to_save <- df_list[[selected]]
        } else {
          data_to_save <- data.frame()
        }
      }

      write.csv(data_to_save, file, row.names = TRUE)
    }
  )

}

# ============================================================
# Launch
# ============================================================
shinyApp(ui, server)
