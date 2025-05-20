library(shiny)
library(bs4Dash)
options(shiny.maxRequestSize = 5 * 1024^3)

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

source("gpt-functions.R")

# Keys
dotenv::load_dot_env(".keys")
api_key <- Sys.getenv("API_KEY")
org_id <- Sys.getenv("ORG_ID") 

# UI
ui <- bs4DashPage(
  title = "GPT Shiny Seurat Plotter",
  help = NULL,
  fullscreen = T,
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(
    collapsed = T,
    skin = "light",
    status = "primary",
    title = "GPT Plotter",
    bs4SidebarMenu(
      bs4SidebarMenuItem("SC-Genomics Dashboard", tabName = "custom_sc", icon = icon("flask"))
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(
        tabName = "custom_sc",
        fluidRow(
          column(
            width = 5,  # Stack these two cards vertically in this column
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
              collapsed = F,
              side = "left",
              tabPanel(
                title = "Bio Chat",
                fluidRow(
                  column(
                    width = 9,
                    textInput("chat_input", label = "Chat about your Seurat object", value = "")
                  ),
                  column(
                    width = 3,
                    actionButton("chat_send_btn", "Send", icon = icon("paper-plane")),
                    style = "margin-top: 32px;"
                  )
                ),
                uiOutput("chat_output")
              ),
              tabPanel(
                title = "Plot Code",
                fluidRow(
                  column(
                    width = 9,
                    textInput("plot_code_input", label = "Type instructions to generate plot", value = "")
                  ),
                  column(
                    width = 3,
                    actionButton("plot_code_send_btn", "Send", icon = icon("paper-plane")),
                    style = "margin-top: 32px;"
                  )
                ),
                uiOutput("plot_code_output")
              ),
              tabPanel(
                title = "Sheet Code",
                fluidRow(
                  column(
                    width = 9,
                    textInput("sheet_code_input", label = "Type instructions to generate table", value = "")
                  ),
                  column(
                    width = 3,
                    actionButton("sheet_code_send_btn", "Send", icon = icon("paper-plane")),
                    style = "margin-top: 32px;"
                  )
                ),
                uiOutput("sheet_code_output")
              )
            ),
            bs4Card(
              title = "Toggle Data Tables",
              width = 12,
              collapsible = TRUE,
              collapsed = F,
              uiOutput(outputId = "selectDataTable_UI")
            ),
            bs4Card(
              title = "Save Plot",
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              textInput("image_name", "Image Name", value = "Plot"),
              fluidRow(
                column(6, selectInput("image_height", "Image Height", choices = c(rep(1:20)), selected = 8)),
                column(6, selectInput("image_width", "Image Width", choices = c(rep(1:20)), selected = 12))
              ),
              downloadButton("download_plot_pdf", "Save Plot PDF")
            )
          ),
          column(
            width = 7,
            bs4Card(
              title = "Generated Spreadsheets",
              width = 12,
              collapsible = TRUE,
              div(
                style = "overflow-x: auto; overflow-y: auto; max-height: 400px;",  # control both width & height scroll
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

# Server
server <- function(input, output, session) {
  
  # Reactive Values
  ## Seu obj
  seu_obj <- reactive({
    req(input$rds_file)
    readRDS(input$rds_file$datapath)
  })
  
  ## List of Datatables
  reactive_df_list <- reactiveVal(list())
  
  # Bioinformatics Chat Bot
  # ----------------------------------------------------
  # Reactive values
  chat_text <- reactiveVal("")
  chat_history <- reactiveVal(list("user_query" = list(), 
                                   "query_response" = list()))
  
  # Observe send button click
  observeEvent(input$chat_send_btn, {
    req(input$chat_input)
    
    current_history <- chat_history()
    
    # For memory, exclude the current question and response
    # (i.e., only pass memory from previous Q/A pairs)
    memory_for_gpt <- current_history
    
    # Call GPT with memory only of previous Q/A pairs (no current question yet)
    if (!is.null(input$rds_file)) {
      gpt_response <- chatgpt_seu_query(
        prompt = input$chat_input,
        api_key = api_key,
        seu_obj = seu_obj(),
        org_id = org_id,
        role = "assistant",
        memory = memory_for_gpt
      )
    } else {
      gpt_response <- "<em>Please upload a Seurat RDS file.</em>"
    }
    
    # Now update chat history: append new question AND response *after* GPT call
    current_history$user_query <- append(current_history$user_query, input$chat_input)
    current_history$query_response <- append(current_history$query_response, gpt_response)
    
    # Save updated history
    chat_history(current_history)
    
    # Update chat text reactive
    chat_text(input$chat_input)
  })
  
  
  # Render the chat output
  output$chat_output <- renderUI({
    history <- chat_history()
    if (length(history$user_query) == 0) {
      return(HTML("<strong>Chat:</strong><br><pre style='background:#f8f9fa;padding:10px;border-radius:5px;'># Ask questions about your analysis!</pre>"))
    }
    
    chat_blocks <- mapply(function(q, r) {
      glue::glue(
        "<div style='margin-bottom: 15px;'>
         <strong>You:</strong><br>
         <div style='background:#e8f0fe;padding:10px;border-radius:5px;margin-bottom:5px;'>{q}</div>
         <strong>Assistant:</strong><br>
         <div style='background:#f8f9fa;padding:10px;border-radius:5px;'>{gsub('\n', '<br>', r)}</div>
       </div>"
      )
    }, history$user_query, history$query_response, SIMPLIFY = FALSE)
    
    HTML(glue::glue(
      "<strong>Chat:</strong><br>
     <div style='height:300px; overflow-y:auto; padding:10px; border:1px solid #ccc; border-radius:5px; background:#ffffff;'>
       {paste(chat_blocks, collapse = '')}
     </div>"
    ))
  })
  
  # ----------------------------------------------------
  
  # Plot Code Chat Bot
  # ----------------------------------------------------
  # Reactive values
  code_text <- reactiveVal("")
  query_response_reactive <- reactiveVal(NULL)
  
  # Store full code chat history
  code_chat_history <- reactiveVal(list(user_query = list(), query_response = list()))
  
  # Button triggers everything
  observeEvent(input$plot_code_send_btn, {
    req(input$plot_code_input, input$rds_file)
    
    current_code_chat_history <- code_chat_history()
    
    # Update the query
    query <- input$plot_code_input
    code_text(query)
    
    # Get GPT response
    response <- chatgpt_seu_query(
      prompt = query,
      api_key = api_key,
      seu_obj = seu_obj(),
      org_id = org_id,
      role = "coder_plot",
      memory = current_code_chat_history
    )
    
    # Save latest response for plotting
    query_response_reactive(response)
    
    # Append to history
    hist <- code_chat_history()
    updated <- list(
      user_query = append(hist$user_query, query),
      query_response = append(hist$query_response, response)
    )
    code_chat_history(updated)
  })
  
  # Scrollable code chat history
  output$plot_code_output <- renderUI({
    history <- code_chat_history()
    
    if (length(history$user_query) == 0) {
      return(HTML("<strong>Code:</strong><br><pre style='background:#f8f9fa;padding:10px;border-radius:5px;'># Waiting for your input...</pre>"))
    }
    
    chat_blocks <- mapply(function(q, r) {
      glue::glue(
        "<div style='margin-bottom: 15px;'>
         <strong>You:</strong><br>
         <div style='background:#e8f0fe;padding:10px;border-radius:5px;margin-bottom:5px;'>{q}</div>
         <strong>Code:</strong><br>
         <div style='background:#f8f9fa;padding:10px;border-radius:5px;white-space:pre-wrap;border:1px solid #ccc;'>{gsub('\n', '<br>', r)}</div>
       </div>"
      )
    }, history$user_query, history$query_response, SIMPLIFY = FALSE)
    
    HTML(glue::glue(
      "<strong>Code Chat:</strong><br>
     <div style='height:300px; overflow-y:auto; background:#ffffff; padding:10px; border-radius:5px; border:1px solid #ddd;'>
       {paste(chat_blocks, collapse = '')}
     </div>"
    ))
  })
  
  # Download latest plot
  output$download_plot_pdf <- downloadHandler(
    filename = function() {
      glue("{input$image_name}.pdf")
    },
    content = function(file) {
      plot <- eval_seu_gpt_query(seu_obj(), query_response_reactive())
      pdf(file, height = as.numeric(input$image_height), width = as.numeric(input$image_width))
      print(plot)
      dev.off()
    }
  )

  # Use the same query_response_reactive for the plot
  output$scPlot <- renderPlot({
    req(query_response_reactive())  # Ensure query_response is available

    if (!is.null(input$rds_file)) {
      eval_seu_gpt_query(seu_obj(), query_response_reactive()) # Get current state of seu obj & query
    }
  })

  # Download plot as pdf
  output$download_plot_pdf <- downloadHandler(
    filename = function() {glue("{input$image_name}.pdf")},
    content = function(file) {
      plot <- eval_seu_gpt_query(seu_obj(), query_response_reactive())
      pdf(file, height = as.numeric(input$image_height), width = as.numeric(input$image_width))
      print(plot)
      dev.off()
    }
  )
  
  # Sheet Code Chat Bot
  # ----------------------------------------------------
  
  # Reactive values for sheet chatbot
  sheet_code_text <- reactiveVal("")
  sheet_query_response_reactive <- reactiveVal(NULL)
  
  # Store full chat history (user + GPT)
  sheet_code_chat_history <- reactiveVal(list(user_query = list(), query_response = list()))
  
  # When Send button is clicked
  observeEvent(input$sheet_code_send_btn, {
    req(input$sheet_code_input, input$rds_file)
    
    current_sheet_code_chat_history <- sheet_code_chat_history()
    
    query <- input$sheet_code_input
    sheet_code_text(query)
    
    # Call GPT once
    response <- chatgpt_seu_query(
      prompt = query,
      api_key = api_key,
      seu_obj = seu_obj(),
      org_id = org_id,
      role = "coder_sheet",
      memory = current_sheet_code_chat_history
    )
    
    # Save latest response for use
    sheet_query_response_reactive(response)
    
    # Add to chat history
    hist <- sheet_code_chat_history()
    updated <- list(
      user_query = append(hist$user_query, query),
      query_response = append(hist$query_response, response)
    )
    sheet_code_chat_history(updated)
  })
  
  # Render sheet code chat UI
  output$sheet_code_output <- renderUI({
    history <- sheet_code_chat_history()
    
    if (length(history$user_query) == 0) {
      return(HTML("<strong>Code:</strong><br><pre style='background:#f8f9fa;padding:10px;border-radius:5px;'># Waiting for your input...</pre>"))
    }
    
    chat_blocks <- mapply(function(q, r) {
      glue::glue(
        "<div style='margin-bottom: 15px;'>
         <strong>You:</strong><br>
         <div style='background:#e8f0fe;padding:10px;border-radius:5px;margin-bottom:5px;'>{q}</div>
         <strong>Code:</strong><br>
         <div style='background:#f8f9fa;padding:10px;border-radius:5px;white-space:pre-wrap;border:1px solid #ccc;'>{gsub('\n', '<br>', r)}</div>
       </div>"
      )
    }, history$user_query, history$query_response, SIMPLIFY = FALSE)
    
    HTML(glue::glue(
      "<strong>Code Chat:</strong><br>
     <div style='height:300px; overflow-y:auto; background:#ffffff; padding:10px; border-radius:5px; border:1px solid #ddd;'>
       {paste(chat_blocks, collapse = '')}
     </div>"
    ))
  })
  
  # Select UI (Metadata and Generated)
  observeEvent(seu_obj(), {
    output$selectDataTable_UI <- renderUI({
      tagList(
        selectInput(
          inputId = "selectDataTable",
          label = "Choose Table:",
          choices = c("MetaData"),
          selected = "MetaData"
        ),
        downloadButton("download_csv", "Save CSV")
      )
    })
    
    output$csv_table <- DT::renderDT({
      DT::datatable(
        seu_obj()@meta.data,
        options = list(scrollX = TRUE)
      )
    })
  })
  
  # Evaluate response and update table choices
  observeEvent(sheet_query_response_reactive(), {
    req(input$selectDataTable)
    req(seu_obj())
    
    code <- sheet_query_response_reactive()
    seu_obj_resolved <- seu_obj()
    eval_env <- list2env(list(seu_obj = seu_obj_resolved), parent = globalenv())
    
    eval(parse(text = code), envir = eval_env)
    
    if (exists("generated_df_list", envir = eval_env)) {
      df_list <- get("generated_df_list", envir = eval_env)
      reactive_df_list(df_list)
      
      updateSelectInput(
        session = session,
        inputId = "selectDataTable",
        choices = c("MetaData", names(df_list)),
        selected = names(df_list)[1]
      )
    } else {
      reactive_df_list(NULL)
      updateSelectInput(
        session = session,
        inputId = "selectDataTable",
        choices = c("MetaData"),
        selected = "MetaData"
      )
    }
  })
  
  # Show selected table
  observeEvent(input$selectDataTable, {
    selected <- input$selectDataTable
    
    if (selected == "MetaData") {
      output$csv_table <- DT::renderDT({
        DT::datatable(
          seu_obj()@meta.data,
          options = list(scrollX = TRUE)
        )
      })
    } else {
      df_list <- reactive_df_list()
      
      if (!is.null(df_list) && !is.null(df_list[[selected]])) {
        output$csv_table <- DT::renderDT({
          DT::datatable(
            df_list[[selected]],
            options = list(scrollX = TRUE)
          )
        })
      } else {
        output$csv_table <- DT::renderDT({
          DT::datatable(data.frame())
        })
      }
    }
  })
  
  
  # Save CSV
  # Download plot as pdf
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0(input$selectDataTable, "_table.csv")
    },
    content = function(file) {
      selected <- input$selectDataTable
      
      if (selected == "MetaData") {
        data_to_save <- seu_obj()@meta.data
      } else {
        df_list <- reactive_df_list()
        if (!is.null(df_list) && !is.null(df_list[[selected]])) {
          data_to_save <- df_list[[selected]]
        } else {
          data_to_save <- data.frame()  # empty fallback
        }
      }
      
      write.csv(data_to_save, file, row.names = TRUE)
    }
  )
  
  # ----------------------------------------------------
  
}

# Run App
shinyApp(ui, server)

