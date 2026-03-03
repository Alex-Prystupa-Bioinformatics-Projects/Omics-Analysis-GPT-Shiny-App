# ============================================================
# Sheet Code Module
# GPT generates R code that produces a named list of data frames.
# Returns sheet_response (reactive) for use by main server.
# ============================================================

sheetCodeUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 9,
        textInput(ns("sheet_input"), label = "Type instructions to generate table", value = "")
      ),
      column(
        width = 3,
        div(
          style = "margin-top: 32px;",
          actionButton(ns("sheet_send_btn"), "Send", icon = icon("paper-plane"))
        )
      )
    ),
    uiOutput(ns("sheet_code_output"))
  )
}

sheetCodeServer <- function(id, seu_obj, api_key, org_id) {
  moduleServer(id, function(input, output, session) {

    sheet_chat_history <- reactiveVal(list(user_query = list(), query_response = list()))
    sheet_response     <- reactiveVal(NULL)

    observeEvent(input$sheet_send_btn, {
      req(input$sheet_input)
      req(seu_obj())

      query <- input$sheet_input

      # 1. Lock UI while waiting for response
      shinyjs::disable("sheet_send_btn")
      updateTextInput(session, "sheet_input", value = "")

      # 2. Call GPT with current sheet history as memory
      response <- tryCatch({
        chatgpt_seu_query(
          prompt  = query,
          api_key = api_key,
          seu_obj = seu_obj(),
          org_id  = org_id,
          role    = "coder_sheet",
          memory  = sheet_chat_history()
        )
      }, error = function(e) {
        showNotification(paste("API error:", e$message), type = "error", duration = 8)
        NULL
      })

      # 3. Re-enable button regardless of outcome
      shinyjs::enable("sheet_send_btn")
      req(response)

      # 4. Save response and append to history
      sheet_response(response)

      hist <- sheet_chat_history()
      sheet_chat_history(list(
        user_query     = append(hist$user_query, query),
        query_response = append(hist$query_response, response)
      ))
    })

    # Render scrollable sheet code chat history
    output$sheet_code_output <- renderUI({
      history <- sheet_chat_history()

      if (length(history$user_query) == 0) {
        return(HTML("<strong>Code:</strong><br><pre style='background:#f8f9fa;padding:10px;border-radius:5px;'># Waiting for your input...</pre>"))
      }

      chat_blocks <- mapply(function(q, r) {
        glue::glue(
          "<div style='margin-bottom:15px;'>
             <strong>You:</strong><br>
             <div style='background:#e8f0fe;padding:10px;border-radius:5px;margin-bottom:5px;'>{q}</div>
             <strong>Code:</strong><br>
             <div style='background:#f8f9fa;padding:10px;border-radius:5px;white-space:pre-wrap;border:1px solid #ccc;'>{gsub('\n', '<br>', r)}</div>
           </div>"
        )
      }, history$user_query, history$query_response, SIMPLIFY = FALSE)

      HTML(glue::glue(
        "<strong>Code Chat:</strong><br>
         <div style='height:300px;overflow-y:auto;background:#ffffff;padding:10px;border-radius:5px;border:1px solid #ddd;'>
           {paste(chat_blocks, collapse = '')}
         </div>"
      ))
    })

    # Return latest GPT sheet response for table rendering in main server
    return(sheet_response)
  })
}
