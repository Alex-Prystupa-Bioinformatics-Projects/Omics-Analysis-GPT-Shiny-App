# ============================================================
# Bio Chat Module
# Conversational assistant for Seurat object exploration.
# Returns analysis suggestions — never R code.
# ============================================================

bioChatUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 9,
        textInput(ns("chat_input"), label = "Chat about your Seurat object", value = "")
      ),
      column(
        width = 3,
        div(
          style = "margin-top: 32px;",
          actionButton(ns("chat_send_btn"), "Send", icon = icon("paper-plane"))
        )
      )
    ),
    uiOutput(ns("chat_output"))
  )
}

bioChatServer <- function(id, seu_obj, api_key, org_id) {
  moduleServer(id, function(input, output, session) {

    chat_history <- reactiveVal(list(user_query = list(), query_response = list()))

    observeEvent(input$chat_send_btn, {
      req(input$chat_input)
      req(seu_obj())

      query <- input$chat_input

      # 1. Lock UI while waiting for response
      shinyjs::disable("chat_send_btn")
      updateTextInput(session, "chat_input", value = "")

      # 2. Call GPT with current history as memory
      gpt_response <- tryCatch({
        chatgpt_seu_query(
          prompt  = query,
          api_key = api_key,
          seu_obj = seu_obj(),
          org_id  = org_id,
          role    = "assistant",
          memory  = chat_history()
        )
      }, error = function(e) {
        showNotification(paste("API error:", e$message), type = "error", duration = 8)
        NULL
      })

      # 3. Re-enable button regardless of outcome
      shinyjs::enable("chat_send_btn")
      req(gpt_response)

      # 4. Append Q&A to history
      hist <- chat_history()
      chat_history(list(
        user_query     = append(hist$user_query, query),
        query_response = append(hist$query_response, gpt_response)
      ))
    })

    # Render scrollable chat history
    output$chat_output <- renderUI({
      history <- chat_history()

      if (length(history$user_query) == 0) {
        return(HTML("<strong>Chat:</strong><br><pre style='background:#f8f9fa;padding:10px;border-radius:5px;'># Ask questions about your analysis!</pre>"))
      }

      chat_blocks <- mapply(function(q, r) {
        glue::glue(
          "<div style='margin-bottom:15px;'>
             <strong>You:</strong><br>
             <div style='background:#e8f0fe;padding:10px;border-radius:5px;margin-bottom:5px;'>{q}</div>
             <strong>Assistant:</strong><br>
             <div style='background:#f8f9fa;padding:10px;border-radius:5px;'>{gsub('\n', '<br>', r)}</div>
           </div>"
        )
      }, history$user_query, history$query_response, SIMPLIFY = FALSE)

      HTML(glue::glue(
        "<strong>Chat:</strong><br>
         <div style='height:300px;overflow-y:auto;padding:10px;border:1px solid #ccc;border-radius:5px;background:#ffffff;'>
           {paste(chat_blocks, collapse = '')}
         </div>"
      ))
    })

  })
}
