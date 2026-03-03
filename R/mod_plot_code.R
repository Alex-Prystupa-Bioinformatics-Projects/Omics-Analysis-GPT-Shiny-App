# ============================================================
# Plot Code Module
# GPT generates R plotting code; app evaluates and renders it.
# Returns query_response (reactive) for use by main server.
# ============================================================

plotCodeUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 9,
        textInput(ns("plot_input"), label = "Type instructions to generate plot", value = "")
      ),
      column(
        width = 3,
        div(
          style = "margin-top: 32px;",
          actionButton(ns("plot_send_btn"), "Send", icon = icon("paper-plane"))
        )
      )
    ),
    uiOutput(ns("plot_code_output"))
  )
}

plotCodeServer <- function(id, seu_obj, api_key, org_id) {
  moduleServer(id, function(input, output, session) {

    code_chat_history  <- reactiveVal(list(user_query = list(), query_response = list()))
    query_response     <- reactiveVal(NULL)

    observeEvent(input$plot_send_btn, {
      req(input$plot_input)
      req(seu_obj())

      query <- input$plot_input

      # 1. Lock UI while waiting for response
      shinyjs::disable("plot_send_btn")
      updateTextInput(session, "plot_input", value = "")

      # 2. Call GPT with current code history as memory
      response <- tryCatch({
        chatgpt_seu_query(
          prompt  = query,
          api_key = api_key,
          seu_obj = seu_obj(),
          org_id  = org_id,
          role    = "coder_plot",
          memory  = code_chat_history()
        )
      }, error = function(e) {
        showNotification(paste("API error:", e$message), type = "error", duration = 8)
        NULL
      })

      # 3. Re-enable button regardless of outcome
      shinyjs::enable("plot_send_btn")
      req(response)

      # 4. Save response and append to history
      query_response(response)

      hist <- code_chat_history()
      code_chat_history(list(
        user_query     = append(hist$user_query, query),
        query_response = append(hist$query_response, response)
      ))
    })

    # Render scrollable code chat history
    output$plot_code_output <- renderUI({
      history <- code_chat_history()

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

    # Return latest GPT code response for plot rendering in main server
    return(query_response)
  })
}
