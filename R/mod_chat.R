# ============================================================
# Unified Chat Module
# Handles all interaction types: conversational, plot, sheet.
# Returns list(plot_code, sheet_code) reactives to app.R.
# ============================================================

# Auto-scroll JS — injected once in UI
chat_scroll_js <- tags$script(HTML("
  Shiny.addCustomMessageHandler('scroll_chat', function(id) {
    var el = document.getElementById(id);
    if (el) el.scrollTop = el.scrollHeight;
  });
"))

chatUI <- function(id) {
  ns <- NS(id)
  tagList(
    chat_scroll_js,

    # Chat history container
    div(
      id    = ns("chat_container"),
      class = "chat-container",
      uiOutput(ns("chat_history_ui"))
    ),

    # Typing indicator (shown while waiting for response)
    uiOutput(ns("typing_ui")),

    # Input row
    div(
      class = "chat-input-row",
      textAreaInput(
        ns("chat_input"),
        label       = NULL,
        placeholder = "Ask about your data...",
        rows        = 2,
        width       = "100%"
      ),
      actionButton(
        ns("send_btn"),
        label = tagList(icon("paper-plane"), " Send"),
        class = "btn btn-primary btn-sm chat-send-btn"
      )
    )
  )
}

chatServer <- function(id, seu_obj, api_key, org_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- State ----
    chat_history <- reactiveVal(list(user_query = list(), query_response = list()))
    is_thinking  <- reactiveVal(FALSE)
    plot_code    <- reactiveVal(NULL)
    sheet_code   <- reactiveVal(NULL)

    # ---- Send handler ----
    observeEvent(input$send_btn, {
      req(input$chat_input, seu_obj())
      query <- trimws(input$chat_input)
      req(nchar(query) > 0)

      # 1. Lock UI and show thinking state
      shinyjs::disable(ns("send_btn"))
      updateTextAreaInput(session, "chat_input", value = "")
      is_thinking(TRUE)

      # 2. Append user message to history immediately
      hist <- chat_history()
      chat_history(list(
        user_query     = append(hist$user_query, query),
        query_response = hist$query_response
      ))

      # 3. Scroll to bottom after user message
      session$sendCustomMessage("scroll_chat", ns("chat_container"))

      # 4. Call GPT
      gpt_result <- tryCatch({
        chatgpt_seu_query(
          prompt  = query,
          api_key = api_key,
          org_id  = org_id,
          seu_obj = seu_obj(),
          memory  = hist
        )
      }, error = function(e) {
        showNotification(paste("API error:", e$message), type = "error", duration = 8)
        NULL
      })

      # 5. Re-enable UI
      is_thinking(FALSE)
      shinyjs::enable(ns("send_btn"))
      req(gpt_result)

      # 6. Route by response type
      msg  <- gpt_result$message %||% ""
      type <- gpt_result$type    %||% "chat"
      code <- gpt_result$code    %||% ""

      if (type == "plot" && nchar(trimws(code)) > 0) {
        plot_code(code)
      } else if (type == "sheet" && nchar(trimws(code)) > 0) {
        sheet_code(code)
      }

      # 7. Store assistant response (message + code + type) as a list
      response_entry <- list(type = type, message = msg, code = code)

      hist2 <- chat_history()
      chat_history(list(
        user_query     = hist2$user_query,
        query_response = append(hist2$query_response, list(response_entry))
      ))

      # 8. Scroll to bottom after response
      session$sendCustomMessage("scroll_chat", ns("chat_container"))
    })

    # ---- Render chat history ----
    output$chat_history_ui <- renderUI({
      history <- chat_history()
      n_queries   <- length(history$user_query)
      n_responses <- length(history$query_response)

      if (n_queries == 0) {
        return(div(class = "chat-empty", "Upload a Seurat object and start asking questions."))
      }

      bubbles <- lapply(seq_len(n_queries), function(i) {
        user_bubble <- div(
          class = "chat-bubble-row user-row",
          div(class = "chat-bubble user-bubble", history$user_query[[i]])
        )

        if (i > n_responses) {
          return(user_bubble)
        }

        resp <- history$query_response[[i]]
        type <- resp$type    %||% "chat"
        msg  <- resp$message %||% ""
        code <- resp$code    %||% ""

        code_block <- if (type %in% c("plot", "sheet") && nchar(trimws(code)) > 0) {
          tags$details(
            class = "code-details",
            tags$summary(
              class = "code-summary",
              icon(if (type == "plot") "chart-bar" else "table"), " View Code"
            ),
            tags$pre(class = "code-pre", code)
          )
        } else NULL

        assistant_bubble <- div(
          class = "chat-bubble-row assistant-row",
          div(
            class = "chat-bubble assistant-bubble",
            p(msg),
            code_block
          )
        )

        tagList(user_bubble, assistant_bubble)
      })

      do.call(tagList, bubbles)
    })

    # ---- Typing indicator ----
    output$typing_ui <- renderUI({
      if (!is_thinking()) return(NULL)
      div(
        class = "chat-bubble-row assistant-row",
        div(
          class = "chat-bubble assistant-bubble typing-bubble",
          div(
            class = "typing-indicator",
            span(), span(), span()
          )
        )
      )
    })

    # ---- Return reactives to app.R ----
    return(list(plot_code = plot_code, sheet_code = sheet_code))
  })
}

# Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !identical(a, "")) a else b
