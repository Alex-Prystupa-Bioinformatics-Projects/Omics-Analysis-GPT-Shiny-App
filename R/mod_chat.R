# ============================================================
# Unified Chat Module
# Handles all interaction types: conversational, plot, sheet.
# Returns list(plot_code, sheet_code, plot_title) reactives to app.R.
# ============================================================

# Auto-scroll JS — injected once in UI
chat_scroll_js <- tags$script(HTML("
  Shiny.addCustomMessageHandler('scroll_chat', function(id) {
    var el = document.getElementById(id);
    if (el) el.scrollTop = el.scrollHeight;
  });
"));

chatUI <- function(id) {
  ns <- NS(id)

  # Enter-to-send: capture text, clear immediately, fire query as trigger value
  enter_to_send_js <- tags$script(HTML(sprintf("
    $(document).on('keydown', '#%s', function(e) {
      if (e.key === 'Enter' && !e.shiftKey) {
        e.preventDefault();
        var query = $(this).val().trim();
        if (query.length === 0) return;
        $(this).val('');
        Shiny.setInputValue('%s', query, {priority: 'event'});
      }
    });
  ", ns("chat_input"), ns("send_trigger"))))

  tagList(
    chat_scroll_js,
    enter_to_send_js,

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
        placeholder = "Ask about your data... (Enter to send, Shift+Enter for newline)",
        rows        = 2,
        width       = "100%"
      )
    )
  )
}

chatServer <- function(id, seu_obj, api_key, org_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- State ----
    # api_messages: completed turns in OpenAI {role, content} format
    # display_history: list of {user_msg, parsed} for UI rendering
    api_messages    <- reactiveVal(list())
    display_history <- reactiveVal(list())
    is_thinking     <- reactiveVal(FALSE)
    plot_code       <- reactiveVal(NULL)
    plot_title      <- reactiveVal(NULL)
    sheet_code      <- reactiveVal(NULL)

    # ---- Send handler ----
    # input$send_trigger carries the query text (set by JS before clearing the textarea)
    observeEvent(input$send_trigger, {
      req(seu_obj())
      query <- trimws(input$send_trigger)
      req(nchar(query) > 0)

      # 1. Show thinking state
      is_thinking(TRUE)

      # 2. Snapshot current conversation history before sending
      curr_api_msgs <- api_messages()

      # 3. Append user message to display immediately (parsed = NULL until response)
      disp <- display_history()
      display_history(append(disp, list(list(user_msg = query, parsed = NULL))))
      session$sendCustomMessage("scroll_chat", ns("chat_container"))

      # 4. Call GPT with conversation history
      gpt_result <- tryCatch({
        chatgpt_seu_query(
          prompt      = query,
          api_key     = api_key,
          org_id      = org_id,
          seu_obj     = seu_obj(),
          api_messages = curr_api_msgs
        )
      }, error = function(e) {
        showNotification(paste("API error:", e$message), type = "error", duration = 8)
        NULL
      })

      # 5. Clear thinking state
      is_thinking(FALSE)
      req(gpt_result)

      # 6. Route by response type
      msg  <- gpt_result$message %||% ""
      type <- gpt_result$type    %||% "chat"
      code <- gpt_result$code    %||% ""

      if (type == "plot" && nchar(trimws(code)) > 0) {

        # 6a. Eagerly eval plot code — catch runtime errors before touching plot history
        eval_result <- tryCatch({
          eval_env <- list2env(list(seu_obj = seu_obj()), parent = globalenv())
          eval(parse(text = code), envir = eval_env)
          if (exists("plot", envir = eval_env)) get("plot", envir = eval_env) else TRUE
        }, error = function(e) simpleError(e$message))

        if (inherits(eval_result, "error")) {
          # 6b. Eval failed — ask GPT to explain; no blank plot added to history
          error_api_msgs <- c(
            curr_api_msgs,
            list(list(role = "user",      content = query)),
            list(list(role = "assistant", content = as.character(jsonlite::toJSON(gpt_result, auto_unbox = TRUE))))
          )
          gpt_result <- tryCatch({
            chatgpt_seu_query(
              prompt       = paste0("The code you generated failed with this error: ",
                                    eval_result$message,
                                    ". Explain what went wrong and what the user would need to do differently."),
              api_key      = api_key,
              org_id       = org_id,
              seu_obj      = seu_obj(),
              api_messages = error_api_msgs
            )
          }, error = function(e) {
            list(type = "chat", message = paste("Eval error:", eval_result$message))
          })
          msg  <- gpt_result$message %||% ""
          type <- "chat"
          code <- ""
        } else {
          # 6c. Eval succeeded — expose to app.R which appends to plot history
          plot_code(code)
          plot_title(gpt_result$title %||% "")
        }

      } else if (type == "sheet" && nchar(trimws(code)) > 0) {
        sheet_code(code)
      }

      # 7. Append completed user + assistant turns to api_messages
      raw_response <- jsonlite::toJSON(gpt_result, auto_unbox = TRUE)
      api_messages(c(
        curr_api_msgs,
        list(list(role = "user",      content = query)),
        list(list(role = "assistant", content = as.character(raw_response)))
      ))

      # 8. Update display history — fill in parsed result for the last entry
      disp2 <- display_history()
      disp2[[length(disp2)]] <- list(user_msg = query, parsed = gpt_result)
      display_history(disp2)

      session$sendCustomMessage("scroll_chat", ns("chat_container"))
    })

    # ---- Render chat history ----
    output$chat_history_ui <- renderUI({
      history <- display_history()

      if (length(history) == 0) {
        return(div(class = "chat-empty", "Upload a Seurat object and start asking questions."))
      }

      bubbles <- lapply(history, function(entry) {
        user_bubble <- div(
          class = "chat-bubble-row user-row",
          div(class = "chat-bubble user-bubble", entry$user_msg)
        )

        # Still waiting for response
        if (is.null(entry$parsed)) {
          return(user_bubble)
        }

        resp <- entry$parsed
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
    return(list(plot_code = plot_code, plot_title = plot_title, sheet_code = sheet_code))
  })
}

# Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !identical(a, "")) a else b
