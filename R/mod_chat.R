# ============================================================
# Unified Chat Module
# Handles all interaction types: conversational, plot, sheet.
# Returns list(plot_code, sheet_code, plot_title) reactives to app.R.
# ============================================================

# Utility JS — injected once in UI
chat_scroll_js <- tags$script(HTML("
  // Auto-scroll: on first call, set up a MutationObserver so any DOM change inside
  // the container (Shiny re-render, JS injection) scrolls to bottom automatically.
  var chatScrollObserverReady = false;
  Shiny.addCustomMessageHandler('scroll_chat', function(id) {
    var el = document.getElementById(id);
    if (!el) return;
    el.scrollTop = el.scrollHeight;
    if (!chatScrollObserverReady) {
      chatScrollObserverReady = true;
      new MutationObserver(function() {
        el.scrollTop = el.scrollHeight;
      }).observe(el, { childList: true, subtree: true });
    }
  });

  // Remove both JS-injected bubbles when the server response renders
  Shiny.addCustomMessageHandler('remove_js_chat_bubbles', function(dummy) {
    ['js_user_bubble', 'js_typing_bubble'].forEach(function(id) {
      var el = document.getElementById(id);
      if (el) el.parentNode.removeChild(el);
    });
  });
"));

chatUI <- function(id) {
  ns <- NS(id)

  # Enter-to-send: capture text, clear, fire trigger, then inject user bubble + typing
  # dots directly into the DOM — Shiny only flushes after the blocking API call returns,
  # so we can't rely on renderUI or reactive state for any of this immediate feedback.
  enter_to_send_js <- tags$script(HTML(sprintf("
    $(document).on('keydown', '#%s', function(e) {
      if (e.key === 'Enter' && !e.shiftKey) {
        e.preventDefault();
        var query = $(this).val().trim();
        if (query.length === 0) return;
        // Guard against double-send without greying/disabling the textarea
        if (document.getElementById('js_typing_bubble')) return;
        $(this).val('');
        Shiny.setInputValue('%s', query, {priority: 'event'});

        var container = document.getElementById('%s');
        if (container) {
          // 1. Inject user bubble immediately
          var escaped = query
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/\\n/g, '<br>');
          var userBubble = document.createElement('div');
          userBubble.id = 'js_user_bubble';
          userBubble.className = 'chat-bubble-row user-row';
          userBubble.innerHTML = '<div class=\"chat-bubble user-bubble\">' + escaped + '</div>';
          container.appendChild(userBubble);

          // 2. Inject typing dots immediately after
          var typing = document.createElement('div');
          typing.id = 'js_typing_bubble';
          typing.className = 'chat-bubble-row assistant-row';
          typing.innerHTML =
            '<div class=\"chat-bubble assistant-bubble typing-bubble\">' +
            '<div class=\"typing-indicator\"><span></span><span></span><span></span></div>' +
            '</div>';
          container.appendChild(typing);
          container.scrollTop = container.scrollHeight;
        }
      }
    });
  ", ns("chat_input"), ns("send_trigger"), ns("chat_container"))))

  # Outer div is a flex column — grows to fill leftover sidebar height after upload inputs
  div(
    class = "chat-module",
    chat_scroll_js,
    enter_to_send_js,

    # Scrollable chat history — grows to fill all available space
    div(
      id    = ns("chat_container"),
      class = "chat-container",
      uiOutput(ns("chat_history_ui"))
    ),

    # Input pinned to bottom of the module
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
    plot_code       <- reactiveVal(NULL)
    plot_title      <- reactiveVal(NULL)
    sheet_code      <- reactiveVal(NULL)

    # ---- Send handler ----
    # input$send_trigger carries the query text (set by JS before clearing the textarea)
    observeEvent(input$send_trigger, {
      req(seu_obj())
      query <- trimws(input$send_trigger)
      req(nchar(query) > 0)

      # 1. Double-send is guarded in JS (checks for js_typing_bubble existence)

      # 2. Snapshot current conversation history before sending
      curr_api_msgs <- api_messages()

      # 3. Append user message to display immediately (parsed = NULL until response)
      disp <- display_history()
      display_history(append(disp, list(list(user_msg = query, parsed = NULL))))
      session$sendCustomMessage("scroll_chat", ns("chat_container"))

      # 4. Call GPT with conversation history
      gpt_result <- tryCatch({
        chatgpt_seu_query(
          prompt       = query,
          api_key      = api_key,
          org_id       = org_id,
          seu_obj      = seu_obj(),
          api_messages = curr_api_msgs
        )
      }, error = function(e) {
        showNotification(paste("API error:", e$message), type = "error", duration = 8)
        NULL
      })

      # 5. Remove JS-injected user bubble + typing dots now that Shiny will render the real content
      session$sendCustomMessage("remove_js_chat_bubbles", TRUE)
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
    # Typing indicator is JS-injected (see enter_to_send_js) — not rendered here.
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

        # Still waiting for response — show user message only
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

    # ---- Return reactives to app.R ----
    return(list(plot_code = plot_code, plot_title = plot_title, sheet_code = sheet_code))
  })
}

# Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !identical(a, "")) a else b
