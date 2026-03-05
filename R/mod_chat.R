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

  // Auto-send voice transcript: inject bubbles then fire send_trigger
  Shiny.addCustomMessageHandler('auto_send', function(msg) {
    if (document.getElementById('js_typing_bubble')) return;
    Shiny.setInputValue(msg.send_trigger_id, msg.text, { priority: 'event' });
    var container = document.getElementById(msg.container_id);
    if (container) {
      var escaped = msg.text
        .replace(/&/g, '&amp;').replace(/</g, '&lt;')
        .replace(/>/g, '&gt;').replace(/\\n/g, '<br>');
      var userBubble = document.createElement('div');
      userBubble.id = 'js_user_bubble';
      userBubble.className = 'chat-bubble-row user-row';
      userBubble.innerHTML = '<div class=\"chat-bubble user-bubble\">' + escaped + '</div>';
      container.appendChild(userBubble);
      var typing = document.createElement('div');
      typing.id = 'js_typing_bubble';
      typing.className = 'chat-bubble-row assistant-row';
      typing.innerHTML = '<div class=\"chat-bubble assistant-bubble typing-bubble\"><div class=\"typing-indicator\"><span></span><span></span><span></span></div></div>';
      container.appendChild(typing);
      container.scrollTop = container.scrollHeight;

      // One-shot MutationObserver: remove JS bubbles when Shiny renders real content
      var outputDiv = document.getElementById(msg.output_id);
      if (outputDiv && !outputDiv._bubbleObserver) {
        outputDiv._bubbleObserver = true;
        var obs = new MutationObserver(function() {
          obs.disconnect();
          outputDiv._bubbleObserver = false;
          ['js_user_bubble', 'js_typing_bubble'].forEach(function(id) {
            var el = document.getElementById(id);
            if (el && el.parentNode) el.parentNode.removeChild(el);
          });
        });
        obs.observe(outputDiv, { childList: true, subtree: true });
      }
    }
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

          // 3. One-shot MutationObserver: remove JS bubbles when Shiny renders real content
          var outputDiv = document.getElementById('%s');
          if (outputDiv && !outputDiv._bubbleObserver) {
            outputDiv._bubbleObserver = true;
            var obs = new MutationObserver(function() {
              obs.disconnect();
              outputDiv._bubbleObserver = false;
              ['js_user_bubble', 'js_typing_bubble'].forEach(function(id) {
                var el = document.getElementById(id);
                if (el && el.parentNode) el.parentNode.removeChild(el);
              });
            });
            obs.observe(outputDiv, { childList: true, subtree: true });
          }
        }
      }
    });
  ", ns("chat_input"), ns("send_trigger"), ns("chat_container"), ns("chat_history_ui"))))

  # MediaRecorder: hold to record, release to send — mouseup bound on document so
  # releasing outside the button still stops recording correctly.
  mic_js <- tags$script(HTML(sprintf("
    (function() {
      var mediaRecorder, audioChunks = [];

      $(document).on('mousedown', '#%s', function(e) {
        e.preventDefault();
        navigator.mediaDevices.getUserMedia({ audio: true }).then(function(stream) {
          audioChunks = [];
          mediaRecorder = new MediaRecorder(stream);
          mediaRecorder.ondataavailable = function(e) { audioChunks.push(e.data); };
          mediaRecorder.onstop = function() {
            var blob = new Blob(audioChunks, { type: 'audio/webm' });
            var reader = new FileReader();
            reader.onloadend = function() {
              var b64 = reader.result.split(',')[1];
              Shiny.setInputValue('%s', b64, { priority: 'event' });
            };
            reader.readAsDataURL(blob);
            stream.getTracks().forEach(function(t) { t.stop(); });
          };
          mediaRecorder.start();
          $('#%s').addClass('recording').attr('title', 'Release to send');

          $(document).one('mouseup', function() {
            if (mediaRecorder && mediaRecorder.state === 'recording') {
              mediaRecorder.stop();
              $('#%s').removeClass('recording').attr('title', 'Hold to talk');
            }
          });
        });
      });
    })();
  ", ns("mic_btn"), ns("whisper_audio"), ns("mic_btn"), ns("mic_btn"))))

  # Outer div is a flex column — grows to fill leftover sidebar height after upload inputs
  div(
    class = "chat-module",
    chat_scroll_js,
    enter_to_send_js,
    mic_js,

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
      ),
      actionButton(ns("mic_btn"), label = NULL, icon = icon("microphone"),
                   class = "mic-btn", title = "Hold to talk")
    )
  )
}

chatServer <- function(id, seu_obj, api_key, org_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- State ----
    # api_messages: completed turns in OpenAI {role, content} format
    # display_history: list of {user_msg, parsed} for UI rendering
    # pending_sheet_*: holds sheet code awaiting user approval
    api_messages        <- reactiveVal(list())
    display_history     <- reactiveVal(list())
    plot_code           <- reactiveVal(NULL)
    plot_title          <- reactiveVal(NULL)
    sheet_code          <- reactiveVal(NULL)
    pending_sheet_code  <- reactiveVal(NULL)
    pending_sheet_index <- reactiveVal(NULL)

    # ---- Voice input handler ----
    # Receives base64 webm audio, transcribes via Whisper, fires auto_send in JS
    observeEvent(input$whisper_audio, {
      req(input$whisper_audio)
      transcript <- tryCatch(
        whisper_transcribe(input$whisper_audio, api_key),
        error = function(e) {
          showNotification(paste("Whisper error:", e$message), type = "error", duration = 6)
          ""
        }
      )
      req(nchar(trimws(transcript)) > 0)
      session$sendCustomMessage("auto_send", list(
        send_trigger_id = ns("send_trigger"),
        text            = transcript,
        container_id    = ns("chat_container"),
        output_id       = ns("chat_history_ui")
      ))
    })

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

      # 5. MutationObserver in JS handles removal of JS bubbles when Shiny renders real content
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
          gpt_result$code       <- code                  # preserve original failed code for display
          gpt_result$eval_error <- eval_result$message   # raw R error for display
          code <- ""
        } else {
          # 6c. Eval succeeded — expose to app.R which appends to plot history
          plot_code(code)
          plot_title(gpt_result$title %||% "")
        }

      } else if (type == "sheet" && nchar(trimws(code)) > 0) {
        # Mark pending — Run/Dismiss buttons rendered in chat bubble before eval
        gpt_result$sheet_status <- "pending"
        pending_sheet_code(code)
        pending_sheet_index(length(display_history()))
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

    # ---- Sheet approval handlers ----
    observeEvent(input$run_sheet, {
      code <- pending_sheet_code()
      req(code)

      # 1. Fire sheet evaluation
      sheet_code(code)

      # 2. Mark history entry as run
      disp <- display_history()
      idx  <- pending_sheet_index()
      if (!is.null(idx) && idx >= 1L && idx <= length(disp)) {
        disp[[idx]]$parsed$sheet_status <- "run"
        display_history(disp)
      }

      # 3. Clear pending state
      pending_sheet_code(NULL)
      pending_sheet_index(NULL)
    })

    observeEvent(input$dismiss_sheet, {
      disp <- display_history()
      idx  <- pending_sheet_index()
      if (!is.null(idx) && idx >= 1L && idx <= length(disp)) {
        disp[[idx]]$parsed$sheet_status <- "dismissed"
        display_history(disp)
      }
      pending_sheet_code(NULL)
      pending_sheet_index(NULL)
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

        eval_error <- resp$eval_error %||% ""

        code_block <- if (nchar(trimws(code)) > 0) {
          block_icon <- if (type == "sheet") "table" else "code"
          tags$details(
            class = "code-details",
            tags$summary(
              class = "code-summary",
              icon(block_icon), " View Code"
            ),
            tags$pre(class = "code-pre", code),
            if (nchar(trimws(eval_error)) > 0)
              tags$pre(class = "code-pre", style = "color:#ff6b6b; margin-top:4px;",
                       paste0("Error: ", eval_error))
          )
        } else NULL

        # Run/Dismiss buttons for pending sheets; status label after action
        sheet_actions <- if (type == "sheet") {
          status <- resp$sheet_status %||% ""
          if (status == "pending") {
            div(
              style = "margin-top: 8px; display: flex; gap: 6px;",
              actionButton(ns("run_sheet"),     tagList(icon("play"),  " Run"),
                           class = "btn btn-sm btn-success"),
              actionButton(ns("dismiss_sheet"), tagList(icon("times"), " Dismiss"),
                           class = "btn btn-sm btn-secondary")
            )
          } else if (status == "run") {
            div(style = "margin-top: 6px; font-size: 0.78rem; color: #6bcb77;",
                icon("check"), " Sheet generated")
          } else if (status == "dismissed") {
            div(style = "margin-top: 6px; font-size: 0.78rem; color: #888;",
                icon("times"), " Dismissed")
          } else NULL
        } else NULL

        assistant_bubble <- div(
          class = "chat-bubble-row assistant-row",
          div(
            class = "chat-bubble assistant-bubble",
            shiny::markdown(msg),
            code_block,
            sheet_actions
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
