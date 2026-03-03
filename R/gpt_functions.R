# ============================================================
# Metadata summary for system prompt
# ============================================================
meta_data_str <- function(seu_obj) {
  meta   <- seu_obj@meta.data
  header <- sprintf("'data.frame':\t%d obs. of  %d variables:", nrow(meta), ncol(meta))

  output <- sapply(names(meta), function(colname) {
    col <- meta[[colname]]
    if (is.factor(col)) {
      levels_str <- paste(levels(col), collapse = ", ")
      sprintf(" $ %s: factor (%s)", colname, levels_str)
    } else if (is.numeric(col) || is.integer(col)) {
      s <- summary(col)
      sprintf(" $ %s: %s (min=%.2f, median=%.2f, mean=%.2f, max=%.2f)",
              colname, class(col)[1], s["Min."], s["Median"], s["Mean"], s["Max."])
    } else {
      sprintf(" $ %s: %s", colname, class(col)[1])
    }
  }, USE.NAMES = FALSE)

  paste(c(header, output), collapse = "\n")
}

# ============================================================
# Conversation memory formatter
# ============================================================
get_system_memory_prompt <- function(memory_list) {
  if (length(memory_list$user_query) == 0) return("")
  output <- character(length(memory_list$user_query))
  for (i in seq_along(memory_list$user_query)) {
    question <- memory_list$user_query[[i]]
    response <- if (length(memory_list$query_response) >= i) memory_list$query_response[[i]] else "<No response yet>"
    output[i] <- glue("Q{i}: {question}\nA{i}: {response}\n")
  }
  paste(output, collapse = "\n\n")
}

# ============================================================
# Unified system prompt
# ============================================================
get_system_prompt <- function(seu_obj, memory) {

  unified_prompt <- paste(
    "You are a concise bioinformatics expert assistant specializing in single-cell, spatial, and multiome analysis using Seurat in R.",
    "Always respond with a single valid JSON object — no markdown, no text outside the JSON.",
    "The JSON must have exactly these fields:",
    '  "type": one of "chat", "plot", or "sheet"',
    '  "message": your response in plain text (2-3 sentences max — answer only what was asked, no unsolicited lists)',
    '  "code": R code string (only include this field for "plot" or "sheet" types; omit entirely for "chat")',
    "",
    "Rules for type selection:",
    '  - "chat": questions, clarifications, interpretation — no code output needed',
    '  - "plot": user wants a visualization; assign a ggplot/Seurat plot object to `plot`',
    '  - "sheet": user wants tabular data (DE genes, summaries, etc.); assign a named list of data frames to `generated_df_list`',
    "",
    "Rules for code:",
    "  - Seurat object is always `seu_obj`",
    "  - No markdown code fences (no ```R or ```)",
    "  - Do not print or display results — only assign them",
    "  - For DE: always set Idents first and always include test.use = 'wilcox'",
    "",
    "Rules for message:",
    "  - Never include R code in message",
    "  - Be direct — say what you did or what you know, nothing more",
    "  - If off-topic, briefly redirect to bioinformatics",
    sep = "\n"
  )

  seu_summary <- paste(
    "Seurat object summary:",
    paste("Metadata:", meta_data_str(seu_obj)),
    paste("Assays:", paste(names(seu_obj@assays), collapse = ", ")),
    paste("Active assay:", seu_obj@active.assay),
    paste("Reductions:", paste(names(seu_obj@reductions), collapse = ", ")),
    paste("Previous conversation:\n", get_system_memory_prompt(memory)),
    sep = "\n"
  )

  paste(unified_prompt, seu_summary, sep = "\n\n")
}

# ============================================================
# Parse GPT JSON response — fallback to raw chat if malformed
# ============================================================
parse_gpt_response <- function(raw) {
  tryCatch(
    jsonlite::fromJSON(raw),
    error = function(e) list(type = "chat", message = raw, code = NULL)
  )
}

# ============================================================
# OpenAI API call
# ============================================================
chatgpt_seu_query <- function(prompt, api_key, org_id, seu_obj,
                               memory = list(user_query = list(), query_response = list())) {

  system_prompt <- get_system_prompt(seu_obj = seu_obj, memory = memory)

  res <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(
      Authorization         = paste("Bearer", api_key),
      `Content-Type`        = "application/json",
      `OpenAI-Organization` = org_id
    ),
    body = toJSON(list(
      model           = "gpt-5-mini",
      response_format = list(type = "json_object"),
      messages        = list(
        list(role = "system", content = system_prompt),
        list(role = "user",   content = prompt)
      )
    ), auto_unbox = TRUE)
  )

  parsed <- content(res, as = "parsed", encoding = "UTF-8")
  raw    <- parsed$choices[[1]]$message$content
  parse_gpt_response(raw)
}

# ============================================================
# Eval helper — execute GPT-generated R code with seu_obj in scope
# ============================================================
eval_seu_gpt_query <- function(seu_obj, gpt_out) {
  if (nchar(trimws(gpt_out)) == 0) stop("GPT output is empty.")
  eval_env <- list2env(list(seu_obj = seu_obj), parent = globalenv())
  tryCatch(
    eval(parse(text = gpt_out), envir = eval_env),
    error = function(e) {
      cat("Eval error:", e$message, "\n")
      NULL
    }
  )
}
