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
# System prompt — Seurat object context only (no memory text)
# ============================================================
get_system_prompt <- function(seu_obj) {

  unified_prompt <- paste(
    "You are a concise bioinformatics expert assistant specializing in single-cell, spatial, and multiome analysis using Seurat in R.",
    "Always respond with a single valid JSON object — no markdown, no text outside the JSON.",
    "The JSON must have exactly these fields:",
    '  "type": one of "chat", "plot", or "sheet"',
    '  "message": your response — format depends on type (see rules below)',
    '  "title": a 3-5 word descriptive label for the plot (only for "plot" type, e.g. "UMAP by Cluster"; omit for chat/sheet)',
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
    "  - For CoveragePlot: follow these rules strictly:",
    "    1. NEVER subset() the Seurat object. To filter cell types, build a cells vector and pass via `cells`:",
    "       BAD:  seu_sub <- subset(seu_obj, subset = Annotation_Broad == 'X'); CoveragePlot(object = seu_sub, ...)",
    "       GOOD: cells_use <- colnames(seu_obj)[seu_obj$Annotation_Broad == 'X']; CoveragePlot(object = seu_obj, cells = cells_use, ...)",
    "       The full seu_obj must always be passed as `object`; group.by still works normally on top of cells.",
    "    2. To overlay RNA expression, use `features` — NOT `expression` (that parameter does not exist):",
    "       BAD:  CoveragePlot(object = seu_obj, region = 'Calca', expression = 'Calca', ...)",
    "       GOOD: CoveragePlot(object = seu_obj, region = 'Calca', features = 'Calca', expression.assay = 'RNA', ...)",
    "",
    "Rules for message:",
    "  - Never include R code in message",
    "  - If off-topic, briefly redirect to bioinformatics",
    "",
    '  For "chat" type: use markdown formatting — bold section headers, bullet points, backtick-formatted column/variable names.',
    "  Write like a biologist reading a structured lab report, not a text message. Group information logically.",
    "",
    "  BAD example (flat, wall-of-text, no structure):",
    '  "The Seurat object metadata contains 35,083 cells with 15 variables including SampleID, Condition, Time, Condition_Time, broad and narrow cell-type annotations (Annotation_Broad, Annotation_Narrow), and multiple RNA/ATAC QC metrics (nCount_RNA, nFeature_RNA, percent.mt, nCount_peaks, nFeature_peaks, nucleosome_signal, TSS.enrichment and percentiles). Notable ranges: nCount_RNA 277-123,812 (median 1,168), nFeature_RNA 251-9,983 (median 806), percent.mt 0-19.97% (median 1.44%); active assay is peaks and reductions include pca, lsi, rna.harmony, atac.harmony, umap.rna, umap.atac and wnn.umap."',
    "",
    "  GOOD example (grouped, structured, markdown formatted):",
    '"**35,083 cells · 15 metadata variables**\\n\\n**Sample & Experimental Design**\\n- `SampleID` — individual sample identifiers\\n- `Condition` — CTRL, IMQ\\n- `Time` — CTRL, D2_IMQ, D6_IMQ, D30_IMQ\\n- `Condition_Time` — combined condition × timepoint labels\\n\\n**Cell-Type Annotations**\\n- `Annotation_Broad` — coarse cell-type labels\\n- `Annotation_Narrow` — fine-grained cell-type labels\\n\\n**RNA QC**\\n- `nCount_RNA`: 277 – 123,812 (median 1,168)\\n- `nFeature_RNA`: 251 – 9,983 (median 806)\\n- `percent.mt`: 0 – 19.97% (median 1.44%)\\n\\n**ATAC QC**\\n- `nCount_peaks`, `nFeature_peaks`, `nucleosome_signal`, `TSS.enrichment`\\n\\n**Object State**\\n- Active assay: `peaks`\\n- Reductions: `pca`, `lsi`, `rna.harmony`, `atac.harmony`, `umap.rna`, `umap.atac`, `wnn.umap`"',
    "",
    '  For "plot" type: one short sentence confirming what was plotted. No filler like "Use that object to display or further customize the figure."',
    '  For "sheet" type: 1-2 sentences summarizing what was computed and what the result contains.',
    sep = "\n"
  )

  seu_summary <- paste(
    "Seurat object summary:",
    paste("Metadata:", meta_data_str(seu_obj)),
    paste("Assays:", paste(names(seu_obj@assays), collapse = ", ")),
    paste("Active assay:", seu_obj@active.assay),
    paste("Reductions:", paste(names(seu_obj@reductions), collapse = ", ")),
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
# OpenAI API call — multi-turn messages format
# ============================================================
chatgpt_seu_query <- function(prompt, api_key, org_id, seu_obj,
                               api_messages = list()) {

  # 1. Build system prompt (Seurat context only)
  system_prompt <- get_system_prompt(seu_obj = seu_obj)

  # 2. Assemble full messages array: system + history + new user turn
  messages <- c(
    list(list(role = "system", content = system_prompt)),
    api_messages,
    list(list(role = "user", content = prompt))
  )

  # 3. Call OpenAI API
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
      messages        = messages
    ), auto_unbox = TRUE)
  )

  # 4. Parse and return response
  parsed <- content(res, as = "parsed", encoding = "UTF-8")
  raw    <- parsed$choices[[1]]$message$content
  parse_gpt_response(raw)
}

# ============================================================
# Whisper API call — transcribe base64-encoded audio blob
# ============================================================
whisper_transcribe <- function(audio_b64, api_key) {
  # 1. Decode base64 to a temp .webm file
  tmp <- tempfile(fileext = ".webm")
  writeBin(jsonlite::base64_dec(audio_b64), tmp)
  on.exit(unlink(tmp))

  # 2. POST to Whisper transcriptions endpoint
  res <- POST(
    url = "https://api.openai.com/v1/audio/transcriptions",
    add_headers(Authorization = paste("Bearer", api_key)),
    body = list(
      model = "whisper-1",
      file  = upload_file(tmp, type = "audio/webm")
    ),
    encode = "multipart"
  )

  # 3. Return transcript text or empty string on failure
  parsed <- content(res, as = "parsed", encoding = "UTF-8")
  parsed$text %||% ""
}

# ============================================================
# Eval helper — execute GPT-generated R code with seu_obj in scope
# ============================================================
eval_seu_gpt_query <- function(seu_obj, gpt_out) {
  if (nchar(trimws(gpt_out)) == 0) stop("GPT output is empty.")
  eval_env <- list2env(list(seu_obj = seu_obj), parent = globalenv())
  tryCatch({
    eval(parse(text = gpt_out), envir = eval_env)
    # Explicitly return the plot object so renderPlot can print it
    if (exists("plot", envir = eval_env)) get("plot", envir = eval_env)
  }, error = function(e) {
    cat("Eval error:", e$message, "\n")
    NULL
  })
}
