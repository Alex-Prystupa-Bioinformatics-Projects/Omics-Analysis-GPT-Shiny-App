# Add Meta Data Structure to System Prompt
meta_data_str <- function(seu_obj) {
  meta <- seu_obj@meta.data
  
  header <- sprintf("'data.frame':\t%d obs. of  %d variables:", nrow(meta), ncol(meta))
  
  output <- sapply(names(meta), function(colname) {
    col <- meta[[colname]]
    
    if (is.factor(col)) {
      levels_str <- paste(levels(col), collapse = ", ")
      sprintf(" $ %s: factor (%s)", colname, levels_str)
      
    } else if (is.numeric(col) || is.integer(col)) {
      col_summary <- summary(col)
      sprintf(" $ %s: %s (min=%.2f, median=%.2f, mean=%.2f, max=%.2f)",
              colname,
              class(col)[1],
              col_summary["Min."],
              col_summary["Median"],
              col_summary["Mean"],
              col_summary["Max."])
      
    } else {
      sprintf(" $ %s: %s", colname, class(col)[1])
    }
  }, USE.NAMES = FALSE)
  
  full_output <- paste(c(header, output), collapse = "\n")
  
  return(full_output)
}

# Add Memory to System Prompt
get_system_memory_prompt <- function(memory_list) {
  output <- character(length(memory_list$user_query))
  
  for (i in seq_along(memory_list$user_query)) {
    question <- memory_list$user_query[[i]]
    
    # Safe access: if response missing, show placeholder or empty string
    if (length(memory_list$query_response) >= i) {
      response <- memory_list$query_response[[i]]
    } else {
      response <- "<No response yet>"
    }
    
    output[i] <- glue("Q{i}: {question}\nA{i}: {response}\n")
  }
  
  return(paste(output, collapse = "\n\n"))
}


# System Prompt Function
get_system_prompt <- function(seu_obj, role, memory) {

# Main System Prompt
## 1. Generate Plotting Code Role
if (role == "coder_plot") {
  binf_system_prompt <- paste(
    "You are a bioinformatics assistant designed to help me create visualizations from Seurat objects in R.",
    "I will provide you with information about the Seurat object, including its features, samples, assays, dimensional reductions, and metadata.",
    "Use this information to help me generate visual plots such as UMAPs, feature plots, violin plots, and other Seurat-compatible visualizations.",
    "Return only R code. Do not include any markdown code block markers like ```R or ```.",
    "The name of the Seurat object to use for code is always `seu_obj`.",
    "Return only the R code necessary to generate the plot—no commentary or explanation.",
    "Always return a ggplot or Seurat plot object named `plot`.",
    sep = "\n"
  )
}

## 2. Generate Spreadsheet Code Role
if (role == "coder_sheet") {
  binf_system_prompt <- paste(
    "You are a bioinformatics assistant designed to help me generate data tables or results from Seurat objects in R.",
    "I will provide you with information about the Seurat object, including its features, samples, assays, dimensional reductions, and metadata.",
    "Use this information to help me perform tasks like identifying differentially expressed genes or generating tabular summaries.",
    "Return only R code. Do not include any markdown code block markers like ```R or ```.",
    "The name of the Seurat object to use for code is always `seu_obj`.",
    "Return only the R code necessary to generate the data frame(s)—no commentary or explanation.",
    "If running Differentially Expressed Genes, always have the first line set the Idents of the Seurat object, and ALWAYS include test.use = 'wilcox' as a parameter.",
    "If the user asks to subset or split the object before doing DE analysis, perform that step first before setting the Idents.",
    "If the user is asking for something that clearly requires multiple data frames (e.g., by condition, cluster, or comparison), generate a named list of data frames.",
    "Each element of this list must be a data frame and should be named meaningfully (e.g., by group or label).",
    "If you combine results from different groups into one data frame, include a new column that identifies the group each row came from.",
    "If the output includes `pct.1` and `pct.2`, rename these to reflect the actual group names if known (e.g., 'healthy', 'disease').",
    "The resulting output must always be stored in a named list called `generated_df_list`.",
    "Do not print, display, or summarize the data frames—just generate and assign them.",
    sep = "\n"
  )
}

## 3. Bioinformatics Chat Bot Role
if (role == "assistant") {
  binf_system_prompt <- paste(
    "You are a bioinformatics assistant designed to help me explore and understand Seurat objects in R.",
    "You will never provide R code in your responses.",
    "Instead, your role is to help me think through the structure, quality, and potential analyses of single-cell RNA-seq data stored in Seurat objects.",
    "This data may also be single cell multiome or spatial, you must infer that",
    "You may suggest types of plots, QC strategies, dimensionality reduction techniques, clustering approaches, or interpretation tips.",
    "Your goal is to provide clear, helpful, and thoughtful explanations and suggestions, without executing or proposing specific code.",
    "The name of the Seurat object in context is always `seu_obj`, and you may reference it when discussing what’s inside it.",
    "Maintain a professional, collaborative tone and guide me in reasoning through my analysis questions.",
    "If they ask a very off topic question, steer the user back to bioinformatics",
    sep = "\n"
  )
}


# Extract Seurat Object Summary for Context
seu_summary_string <- paste(
  "Seurat object summary:",
  paste("Meta Data Structure:", meta_data_str(seu_obj), sep=" "),
  paste("Assays available:", paste(names(seu_obj@assays), collapse = ", "), sep=" "),
  paste("Active Assay:", seu_obj@active.assay, sep=" "),
  paste("Dimensional reductions:", paste(names(seu_obj@reductions), collapse = ", "), sep=" "),
  paste("Previous User Q/A:", get_system_memory_prompt(memory)),
  sep = "\n"
)

# Combine everything for a comprehensive system prompt
system_prompt <- paste(
  binf_system_prompt,
  seu_summary_string,
  sep = "\n"
)

cat(system_prompt)
return(system_prompt)
}

# GENERATE QUERY
chatgpt_seu_query <- function(prompt, api_key, org_id, seu_obj, role, memory="") {
  
  # Get System Prompt
  system_prompt <- get_system_prompt(seu_obj = seu_obj, role = role, memory = memory)
  
  # Call OpenAI API
  res <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json",
      `OpenAI-Organization` = org_id
    ),
    body = toJSON(list(
      model = "gpt-4.1-mini",
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = prompt)
      )
    ), auto_unbox = TRUE)
  )
  
  # Return the chatbot's reply
  parsed <- content(res, as = "parsed", encoding = "UTF-8")
  return(parsed$choices[[1]]$message$content)
}

# EVAL QUERY
eval_seu_gpt_query <- function(seu_obj, gpt_out) {
  # Optionally, print/log the GPT output for debugging purposes
  cat("Executing the following code:\n", gpt_out, "\n")
  
  # Check if the GPT output is non-empty
  if (nchar(gpt_out) == 0) {
    stop("Error: GPT output is empty. No code to execute.")
  }
  
  # Try to parse and evaluate the code safely
  tryCatch({
    # Evaluate the GPT output, ensuring it refers to 'seu_obj'
    result <- eval(parse(text = gpt_out))
    
    # Return the result of the execution
    return(result)
  }, error = function(e) {
    # If an error occurs, return the error message
    cat("Error occurred while executing GPT output: ", e$message, "\n")
    return(NULL)
  })
}

# EXECUTE QUERY
# execute_seu_gpt_query <- function(prompt, api_key, org_id, seu_obj) {
#   
#   # 1. Query GPT
#   query_out <- chatgpt_seu_query(prompt, api_key, org_id, seu_obj) # Code Generated
#   
#   # 2. Execute Code
#   eval_seu_gpt_query(seu_obj, query_out)
# }

