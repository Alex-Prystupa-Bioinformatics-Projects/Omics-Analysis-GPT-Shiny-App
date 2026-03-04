# Omics-Analysis-GPT Shiny App

An interactive Shiny dashboard for exploring fully processed Seurat objects using a GPT-powered chatbot. Upload your RDS file, ask questions, generate plots and tables — all in one interface.

---

## Features

- Single unified chatbot that knows when to talk, plot, or produce a data table
- Automatically generates and renders Seurat/ggplot visualizations based on natural language prompts
- Runs DE analysis and other tabular outputs on request
- Collapsible code blocks so you can inspect the generated R code
- Drag-to-resize sidebar and panel layout
- Download plots as PDF and tables as CSV
- Supports standard single-cell, spatial, and multiome Seurat objects

---

## Project Structure

```
Omics-Analysis-GPT-Shiny-App/
├── app.R                  # Entry point — UI, server, layout
├── R/
│   ├── gpt_functions.R    # OpenAI API call, system prompt, eval helper
│   └── mod_chat.R         # Unified chat module (UI + server)
└── .keys                  # API keys (never committed)
```

---

## Setup

1. Clone the repo
2. Add a `.keys` file at the project root:
   ```
   API_KEY=your_openai_api_key
   ORG_ID=your_openai_org_id
   ```
3. Install dependencies:
   ```r
   install.packages(c("shiny", "bslib", "shinyjs", "DT", "httr", "jsonlite",
                       "glue", "ggplot2", "dplyr", "dotenv"))
   # Bioconductor
   BiocManager::install(c("Seurat", "Signac", "presto"))
   ```
4. Run the app:
   ```r
   shiny::runApp(".")
   ```

---

## Usage

Upload a fully processed Seurat RDS object (single-cell, spatial, or multiome). The chatbot ingests the object's metadata structure, assays, and reductions as context. Ask anything — it will respond conversationally, generate a plot, or produce a table depending on what you need.
