# OMICS-ANALYSIS-GPT-SHINY-APP

---

## Overview

**Omics-Analysis-GPT-Shiny-App**

Finally, a way to get the biologist off your back for basic plots and analysis (just kidding!). This tool is designed to use GPT to help with basic analysis,
by uploading a fully processed RDS file and allowing the user to chat away! Features include:

- An interactive Shiny dashboard for uploading and exploring Seurat RDS files using GPT-4.
- Uses bs4Dash for modern layout and responsive UI components
- 3 different chat bots:
  - Direct chat to ask questions about analysis and formulate some sort of analysis strategy
  - Plot chat bot that always returns R code that plots and object
  - Spreadsheet chat bot that always returns a list of dataframes and then presents dataframe to user (ex. run DGE markers)
- All chat bots ingest the structure & summary of the seurat object meta data (in a custom structure function) to have way better context of the object
- Download generated plots and tables
- GPT memory is preserved per tab (chat, plot, sheet)

---

## Project Structure

```sh
└── Omics-Analysis-GPT-Shiny-App/
    ├── app.R
    └── gpt-functions.R
```

### Project Index

<details open>
	<summary><b><code>OMICS-ANALYSIS-GPT-SHINY-APP/</code></b></summary>
	<!-- __root__ Submodule -->
	<details>
		<summary><b>__root__</b></summary>
		<blockquote>
			<div class='directory-path' style='padding: 8px 0; color: #666;'>
				<code><b>⦿ __root__</b></code>
			<table style='width: 100%; border-collapse: collapse;'>
			<thead>
				<tr style='background-color: #f8f9fa;'>
					<th style='width: 30%; text-align: left; padding: 8px;'>File Name</th>
					<th style='text-align: left; padding: 8px;'>Summary</th>
				</tr>
			</thead>
				<tr style='border-bottom: 1px solid #eee;'>
					<td style='padding: 8px;'><b><a href='/Users/aleksandrprystupa/Projects/Alex-BINF-Pipelines/Omics-Analysis-GPT-Shiny-App/blob/master/app.R'>app.R</a></b></td>
					<td style='padding: 8px;'>- Create an interactive Shiny app for bioinformatics analysis<br>- Users can upload Seurat RDS files, chat with a bioinformatics assistant, generate plots and tables using GPT, and save results as PDFs or CSVs<br>- The app provides a seamless interface for data exploration and visualization in genomics research.</td>
				</tr>
				<tr style='border-bottom: 1px solid #eee;'>
					<td style='padding: 8px;'><b><a href='/Users/aleksandrprystupa/Projects/Alex-BINF-Pipelines/Omics-Analysis-GPT-Shiny-App/blob/master/gpt-functions.R'>gpt-functions.R</a></b></td>
					<td style='padding: 8px;'>- Generate comprehensive system prompts for bioinformatics tasks based on user roles, providing structured guidance for creating visualizations, data tables, or offering analysis insights<br>- Utilize functions to extract Seurat object summaries and handle user queries through an OpenAI API, ensuring seamless interaction and code evaluation within the bioinformatics context.</td>
				</tr>
			</table>
		</blockquote>
	</details>
</details>

---

## Usage

Upload a fully processed RDS Object. It can be standard single cell, spatial, or multiome. Then have fun and ask away!
As of right now you cannot ask it reprocess and save another RDS although this funcionality will hopefully be added in the future.