# Telegram Network Analysis Toolkit

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Language: R](https://img.shields.io/badge/Language-R-brightgreen.svg)](https://www.r-project.org/)

This repository contains a suite of R scripts designed for ingesting, processing, and structuring raw data from the Telegram messaging platform for large-scale network and content analysis.

## Overview

The toolkit provides a complete pipeline to transform messy, nested JSON data from Telegram into structured datasets ready for analysis. It is built to handle the two primary types of data exports—from the backend API and the frontend Desktop application—and merges them into a unified format. The primary goal is to extract and clean entities (users, channels) as network nodes and their interactions (forwards, mentions, links) as network edges, while also enriching the message data with language and keyword labels.

The scripts are designed for efficiency, leveraging parallel processing to handle large volumes of data. The final output is a set of analysis-ready files, including an edge list, a node attribute table, and a content feature matrix.

## Core Features

-   **Dual-Format Ingestion**: Automatically detects and parses JSON data from both Telegram's backend API and frontend Desktop App exports.
-   **Robust Network Construction**:
    -   Creates standardized and disambiguated identifiers for network nodes (channels, groups, users).
    -   Extracts multiple types of network links, including forwards, @mentions, and in-text `t.me` URLs.
    -   Generates a clean node dictionary with consolidated metadata (e.g., channel name, URL).
-   **Advanced Content Processing**:
    -   Performs sophisticated, multi-pass language detection on message text using a combination of `cld2`, `cld3`, and `fastText`.
    -   Applies custom keyword lists (using regular expressions) to label message content efficiently, outputting a sparse matrix for easy integration with other analyses.
-   **High Performance**: Utilizes parallel processing for computationally intensive tasks like data import and content labeling, significantly speeding up the workflow.
-   **Efficient Storage**: Saves processed data using the fast and memory-efficient `.fst` file format.

## The Processing Pipeline

The toolkit is designed to be run as a sequential pipeline, where the output of one script becomes the input for the next.

1.  **Data Ingestion**: Place raw Telegram JSON files into an input directory. The `full_import.R` script serves as the main entry point, automatically identifying the JSON format (API vs. Desktop App) and calling the appropriate helper scripts (`ref/import_tg_backend.R`, `ref/import_tg_frontend.R`) to parse all files into a single, clean data frame.

2.  **Node and Link Generation**: Process the imported data with `add_node_ids.R` and `add_link_id.R`. These scripts create unique, standardized identifiers for all network entities (nodes) and the connections between them (edges), forming the foundational structure of the network.

3.  **Data Enrichment**: Enhance the dataset by running the following scripts:
    -   `add_lang.R`: Identifies the primary language of each message.
    -   `keyword_label.R`: Tags each message with keywords based on a predefined list of regular expressions.

4.  **Node Consolidation**: Execute `build_node_dict.R` to aggregate all identified nodes and their metadata into a master lookup table, or "node dictionary." This ensures a clean set of attributes for each node in the network.

5.  **Analysis**: The resulting structured files (e.g., node dictionary, edge list, content labels) can be used for network analysis and visualization in R with packages like `igraph`, or exported for use in other tools like Gephi.

## Scripts Overview

-   `full_import.R`: The master script for ingesting, detecting, and merging raw Telegram JSON data from multiple files.
-   `add_node_ids.R`: Creates standardized unique IDs for network nodes (channels, users) from source, forward, and mention data.
-   `add_link_id.R`: Extracts and formats explicit `t.me` links found within message text to identify additional network edges.
-   `build_node_dict.R`: Consolidates all node information into a master attribute table or dictionary for clean network analysis.
-   `add_lang.R`: Performs robust, multi-method language identification on message text.
-   `keyword_label.R`: Applies a list of regex patterns to label messages with relevant keywords, producing a sparse matrix of results.
-   `ref/`: A directory containing helper scripts specifically for parsing the two different Telegram JSON formats.

## Requirements

The scripts are written in R. Key package dependencies include:
-   `dplyr`
-   `jsonlite`
-   `parallel` & `pbmcapply`
-   `fst`
-   `data.table`
-   `cld2`, `cld3`, `fastText` (for language detection)
-   `Matrix`

## Usage

1.  Configure the input and output paths within the relevant scripts (e.g., `full_import.R`, `keyword_label.R`).
2.  Place your raw Telegram JSON data in the specified input directory.
3.  Execute the R scripts in the order described in the **Workflow Pipeline** section.
4.  Use the resulting `.fst` and `.rds` files for your downstream analysis tasks.

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.
