# griffins

## Overview

`griffins` is an R-based analysis bundle for combining Amsterdam demographic data with health-related event data and producing stratified rates, ratios, and maps.

The code appears to focus on:
- building analysis-ready demographic tables
- standardizing event rates across population subgroups
- comparing tests, hospitalizations, deaths, and escalation events
- generating summary plots and geographic visualizations by neighborhood and district

The repository is organized around reusable analysis functions plus scripts for data preparation, results generation, and mapping.

## Linked Datasets

This bundle references several CBS- and Amsterdam-related source datasets and crosswalks. Based on the code previews, the main inputs are:

- **Demographic stapeling data**
  - Used to construct the core population table
  - Includes age, sex, income, SES, household, and neighborhood attributes
  - Read from parquet files under `H:/data/demog/...`

- **Demog stapeling raw / extended attributes**
  - Adds household composition, Wmo indicators, and other socio-demographic fields
  - Loaded via helper functions from external utilities

- **Amsterdam wijk/stadsdeel crosswalk**
  - Excel mapping between `WK_CODE`, `Stadsdelen`, and `AMS_Wijk 22`
  - Used to translate CBS neighborhood codes into analysis groupings
  - Also exported as `data/tijn/wijk22_stadsdeel_mapping.csv`

- **Geographic boundary file**
  - Shapefile for Amsterdam neighborhoods (`wk_2021.shp`)
  - Used to build choropleth maps for wijk22 and stadsdeel levels

- **Event datasets**
  - Positive tests
  - Hospitalizations
  - Deaths
  - Escalation events (`escalaties_pos_test.parquet`)
  - These are joined to the demographic data to compute crude and standardized rates

### What the datasets add

- **Demographic data** provides denominators and stratification variables.
- **Event data** provides numerators for rates and ratios.
- **Crosswalks and shapefiles** enable consistent geographic aggregation and mapping.
- **Extended socio-economic fields** support subgroup analysis by income, SES, household type, Wmo use, and neighborhood.

## Repository Structure

```text
src/
  analysis_functions.R   # Core rate/ratio computation helpers
  data_functions.R       # Demographic wrangling and input preparation
  inputs.R               # Shared variable definitions and package/source setup
  maps.R                 # Spatial joins and map generation
  results.R              # Example result generation and plotting workflow
```

## Source Code Summary

### `src/inputs.R`
- Loads shared utility scripts and R packages.
- Defines the key variable lists used throughout the project:
  - demographic columns
  - LBZ medical variables
  - medication/ATC groupings
  - disease groupings such as diabetes, metabolic disease, and heart disease
  - COVID diagnosis codes
- Acts as the central configuration file for the analysis pipeline.

### `src/data_functions.R`
- Prepares the demographic analysis table.
- Reads and formats stapeling data for each year.
- Filters to Amsterdam (`gem == "0363"`).
- Creates derived categories such as:
  - `leeftijd_8`
  - `leeftijd_3`
  - `seswoa_small`
  - `inkomen_klasse_small`
  - household groupings
  - Wmo indicator
- Joins in wijk/stadsdeel crosswalk data and raw stapeling attributes.

### `src/analysis_functions.R`
- Contains reusable functions for rate calculations.
- `add_file_year()` adjusts file-year assignment depending on whether the analysis is by wave or week.
- `compute_rates()`:
  - builds population denominators
  - counts events by subgroup and time unit
  - merges denominators and standard-population weights
  - computes crude rates
  - computes standardized rates using direct standardization
- The preview suggests the file is intended to support multiple event types and subgroup comparisons.

### `src/results.R`
- Demonstrates how the analysis functions are used in practice.
- Loads prepared parquet outputs for:
  - demographic data
  - tests
  - escalations
- Defines subgroup options and standardization combinations.
- Runs escalation-rate calculations and produces plots such as:
  - ratio plots by wave
  - scatter plots comparing standardized rates
- Includes commented examples for alternative stratifications and plotting workflows.

### `src/maps.R`
- Builds geographic outputs from wijk and stadsdeel data.
- Reads the wijk22/stadsdeel crosswalk and Amsterdam shapefile.
- Aggregates geometries to wijk22 and stadsdeel levels.
- Joins map geometries with result tables from Excel outputs.
- Produces choropleth maps, including test-to-hospitalization ratios by wave.

## Output Artifacts

No output files were included in the uploaded bundle, but the code indicates the project is designed to generate:

- **CSV mapping files**
  - e.g. `data/tijn/wijk22_stadsdeel_mapping.csv`

- **Excel result tables**
  - e.g. `rates_ratios_leeftijd_3_geslacht.xlsx`

- **Plots**
  - wave-based ratio plots
  - scatter plots of standardized rates
  - choropleth maps saved as PNG files

- **Intermediate parquet-based analysis tables**
  - demographic and event datasets prepared for downstream analysis

## Next Steps

- Add a short project description in the repository root to clarify the analysis goal.
- Document the required input datasets, file locations, and expected schemas.
- Replace hard-coded local paths (`H:/`, `K:/`) with configurable project paths.
- Add a reproducible pipeline entry point, such as:
  - an R script
  - `targets`
  - or `renv`-backed workflow
- Include example outputs or screenshots of the main plots and maps.
- Add function-level documentation for the core analysis helpers.
- Consider adding a minimal synthetic example dataset so the workflow can be run without access to internal data.
