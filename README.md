## Descrption
This is code of a current Research project at the University of St.Gallen and is not finished. The repository contains R skripts for the data gathering, analysis and visualization regarding zombie firms. The data consists of quarterly and yearly financial data (Compustat) and market data (CRSP) for all US firms.

## Overview:

### 1 queries:
  - Establish connection to Wharton Research Data Services (WRDS) interface
  - Retrieve different datasets from Compustat and CRSP databases with interrelated queries and first adjustments to save the raw data

### 2 data wrangling:
  - Combine with other datasets (e.g. inflation data from FRED)
  - Clean data and adjust scales
  - Approximate and fill missing values

### 3 calculations:
  - Make calculations and create new variables
  - Apply bankrupcty models (O-score, CHS model) and economic profit figure

### 4 analysis:
  - Adjustments to data after calculations
  - Workbench of all code for visualizations (no final script, just archive of previous code)
