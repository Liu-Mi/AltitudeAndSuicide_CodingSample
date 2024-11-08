# Coding sample - Altitude and Suicide

Authors: Mi Liu, Timo Grötzl

This is the code used for the analysis of the paper "Altitude and Suicide".

Mi Liu was the sole author of the `code/functions.R`, `code/models.R` and `code/visualisation.R` files.
He also contributed to `code/clean_data.R`, and was the sole author from line 171 on.

## Overview of file structure

- `code`
  - `clean_data.R` cleans the data and consolidates it into a single dataset
  - `visualisation.R` creates plots for exploratory analysis
  - `functions.R` contains the functions used in the main analysis
  - `main_analysis.R` contains the main analysis
- `data`
  - omitted due to licensing restrictions
- `.gitignore` is the configuration file for what files git should ignore
- `.lintr` is the configuration file for the R code linter

## Other notes

Output files are not supplied for this coding sample.
When supplied with the data files, the `code/main_analysis.R` file will call all other code files in order.

As this analysis is ongoing, please excuse some cosmetic deficiencies, especially regarding the LaTeX output tables.
