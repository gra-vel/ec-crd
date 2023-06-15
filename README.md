# ec-crd

### Libraries
- tidyverse
- openxlsx
- Rcpp
- readxl
- lubridate

### Description
This project was designed to gather data from multiple sources for the CRD update. There is a script for each risk area in the CRD, with each function in these scripts specifically built to extract data from a single source. Some functions can collect data for multiple indicators. No function is designed to download the source data; this process has be executed manually.

The structure of most of the functions in the risk area scripts follows these steps:

- Retrieve historic data
- Retrieve new data
- Collect new data
- Adjust data for geographic information (when necessary)
- Join historic data with new data
- Save updated data
- Save log file with information about recent update

### Folders and files
`source data` - folders with raw data from each source

`final data` - database with xlsx files for each risk area in CRD. Folders contain datasets (csv files) for each indicator in risk area.

`complement` - complementary functions to process raw data

`areas` - scripts for each risk area

`CRD_pr.R` - main script

`log.xlsx` - log file for updates

### Risk areas
1 - Displacement and migration

2 - Social cohesion, equality & nondiscrimination

3 - Economic stability

4 - Internal security

5 - Justice & Rule of Law

6 - Public health

7 - Infrastructure & access to social services

8 - Environment & climate

9 - Food security & agriculture

10 - Gender equality
