# HHD+Rice CDC Center of Excellence for Wastewater Epidemiology
https://hou-wastewater-epi.org

Contact email: info@hou-wastewater-epi.org

Paper: “Wastewater surveillance of SARS-CoV-2 and influenza in preK-12 schools shows school, community, and citywide infections."

PI of Analytics Group: Dr. Katherine B. Ensor, Department of Statistics, Rice University

Principal programmer for paper: Thomas Sun, Department of Statistics, Rice University

Principal programmer for HHD: Rebecca Schneider, Houston Health Department

Shared under Creative Commons License 4.0 CC BY-SA 4.0 (https://creativecommons.org/licenses/by-sa/4.0/)

## Description
Code for analysis and results of "Wastewater surveillance of SARS-CoV-2 and influenza in preK-12 schools shows school, community, and citywide infections". 

### Code

The following R files are included to produce the analyses used in the paper:

* `formatting-plot-funs.R`, contains helper functions for producing nice plots.
* `schools-eda.R`, exploratory data analysis including boxplots by grade level, etc.
* `schools-linearreg-flu.R`, linear regression analysis for citywide discharge diagnosed Influenza rates versus school Influenza wastewater data.
* `schools-linearreg.R`, linear regression analysis for ZIP code level COVID-19 positivity rates versus school SARS-CoV-2 wastewater data.
* `schools-logisticreg.R`, logistic regression analysis for school positive test results versus school SARS-CoV-2 wastewater data.

### Data

Fake example data is provided in the `example data` folder to produce analysis and figures similar to those found in the paper. The names, ZIP codes, measurements, etc. have been replaced with arbitrary values. The datasets used as input for the analysis are:

* `city-weeklyrate-flu.xlsx`, citywide weekly percentage of visits with discharge diagnosed Influenza.
* `schools-hisddata.csv`, metadata of HISD schools including enrollment.
* `schools-ids.csv`, IDs and names of schools for matching.
* `schools-ww-covid.xlsx`, weekly school SARS-CoV-2 wastewater concentrations and positivity rates.
* `schools-ww-flu.xlsx`, weekly school Influenza wastewater concentrations and positivity rates.
* `schools-zipcodePR-covid.xlsx`, daily COVID-19 positivity rates at the ZIP code level.


[![CC BY-SA 4.0][cc-by-sa-shield]][cc-by-sa]

[![CC BY-SA 4.0][cc-by-sa-image]][cc-by-sa]

[cc-by-sa]: http://creativecommons.org/licenses/by-sa/4.0/
[cc-by-sa-image]: https://licensebuttons.net/l/by-sa/4.0/88x31.png
[cc-by-sa-shield]: https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg
