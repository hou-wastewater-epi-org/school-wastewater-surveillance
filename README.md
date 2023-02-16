# HHD+Rice CDC Center of Excellence for Wastewater Epidemiology
https://hou-wastewater-epi.org

Contact email: info@hou-wastewater-epi.org

Paper: “Wastewater surveillance of SARS-CoV-2 and influenza in preK-12 schools shows school, community, and citywide infections."

PI of Analytics Group: Dr. Katherine B. Ensor, Department of Statistics, Rice University

Principal programmer for paper: Thomas Sun, Department of Statistics, Rice University

Principal programmer for HHD: Rebecca Schneider, Houston Health Department

## Description
Code for analysis and results of "Wastewater surveillance of SARS-CoV-2 and influenza in preK-12 schools shows school, community, and citywide infections". 

### Code

The following R files are included to produce the analyses used in the paper:

* `formatting-plot-funs.R`, contains helper functions for producing nice plots.
* `schools-eda.R`, exploratory data analysis including boxplots by grade level, etc.
* `schools-linearreg-flu.R`, linear regression analysis for citywide discharge diagnosed Influenza rates versus school Influenza wastewater data.
* `schools-linearreg.R`, linear regression analysis for ZIP code level COVID-19 positivity rates versus school SARS-CoV-2 wastewater data.
* `schools-logisticreg.R`, logistic regression analysis for school positive test results versus school SARS-CoV-2 wastewater data.

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)



### Data

Fake example data is provided in the `Data` folder to produce analysis and figures similar to those found in the paper. The names, ZIP codes, measurements, etc. have been replaced with arbitrary values. The datasets used as input for the analysis are:

* `city-weeklyrate-flu.xlsx`, citywide weekly percentage of visits with discharge diagnosed Influenza.
* `schools-hisddata.csv`, metadata of HISD schools including enrollment.
* `schools-ids.csv`, IDs and names of schools for matching.
* `schools-ww-covid.xlsx`, weekly school SARS-CoV-2 wastewater concentrations and positivity rates.
* `schools-ww-flu.xlsx`, weekly school Influenza wastewater concentrations and positivity rates.
* `schools-zipcodePR-covid.xlsx`, daily COVID-19 positivity rates at the ZIP code level.

[![CC BY-NC-SA 4.0][cc-by-nc-sa-shield]][cc-by-nc-sa]



[cc-by-nc-sa]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[cc-by-nc-sa-shield]: https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg


### Licensing
Because code and intellectual work have different licensing needs, a separate `LICENSE` file is contained in each folder and applies to the files in that folder:

- Files in the `Code` folder are licensed under the GNU General Public License, Version 3 (GPL-3).

- Files in the `Data` folder are licensed under the Creative Commons NonCommercial ShareAlike (CC by-NC-SA) license.

We are happy to discuss the possibility of an alternate (dual) license for the files in either folder if you encounter a situation where your work's existing licenses are incompatible with our choices. Please reach out to  info@hou-wastewater-epi.org.
