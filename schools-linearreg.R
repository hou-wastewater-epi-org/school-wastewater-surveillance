#######################################################################
#
# HHD+Rice CDC Center of Excellence for Wastewater Epidemiology
# https://hou-wastewater-epi.org
# Contact email: info@hou-wastewater-epi.org
#
# Paper: "Wastewater surveillance of SARS-CoV-2 and influenza in preK-12 
#               schools shows school, community, and citywide infections."
#
# PI of Analytics Group: Dr. Katherine B. Ensor, Department of Statistics, Rice University
# Principal programmer for paper: Thomas Sun, Department of Statistics, Rice University
# Principal programmer for HHD: Rebecca Schneider, Houston Health Department
#
# Shared under Creative Commons License 4.0 CC BY-SA 4.0 
#              (https://creativecommons.org/licenses/by-sa/4.0/)
#
#######################################################################

######### Linear regression analysis of school WW measurements and ZIP code positivity rates

library(readxl)
library(tidyverse)
library(knitr)
library(lme4)
library(caret)

###### Load files: 

### Files contain the weekly WW measurements of SARS-CoV-2 for each school
all_data <- read_excel("example data/schools-ww-covid.xlsx")

### Files containing school names, IDs, ZIP codes, and facilities for matching
wwschoolnames <- read_excel("../example data/schools-ids.xlsx", na = "NA")
dfhisd <- read_excel("example data/schools-hisddata.xlsx")

### File contains daily COVID-19 positivity rates by ZIP code
dfwwpr <- read_excel("example data/schools-zipcodePR-covid.xlsx", na = ".")


###### Data cleaning

# Obtain school IDs
df0 <- inner_join(all_data, wwschoolnames[,c("school_id", "Facility")], by = "Facility") 

# Remove Baylor data (logistic regression data doesn't have it)
df0 <- df0[df0$Lab=="Rice" & df0$`Current Result`!="Not Sampled", ] 

# Match school to ZIP code
df <- inner_join(df0, dfhisd[,c("school_id", "Zip5")], by = c("school_id" = "school_id"))

# Maxwell to Chemagic adjustment for dates 12-7-20 thru 1-25-21 based on regression coefficient 1.7989
maxwelldates <- as.POSIXlt(seq(as.Date("2020-12-07 UTC"), as.Date("2021-01-25 UTC"), by="day"), tz = "UTC")

adj <- 1.7989

df[as.POSIXlt(df$Date) %in% maxwelldates, c("Rep1_N1", "Rep1_N2", "Rep2_N1", "Rep2_N2")] <- df[as.POSIXlt(df$Date) %in% maxwelldates, c("Rep1_N1", "Rep1_N2", "Rep2_N1", "Rep2_N2")]*adj

# Imputation to get rid of zeros. Use max(obs value, 0.5*LOD).
df[,c("Rep1_N1", "Rep1_N2", "Rep2_N1", "Rep2_N2")] <- apply(df[,c("Rep1_N1", "Rep1_N2", "Rep2_N1", "Rep2_N2")], 2, function(x) sapply(x, function(y) max(y, 0.5*LOD, na.rm = T)))

# Make variable for sum of 4 ww measurements
df$Rep_mean <- rowMeans(cbind(df$Rep1_N1, df$Rep1_N2, df$Rep2_N1, df$Rep2_N2))

# Match with PR data
df <- left_join(df , dfwwpr[,c("Date", "Zipcode", "Positivity_Rate")], by = c("Zip5" = "Zipcode", "Date" = "Date"))
df <- df[df$Lab=="Rice" & df$`Current Result`!="Not Sampled", ] 

# Avg WW measurement grouped by ZIP code
df1 <- df %>%
  group_by(Date, Zip5) %>%
  summarise_at(.vars = c("Rep_mean", "Positivity_Rate"), .funs = mean)

# log wastewater measurement
df1$logRep_mean <- log(df1$Rep_mean+1, 10)


###### Regression analysis

m1 <- lm(Positivity_Rate ~ logRep_mean, df1)
summary(m1)

# linear trend + confidence interval
regplot <- 
  {ggplot(df1, aes(x=logRep_mean, y=Positivity_Rate) ) +
      
    geom_point() +
      
    geom_smooth(method=lm , color="blue", fill="#69b3a2", se=TRUE) +
      
    theme_bw(base_size=12) +
      
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(size = 12)) +
      
    theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) + 
      
    ggtitle("") +
      
    xlab("Average wastewater SARS-CoV-2 concentration of \n schools in same zip code (log10 copies/L)") +
    ylab("Zip Code Positivity Rate")
  
  } %>%
  
  print()






















