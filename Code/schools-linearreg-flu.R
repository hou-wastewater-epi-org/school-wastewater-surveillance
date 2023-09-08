#######################################################################
#
# HHD+Rice CDC Center of Excellence for Wastewater Epidemiology
#?https://hou-wastewater-epi.org
# Contact email:?info@hou-wastewater-epi.org
#
# Paper: "Wastewater surveillance of SARS-CoV-2 and influenza in preK-12?
#               schools shows school, community, and citywide infections."
#
# PI of Analytics Group: Dr. Katherine B. Ensor, Department of Statistics, Rice University
# Principal programmer for paper: Thomas Sun, Department of Statistics, Rice University
# Principal programmer for HHD: Rebecca Schneider, Houston Health Department
#
# Code is shared under a GPL-3 License. See LICENSE file in Code folder.
# Data is shared under a CC by-NC-SA license. see LICENSE file in Data folder.
#
#######################################################################

# Linear regression analysis of school flu WW measurements and citywide influenza rates

library(readxl)
library(tidyverse)
library(knitr)

###### Load (synthetic data) files: 

### File contains weekly school WW flu measurements
wwflu0 <- read_excel("./Data/schools-ww-flu.xlsx")

### File contains weekly citywide discharge diagnosed influenza rates
flurate0 <- read_excel("./Data/city-weeklyrate-flu.xlsx")

###### Data cleaning

wwflu <- wwflu0[wwflu0$`Facility Type`=="School",]

# Match WW measurement date to reported citywide date 2 days prior
wwflu$Date <- wwflu$Date - 172800

# Calculate proportion of schools with positive wastewater detections for each week
wwflu$flu_pos <- ifelse(wwflu$`Current Result` %in% c("Positive") == T, 1, 0)

wwflu1 <- wwflu %>%
  group_by(Date) %>%
  summarise_at(.vars = c("flu_pos"), .funs = mean)

# Join WW measurement with flu rate
df <- inner_join(wwflu1 , flurate0, by = c("Date" = "Week Start Date"))


###### Regression analysis

names(df)[names(df) == 'Percentage of Visits with DD Influenza'] <- "dd"

m1 <- lm(dd ~ flu_pos, df)
summary(m1)

# linear trend + confidence interval
regplotflu <- 
  {ggplot(df, aes(x=flu_pos, y=dd) ) +
      
    geom_point() +
      
    geom_smooth(method=lm , color="yellowgreen", fill="#69b3a2", se=TRUE) +
      
    theme_bw(base_size=12) +
      
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_text(size = 16), text = element_text(size=16)) +
    
      theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+ 
      
    xlab("Proportion of schools with positive wastewater detections") + 
    ylab("Citywide % visits with discharge \n diagnosed influenza")
  } %>%
  
  print()





