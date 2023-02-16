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

# Logistic regression analysis of school WW measurements and school positivity rates

library(readxl)
library(tidyverse)
library(knitr)
library(lme4)
library(caret)

####### Load files: 

### File contains the weekly WW measurements and positivity rate of SARS-CoV-2 for each school
ww0 <- read_excel("./Data/schools-ww-covid.xlsx")

###### Data cleaning

### Imputation to get rid of zeros. Use max(obs value, 0.5*LOD).

LOD <- 2556.4

ww <- ww0

# Imputation
ww[,c("Rep1_N1", "Rep1_N2", "Rep2_N1", "Rep2_N2")] <- apply(ww[,c("Rep1_N1", "Rep1_N2", "Rep2_N1", "Rep2_N2")],
                                                            2,
                                                            function(x) 
                                                              sapply(x, function(y) max(y, 0.5*LOD, na.rm = T))
                                                            )

# Average N1 and N2 for each sample
ww$Rep1_mean <- rowMeans(cbind(ww$Rep1_N1, ww$Rep1_N2))
ww$Rep2_mean <- rowMeans(cbind(ww$Rep2_N1, ww$Rep2_N2))

# Average of 4 ww measurements
ww$Rep_mean <- rowMeans(cbind(ww$Rep1_N1, ww$Rep1_N2, ww$Rep2_N1, ww$Rep2_N2))
ww$logRep_mean <- log(ww$Rep_mean, 10)


###### Tables

# make binary variable (1 for positive PR, 0 for 0 PR)
ww$positive <- ww$Total_PR > 0

ww$result <- ifelse(ww$`Current Result` %in% c("High Positive", "Positive") == 1, 1, 0)
ww$resultinc <- ifelse(ww$`Current Result` %in% c("High Positive", "Positive", "Inconclusive") == 1, 1, 0)
wwnoinc <- ww[ww$`Current Result` != "Inconclusive",]

# Confusion matrix
cm3 <- confusionMatrix(factor(wwnoinc$result), factor(as.numeric(wwnoinc$positive)), positive = "1", dnn = c("Wastewater", "Positive Result"))
cm3



###### Logistic regression, positivity rate (y) on wastewater (x)

m1<-glm(positive ~ logRep_mean,data=ww,family="binomial")
m1s <- summary(m1)
m1s
m1coef <- m1s$coefficients[2,1]
m1pv <- m1s$coefficients[2,4]

labs2 <- c("Not Detected", "Detected")

# get fitted line
newx <- seq(from = min(ww$logRep_mean), to = max(ww$logRep_mean), by = .1)
MyData=data.frame(logRep_mean= newx)
preds <- predict(m1, newdata = MyData, type = "link", se.fit = TRUE)
critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit
fit2 <- m1$family$linkinv(fit)
upr2 <- m1$family$linkinv(upr)
lwr2 <- m1$family$linkinv(lwr)
MyData$lwr <- lwr2 
MyData$upr <- upr2 

logregplot <- 
  {ggplot(data=ww,
         mapping=aes(x=logRep_mean,y=as.numeric(positive))) + geom_jitter(height=.01,width=0) +         
    
    stat_smooth(method="glm", method.args=list(family=binomial)) +
  
    theme_bw(base_size=12) +
  
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  
    xlab("Wastewater Concentration (log10, copies/L)") + ylab("Probability of Nonzero Positivity Rate") +
  
    scale_y_continuous(
      sec.axis = sec_axis(~.,
                          breaks = 0:1,
                          labels = labs2))
  } %>%
  
  print()
  
  










