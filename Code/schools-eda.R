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

######### Plotting school samples by grade and enrollment from collated data sheet
library(tidyverse)
library(readxl)

source('Code/formatting-plot-funs.R') # Source the general_functions file

title_name <- 'Schools ordered by enrollment_7-April-22'

###### Load files: 

### Files contain the weekly WW measurements of SARS-CoV-2 and Influenza for each school
all_data <- read_excel("./Data/schools-ww-covid.xlsx")
flu_all_data <- read_xlsx('./Data/schools-ww-flu.xlsx')

### File containing school names and IDs for matching
school_name_number <- read_csv("./Data/schools-ids.csv") %>%
  rename(School_Number = Facility)

### File with school names and short names, as well as enrollment
school_metadata <- read_csv('./Data/schools-hisddata.csv') %>% 
  # reducing redundant columns -- Sharpstown International only one different : Elementary/Secondary and High school
  select(-Grade_Level) %>% rename(Grade_Level = Grade_Level_2) 


###### Data cleaning

school_metadata <- school_metadata %>%  # retain Grade_Level_2 and rename to Grade_Level 
  inner_join(school_name_number, by = c('Facility' = 'School_Code'))

LOD <- 2556.4

# filter rice only data that is positive or inconclusive
school_rice.data <- 
  
  all_data %>%
  
  # Filter relevant data
  filter(Lab == 'Rice',             # select 'Rice' data
         !(`Current Result` %in% c('Negative', 'Inconclusive')),      # positive only
         `Facility Type` == 'School') %>%   # only from Schools
  # filter(Lab == 'Rice',             # select 'Rice' data
  #      `Facility Type` == 'School') %>%   # only from Schools
  
  # imputation
  mutate(across(c("Rep1_N1", "Rep1_N2", "Rep2_N1", "Rep2_N2"),
                function(x) sapply(x, function(y) max(y, 0.5*LOD, na.rm = T)))) %>%
  
  # Attach school metadata
  inner_join(school_metadata, by = 'Facility') %>% 
  
  mutate(School_Number = factor(School_Number)) %>%
  
  # Order by enrollment
  arrange(Enrollment_as_Oct2019) %>% 
  mutate(across(c('Facility', 'School_Number'), # freeze the order of samples 
                ~ fct_inorder(factor(.x)))) %>% 
  
  # Collect all replicates and N1-N2 under the same column
  pivot_longer(cols = starts_with('Rep'),
               names_to = 'Replicate',
               values_to = 'Copies_per_litre') %>% 
  
  # order the grade levels for proper plotting
  mutate(across(Grade_Level, ~ fct_relevel(.x, 'Elementary', 'Middle', 'High School')))

# Average replicates and N1, N2
school_averaged <- 
  group_by(school_rice.data, across(-c(Copies_per_litre, Replicate))) %>% 
  summarize(across(Copies_per_litre, mean, na.rm = TRUE)) # takes the mean of stuff

# function to label the number of data points on the plot
give.n <- function(x){ 
  return(c(y = mean(x), label = length(x)))
}


# Same but for flu data

# filter rice only data that is positive or inconclusive
school_flu.data <- 
  
  flu_all_data %>%
  
  # Filter relevant data
  filter(Lab == 'Rice',             # select 'Rice' data
         !(`Current Result` %in% c('Negative', 'Inconclusive')),      # positive only
         `Facility Type` == 'School') %>%   # only from Schools
  
  # Attach school metadata
  left_join(school_metadata, by = 'Facility') %>% 
  
  mutate(School_Number = factor(School_Number)) %>%
  
  # Order by enrollment
  arrange(Enrollment_as_Oct2019) %>% 
  mutate(across(c('Facility', 'School_Number'), # freeze the order of samples 
                ~ fct_inorder(factor(.x)))) %>% 
  
  # Leave out columns
  select(-Rep1_B, -Rep2_B) %>% # leaving out Influenza B samples ; mostly 0's or below LOD
  
  # Collect all replicates of Influenza-A under the same column
  pivot_longer(cols = starts_with('Rep'),
               names_to = 'Replicate',
               values_to = 'Copies_per_litre') %>% 
  
  # leave out rows with 0 Copies_per_litre
  filter(Copies_per_litre != 0) %>% 
  
  # order the grade levels for proper plotting
  mutate(across(Grade_Level, ~ fct_relevel(.x, 'Elementary', 'Middle', 'High School')))

# Average replicates and N1, N2
school_flu.avg <- 
  group_by(school_flu.data, across(-c(Copies_per_litre, Replicate))) %>% 
  summarize(across(Copies_per_litre, mean, na.rm = TRUE)) # takes the mean of stuff

# CoV2 and Flu

joined.avg <- list(school_averaged, school_flu.avg) %>% 
  map2(., c('SARS-CoV2', 'Influenza-A'),
       ~ mutate(.x, Virus = .y)) %>%  # add a column for the virus type
  
  bind_rows() # Join by common columns, other cols filled with NA 


###### Plots

### Boxplot of CoV by school level
set.seed(1)
plt.avg <- 
  {ggplot(school_averaged,
          aes(x = Grade_Level, y = Copies_per_litre,
              fill = Grade_Level,
              label = Facility)) + 
      
      # boxplot
      geom_boxplot(outlier.shape = NA, alpha = .6) +
      
      # points, black outline, semi transparent, position matches the boxplot, scattered along x-axis for visual 
      geom_point(shape = 21,
                 alpha = 0.2, 
                 position = position_jitterdodge(dodge.width = .75, 
                                                 jitter.width = .25, jitter.height = 0)) + 
      # # text labels for number of points
      # stat_summary(fun.data = give.n, geom = 'text',
      #              position = position_dodge2(width = .75)) +
      
      # annotations
      annotation_logticks(sides = 'l') +  # tick marks to indicate log y axis
      
      theme(legend.position = 'none', text = element_text(size = 18)) +
      
      # ggtitle('') +
      xlab("") + ylab("Copies/L-WW")
    
  } %>% 
  
  format_logscale_y() %>%
  
  print()


### Plot of CoV by each school - ordered by enrollment
plt.eachschool <- 
  {ggplot(school_averaged,
          aes(x = Copies_per_litre, y = School_Number,
              label = Facility)) + 
      
      # points, black outline, semi transparent, position matches the boxplot, scattered along x-axis for visual 
      geom_point(alpha = 0.4, colour = 'black') + # colour points light red
      
      # annotations
      annotation_logticks(sides = 'b') +  # tick marks to indicate log y axis
      
      geom_text(aes(x = 3e2, label = Enrollment_as_Oct2019),
                colour = '#5d7896',
                show.legend = F, size = 4.7) + 
      
      # ggtitle('',
      #         subtitle = 'School enrollment numbers as of Oct 2019 are shown on the left') +
      ylab('School') + xlab("Copies/L-WW") +
      
      theme(legend.position = 'top', text = element_text(size = 20))
    
  } %>% 
  
  format_logscale_x() %>%
  
  print()

### Boxplot of CoV and Flu
set.seed(1)
plt_CoV.flu.avg <- 
  {ggplot(joined.avg,
          aes(x = Virus, y = Copies_per_litre,
              fill = Virus,
              label = Facility)) + 
      
      # boxplot
      geom_boxplot(outlier.shape = NA, alpha = .6) +
      
      # points, black outline, semi transparent, position matches the boxplot, scattered along x-axis for visual 
      geom_point(shape = 21,
                 alpha = 0.2,
                 position = position_jitterdodge(dodge.width = .75, 
                                                 jitter.width = .12, jitter.height = 0)) + 
      # text labels for number of points
      # stat_summary(fun.data = give.n, geom = 'text',
      #              position = position_dodge2(width = .75)) +
      
      # annotations
      annotation_logticks(sides = 'l') +  # tick marks to indicate log y axis
      
      # ggtitle('',
      #         subtitle = '') +
      ylab("Copies/L-WW") +
      
      theme(legend.position = 'top')
    
  } %>% 
  
  format_logscale_y() %>%
  
  print()


### Boxplot of CoV and Flu by grade
plt_CoV.flu.avg_grade <- 
  {ggplot(joined.avg,
          aes(x = Virus, y = Copies_per_litre,
              fill = Grade_Level,
              label = Facility)) + 
      
      # boxplot
      geom_boxplot(outlier.shape = NA, alpha = .6) +
      
      # points, black outline, semi transparent, position matches the boxplot, scattered along x-axis for visual 
      geom_point(shape = 21,
                 alpha = 0.2, 
                 position = position_jitterdodge(dodge.width = .75, 
                                                 jitter.width = .12, jitter.height = 0)) + 
      # text labels for number of points
      # stat_summary(fun.data = give.n, geom = 'text',
      #              position = position_dodge2(width = .75)) +
      
      # annotations
      annotation_logticks(sides = 'l') +  # tick marks to indicate log y axis
      
      scale_fill_discrete(name = "Grade Level") +
      
      # ggtitle('',
      #         subtitle = '') +
      ylab("Copies/L-WW") +
      
      theme(legend.position = 'top')
    
  } %>% 
  
  format_logscale_y() %>%
  
  print()

