#load in packages specified as "imports"#
library(psych)
library(tidyverse)
library(knitr)

#read in demo data#
demo_data <- read_csv("sim_data.csv")

#load CTTeval package#
library(CTTeval)

#apply CTT_eval() function to demo data#

ctt_eval(demo_data)

#TESTING ctt_eval function using simulated data (100 participants completing a 10-item self-report scale)

#Step 1: Read in data. Data must be formatted as a dataframe. Each column should correspond to each item of the scale. Each row should represent one participants data. Scores for each item should be listed accordingly.
data <-read_csv("sim_data.csv")

#Step 2: apply the "ctt_eval" function to your data. The results will give you two seperate tables. One table for item-level statistics and one table for scale-level statistics. All indices can be used to evaluate the quality of your scale from a Classical Test Theory Perspective. Descriptive labels are applied to certain statistics to aid interpretation.
ctt_results <- ctt_eval(data)

