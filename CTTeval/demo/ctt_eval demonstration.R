#load in packages specified as "imports"#
library(psych)
library(tidyverse)
library(knitr)

#read in demo data#
demo_data <- read_csv("sim_data.csv")

#load CTTeval package#
library(CTTeval)

#TESTING ctt_eval function using demo data (100 participants completing a 10-item self-report scale)
ctt_eval(demo_data)

