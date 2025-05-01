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
