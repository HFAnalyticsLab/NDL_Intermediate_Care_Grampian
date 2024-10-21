# packages

library(tidyverse) 
library(ggridges) #
library(janitor) # clean names
library(knitr) # for markdown tables
library(glue) 
# library(spatstat) # weighted median
library(PHEindicatormethods) 
library(readxl)
library(viridis) # colour blind pallete
library(comorbidity)

source("int_hosp_codes_names.R")

# theme for ggplot i.e + theme_intcare() 

theme_intcare <- function() {theme_set(theme_classic()) +
    theme(legend.title=element_blank(),
          legend.position="top",
          legend.text = element_text(size=14),
          axis.text = element_text(size=11),
          axis.title.y = element_text(size=14))}

# annotate ggplot with covid period i.e. + annotate_intcare()

annotate_intcare <- function( ) {annotate("rect", xmin = as.POSIXct('2020-03-01'),
                                          xmax = as.POSIXct('2022-01-31'),
                                          ymin = -Inf,
                                          ymax = Inf,
                                          alpha = 0.2, fill = "gray") }