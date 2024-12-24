# global.R
library(shiny)
library(shinydashboard)
library(DT)
library(echarts4r)
library(dplyr)
library(tidyverse)

# pie col line
source("helpers/01-DataColPieLine.R")
source("helpers/02-GeneratorColPieLine.R")

# map
source("helpers/03-DataMap.R")
source("helpers/04-GeneratorMap.R")

# boxplot
source("helpers/05-DataBoxPlot.R")
source("helpers/06-GeneratorBoxPlot.R")

# scatter
source("helpers/07-DataScatter.R")
source("helpers/08-GeneratorScatter.R")

# Create sample dataset ----
sample_data = read_csv("data/sampleData.csv")
# glimpse(sample_data)
