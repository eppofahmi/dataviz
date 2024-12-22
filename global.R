# global.R
library(shiny)
library(shinydashboard)
library(DT)
library(echarts4r)
library(dplyr)
library(tidyverse)

source("helpers/preprocessor.R")
source("helpers/plotGenerator.R")

# Create sample dataset ----
sample_data <- tibble::tribble(
  ~province, ~region, ~year, ~month, ~sales, ~revenue, ~temperature, ~latitude, ~longitude, ~category, ~product_rating,
  "DKI Jakarta", "Java", 2023, "Jan", 2500, 50000, 28.5, -6.2088, 106.8456, "Electronics", 4.5,
  "Surabaya", "Java", 2023, "Jan", 2000, 40000, 29.0, -7.2575, 112.7521, "Electronics", 4.3,
  "Medan", "Sumatra", 2023, "Jan", 1800, 36000, 27.8, 3.5952, 98.6722, "Clothing", 4.2,
  "Makassar", "Sulawesi", 2023, "Feb", 1500, 30000, 28.2, -5.1477, 119.4327, "Electronics", 4.4,
  "Bandung", "Java", 2023, "Feb", 1700, 34000, 23.6, -6.9175, 107.6191, "Clothing", 4.6,
  "Palembang", "Sumatra", 2023, "Feb", 1400, 28000, 27.3, -2.9761, 104.7754, "Food", 4.1,
  "Banjarmasin", "Kalimantan", 2023, "Mar", 1200, 24000, 28.0, -3.3186, 114.5944, "Electronics", 4.3,
  "Denpasar", "Bali", 2023, "Mar", 1600, 32000, 27.9, -8.6500, 115.2167, "Clothing", 4.7,
  "Manado", "Sulawesi", 2023, "Mar", 1300, 26000, 27.5, 1.4748, 124.8421, "Food", 4.2,
  "Jayapura", "Papua", 2023, "Apr", 1100, 22000, 27.0, -2.5916, 140.6690, "Electronics", 4.0
)