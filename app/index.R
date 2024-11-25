library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(openxlsx)

source("app/functions.R")

options <- get_options("app/options.R")

xlfile <- "notes/20241111 Macroalgae Index/examples/test2.xlsx"

# get all data from the Excel file
df_list <- read_excel_all(xlfile)

# get the sheet containing species observations (default = 'Strandobservasjoner')
df <- df_list[[default_sheet()]]

# get station information, including points for site characteristics
dfstns <- observation_info(df, options)

# get the species observations
dfobs <- species_data(df, dfstns, options)

# read species lists for matching with observations
dflist <- read.table("app/species_lists.txt", sep=";", header=T)
