library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(openxlsx)

source("app/options.R")
source("app/functions.R")

xlfile <- "notes/20241111 Macroalgae Index/examples/test2.xlsx"

df <- readxl::read_excel(path=xlfile, col_names=F, col_types = "text")


df_list <- read_excel_all(xlfile)

df <- df_list[[default_sheet()]]

dfStns <- observation_info(df, options)


