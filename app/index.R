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
xlfile <- "notes/20241111 Macroalgae Index/examples/test_2stns.xlsx"


# read types for station codes
dftype <- read.table("app/stn_index_type.txt", sep=";", header=T)

# read species lists for matching with observations
dfgrps  <- read.table("app/species_lists.txt", sep=";", header=T)

dfgrps <- dfgrps  %>%
  mutate(taxaID=row_number()) %>%
  relocate(taxaID, .before=1) %>%
  mutate(RSLA2=RSLA.1.2) %>%
  rename(RSLA1=RSLA.1.2, RSLA3 = RSLA.3, RSL4=RSL.4) %>% 
  relocate(RSLA2, .after=RSLA1)

# get all data from the Excel file
df_list <- read_excel_all(xlfile)

# get the sheet containing species observations (default = 'Strandobservasjoner')
df <- df_list[[default_sheet()]]

# get station information, including points for site characteristics
dfstns <- observation_info(df, options)

# get the species observations
dfobs <- species_data(df, dfstns, options)


# match observations with species lists
df <- match_obs_species_lists(dfstns, dfobs, dfgrps, dftype)


res <- obs_indices(df, dfstns)




