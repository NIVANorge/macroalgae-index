library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)

source("app/functions.R")
source("app/options.R")

xlfile <- "notes/20241111 Macroalgae Index/examples/test2.xlsx"

df_list <- read_excel_all(xlfile)

df <- df_list[[default_sheet()]]


check_rows <- c(row_name_project,
                row_name_stn_code,
                row_name_old_code,
                row_name_stn_name,
                row_name_date)

row_names <- df %>% 
  purrr::pluck(1)

row_ix <- check_rows %>% 
  purrr::map(match, row_names) %>% 
  unlist()

if(sum(is.na(row_ix))>0){
  msg <- check_rows[is.na(row_ix)] %>% 
    paste0(collapse=";")
  msg <- paste0("missing rows: ",msg)
  cli::cli_warn(msg)
}

row_ix_project  <- row_ix[match(row_name_project,row_names)]
row_ix_stn_code <- row_ix[match(row_name_stn_code,row_names)]
row_ix_old_code <- row_ix[match(row_name_old_code,row_names)]
row_ix_stn_name <- row_ix[match(row_name_stn_name,row_names)]
row_ix_date     <- row_ix[match(row_name_date,row_names)]

projects  <- df[row_ix_project,2:ncol(df)] %>%
  pivot_longer(cols=everything(), names_to = "col", values_to = "value") %>%
  mutate(col=stringr::str_remove_all(col, "\\.") %>% as.numeric())


projects  <- stn_param_values(row_ix_project, df, name="project")
stn_codes <- stn_param_values(row_ix_stn_code,df, name="stn_code")
old_codes <- stn_param_values(row_ix_old_code,df, name="old_code")
stn_names <- stn_param_values(row_ix_stn_name,df, name="stn_name")
dates     <- stn_param_values(row_ix_date, df, name="date", type="date")

# we assume that stn name and old code are not strict requirements


col_ix <- 2:ncol(df)

col_ix_project  <- col_ix[match(row_name_project,row_names)]
col_ix_stn_code <- col_ix[match(row_name_stn_code,row_names)]
col_ix_old_code <- col_ix[match(row_name_old_code,row_names)]
col_ix_stn_name <- col_ix[match(row_name_stn_name,row_names)]
col_ix_date     <- col_ix[match(row_name_date,row_names)]
