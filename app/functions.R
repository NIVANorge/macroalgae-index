

stn_param_values <- function(row_ix, df, x="value", type=NA_character_, first_col=2){
  res  <- df[row_ix,first_col:ncol(df)] %>%
    pivot_longer(cols=everything(), names_to = "col", values_to = "value") %>%
    mutate(col=stringr::str_remove_all(col, "\\.") %>% as.numeric())
  res <- res %>%
    filter(!is.na(value))
  return(res)
}



read_excel_sheet <- function(sheet, xl_path){
  require(readxl)
  df <- readxl::read_excel(sheet=sheet, path=xl_path, col_names = F)
  return(df)
}

read_excel_all <- function(xl_path, progress=NULL){
  require(readxl)
  sheets <- readxl::excel_sheets(xl_path)
  
  list_df <- list()

  for(i in 1:length(sheets)){
    df <- read_excel_sheet(sheets[i], xl_path)
    list_df[[i]] <- df
    if(!is.null(progress)){
      progress$set(value = i+1)
    }
  }
  #list_df <- lapply(sheets, read_excel_sheet, xl_path)
  names(list_df) <- sheets
  return(list_df)
}

default_sheet <- function(){
  return("Strandobservasjon")
} 


defaults <-function(){
  read.table("defaults.csv",sep=";",header=T)
}


