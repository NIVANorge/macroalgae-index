library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)

source("functions.R")

function(input, output, session) {
  
  xl_sheets <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext %in% c("xlsx","xlsm","xls"), "Please upload an Excel file"))
    
    progress <- Progress$new(session, min=1, max=10)
    on.exit(progress$close())
    
    progress$set(message = 'Reading Excel data',
                 detail = 'This may take a while...')
    
    list_df <- read_excel_all(file$datapath, progress)
        
    return(list_df)
  })
  
  sheet_initial <- reactive({
    req(xl_sheets())
    default <- default_sheet() 
    sheets <- names(xl_sheets())
    sheets <- sheets[tolower(sheets)==tolower(default)]
    
    if(length(sheets)>0){
      return(sheets)
    }else{
      return(NULL)
    }   

  })
  
  xl_names <- reactive({
    req(xl_sheets())
    sheet_names <- names(xl_sheets())
    return(sheet_names)
  })
  
  xl_data <- reactive({
    req(xl_sheets())
    req(input$selectSheet)
    df_list <- xl_sheets()
    return(df_list[[input$selectSheet]])
  })
  
  sheet_options <- reactive({
    req(xl_names())
    req(input$selectSheet)
    df_list <- xl_sheets()
    if("Instillinger" %in% xl_names()){
      return(df_list[["Instillinger"]])
    }else{
      return(defaults())
    }
  })
  
  station_data <- reactive({
    req(xl_data())
    return(NULL)
  })
  
  
  stations_ok <- reactive({
    req(xl_data())
    req(sheet_options())

    df <- xl_data()
    
    check_rows <- c("Prosjekt",
                    "Stasjonskode",
                    "Gammel kode",
                    "Stasjonsnavn",
                    "Dato (tid)")
    
    # row_names <- df[,1]
    row_names <- df %>% purrr::pluck(1)
    row_names <- row_names[tolower(row_names) %in% tolower(check_rows)]
    
    if(length(row_names)<length(check_rows)){
      ok = FALSE
      msg <- "This does not appear to be a macroalgae observation sheet. Please check your input selection"
    }else{
      ok = TRUE
      msg <- "Station data OK"
    }
    return(list(ok=ok, msg=msg))
  })
  
  observe({
    xl_data()
    accordion_panel_open("setup","Stations")
  })
  
  output$selectSheet <- renderUI(
    tagList(selectInput(
      "selectSheet", 
      "Select sheet:", 
      choices = xl_names(),
      selected = sheet_initial(),
      selectize = T
      ))
  )
  
  output$station_warning <- renderUI({
    ok <- stations_ok()[["ok"]]
    msg <- stations_ok()[["msg"]]
    if(ok){
      return(
        tagList(
          div()
        )
      )
    }else{
      return(
        tagList(
          div(style="color:red", msg)
        )
      )
    }
    })
  
  
  output$stations <- renderTable({
    req(stations_ok())
    if(stations_ok()$ok){
      return(xl_data())
    }else{
      return(data.frame())
    }
  })
  
  output$contents <- renderTable({
    xl_data()
  })
  
   

}
