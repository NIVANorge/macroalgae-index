library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(reactable)
library(htmltools)

source("options.R")
source("functions.R")

function(input, output, session) {
  
  xl_sheets <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext %in% c("xlsx","xlsm","xls"), "Please select an Excel file to read"))
    
    progress <- Progress$new(session, min=1, max=10)
    on.exit(progress$close())
    
    progress$set(message = 'Reading Excel data',
                 detail = "shouldn't take long...")
    
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
      msg <- paste0(input$selectSheet, 
                    ": does not appear to be a macroalgae observation sheet. Check your input selection")
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
  
  
  stations <- reactive({
    req(stations_ok())
    req(xl_data())
    if(stations_ok()$ok){
      return(observation_info(xl_data(), options))
    }else{
      return(NULL) #dfobs) #data.frame())
    }
  })
  
  shore_types <- reactive({
    req(stations_ok())
    req(xl_data())
    req(stations)
    if(stations_ok()$ok){
      return(observation_info(xl_data(), options))
    }else{
      return(NULL)
    }
  })
  
  

  output$stations <- renderReactable({
    req(stations_ok())
    req(stations())
    df <- stations()
    if(!is.null(df)){
          if(ncol(df)>0){
      if("date" %in% names(df)){
        df <- df %>%
          mutate(date=format.POSIXct(date)) 
      }
    }else{
      df <- data.frame(project=c(), stn_code=c(), old_code=c(), stn_name=c(), date=c(), observer=c())
    }
    
      reactable(df,
                selection = "single", 
                onClick = "select",
                style = list(fontSize = "0.8rem"),
                columns = list(
                  col = colDef(show=F),
                  project = colDef(show=T, name = options$station$project$row_name, width = 100),
                  stn_code = colDef(show=T, name = options$station$stn_code$row_name, width = 100),
                  old_code = colDef(show=F, name = options$station$old_code$row_name),
                  stn_name = colDef(show=T, name = options$station$stn_name$row_name),
                  date = colDef(show=T, name = options$station$date$row_name, width = 200),
                  observer = colDef(show=T, name = options$station$observer$row_name),
                  points = colDef(show = T, name ="Sum poeng", width = 100),
                  .selection = colDef(show=T)
                  
                ), # columns
                defaultColDef = colDef(minWidth = 150, show=F, vAlign = "bottom"),
                compact = TRUE,
                wrap = FALSE,
                fullWidth = FALSE,
                resizable = TRUE,
                bordered = TRUE,
                defaultPageSize = 15,
                highlight = TRUE,
                theme = reactableTheme(
                  headerStyle = list(background = "#f7f7f8"),
                  rowSelectedStyle=list(backgroundColor = "#c0d6e4", color = "#000")
                )
      ) # reactable
    }else{
      return(NULL)
    }    
  })
  
  stn_points <- reactive({
    req(stations_ok())
    req(stations())
    selected <- getReactableState("stations", "selected")
    
    if(is.null(selected)){
      return(NULL)
    }else{

      df <- stations()
      df <- df[selected,] 
      df <- df %>%
        select(turbid_water, sand_scouring, ice_scouring, 
               ravine, fractured, boulders, 
               steep, unspec_hard, rocks, shingle, 
               shallow_pool, large_pool, deep_pool, 
               small_pool, cave, overhang,
               other_sub, 
               points_adjust)
      df <- df %>%
        pivot_longer(cols=1:ncol(df), names_to = "Parameter", values_to="Points")
      
      df <- df %>%
        add_param_names(options)
      return(df)
    }
  })
  
  output$points_table <- renderReactable({
    req(stations_ok())
    req(stations())
    req(stn_points())
    
    df <- stn_points()
    
    if(is.null(df)){
      return(NULL)
    }else{
      reactable(df,
                sortable = F,
                style = list(fontSize = "0.8rem"),
                columns = list(
                  Parameter = colDef(show=F),
                  group = colDef(show=T, name="Gruppe"),
                  name = colDef(show=T, name="Karakteristik",),
                  Points = colDef(name="Poeng", show=T, aggregate = "sum")
                ), # columns
                defaultColDef = colDef(minWidth = 150, show=F, vAlign = "bottom"),
                compact = TRUE,
                wrap = FALSE,
                fullWidth = FALSE,
                resizable = TRUE,
                bordered = TRUE,
                defaultPageSize = 100,
                highlight = TRUE,
                groupBy = "group",
                theme = reactableTheme(
                  headerStyle = list(background = "#f7f7f8"),
                  cellPadding = "3px 3px"
                ))
    }
  })
  
  output$row_selected <- renderText({
    selected <- getReactableState("stations", "selected")
    paste0("selected: ", input$stations_rows_selected)
  })
  
  obs_data <- reactive({
    
    req(stations_ok())
    req(stations())

  
    dfstns <- stations()
    df <- xl_data()
    
    if(!is.null(df)){
      
    col1 <- df[,1] %>% 
      unlist()
    
    col_stn_first <- min(dfstns$col)
    col_max <- max(dfstns$col)
    
    
    row_match <- match(options$species_header, col1)
    row_head <- df[row_match,] %>% 
      unlist()
    df <- df[(row_match+1):nrow(df),1:col_max]
    
    col_names <- row_head[1:(col_stn_first-1)]
    col_names <- c(col_names, dfstns$stn_code)
    
    names(df) <- col_names
    
    df <- df %>%
      mutate(across(all_of(dfstns$stn_code), as.numeric))
    
    return(df)
    }else{
      return(NULL)
    }
    
  })
  
  output$observations <-renderReactable({
    
    req(stations_ok())
    req(stations())
    req(obs_data())
    
    df <- obs_data()
    
    if(is.null(df)){
      return(NULL)
    }else{
      reactable(df,
                sortable = F,
                style = list(fontSize = "0.8rem"),
                columns = list(
                  Kode = colDef(width = 60),
                  CF = colDef(width = 30),
                  SP = colDef(width = 30),
                  NB = colDef(width = 30),
                  Navn = colDef(width = 300)
                ), # columns
                defaultColDef = colDef(minWidth = 55, show=T, vAlign = "bottom"),
                compact = TRUE,
                wrap = FALSE,
                fullWidth = FALSE,
                resizable = TRUE,
                bordered = TRUE,
                defaultPageSize = 999,
                highlight = TRUE,
                theme = reactableTheme(
                  headerStyle = list(background = "#f7f7f8"),
                  cellPadding = "1px 1px"
                ))
    }
  })

}
