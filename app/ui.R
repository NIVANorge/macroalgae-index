library(shiny)
library(bslib)
library(bsicons)

page_navbar( 
  title = "Macroalgae index calculations", 
  id = "page", 
  nav_panel("A", 
    accordion(id="setup",
              open = "Choose Excel file",  
    accordion_panel( 
      title = "Import Excel data",  
      icon = bsicons::bs_icon("file-earmark-excel"), # menu
      layout_columns(
        fileInput("file1", 
                "Choose a file containing macroalgae observations",
                accept=c(".xlsx",".xls",".xlsm"),
                width='100%'),
       uiOutput("selectSheet"),
       col_widths = c(4,4))
    ),  
    accordion_panel(
      title = "Stations",
      icon = bsicons::bs_icon("geo-alt"), # sliders
      uiOutput("station_warning"),
      tableOutput("stations")
    ),  
    accordion_panel(
      title = "Observations",
      icon = bsicons::bs_icon("file-earmark-spreadsheet"),  # bar-chart
      tableOutput("contents")
    ),  
    accordion_panel(
      title = "Options", 
      icon = bsicons::bs_icon("check2-square"), # calendar-date
      "Section D content"  
    ),  
    
  )  
),
nav_panel("B", "Page B content"), 
nav_panel("C", "Page C content"))






