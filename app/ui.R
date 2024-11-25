library(shiny)
library(bslib)
library(bsicons)
library(reactable)

page_navbar( 
  title = "Macroalgae index calculations",
  id = "page", 
  nav_panel(title="Observations",
            icon = bsicons::bs_icon("file-earmark-arrow-up"),
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
                        layout_columns(
                          reactableOutput("stations"),
                          reactableOutput("points_table"),
                          col_widths = c(8,4)),
                        uiOutput("station_warning"),
                      ),  
                      accordion_panel(
                        title = "Observations",
                        icon = bsicons::bs_icon("file-earmark-spreadsheet"),  # bar-chart
                        reactableOutput("observations")
                      ),  
                      accordion_panel(
                        title = "Options", 
                        icon = bsicons::bs_icon("check2-square"), # calendar-date
                        "Options"  
                      ),  
                      
            )
            ),
  nav_panel(title="Results", 
            icon = bsicons::bs_icon("calculator"),
            "Results"
            ), 
  nav_panel(title="Information", 
            icon = bsicons::bs_icon("info-square"),
            "Information"
            )
)


