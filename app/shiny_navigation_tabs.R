# --------------Tabset with pill list navigation -------------- 
# need to remove this
library(shiny)
library(bslib)

ui <- page_fluid(
  navset_pill_list( 
    nav_panel("A", "Page A content"), 
    nav_panel("B", "Page B content"), 
    nav_panel("C", "Page C content"), 
    nav_menu( 
      "Other links", 
      nav_panel("D", "Panel D content"), 
      "----", 
      "Description:", 
      nav_item( 
        a("Shiny", href = "https://shiny.posit.co", target = "_blank") 
      ), 
    ), 
  ), 
  id = "tab" 
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)


# -------------- Tabset with tab navigation -------------- 
library(shiny)
library(bslib)

ui <- page_fluid(
  navset_tab( 
    nav_panel("A", "Page A content"), 
    nav_panel("B", "Page B content"), 
    nav_panel("C", "Page C content"), 
    nav_menu( 
      "Other links", 
      nav_panel("D", "Panel D content"), 
      "----", 
      "Description:", 
      nav_item( 
        a("Shiny", href = "https://shiny.posit.co", target = "_blank") 
      ), 
    ), 
  ), 
  id = "tab" 
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)

# -------------- Vertically collapsing accordion panels -------------- 

library(shiny)
library(bslib)

ui <- page_fluid(
  accordion(  
    accordion_panel( 
      title = "Section A", 
      icon = bsicons::bs_icon("menu-app"),
      "Section A content"
    ),  
    accordion_panel(
      title = "Section B",
      icon = bsicons::bs_icon("sliders"),
      "Section B content"
    ),  
    accordion_panel(
      title = "Section C",
      icon = bsicons::bs_icon("bar-chart"),
      "Section C content"
    ),  
    accordion_panel(
      title = "Section D",
      icon = bsicons::bs_icon("calendar-date"), 
      "Section D content" 
    ),  
    id = "acc",  
    open = "Section A"  
  )  
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)

