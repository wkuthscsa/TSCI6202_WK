# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel("ui will go here"
    ),)
    
    # Main Panel ----
    # 4 minus after a comment will create a outline header
    mainPanel(
      tabsetPanel(id="main_panel", 
                  tabPanel(title="Overview", value="overview_tab", "TSCI 6202 Class activities performed during Spring 2026"),
                  tabPanel(title="Survival", value="survival_tab", 
                           plotOutput("survivalPlot1")
                  ),
                  tabPanel(title="Relationships", value="relationships_tab", 
                           plotOutput("relationshipPlot1"),plotOutput("relationshipPlot2")),
                  tabPanel(title="Map", value="map_tab",
                           tmapOutput("Geomap1")),
                  tabPanel(title="Debug", value="debug_tab")
      )
    )
  )
)