library (shiny)

fluidPage(titlePanel("Old Faithful Geyser Data"),
          
          sidebarLayout(sidebarPanel(
            sliderInput(
              "bins",
              "Number of bins:",
              min = 1,
              max = 50,
              value = 30
            )
          ),
          #mainPanel ----
          mainPanel(
            tabsetPanel(
              id = "main_panel",
              tabPanel(
                title = "Overview",
                value = "overview_tab",
                "TSCI 6202 Class activities 2026"
              ),
              tabPanel(title = "Survival", value = "survival_tab",
                       plotOutput("survivalPlot1")
                       ),
              tabPanel(title = "Relationships", value = "relationships_tab",
                       plotOutput("relationshipPlot1"),plotOutput("relationshipPlot2")
                       ),
              tabPanel(title = "Map", value = "map_tab"),
              tabPanel(title = "Debug", value = "debug_tab")
            )
          )))





