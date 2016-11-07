## load libraries

  library(shiny)
  library(shinydashboard)
  library(dplyr)
  library(leaflet)
  library(plotly)
  library(readr)
  library(DT)
  

## read in the data
  
## create the ui
  header <- dashboardHeader(title = "Unilever Global Snapshot")
  
  sidebar <- dashboardSidebar(
    disable = T
  )
  
  body <- dashboardBody(
    fluidRow(
      valueBoxOutput("KPIvisit",width = 2),
      valueBoxOutput("KPIpv",width = 2),
      valueBoxOutput("KPIduration",width = 2),
      box( width = 6,
        plotOutput("trendggPlot",brush  = brushOpts(id="select_dates",direction = c("x")),width = "100%",height = "80px")
      )
    ),
    fluidRow(
      box(width = 4, height = "450px",
        plotlyOutput("mediumPlot")
      ),
      box(width = 4, height = "450px",
        plotlyOutput("brandPlot")
      ),
      box(width = 4, height = "450px",
        plotlyOutput("countryPlot")
      )
    ),
    fluidRow(
      box(width = 4,height = "450px",
        plotlyOutput("piePlot")
      ),
      box(width = 4,height = "450px",
        plotlyOutput("trendCategoryPlot")
      ),
      box(width = 4,height = "450px",
          plotlyOutput("subcatPlot")
      )
    ),
    fluidRow(
      box(title = "Traffic Sources Table",width = 6,status="primary",solidHeader = TRUE,collapsible = T,
          DT::dataTableOutput("mediumTable")
      ),
      box(width = 4,height = "450px",
          verbatimTextOutput("diag")
      )
    )
  )
  
  dashboardPage(header, sidebar, body)