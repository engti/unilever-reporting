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
      box(title = "Countries and Brands",width = 12,status="primary",solidHeader = TRUE,collapsible = T,
          box(
            plotlyOutput("scatterCountry")
          ),
          box(
            DT::dataTableOutput("BrandTable")
          )
      )
    ),
    fluidRow(
      box(width = 12,status="primary",solidHeader = TRUE,collapsible = T,
        box(width = 4, height = "450px",
        plotlyOutput("mediumPlot")
      ),
      box(width = 4, height = "450px",
        plotlyOutput("brandPlot")
      ),
      box(width = 4, height = "450px",
        plotlyOutput("countryPlot")
      ))
    ),
    fluidRow(
      box(width = 12,status="primary",solidHeader = TRUE,collapsible = T,
        box(width = 4,height = "450px",
        plotlyOutput("piePlot")
      ),
      box(width = 4,height = "450px",
        plotlyOutput("trendCategoryPlot")
      ),
      box(width = 4,height = "450px",
          plotlyOutput("subcatPlot")
      ))
    ),
    fluidRow(
      box(title = "Traffic Sources Table",width = 6,status="primary",solidHeader = TRUE,collapsible = T,
          DT::dataTableOutput("mediumTable")
      ),
      box(title = "Top Brands",width = 6,status="primary",solidHeader = TRUE,collapsible = T,
          DT::dataTableOutput("topGainers")
      )
    ),
    fluidRow(
      box(width = 4,height = "450px",
          verbatimTextOutput("diag")
      )
    )
  )
  
  dashboardPage(header, sidebar, body,title = "Unilever Reporting Snapshot")