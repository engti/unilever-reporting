## load libraries

  library(shiny)
  library(shinydashboard)
  

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
        plotOutput("trendggPlot",brush  = brushOpts(id="select_dates",direction = c("x")),width = "100%",height = "100px")
      )
    ),
    fluidRow(
      box(width = 4, height = "450px",
        plotlyOutput("piePlot")
      ),
      box(width = 4, height = "450px",
        plotlyOutput("brandPlot")
      ),
      box(width = 4, height = "450px",
        plotlyOutput("subcatPie")
      )
    ),
    fluidRow(
      box(
        plotlyOutput("trendCategoryPlot")
      ),
      box(
        verbatimTextOutput("diag")
      )
    ),
    fluidRow(
      
    )
  )
  
  dashboardPage(header, sidebar, body)