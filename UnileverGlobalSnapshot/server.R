## Load libraries
  library(shiny)
  library(dplyr)
  library(leaflet)
  library(plotly)
  library(readr)
  library(DT)
  library(magrittr)
  
## read in data
  mainData <- read_csv("siteDataMerged.csv") %>%
    mutate(
      Brand = iconv(Brand, 'UTF-8', 'ASCII'),
      medium = iconv(medium, 'UTF-8', 'ASCII')
    )

# Define server logic 
  
shinyServer(function(input, output) {
  
  derivedData <- reactive({
    # df1 <- event_data("plotly_selected",source = "dateSelect")
    # if(is.null(df1) || length(df1) == 0){
    #   mainData
    # }else{
    #   mainData %>% filter(week >= df1$`xaxis.range[0]`,week <= df1$`xaxis.range[1]`)
    # }
    # df2 <- mainData %>% filter(week >= df1$`xaxis.range[0]`,week <= df1$`xaxis.range[1]`)
    # sum(df2$sessions)
    if(is.null(input$select_dates)){
      df1 <- mainData
    }else{
      df1 <- brushedPoints(mainData,input$select_dates)
    }
    df1
    
  })
  
  kpiData <- reactive({
    # df1 <- derivedData() %>% select(week,sessions,users,bounces) %>% filter(week >= abs(input$select_dates$xmin),week <= abs(input$select_dates$xmax))
    df1 <- derivedData() %>% select(week,sessions,users,bounces)
    df1
  })
  
  brandData <- reactive({
    df1 <- derivedData() %>% 
      select(Brand,sessions) %>%
      group_by(Brand) %>%
      summarise_each(funs(sum)) %>%
      ungroup()
    df1
  })
  
  output$trendggPlot <- renderPlot({
    df1 <- mainData %>% 
      select(week,sessions) %>% 
      group_by(week) %>% 
      summarise_each(funs(sum)) %>% 
      ungroup()
    
    ggplot(df1,aes(x=week,y=sessions)) + geom_line() + ggtitle("Weekly Trend - Drag to Select reporting period") + theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                             axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                             axis.title.x=element_blank(),
                                                             axis.title.y=element_blank(),legend.position="none",
                                                             panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                                                             panel.grid.minor=element_blank(),plot.background=element_blank())
    
  })
  
  output$KPIvisit <- renderValueBox({
    valueBox(
      paste0(round(sum(as.numeric(sum(kpiData()$users))/1000000)),"M"),
      "Users")
  })
   
  output$KPIpv <- renderValueBox({
    valueBox(
      paste0(round(sum(as.numeric(sum(kpiData()$sessions))/1000000)),"M"),
      "Sessions")
  })
  
  output$KPIduration <- renderValueBox({
    bounceRate <-  round((sum(kpiData()$bounces) / sum(kpiData()$sessions)) * 100)
    valueBox(
      paste0(bounceRate,"%"),
      "Bounce Rate")
  })
  
  output$piePlot <- renderPlotly({
    ## do the data
    df1 <- derivedData() %>% select(Super.Category,sessions) %>% 
      group_by(Super.Category) %>% 
      summarise_each(funs(sum)) %>%
      ungroup() %>%
      arrange(-sessions)
    ## create the plot
    plot_ly(df1, labels = ~Super.Category, values = ~sessions,type = "pie") %>%
      layout(title = 'Product Categories - Visit Share',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      layout(legend = list(orientation = 'h',
                           font = list(
                             family = "sans-serif",
                             size = 10,
                             color = "#000")
          )
        )
  })
  
  output$trendCategoryPlot <- renderPlotly({
    df2 <- derivedData() %>% 
      select(week,Super.Category,sessions) %>%
      group_by(week,Super.Category) %>%
      summarise_each(funs(sum)) %>%
      ungroup()
    ## create the plot
    plot_ly(df2, x = ~week, y = ~sessions,type = "scatter",mode = "lines", color = ~Super.Category,source = "dateSelect") %>%
      layout(title = "Product Category Visit Trend",legend = list(orientation = 'h',
                           font = list(
                             family = "sans-serif",
                             size = 10,
                             color = "#000")
      )
      ) %>%
      layout(dragmode = "select")
  })
  
  output$subcatPlot <- renderPlotly({
    df2 <- derivedData() %>% 
      select(Sub.Category,sessions) %>%
      group_by(Sub.Category) %>%
      summarise_each(funs(sum)) %>%
      ungroup() %>%
      arrange(-sessions)
    ## create the plot
    plot_ly(df2[1:10,], x=~sessions ,y=~reorder(Sub.Category,sessions),type = "bar",orientation = 'h') %>%
      layout(title = 'Product Sub Categories',yaxis = list(title = ""),xaxis = list(title = "Sessions"),margin = list(l = 150)) 
    # %>%
    #   layout(yaxis = list(categoryarray = ~sessions, categoryorder = "array"))
    # 
  })
  
  output$brandPlot <- renderPlotly({
    df1 <- derivedData() %>% 
      select(Brand,sessions) %>%
      group_by(Brand) %>%
      summarise_each(funs(sum)) %>%
      ungroup() %>%
      arrange(-sessions)
    ## create the plot
    plot_ly(df1[1:10,], x=~sessions ,y=~reorder(Brand,sessions),type = "bar",orientation = 'h') %>%
      layout(title = 'Top Brands',yaxis = list(title = ""),xaxis = list(title = "Sessions"),margin = list(l = 100)) 
    # %>%
    #   layout(yaxis = list(categoryarray = ~sessions, categoryorder = "array"))
    # 
  })
  
  output$mediumPlot <- renderPlotly({
    ## do the data
    df1 <- derivedData() %>% select(medium,sessions) %>% 
      group_by(medium) %>% 
      summarise_each(funs(sum)) %>%
      ungroup() %>%
      arrange(-sessions)
    
    df1 <- rbind(top_n(df1,8),
                 slice(df1,9:n()) %>% summarise(medium="other",sessions=sum(sessions))
    )
      
    ## create the plot
    plot_ly(df1[1:10,], labels = ~medium, values = ~sessions,type = "pie") %>%
      layout(title = 'Traffic Sources',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      layout(legend = list(orientation = 'h',
                           font = list(
                             family = "sans-serif",
                             size = 10,
                             color = "#000")
      )
      )
  })
  
  output$countryPlot <- renderPlotly({
    df1 <- derivedData() %>% 
      select(Market,sessions) %>%
      group_by(Market) %>%
      summarise_each(funs(sum)) %>%
      ungroup() %>%
      arrange(-sessions)
    ## create the plot
    plot_ly(df1[1:10,], x=~sessions ,y=~reorder(Market,sessions),type = "bar",orientation = 'h') %>%
      layout(title = 'Top Market',yaxis = list(title = ""),xaxis = list(title = "Sessions"),margin = list(l = 100)) 
    # %>%
    #   layout(yaxis = list(categoryarray = ~sessions, categoryorder = "array"))
    # 
  })
  
  output$mediumTable <- DT::renderDataTable({
    df1 <- derivedData() %>%
      select(medium,users,sessions,bounces) %>%
      group_by(medium) %>%
      summarise_each(funs(sum)) %>%
      ungroup() %>%
      arrange(-sessions)
    
    df1 <- rbind(top_n(df1,8),
                 slice(df1,9:n()) %>% summarise(medium="other",users=sum(users),sessions=sum(sessions),bounces=sum(bounces))
    )
    df1 %<>% mutate(
      bounceRate = bounces / sessions
    ) %>%
      select(-bounces) %>%
      arrange(-users)
    
    datatable(df1) %>% formatPercentage(4,digits = 0) %>% formatCurrency(1:3,"",digits = 0)
  })
  
  output$diag <- renderPrint({
    # df1 <- event_data("plotly_selected",source = "dateSelect")
    # if(is.null(df1) == T) return(NULL)
    # df1$`xaxis.range[0]`
    # 
    # df2 <- mainData %>% filter(week >= df1$`xaxis.range[0]`,week <= df1$`xaxis.range[1]`)
    # sum(df2$sessions)
    str(input$select_dates)
  })
  
})
