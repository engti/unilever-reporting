## load library
  library(ggplot2)
  library(plotly)

## read in data
  siteData <- read.csv("allBrands_medium_weekly_curated.csv",stringsAsFactors = F)
  siteMetaData <- read.csv("account_common_focus.csv",stringsAsFactors = F)

## merge traffic data with site meta data
  siteDataMerged <- merge(siteData,siteMetaData,by.x = "ViewID",by.y ="Profile.ID" )
  write.csv(siteDataMerged,"siteDataMerged.csv",row.names = F)
  
## do some basic graphing
  ### shape data for super category then plot it
  df1 <- siteDataMerged %>% select(week,Super.Category,sessions) %>% 
    group_by(week,Super.Category) %>% summarise_each(funs(sum))
  
  plot <- ggplot(df1,aes(x= week,y=sessions,fill = Super.Category,colour = Super.Category)) + geom_area()
  plot
  
  ## use plotly to make the pie chart
  df2 <- siteDataMerged %>% select(Super.Category,sessions) %>% 
    group_by(Super.Category) %>% 
    summarise_each(funs(sum)) %>%
    ungroup() %>%
    arrange(-sessions)
  
  plot2 <- plot_ly(df2, labels = ~Super.Category, values = ~sessions,type = "pie") %>%
    layout(title = 'Top Product Categories',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  tmp <- as.Date(paste(2016, siteDataMerged$week, 1, sep="-"), "%Y-%U-%u")
  
  USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
  data <- USPersonalExpenditure[,c('Categorie', 'X1960')]
  
  
  ## ggplotly for trend line selector
  tmp <- df2 %>% select(week,sessions) %>% group_by(week) %>% summarise_each(funs(sum)) %>% ungroup()
  tmpPlot <- ggplot(tmp,aes(x=week,y=sessions)) + geom_line() 
  ggplotly(tmpPlot)
   