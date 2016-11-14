## load libraries
  library(RPostgreSQL)
  library(dplyr)
  library(magrittr)
  
## load the db connect script
  source("redshift-connect.R")
  
## pick some data up  
  requests <- tbl(udmdConnect, "events")
  df2 <- requests %>% 
    filter(sql("collector_tstamp between '2016-01-01' and '2016-10-02'")) %>% 
    select(collector_tstamp,domain_userid) %>%
    group_by(collector_tstamp) %>%
    summarise( 
      countID = n()
      ) %>%
    ungroup() %>%
    mutate(
      collector_tstamp = as.Date(collector_tstamp)
    ) %>%
    collect() 
  
  df1 <- requests %>% 
    select(collector_tstamp,page_urlhost,refr_urlhost) %>% 
    filter(sql("collector_tstamp between '2016-10-01' and '2016-10-02'")) %>% 
    compute() 
  
  df2 <- df1 %>% 
    mutate(collector_tstamp = as.Date(collector_tstamp)) %>% 
    group_by(collector_tstamp) %>% 
    summarise_each(funs(sum)) %>%
    ungroup()
  
  library(ggplot2)
  ggplot(df2,aes(x=collector_tstamp,y=countid)) + geom_line()
  