## load libraries
  library(RPostgreSQL)
  library(dplyr)
  library(magrittr)
  
## load the db connect script
  source("redshift-connect.R")
  
## pick some data up  
  requests <- tbl(udmdConnect, "events")
  df1 <- requests %>% select(geo_country,mkt_medium,collector_tstamp) %>% filter(collector_tstamp=="2016-11-01") %>% collect() 
  
  
  requests <- tbl(udmdConnect, sql("SELECT * from atomic.events WHERE mkt_medium == cpc"))
##   