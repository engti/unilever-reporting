## use sql statements instead of dplyr

## load library
  library(RPostgreSQL)
  library(dplyr)
  library(ggplot2)
  library(readr)
  
## load the db connect script
  source("redshift-connect.R")
  
####Basic Uniques####    
## pull some data   
  
  df1 <- dbGetQuery(con, "
	SELECT
	to_char(collector_tstamp, 'YYYY-MM-DD') AS \"date\",
	count(distinct(domain_userid)) AS uniques
	FROM \"atomic\".\"events\"
	WHERE collector_tstamp >= '2016-01-01'
	GROUP BY \"date\"
	ORDER BY \"date\"
")
  
  df2 <- df1 %>%
    mutate(
      date = as.Date(date)
    )
  
## plot out above unique by day data
  ggplot(df2,aes(x=as.Date(date), y=uniques)) + geom_line() 
  
####User by Country####    
  ## pull uniques by url host with some metrics  
  t1 <- Sys.time()
  geo1 <- dbGetQuery(con,
  "SELECT
  domain_userid,
  domain_sessionidx,
  geo_country,
  min(collector_tstamp) AS \"time_of_first_touch\",
  count(distinct(page_urlpath)) AS \"distinct_pages_visited\",
  count(*) as \"number_of_events\"
  FROM \"atomic\".\"events\"
  WHERE collector_tstamp > '2016-11-01'
  GROUP BY domain_userid, domain_sessionidx, geo_country
  ")
  t2 <- Sys.time()
  t2-t1
  # SELECT "geo_country" AS "geo_country", "collector_tstamp" AS "collector_tstamp" FROM  atomic."events" WHERE "collector_tstamp" between '2016-10-01' AND '2016-10-10' limit 10 
  
  df2 <- df1 %>%
    mutate(
      date = as.Date(date)
    )
  
  ## plot out above unique by day data
  ggplot(df2,aes(x=as.Date(date), y=uniques)) + geom_line()  
  
  write_csv(geo1,"domain_session_large.csv")
  
####Host ID geo####  
####### pull uniques by url host with some metrics  
  t1 <- Sys.time()
  user1 <- dbGetQuery(con,
  "SELECT
  blended_user_id,
  geo_country,
  landing_page_host,
  refr_urlhost,
  count(distinct(domain_sessionidx)) AS \"sessions\",
  sum(event_count) AS \"events\"
  FROM \"derived\".\"sessions\"
  WHERE session_start_tstamp > '2016-11-09'
  GROUP BY blended_user_id, geo_country, landing_page_host,refr_urlhost
  ")
  t2 <- Sys.time()
  t2-t1
  
  t1 <- Sys.time()
  write_csv(user1,"userID_geo_lpge_urlhst.csv")
  t2 <- Sys.time()
  t2-t1
  
  tmp <- user1 %>%
    select(refr_urlhost,geo_country,sessions,events) %>%
    mutate(
      refr_urlhost = gsub("^www.google.*$","Google",refr_urlhost),
      refr_urlhost = gsub(".*facebook.com$","Facebook",refr_urlhost),
      refr_urlhost = gsub("^com.google.*","Google Apps",refr_urlhost),
      refr_urlhost = gsub(".*.yahoo.com$","Yahoo",refr_urlhost),
      refr_urlhost = gsub(".*.yahoo.co.jp$","Yahoo",refr_urlhost),
      refr_urlhost = replace(refr_urlhost,is.na(refr_urlhost),"Direct")
    ) %>%
    group_by(refr_urlhost,geo_country) %>%
    summarise_each(funs(sum)) %>%
    arrange(-sessions) %>%
    ungroup()
  
  
####PV Table Host metrics####  
  ##### pull from page view tableuniques by url host with some metrics  
  pvPage <- dbGetQuery(con,
                      "SELECT
                      page_urlhost,
                      count(domain_sessionidx) AS \"sessions\",
                      sum(event_count) AS \"events\",
                      sum(page_view_count) AS \"pageViews\",
                      sum(page_ping_count) AS \"pagePings\",
                      sum(time_engaged_with_minutes) AS \"timeTotal\"
                      FROM \"derived\".\"page_views\"
                      WHERE first_touch_tstamp > '2016-10-01'
                      GROUP BY page_urlhost
                      ORDER BY sessions
                      ")
  
  pvPage1 <- pvPage %>%
    filter(!grepl("^10.",page_urlhost)) %>%
    mutate(
      eventPerSession = events/sessions,
      timeSpent1 = timetotal/sessions,
      timeSpent2 = pagepings/sessions,
      timeDiff = round(((timeSpent2-timeSpent1)/timeSpent1)*100)
    ) %>%
    arrange(-sessions)
  
####UserID by site####  
  ##### pull from page view tableuniques by url host with some metrics  
  userHost <- dbGetQuery(con,
                       "SELECT
                       network_userid,
                       page_urlhost,
                       refr_urlhost,	
                       count(distinct(domain_sessionidx)) AS \"sessions\"
                       FROM \"atomic\".\"events\"
                       WHERE collector_tstamp > '2016-11-05'
                       GROUP BY network_userid,page_urlhost,refr_urlhost
                       ORDER BY sessions
                       ")
  
  ## impute known values
  userHost1 <- userHost %>%
    filter(!grepl("^10.",page_urlhost)) %>%
    mutate(
      refr_urlhost = gsub("^www.google.*$","Google",refr_urlhost),
      refr_urlhost = gsub(".*facebook.com$","Facebook",refr_urlhost),
      refr_urlhost = gsub("^com.google.*","Google Apps",refr_urlhost),
      refr_urlhost = gsub(".*.yahoo.com$","Yahoo",refr_urlhost),
      refr_urlhost = gsub(".*.yahoo.co.jp$","Yahoo",refr_urlhost),
      refr_urlhost = replace(refr_urlhost,is.na(refr_urlhost),"Direct")
    ) %>% 
    arrange(-sessions)
  
  userHost2 <- userHost1 %>%
    group_by(network_userid) %>%
    summarise(
        sites = n_distinct(page_urlhost),
        referrers = n_distinct(refr_urlhost),
        sessions = sum(sessions)
    ) %>%
    ungroup() %>%
    arrange(sessions)
  
  tmp <- userHost2 %>%
    filter(sites > 1)
  
  tmp2 <- userHost1 %>%
    filter(network_userid == "2ee9f76f-bc35-460e-b637-6431fad14c4d")
  
  ## write to file
  write_csv(userHost2,"userHostRef2.csv")
  # userHost1 <- read_csv("userHostRef.csv")

#### user id by count of sites and referrers ####
  userCounts <- dbGetQuery(con,
                       "SELECT
                       network_userid,
                       count(distinct(domain_sessionidx)) AS \"sessions\",
                       count(distinct(page_urlhost)) AS \"sites\",
                       count(distinct(refr_urlhost)) AS \"referrers\"
                       FROM \"atomic\".\"events\"
                       WHERE collector_tstamp > '2016-01-01'
                       GROUP BY network_userid
                       HAVING count(distinct(page_urlhost)) > 1
                       ORDER BY sessions
                       ")
  ## write to file
  write_csv(userCounts,"userHostRefCount2016All.csv")
  # userHost1 <- read_csv("userHostRefCount2016All.csv")
  userCounts1 <- userCounts %>%
    filter(sessions > 1) %>%
    arrange(-sessions)
  
  #### userid by host and ref ####
  userHostRefDetailed <- dbGetQuery(con,
                      "SELECT
                      network_userid,
                      page_urlhost,
                      refr_urlhost,
                      count(distinct(domain_sessionidx)) AS \"sessions\"
                      FROM \"atomic\".\"events\"
                      WHERE network_userid in (SELECT
                      network_userid
                      FROM \"atomic\".\"events\"
                      WHERE collector_tstamp > '2016-01-01'
                      GROUP BY network_userid
                      HAVING count(distinct(page_urlhost)) > 1
                      )
                      GROUP BY network_userid,page_urlhost,refr_urlhost
                      ")
  
  ## write to file
  write_csv(userHostRefDetailed,"userHostRefDetailed.csv")
  # userHost1 <- read_csv("userHostRefDetailed.csv")
  
  ## build a graph
  
