## load library
  library(googleAnalyticsR)
  library(dplyr)
  
## source the authentication file
  source("options.R")
    
## get account info
  account_list <- google_analytics_account_list()
    # account_main <- account_list %>% filter(viewName == "All Web Site Data") 
    
## get another set of accounts    
    account_curated <- read.csv("account_list_verified.csv",stringsAsFactors = F) %>%
                        mutate(
                          Profile.ID = as.character(Profile.ID)
                        ) %>%
                        unique() 
      
    
## find common profile ids in account_curated and account_list    
    account_common <- account_list %>% 
      semi_join(account_curated,c("viewId"="Profile.ID")) 
    
    
    account_common <- merge(account_curated,account_common,by.x = "Profile.ID",by.y = "viewId") %>%
                      select(-accountId,-UA.number,-Url,-accountName)
    
    account_common_focus <- account_common %>%
                              select(Profile.ID,webPropertyName,Brand,Super.Category,Sub.Category,Market,Region.Market)
    write.csv(account_common_focus,"account_common_focus.csv",row.names = F)
    
## set up the analytics call 
    ## it uses tryCatch since sometimes the analytics call fails
      for(i in 1:nrow(account_common_focus))
        {
        print(i)
        errTrp <- tryCatch({
          flag = TRUE
        tmp <- google_analytics_4(viewId = account_common_focus$Profile.ID[i],
                                  date_range = c("2016-01-03","2016-10-29"),
                                  metrics = c("users","sessions","bounces"),
                                  dimensions = c("week","country"),
                                  max = 40000 )
        
        }, warning = function(w){
          print("warning")
          
        }, error = function(e){
          print("error")
         
          flag = FALSE
          
        }, finally = {
         if(flag)
         {
           # tmp$ProfileName <- paste0(account_common_focus$accountName[i],": ",account_common_focus$webPropertyName[i])
           tmp$ViewID <- account_common_focus$Profile.ID[i]
           if(i == 1){
             df1 <- tmp
           }else if(i > 1 && length(tmp) > 1){
             df1 <- rbind(df1,tmp)
             print(paste0("Total Rows: ",nrow(df1)))
           }
         }
          
        })
      }
    
    ## remove temp variables
    rm(errTrp,tmp)  
    
    ## save the result
    write.csv(df1,"allBrands_weekly_curated.csv",row.names = F)
    
    
    