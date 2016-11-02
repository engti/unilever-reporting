## load library
  library(googleAnalyticsR)
  library(dplyr)
  
## source the authentication file
  source("options.R")
    
## get account info
  account_list <- google_analytics_account_list()
    account_main <- account_list %>% filter(viewName == "All Web Site Data")  

## set up the analytics call 
      for(i in 1:nrow(account_main))
        {
        print(i)
        errTrp <- tryCatch({
          flag = TRUE
        tmp <- google_analytics_4(viewId = account_main$viewId[i],
                                  date_range = c("2016-01-03","2016-10-29"),
                                  metrics = c("users","sessions","bounces"),
                                  dimensions = c("week","country"),
                                  max = 10000 )
        
        }, warning = function(w){
          print("warning")
          
        }, error = function(e){
          print("error")
         
          flag = FALSE
          
        }, finally = {
         if(flag)
         {
           tmp$ProfileName <- paste0(account_main$accountName[i],": ",account_main$webPropertyName[i])
           if(i == 1){
             df1 <- tmp
           }else if(i > 1 && length(tmp) > 1){
             df1 <- rbind(df1,tmp)
             print(paste0("Total Rows: ",nrow(df1)))
           }
         }
          
        })
      }
      
    
    
    
    
  
    
    