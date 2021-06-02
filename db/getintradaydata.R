# get intraday heartrate by 1min interval, TSID is function and date is variable.
library(lubridate)
library(httr)

TSID = function(InputPath,InputDate){
  source = users %>% pull(source) %>% as.character()
  id     = users %>% pull(id)
  utoken = users %>% pull(access_token)
  #InputPath = "calories"
  #InputDate = "2021-04-10"
  url1    = "https://api.fitbit.com/1/user/"
  url2    = "/activities/"
  path    = InputPath#"heart"
  udate   = InputDate
  uperiod = "/1d"
  ulevel  = "/1min"
  filetype= ".json"
  gets = function(fitId){
    url = paste0(url1, fitId, url2, path, "/date/", udate, uperiod, ulevel, filetype)
    req = httr::GET(url,
                    add_headers(Authorization = paste0("Bearer ", unique(utoken[users$id ==fitId]))))
    dd  =httr::content(req, type = "application/json")
    cc  =paste0("activities-",path,"-intraday")
    dd[[which(names(dd)==cc)]]$dataset
  }
  getsD <- lapply(id, gets)
  names(getsD) <- source
  getsD=compact(getsD)
  source=names(getsD)
  
  # time series data step ---------------
  tsget = function(x){
    getsD[[x]] %>% unlist() %>% 
      matrix(., ncol = length(getsD[[x]][[1]]), byrow = TRUE) %>%
      tibble(udate,`time` = .[,length(getsD[[x]][[1]])-1], `value` =as.numeric(.[,length(getsD[[x]][[1]])])) %>%
      mutate(ymdhm = ymd_hms(paste(udate, time))) %>%
      select(ymdhm, value)
    
    
  }
 
  TsGetsIntraDay = lapply(source, tsget)
  
  names(TsGetsIntraDay) = source
  
  gg = dplyr::bind_rows(TsGetsIntraDay,.id ='source') %>% mutate(group = path)
  return(gg)
}




