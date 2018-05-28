make_vwap <- function (currency) {
  
    
  library(mongolite)
  library(jsonlite)
  library(tidyverse)
  library(lubridate)
  library(scales)
  
  url = 'mongodb://cuny:data@ds157639.mlab.com:57639/currency_v2'
  pl_hist_db <- mongolite::mongo(collection = "pl_hist", db = "currency_v2", url = url)
  
  currency <- toupper(currency)
  
  raw_return = pl_hist_db$find()
  
  pl_hist <- data.frame(ticker  = character(),
                        wap = numeric(), rpl = numeric(), position = numeric(), market = numeric(), stringsAsFactors = F)
  
  for (i in 1:length(raw_return)){
    pl_hist <- union_all(pl_hist, filter(raw_return[[i]], is.na(ticker) == F))
  }
  
  times <- colnames(pl_hist_db$find())
  
  pl_hist <- mutate(pl_hist, time = ymd_hms(substring(times, 1, 14)))
  
  
  pl_hist <- filter(pl_hist, ticker == currency)
  
  ggplot(pl_hist) + geom_line(aes(x = time, y = wap), lwd = 1, color = 'blue') + 
    labs(x = 'Date', y = 'VWAP', title = paste(currency, 'VWAP', sep = ' ')) +
    scale_x_datetime(labels = date_format("%Y-%m-%d-%H:%M%S")) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}

make_vwap('BTC')

make_price <- function (currency) {
  
  
  library(mongolite)
  library(jsonlite)
  library(tidyverse)
  library(lubridate)
  
  url = 'mongodb://cuny:data@ds157639.mlab.com:57639/currency_v2'
  blotter_db <- mongolite::mongo(collection = "blotter", db = "currency_v2", url = url)
  
  raw_return = blotter_db$find()
  
  currency <- toupper(currency)

  blotter <- mutate(raw_return, date = as_datetime(date)) %>%
      filter(ticker == currency)
  
  ggplot(blotter) + geom_line(aes(x = date, y = price), lwd = 1, color = 'blue') + 
    labs(x = 'Date', y = 'Price', title = paste(currency, 'Purchase Price', sep = ' ')) +
    scale_x_datetime(labels = date_format("%Y-%m-%d-%H:%M%S")) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
}

make_price('BTC')

make_tpl <- function () {
  
  library(mongolite)
  library(jsonlite)
  library(tidyverse)
  library(lubridate)
  
  url = 'mongodb://cuny:data@ds157639.mlab.com:57639/currency_v2'
  pl_hist_db <- mongolite::mongo(collection = "pl_hist", db = "currency_v2", url = url)
  
  raw_return = pl_hist_db$find()
  
  pl_hist <- data.frame(ticker  = character(),
                        wap = numeric(), rpl = numeric(), position = numeric(), market = numeric(), stringsAsFactors = F)
  
  for (i in 1:length(raw_return)){
    pl_hist <- union_all(pl_hist, filter(raw_return[[i]], is.na(ticker) == F))
  }
  
  times <- colnames(pl_hist_db$find())
  
  pl_hist <- mutate(pl_hist, time = ymd_hms(substring(times, 1, 14)))
  
  
  ggplot(pl_hist) + geom_line(aes(x = time, y = tpl), color = 'blue', lwd = 1) + 
    labs(x = 'Date', y = 'Total PL', title ='Total PL History') + 
    scale_x_datetime(labels = date_format("%Y-%m-%d-%H:%M%S")) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
}

make_tpl()


make_cash <- function () {
  
  
  library(mongolite)
  library(jsonlite)
  library(tidyverse)
  library(lubridate)
  
  url = 'mongodb://cuny:data@ds157639.mlab.com:57639/currency_v2'
  blotter_db <- mongolite::mongo(collection = "blotter", db = "currency_v2", url = url)
  
  raw_return = blotter_db$find()
  
  
  blotter <- mutate(raw_return, date = as_datetime(date))
  
  ggplot(blotter) + geom_line(aes(x = date, y = cash_balance), lwd = 1, color = 'Green') + 
    labs(x = 'Date', y = 'Cash', title = 'Cash Balance History') +
    scale_x_datetime(labels = date_format("%Y-%m-%d-%H:%M%S")) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
}

make_cash()