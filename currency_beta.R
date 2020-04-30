timestamp()
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(xml2))
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(RPushbullet))
suppressPackageStartupMessages(library(readr))
# suppressPackageStartupMessages(library(optparse))
# 
# option_list <- list(
#   make_option(c("-b", "--buy"), default=as.character(39.39), 
#               help="Bank buy price. [Default=39.39]"),
#   make_option(c("-s", "--sell"), default=as.character(38.51), 
#               help="Bank sell price. [Default=38.51]"),
#   make_option(c("-f", "--focus"), default="sell", 
#               help="User focuses on bank buy or sell. [Default = sell]")
# )
# opt_parser <- OptionParser(option_list = option_list)
# opt <- parse_args(opt_parser)
# pbSetup()

comparing_tool <- function(daily_tbl, focus){
  if(focus == "buy"){
    if(daily_tbl$Bank_buy > week_base$buy_avg){
      pbPost(type = "note", title = "Bank buy is higher than week base.", body = str_glue("Bank Buy: {daily_tbl$Bank_buy} > {week_base$buy_avg}"))
    }else if(daily_tbl$Bank_buy > month_base$buy_avg){
      pbPost(type = "note", title = "Bank buy is higher than month base.", body = str_glue("Bank Buy: {daily_tbl$Bank_buy} > {month_base$buy_avg}"))
    }
  }else if(focus == "sell"){
    if(daily_tbl$Bank_sell < week_base$sell_avg){
      pbPost(type = "note", title = "Bank sell is lower than week base.", body = str_glue("Bank Sell: {daily_tbl$Bank_sell} < {week_base$sell_avg}"))
    }else if(daily_tbl$Bank_sell < month_base$sell_avg){
      pbPost(type = "note", title = "Bank sell is lower than month base.", body = str_glue("Bank Sell: {daily_tbl$Bank_sell} < {month_base$sell_avg}"))
    }
  }
}
user_define_price <- function(daily_tbl, buy.price = 39.39, sell.price = 38.51){
  if(daily_tbl$Bank_buy > buy.price){
    pbPost(type = "note", title = "Bank buy is higher than user price.", body = str_glue("Bank Buy: {daily_tbl$Bank_buy} > {buy.price}"))
  }else if(daily_tbl$Bank_sell < sell.price){
    pbPost(type = "note", title = "Bank sell is lower than user price.", body = str_glue("Bank Sell: {daily_tbl$Bank_sell} < {sell.price}"))
  }
}

history_rate <- suppressWarnings(suppressMessages(read_csv("https://rate.bot.com.tw/xrt/flcsv/0/L3M/GBP", local = locale(encoding = "UTF-8"))))
attr(history_rate, 'spec') <- NULL
history_rate <- history_rate[,c(1,2,5,15)]
colnames(history_rate) <- c("Date", "Currency", "We_buy", "We_sell")
history_rate$Date <- as.Date(as.character(history_rate$Date),format="%Y%m%d")

cathy_url <- "https://www.cathaybk.com.tw/cathaybk/personal/deposit-exchange/rate/currency-billboard/#first-tab-01"
exchange_tbl <- read_html(cathy_url)
raw_data <- exchange_tbl %>% 
  html_nodes("font") %>% 
  html_text()

currency_tbl <- data.frame(Currency = raw_data[seq(1,61,3)], Bank_buy = raw_data[seq(2,62,3)], Bank_sell = raw_data[seq(3,63,3)], stringsAsFactors = F)
currency_tbl$Bank_buy <- as.numeric(currency_tbl$Bank_buy)
currency_tbl$Bank_sell <- as.numeric(currency_tbl$Bank_sell)
pound <- currency_tbl %>% filter(str_detect(Currency, "GBP"))
today <- Sys.Date()
times <- Sys.time()
week_base <- history_rate[history_rate$Date >= today-7,] %>% summarise(buy_avg = mean(We_buy), sell_avg = mean(We_sell))
month_base <- history_rate[history_rate$Date >= today-30,] %>% summarise(buy_avg = mean(We_buy), sell_avg = mean(We_sell))

if(!file.exists(str_glue("C://Users/chunyuchen/Documents/currency_daily_tbl_{today}.csv"))){
  daily_tbl <- data.frame(Time = times)
  daily_tbl <- cbind(daily_tbl, pound)
  write.csv(daily_tbl, file = str_glue("C://Users/chunyuchen/Documents/currency_daily_tbl_{today}.csv"), quote = F, row.names = F)
}else{
  tmp_tbl <- data.frame(Time = times) %>% bind_cols(pound)
  daily_tbl <- read.csv(str_glue("C://Users/chunyuchen/Documents/currency_daily_tbl_{today}.csv"))
  daily_tbl$Time <- as.POSIXct(daily_tbl$Time)
  daily_tbl <- rbind(daily_tbl, tmp_tbl)
  comparing_tool(tmp_tbl, focus = "sell")
  user_define_price(tmp_tbl, buy.price = 39.9, sell.price = 38.9)
  write.csv(daily_tbl, file = str_glue("C://Users/chunyuchen/Documents/currency_daily_tbl_{today}.csv"), quote = F, row.names = F)
}


