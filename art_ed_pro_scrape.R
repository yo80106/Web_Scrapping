setwd("F:\\Art-ed/")

library(httr)
library(dplyr)
library(xml2)
library(rvest)
library(RSelenium)
library(stringr)
library(purrr)

user_email <- "yo80106@hotmail.com"
user_password <- "yo19921124"
url <- "https://theartofeducation.edu/account/"
rD <- rsDriver(verbose = F, chromever = "74.0.3729.6")

remDr <- rD$client
remDr$navigate(url)
email <- remDr$findElement(using="xpath", '//*[@id="username"]')
email$sendKeysToElement(list(user_email))
password <- remDr$findElement(using="xpath", '//*[@id="password"]')
password$sendKeysToElement(list(user_password, key = "enter"))

course_url <- "https://theartofeducation.edu/packs/make-art-history-elementary-art-room/"
remDr$navigate(course_url)

course_content <- read_html(course_url)
course_url <- course_content %>% 
  html_nodes(css = ".sub-module-wrap") %>%
  html_attrs() %>%
  map(2) %>%
  unlist() %>% 
  paste0("http://fast.wistia.net/embed/iframe/", .)

course_title <- course_content %>% 
  html_nodes(css = ".quickvideo-title") %>%
  html_text()

tbl <- data.frame(course_title = course_title, course_url = course_url, stringsAsFactors = F)


source_rough <- remDr$findElement(using = "xpath", "/html/body/section[3]/div/div/div/div[3]/div/div[3]/div[2]/div/div")
source_rough_url <- source_rough$findChildElements(using = "class name", "aep-popup-link")
resource_url <- unique(unlist(lapply(source_rough_url, function(x) x$getElementAttribute("data-popup-url"))))
sapply(resource_url, function(x) download.file(x, destfile = basename(x), quiet = T, method = "curl"))

for(i in 1:length(tbl$course_url)){
  video <- read_html(tbl$course_url[i])
  video_rough <- video %>%
    html_nodes(css = "script") %>%
    html_text()
  
  video_url <- video_rough[5] %>%
    str_remove_all(pattern = "\\{\\}") %>%
    str_match(pattern = "[\\{]+([^\\}]+)[\\}]+") %>%
    str_extract("http:.+.bin")
  
  download.file(url = video_url[1], destfile = paste0(i, ". ", tbl$course_title[i], ".mp4"), quiet = T, method = "curl")
}