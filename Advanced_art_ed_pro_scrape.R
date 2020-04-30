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

all_course <- read_html("https://theartofeducation.edu/pro/library/")
course_url <- all_course %>%
  html_nodes(css = ".block__title") %>%
  html_children() %>%
  html_attrs() %>%
  unlist() %>%
  unname()

course_title <- all_course %>%
  html_nodes(css = ".block__title") %>%
  html_children() %>%
  html_text() %>%
  str_trim()

course_tbl <- data.frame(Course_Title = course_title, Course_url = course_url, stringsAsFactors = F)
all_course_ls <- list()

for(i in 72:72){
  dir.create(course_tbl$Course_Title[i])
  course_url <- course_tbl$Course_url[i]
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
  print(tbl)
  all_course_ls[[i]] <- tbl
  
  source_rough <- remDr$findElement(using = "xpath", "/html/body/section[3]/div/div/div/div[3]/div/div[3]/div[2]/div/div")
  source_rough_url <- source_rough$findChildElements(using = "class name", "aep-popup-link")
  resource_url <- unique(unlist(lapply(source_rough_url, function(x) x$getElementAttribute("data-popup-url"))))
  resource_url <- resource_url[-3]
  sapply(resource_url, function(x) download.file(x, destfile = paste0(course_tbl$Course_Title[i], "/", basename(x)), quiet = T, method = "curl"))
}

for(j in 57:102){
  tbl <- all_course_ls[[j]]
  tbl$course_title <- str_replace(tbl$course_title, pattern = "/", replacement = "_")
  print(j)
  print(tbl)
  for(k in 1:length(tbl$course_title)){
    video <- read_html(tbl$course_url[k])
    video_rough <- video %>%
      html_nodes(css = "script") %>%
      html_text()
    
    video_url <- video_rough[5] %>%
      str_remove_all(pattern = "\\{\\}") %>%
      str_match(pattern = "[\\{]+([^\\}]+)[\\}]+") %>%
      str_extract("http:.+.bin")
    
    tryCatch({
      download.file(url = video_url[1], destfile = paste0(course_tbl$Course_Title[j], "/", k, ". ", tbl$course_title[k], ".mp4"), quiet = T, method = "curl")
    }, error = function(download){
      download.file(url = video_url[1], destfile = paste0(course_tbl$Course_Title[j], "/", k, ". ", tbl$course_title[k], ".mp4"), quiet = T, method = "curl")
    })
  }
}