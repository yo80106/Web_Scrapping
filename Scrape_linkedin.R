
setwd("F:/Course/")

library(httr)
library(dplyr)
library(xml2)
library(rvest)
library(RSelenium)
library(stringr)
library(progress)
# install.packages("wdman")
# library(googledrive)

course_url <- "https://www.linkedin.com/learning/integrating-tableau-and-r-for-data-science/"
user_email <- "chunyuchen_no_reply@outlook.com"
user_password <- "yo19921124"
# gd <- drive_get("Understanding Calculus: Problems, Solutions and Tips") 

page <- read_html(course_url)
course_url_df <- page %>%
  html_nodes(css = ".toc__item a") %>%
  html_attr("href") %>%
  str_remove(pattern = "\\?autoplay=true&trk=course_tocItem")

course_title_df <- page %>%
  html_nodes(css = ".toc__sublist__item__content__title") %>%
  html_text() %>%
  str_trim()

course_tbl <- data.frame(Course_Title = course_title_df, Course_url = course_url_df, stringsAsFactors = F)

url <- "https://www.linkedin.com/login?trk=guest_homepage-basic_nav-header-signin"
rD <- rsDriver(verbose = F, chromever = "77.0.3865.10")

remDr <- rD$client
remDr$navigate(url)
email <- remDr$findElement(using="xpath", '//*[@id="username"]')
email$sendKeysToElement(list(user_email))
password <- remDr$findElement(using="xpath", '//*[@id="password"]')
password$sendKeysToElement(list(user_password, key = "enter"))

remDr$navigate(course_url)
remDr$getTitle()

guidebook_info <- remDr$findElement(using='xpath', '//*[@id="page-content"]/section/div[1]/div/div[2]/div/div[2]/span[1]/a')
book_url <- guidebook_info$getElementAttribute("href")
(book_title <- str_extract(string = book_url, pattern = "[0-9]+.*"))
(course_id <- gsub(pattern = "[%_]", replacement = "", x = str_extract(string = book_title, pattern = "[0-9]*[%_]")))

download.file(url = book_url[[1]], destfile = book_title, method = "curl")
#drive_upload(media = book_title, path = paste0(gd$path, book_title))
#file.remove(book_title)

pb <- progress_bar$new(
  format = "  downloading :what [:bar] :percent in :elapsed, eta: :eta",
  total = 100, clear = FALSE, width= 120)

for(i in 1:num){
  pb$tick(0)
  lecture_url <- tbl[i,2]
  remDr$navigate(lecture_url)
  remDr$getTitle()
  
  lecture_title <- gsub(pattern = "[:?/]", replacement = "", x = tbl[i,1])
  test_url <- remDr$findElement(using = "xpath", "/html/body/script")
  script_test <- test_url$getElementAttribute("innerText")
  hd_url <- script_test %>% str_remove_all("[\n|\t]") %>% str_squish() %>% str_extract_all("\\{(.*?)\\}") %>% unlist() %>% str_subset("720p") %>% str_extract("https:.+\\.mp4")
  if(identical(hd_url, character(0))){
    sd_url <- script_test %>% str_remove_all("[\n|\t]") %>% str_squish() %>% str_extract_all("\\{(.*?)\\}") %>% unlist() %>% str_subset("480p") %>% str_extract("https:.+\\.mp4")
    download.file(sd_url[1], destfile = paste0(i,". ", lecture_title,".mp4"), method = "curl", quiet = T)
  }else{
    download.file(hd_url[1], destfile = paste0(i,". ", lecture_title,".mp4"), method = "curl", quiet = T)
  }
  tryCatch({
    sub_url <- script_test %>% str_remove_all("[\n|\t]") %>% str_squish() %>% str_extract_all("\\{(.*?)\\}") %>% unlist() %>% str_subset("captions") %>% str_extract("https:.+\\.srt") %>% str_replace("https", "http")
    download.file(sub_url, destfile = paste0(course_tbl$Course_Title[i], "/", j,". ", lecture_title,".srt"), quiet = T)
  }, error = function(msg){
    message("No caption available.")
  })
  pb$tick(100/num, tokens = list(what = paste0(course_id,"_",str_pad(i,2,pad=0), ".mp4")))
}


