
setwd("F:/")

library(httr)
library(dplyr)
library(xml2)
library(rvest)
library(RSelenium)
library(stringr)
library(progress)
# library(googledrive)

course_url <- "https://www.thegreatcoursesplus.com/show/what_darwin_didnt_know_the_modern_science_of_evolution"
user_email <- "msw56118@eanok.com"
user_password <- "msw56118"
# gd <- drive_get("Understanding Calculus: Problems, Solutions and Tips") 

page <- read_html(course_url)
num_lec <- page %>%
  html_nodes(xpath = '//*[@id="page-content"]/section/div[3]/div[2]/div/div/div/h2') %>%
  html_text() %>%
  strsplit(x = ., split = " ")
num <- as.numeric(num_lec[[1]][1])

title <- c()
id <- c()
for(i in 1:num){
  film_title <- page %>%
    html_nodes(xpath = paste0('//*[@id="page-content"]/section/div[3]/div[2]/div/div/div/div/a[', i, ']/div[4]/div[1]')) %>%
    html_text()
  title[i] <- film_title
  
  film_id <- page %>%
    html_nodes(xpath = paste0('//*[@id="page-content"]/section/div[3]/div[2]/div/div/div/div/a[', i, ']/div[3]/div')) %>%
    html_attr("data-object-id")
  id[i] <- film_id
}
tbl <- data.frame(Title = title, URL = paste0("https://www.thegreatcoursesplus.com/embed/player?filmId=", id))
tbl$Title <- gsub(tbl$Title, pattern = "\U00A0", replacement = " ")
tbl

url <- "https://www.thegreatcoursesplus.com/sign-in"
# binman::list_versions("chromedriver")
rD <- rsDriver(verbose = F, chromever = "76.0.3809.68")

remDr <- rD$client
remDr$navigate(url)
email <- remDr$findElement(using="xpath", '//*[@id="modal"]/div/div/div[2]/div[1]/form/p[1]/input')
email$sendKeysToElement(list(user_email))
password <- remDr$findElement(using="xpath", '//*[@id="modal"]/div/div/div[2]/div[1]/form/p[2]/input')
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
  format = "  downloading [:bar] :percent in :elapsed, eta: :eta",
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
    download.file(sub_url, destfile = paste0(i,". ", lecture_title,".srt"), quiet = T)
  }, error = function(msg){
    message(" No caption available.")
  })
}


