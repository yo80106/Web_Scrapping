
setwd("F:\\")

library(httr)
library(dplyr)
library(xml2)
library(rvest)
library(RSelenium)
library(stringr)
library(progress)
# install.packages("wdman")
# library(googledrive)

course_url <- "https://www.thegreatcoursesplus.com/historys-greatest-voyages-of-exploration"
user_email <- "goy54386@cndps.com"
user_password <- "goy54386"
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
tbl

url <- "https://www.thegreatcoursesplus.com/sign-in"
rD <- rsDriver(verbose = F, chromever = "73.0.3683.68")

# assign the client to a new variable
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
  format = "  downloading :what [:bar] :percent in :elapsed, eta: :eta",
  total = 100, clear = FALSE, width= 120)

for(i in 1:num){
  pb$tick(0)
  lecture_url <- tbl[i,2]
  remDr$navigate(lecture_url)
  remDr$getTitle()
  
  film_info <- remDr$findElement(using="xpath", '//*[@id="snagHtml5Player"]/div[2]/video')
  film_url <- film_info$getElementAttribute("src")
  old_url <- gsub(pattern= "\\?.*", replacement = "", x = film_url[[1]])
  new_url <- gsub(pattern = "[0-9]*kbps", replacement = "9216kbps", x = old_url)
  new_url <- gsub(pattern = "https://", replacement = "http://", x = new_url)
  new_title <- gsub(pattern = "[:?/]", replacement = "", x = tbl[i,1])
  # sub_url <- paste0("http://vtgcmp4-viewlift.akamaized.net/Captions/SRT/", course_id,"_SRT/",course_id,"_",str_pad(i,2,pad=0),".srt")
  
  pb$tick(100/num, tokens = list(what = paste0(course_id,"_",str_pad(i,2,pad=0), ".mp4")))
  download.file(new_url, destfile = paste0(i,". ", new_title,".mp4"), method = "curl", quiet = T)
  # download.file(sub_url, destfile = paste0(course_id,"_",str_pad(i,2,pad=0),".srt"), quiet = T)
  
  #mp4 <- list.files(pattern = ".mp4")
  #srt <- list.files(pattern = ".srt")
  #drive_upload(media = mp4, path = paste0(gd$path, mp4))
  #drive_upload(media = srt, path = paste0(gd$path, srt))
  #file.remove(mp4)
  #file.remove(srt)
}
  
pb <- progress_bar$new(
  format = "  downloading :what [:bar] :percent in :elapsed, eta: :eta",
  total = 100, clear = FALSE, width= 120)
for(i in 1:num){
  sub_url <- paste0("http://vtgcmp4-viewlift.akamaized.net/Captions/SRT/", course_id,"_SRT/",course_id,"_",str_pad(i,2,pad=0),".srt")
  pb$tick(100/num, tokens = list(what = paste0(course_id,"_",str_pad(i,2,pad=0), ".srt")))
  download.file(sub_url, destfile = paste0(course_id,"_",str_pad(i,2,pad=0),".srt"), quiet = T)
}

# pb <- progress_bar$new(
#   format = "  downloading :what [:bar] :percent in :elapsed, eta: :eta",
#   total = 100, clear = FALSE, width= 120)
# 
# for(n in 1:num){
#   sub_url <- paste0("https://vtgc.viewlift.com/ClosedCaptions/2018/08/5012_",str_pad(n,2,pad=0),".srt")
#   pb$tick(100/num, tokens = list(what = paste0("5012_",str_pad(n,2,pad=0),".srt")))
#   download.file(sub_url, destfile = paste0("1530_",str_pad(n,2,pad=0),".srt"), quiet = T)
# }
