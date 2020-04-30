
setwd("F:\\TGC/")

library(httr)
library(dplyr)
library(xml2)
library(rvest)
library(RSelenium)
library(stringr)
library(purrr)
library(googledrive)
library(splashr)

user_email <- "yo80106@hotmail.com"
user_password <- "yo19921124"
url <- "https://www.thegreatcoursesplus.com/customer/account/signin/"
rD <- rsDriver(verbose = F, chromever = "80.0.3987.106")
"\\\\tp-fs01\\Bioinformatics\\"
file.path("\\\\tp-fs01")

remDr <- rD$client
remDr$navigate(url)
email <- remDr$findElement(using="css", '#inputEmailLogin')
email$sendKeysToElement(list(user_email))
password <- remDr$findElement(using="css", '#passwordLogin')
password$sendKeysToElement(list(user_password, key = "enter"))

undownload_ls <- list()
tgc <- drive_find("The Great Course Plus", n_max = 30)
course_catgories <- drive_ls(as_id(tgc[1,]$id))$name[order(drive_ls(as_id(tgc[1,]$id))$name)][-9]
for(i in 1:length(course_catgories)){
  subject_id <- drive_ls(as_id(tgc[1,]$id)) %>% filter(name == course_catgories[i]) %>% select(id) %>% pull()
  subject_ls <- drive_ls(as_id(subject_id)) %>% select(name, id)
  
  catgories_abr <- str_replace_all(course_catgories[i], "[^A-z]", "-") %>% str_to_lower()
  print(catgories_abr)
  all_course <- read_html(str_glue("https://www.thegreatcoursesplus.com/economics-and-finance"))
  course_title <- all_course %>%
    html_nodes(css = ".image__photo") %>%
    html_attrs() %>%
    map(6) %>%
    str_trim() %>%
    unlist()

  course_url <- all_course %>%
    html_nodes(css = ".item__link ") %>%
    html_attrs() %>%
    map(1) %>%
    str_trim() %>%
    unlist()

  course_tbl <- data.frame(Course_Title = course_title, Course_url = course_url)
  course_tbl$Course_Title <- gsub(course_tbl$Course_Title, pattern = "[:?/]", replacement = " -")
  course_tbl$Course_Title <- gsub(course_tbl$Course_Title, pattern = "¡¦", replacement = "'")
  course_tbl$Course_Title <- gsub(course_tbl$Course_Title, pattern = '"', replacement = "")
  course_tbl$Course_Title <- gsub(course_tbl$Course_Title, pattern = '¡X', replacement = " - ")
  course_tbl$Course_url <- as.character(course_tbl$Course_url)
  course_tbl <- course_tbl[!course_tbl$Course_Title %in% subject_ls$name,]
  course_tbl <- course_tbl[order(course_tbl$Course_Title),]
  undownload_ls[[i]] <- course_tbl
}
names(undownload_ls) <- course_catgories
course_tbl <- do.call("rbind", undownload_ls)
rownames(course_tbl) <- 1:nrow(course_tbl)

for(i in 1:nrow(course_tbl)){
  dir.create(course_tbl$Course_Title[i])
  course_url <- course_tbl$Course_url[i]
  
  remDr$navigate(course_url)
  video_title <- remDr$findElement(using="css", '#lectures-list > div:nth-child(3) > div.media-body > div:nth-child(1) > h5 > span.title')
  video_title$getElementText()
  play_trailer <- remDr$findElement(using="css", '#lectures-list > div:nth-child(3) > div.media-left > div > div')
  play_trailer$clickElement()
  remDr$getTitle()
  
  page <- read_html(course_url)
  num_lec <- page %>%
    html_nodes(css = "#course-tabs-row > div > ul > li.active > a") %>%
    html_text() %>%
    str_extract("[0-9]+") %>% 
    as.numeric()
  
  title <- c()
  id <- c()
  for(j in 1:num_lec){
    film_title <- page %>%
      html_nodes(xpath = paste0('//*[@id="page-content"]/section/div[3]/div[2]/div/div/div/div/a[', j, ']/div[4]/div[1]')) %>%
      html_text()
    title[j] <- film_title
    
    film_id <- page %>%
      html_nodes(xpath = paste0('//*[@id="page-content"]/section/div[3]/div[2]/div/div/div/div/a[', j, ']/div[3]/div')) %>%
      html_attr("data-object-id")
    id[j] <- film_id
  }
  tbl <- data.frame(Title = title, URL = paste0("https://www.thegreatcoursesplus.com/embed/player?filmId=", id))
  tbl$Title <- gsub(tbl$Title, pattern = "\U00A0", replacement = " ")
  tbl$Title <- gsub(tbl$Title, pattern = "\"", replacement = " ")
  print(tbl)
  
  remDr$navigate(course_url)
  remDr$getTitle()
  tryCatch({
    guidebook_info <- remDr$findElement(using='xpath', '//*[@id="page-content"]/section/div[1]/div/div[2]/div/div[2]/span[1]/a')
    book_url <- guidebook_info$getElementAttribute("href")
    book_title <- str_extract(string = book_url, pattern = "[0-9]+.*")
    course_id <- gsub(pattern = "[%_]", replacement = "", x = str_extract(string = book_title, pattern = "[0-9]*[%_]"))
    download.file(url = book_url[[1]], destfile = paste0(course_tbl$Course_Title[i], "/", book_title), method = "curl")
  }, error = function(msg){
    message("No course guidebook available.")
  })
  
  for(j in 1:num){
    lecture_url <- tbl[j,2]
    remDr$navigate(lecture_url)
    remDr$getTitle()
    
    lecture_title <- gsub(pattern = "[:?/]", replacement = " -", x = tbl[j,1])
    test_url <- remDr$findElement(using = "xpath", "/html/body/script")
    script_test <- test_url$getElementAttribute("innerText")
    hd_url <- script_test %>% str_remove_all("[\n|\t]") %>% str_squish() %>% str_extract_all("\\{(.*?)\\}") %>% unlist() %>% str_subset("720p") %>% str_extract("https:.+\\.mp4") %>% str_replace("https", "http")
    if(identical(hd_url, character(0))){
      sd_url <- script_test %>% str_remove_all("[\n|\t]") %>% str_squish() %>% str_extract_all("\\{(.*?)\\}") %>% unlist() %>% str_subset("480p") %>% str_extract("https:.+\\.mp4") %>% str_replace("https", "http")
      download.file(sd_url[1], destfile = paste0(course_tbl$Course_Title[i], "/", j,". ", lecture_title,".mp4"), method = "curl", quiet = T)
    }else{
      download.file(hd_url[1], destfile = paste0(course_tbl$Course_Title[i], "/", j,". ", lecture_title,".mp4"), method = "curl", quiet = T)
    }
    tryCatch({
      sub_url <- script_test %>% str_remove_all("[\n|\t]") %>% str_squish() %>% str_extract_all("\\{(.*?)\\}") %>% unlist() %>% str_subset("captions") %>% str_extract("https:.+\\.srt") %>% str_replace("https", "http")
      download.file(sub_url, destfile = paste0(course_tbl$Course_Title[i], "/", j,". ", lecture_title,".srt"), quiet = T)
    }, error = function(msg){
      message("No caption available.")
    })
  }
}
