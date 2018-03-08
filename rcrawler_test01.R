library(rvest)
library(stringr)
rm(list = ls())
url = "https://www.datacamp.com/courses/all"
webpage <- read_html(url, encoding = "UTF-8")

# check items
str_extract_all(webpage, pattern = ".course-block__\\w+\\w*")
table(str_extract_all(webpage, pattern = ".course-block__\\w+\\w*"))

# writing functions
crawl_datacamp <- function(x) {
y <- html_nodes(webpage, str_c('.course-block__', x))
z <- html_text(y)
}

###
title <- crawl_datacamp("title")
author <- crawl_datacamp("author")
description <- crawl_datacamp("description")
description <- str_replace_all(description, "\\n[:blank:]+", "")
length  <- crawl_datacamp("length ")
author <- crawl_datacamp("author-name")
author_info <- crawl_datacamp("author-ocupation")
author_info <- str_replace_all(author_info, "\\n[:blank:]+", "")
# language
str_extract_all(webpage, pattern = ".course-block__technology[:graph:]*")
language <- unlist(str_extract_all(webpage, pattern = ".course-block__technology--\\w*"))
language <- str_replace_all(language, pattern = ".course-block__technology--", replacement = "")

datacamp <- data.frame(title, author, author_info, description, length, language)
View(datacamp)
write.table(datacamp,file="D:/course_DataScienceProgramming/crawler01.csv",sep = ",", row.names = F, na = "NA")
