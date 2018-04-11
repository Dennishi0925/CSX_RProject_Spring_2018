#Xpath語法：// 選取該節點下所包含的路徑、/ 選取該節點、[] 選取類型、@ 選取屬性。
library(tidyverse)
library(tidytext)
library(rvest)
library(lubridate)
rm(list = ls())
#url <- "https://www.ptt.cc/bbs/KoreaDrama/index2025.html"
#url <- "https://www.ptt.cc/bbs/KoreaDrama/index.html"
part1 = "https://www.ptt.cc/bbs/KoreaDrama/index"
part2 = ".html"

options(stringsAsFactors = F)
url_test = "https://www.ptt.cc/bbs/KoreaDrama/M.1522498952.A.5E9.html"

abc <- paste0(part1, 1950:2025 , part2) %>%
  map(read_html) 
abc_title <- abc %>%
  map(html_nodes,css= ".title a") %>%
  map(html_text)

check <- str_detect(unlist(abc_title), pattern = "\\LIVE] tvN 和遊記")
sum(check)

abc_link <- abc %>%
  lapply(html_nodes , css= ".title a") %>%
  lapply(html_attr, "href") %>%
  unlist()

urls <- abc_link[check]
article_links <- paste0("https://www.ptt.cc", urls)  

ptt_article_crawler <- function(url) {
  #main content
  main_author = ".article-metaline:nth-child(1) .article-meta-value"
  main_title = ".article-metaline-right+ .article-metaline .article-meta-value"
  main_datetime = ".article-metaline+ .article-metaline .article-meta-value"
  main_content = "#main-content"
  main_link = ".f2 a"
  #push content
  push_tag = ".push-tag"
  push_id = ".push-userid"
  push_text = ".push-content"
  push_datetime = ".push-ipdatetime"
  # css argument 
  main_column = c(main_author, main_title, main_datetime, main_content, main_link)
  push_column = c(push_tag, push_id, push_text, push_datetime)
  # main and push as two empty lists
  main = list()
  push = list()
  raw_html <- url %>% read_html()
  # for loop
  for(i in 1:length(main_column)){
    content <- raw_html %>% html_nodes(css = main_column[i]) %>% html_text()
    main[[i]] <- content
  }
  main <- as.data.frame(matrix(unlist(main), nrow = 1))
  colnames(main) <- c("main_author", "main_title", "main_datetime", "main_content", "main_link")
  
  for(i in 1:length(push_column)){
    content <- raw_html %>% html_nodes(css = push_column[i]) %>% html_text()
    push[[i]] <- content
  }
  push <- as.data.frame(push)
  colnames(push) <-  c("push_tag", "push_id", "push_text", "push_datetime")
  # clean
  main$main_content <- main$main_content %>%
    str_replace_all("\n", replacement = "") %>% # 清理斷行符號
    str_replace_all("--※.+", replacement = "") %>% # 去尾
    str_replace_all("作者.+:[0-9]{2}\\s[0-9]{4}", replacement = "") %>% #sentfromy
    str_replace_all("-----Sent from.+", replacement = "")
  main$weekday <- main$main_datetime %>% str_sub(1,4)
  main$datetime <- main$main_datetime %>% str_sub(5,-1) %>% parse_datetime("%b %d %H:%M:%S %Y")
  # main$authorip <- main$authorip %>% str_sub(str_locate(., "來")[1] + 4 , -2)
  #######
  push$push_text <- push$push_text%>% str_replace(pattern = ":", replacement = "") %>% str_replace_all(pattern = "\\s", replacement = "")
  #2018/04/0100:42
  push$push_datetime <- toString(year(main$datetime)) %>% str_c(push$push_datetime, sep = "/" ) %>% str_replace_all(" ", "")
  push$date <- push$push_datetime %>% str_sub(1,10) %>% parse_date("%Y/%m/%d", locale = locale(tz = "Asia/Taipei"))
  push$time <- push$push_datetime %>% str_sub(11,-1) %>% parse_time("%H:%M")
  push$weekday <- wday(push$date, label = T)
  push$month <- month(push$date)
  push$day <- day(push$date)
  push$hour <- hour(push$time)
  push$minute <- minute(push$time)
  #######
  article <- list("main" = main, "push" = push)
  return(article)
}

article_Korean_Odyssey = list()
for(i in 1:length(article_links)) {
  article_Korean_Odyssey[[i]] = ptt_push_crawler(article_links[i])
}
names(article_Korean_Odyssey)= str_c("article", c(1:22))

saveRDS(article_Korean_Odyssey, "article_Korean_Odyssey")

article_Korean_Odyssey <- read_rds("article_Korean_Odyssey")
names(article_Korean_Odyssey)= str_c("article", c(1:22))

article_Korean_Odyssey_push = tibble()
for(i in 1:22) {
  article_Korean_Odyssey_push <- article_Korean_Odyssey[[i]]$push %>%
    mutate(article = i) %>% rbind(article_Korean_Odyssey_push)
}
View(article_Korean_Odyssey_push)
rm(article_Korean_Odyssey_push)
gawy = mutate(article_Korean_Odyssey[[1]]$push, article = i)
gawy = as_tibble(gawy)
View(gawy)

article_tidytext <- select(article_Korean_Odyssey_push, push_text, article)
View(article_tidytext)
stop_words_chinese <- read_csv("stopwords_chi_updated.csv", locale = locale(encoding = "UTF-8"), col_names = F)
View(stop_words_chinese)
names(stop_words_chinese) <- c("number", "word")
tidy_KO <- article_tidytext  %>%
  unnest_tokens(word, push_text) %>%
  anti_join(stop_words_chinese, by = c("word"))
View(tidy_KO)
tidy_KO_count <- tidy_KO %>% group_by(article) %>%
  count(article, word, sort = T) %>%
  ungroup()
tidy_KO_tfidf <- tidy_KO_count %>%
  bind_tf_idf(word, article, n)

tidy_KO_tfidf_top5 <- tidy_KO_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(article) %>%
  top_n(5) %>%
  ungroup
tidy_KO_tfidf_top5 %>%
  ggplot(aes(word, tf_idf, fill = article)) + #, fill = article
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~ article, ncol = 4, scales = "free") +
  coord_flip()

