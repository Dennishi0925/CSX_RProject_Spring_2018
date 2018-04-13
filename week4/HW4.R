library(jsonlite)
library(tidyverse)
library(wordcloud2)
options(Encoding="UTF-8")
rm(list = ls())
data <- fromJSON("http://data.taipei/opendata/datalist/apiAccess?scope=resourceAquire&rid=a8fd6811-ab29-40dd-9b92-383e8ebd2a4e") 
str(data)
df <- data$result$results
str(df)
View(df)
ci <- df$CITYZONE %>% na.omit() %>%
  str_replace_all("區", "")
tci <- table(ci)
wordcloud2(tci)
