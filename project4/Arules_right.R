library(tidyverse)
library(arules)
library(arulesViz)
rm(list=ls())
df <- read_csv("D:/R_programming/DSprogramming/groceries.csv",
               col_types = cols(
                 Member_number = col_character(),
                 Date = col_date("%m/%d/%Y"),
                 itemDescription = col_character()))
names(df) <- c("member_ID", "date", "item")
glimpse(df)
df %>% group_by(member_ID, date) %>%
  count(sort = T)
#bf <- df %>% group_by(member_ID, date) %>%
#  mutate(basket = paste(item, collapse = ","), n = n()) %>%
#  ungroup() %>%
#  select(-item) %>%
#  arrange(desc(n)) %>%
#  distinct(basket, .keep_all = T)

#df_tid <- bf %>%
#  select(basket) #%>%
#mutate(ID = row_number())
#glimpse(df_tid)
#write.csv(df_tid, "df_tid.csv", row.names = T)

df2 <- df %>%
  unite(gawy, c(member_ID, date), sep = "-")
df2$group_id <- df2 %>% group_indices(gawy)
df2$gawy <- NULL
#df2 %>% arrange(group_id) %>% View()
df2 <- df2 %>% select(group_id, item) %>% arrange(group_id)

#write.csv(df2, "df2.csv")
order_trans <- read.transactions(
  file = "df2.csv",
  format = "single",
  sep = ",",
  cols=c("group_id","item"),
  rm.duplicates = T
)
order_trans[1:10,1:5]
length(order_trans)
size(order_trans)
itemFrequency(order_trans)*199
itemFrequencyPlot(order_trans, topN=10,type="absolute")
itemFrequencyPlot(order_trans, topN=10)
dim(order_trans)
df2gawy <- as(df2,"transactions")
df3 <- df2 %>% mutate(group_id = as.factor(group_id), item = factor(item))
df2gawy <- as(df3,"transactions")
items(rules)
order_trans
image(order_trans)
gawy <- as(order_trans,"list")
gawy <- LIST(order_trans)
gawy2 <- LIST(order_trans, decode = F)
gawy2
gawy
summary(df2gawy)
summary(order_trans)
basket_rules <- apriori(order_trans,parameter = list(sup = 0.01, conf = 0.5,target="rules"))
rules<-sort(basket_rules, decreasing=TRUE,by="confidence")
inspect(rules) %>% View()
summary(basket_rules) %>% View()

plot(basket_rules)
plot(basket_rules, method = "grouped", control = list(k = 5))
plot(basket_rules, method="graph", control=list(type="items"))
plot(basket_rules, method="paracoord",  control=list(alpha=.5, reorder=TRUE))
#plot(basket_rules,measure=c("support","lift"),shading="confidence",interactive=T)
plot(rules,method="graph",interactive=TRUE,shading=NA)

