library(tidyverse)
wbcd <- read_rds("D:/R_programming/Journalism_homework/NTUSC/datalm")
wbcd <- as_tibble(wbcd)
wbcd$Attnd <- ifelse(wbcd$Attnd_Rate >= 0.6, "H", "L")
#wbcd %>% View()
# table of diagnosis
table(wbcd$Attnd)

# recode diagnosis as a factor
wbcd$Attnd <- factor(wbcd$Attnd, levels = c("H", "L"),
                         labels = c("High", "Low"))

# table or proportions with more informative labels
round(prop.table(table(wbcd$Attnd)) * 100, digits = 1)
colnames(wbcd)
# summarize three numeric features
summary(wbcd[c("support_rate", "vote_rate", "nonobj_Rate")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the wbcd data
wbcd_n <- wbcd %>%
  select(4:12) %>%
  mutate(support_rate = normalize(support_rate),
         nonobj_Rate = normalize(nonobj_Rate),
         vote_rate = normalize(vote_rate),
         college_population_rate = normalize(college_population_rate)) %>%
  select(term, support_rate, nonobj_Rate, vote_rate, college_population_rate, Attnd)
# confirm that normalization worked
summary(wbcd_n$support_rate)

# create training and test data
wbcd_train <- wbcd_n %>% filter(term != "106-1")
wbcd_test <- wbcd_n %>% filter(term == "106-1")

# create labels for training and test data
wbcd_train_labels <- wbcd_train$Attnd
wbcd_test_labels <- wbcd_test$Attnd

wbcd_train_labels <- as_vector(wbcd_train_labels)
wbcd_test_labels <- as_vector(wbcd_test_labels)

# preserve needed column only
wbcd_train <- wbcd_train %>% select(2:4)
wbcd_test <- wbcd_test %>% select(2:4)
## Step 3: Training a model on the data ----

# load the "class" library
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

## Step 4: Evaluating model performance ----
#install.packages("gmodels")
# load the "gmodels" library
library(gmodels)
prop.table(table(wbcd_test_labels, wbcd_test_pred))

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

## Step 5: Improving model performance ----

# use the scale() function to z-score standardize a data frame
wbcd_z <- wbcd %>%
  select(support_rate, nonobj_Rate, vote_rate, Attnd, term)
wbcd_z_scale <- as.data.frame(scale(wbcd_z[-(4:5)]))
# confirm that the transformation was applied correctly
summary(wbcd_z_scale$support_rate)

# create training and test datasets
wbcd_z_train <- wbcd_z %>% filter(term != "106-1")
wbcd_z_train <- scale(wbcd_z_train[-(4:5)])
wbcd_z_test <- wbcd_z %>% filter(term == "106-1")
wbcd_z_test <- scale(wbcd_z_test[-(4:5)])
# re-classify test cases
wbcd_test_pred_z <- knn(train = wbcd_z_train, test = wbcd_z_test,
                      cl = wbcd_train_labels, k = 21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred_z,
           prop.chisq = FALSE)

# try several different values of k

wbcd_test_pred_z <- knn(train = wbcd_z_train, test = wbcd_z_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred_z, prop.chisq=FALSE)

wbcd_test_pred_z <- knn(train = wbcd_z_train, test = wbcd_z_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred_z, prop.chisq=FALSE)

wbcd_test_pred_z <- knn(train = wbcd_z_train, test = wbcd_z_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred_z, prop.chisq=FALSE)

wbcd_test_pred_z <- knn(train = wbcd_z_train, test = wbcd_z_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred_z, prop.chisq=FALSE)

wbcd_test_pred_z <- knn(train = wbcd_z_train, test = wbcd_z_test, cl = wbcd_train_labels, k=17)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred_z, prop.chisq=FALSE)

wbcd_test_pred_z <- knn(train = wbcd_z_train, test = wbcd_z_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred_z, prop.chisq=FALSE)
