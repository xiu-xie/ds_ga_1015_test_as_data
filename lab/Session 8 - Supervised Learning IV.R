# TA: Susmita Karyakarte
# Course: Text as Data
# Date: 03/24/2022

# Lab adapted from: Kevin Munger, Patrick Chester, Leslie Huang, Pedro L. Rodriguez, Lucia Motolinia

#----------------------------------------
# Set up environment                     ---
#----------------------------------------
# clear global environment
rm(list = ls())

set.seed(1234)

# load required libraries
library(dplyr)
library(randomForest)
library(mlbench)
library(caret)

# set working directory
setwd("F:/R/textasdata2022/W8_03_24_22")

#----------------------------------------
# 1. Load, clean and inspect data        ---
#----------------------------------------
news_data <- readRDS("news_data.rds") # same data as last week, but we will look at different categories
table(news_data$category)

View(news_data)

# let's work with 2 categories and get a sample of 500 news
# remember the order of operations matters! We first select category, group by, and then sample 500 obs
news_samp <- news_data %>% 
  filter(category %in% c("MONEY", "LATINO VOICES")) %>% 
  group_by(category) %>%
  sample_n(500) %>%  # sample 500 of each to reduce computation time (for lab purposes)
  ungroup() %>%
  select(headline, category) %>% 
  setNames(c("text", "class"))


# get a sense of how the text looks
dim(news_samp)
head(news_samp$text[news_samp$class == "MONEY"])
head(news_samp$text[news_samp$class == "LATINO VOICES"])

View(news_samp)

# some pre-processing (the rest we'll let dfm do)
news_samp$text <- gsub(pattern = "'", "", news_samp$text)  # replace apostrophes
news_samp$class <- recode(news_samp$class,  "MONEY" = "money", "LATINO VOICES" = "latino")

# what's the distribution of classes?
prop.table(table(news_samp$class))

# randomize order (notice how we split below)
news_samp <- news_samp %>% sample_n(nrow(news_samp))
rownames(news_samp) <- NULL

#----------------------------------------
# 2. Prepare Data                        ---
#----------------------------------------
library(quanteda)

# create document feature matrix, actually a MATRIX object this time!
# keep tokens that appear in at least 5 headlines
news_dfm <- dfm(news_samp$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english")) %>% 
  dfm_trim(min_termfreq = 5) %>% 
  convert("matrix")

View(news_dfm)

ids_train <- createDataPartition(1:nrow(news_dfm), p = 0.8, list = FALSE, times = 1)
train_x <- news_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- news_samp$class[ids_train] %>% as.factor()  # train set labels

View(train_x)
train_y

test_x <- news_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- news_samp$class[-ids_train] %>% as.factor() # test set labels

View(test_x)
test_y

#----------------------------------------
# 3. Using RandomForest                  ---
#----------------------------------------
# number of features to sample at each split
mtry <- sqrt(ncol(train_x))
ncol(train_x)
mtry
# mtry <- 10
ntree <- 51  # num of trees to grow

# Here mtry and ntree are the tuning parameters

# more trees generally improve accuracy but at the cost of computation time
# odd numbers avoid ties (recall default aggregation is "majority voting")

# User CPU time : gives the CPU time spent by the current process 
# (i.e., the current R session) The time that you as a user experienced

# Elapsed time : 'real' elapsed time since the process was
# started

# System time : gives the CPU time spent by the kernel (the operating system) 
# on behalf of the current process for things like opening files, doing input 
# or output, starting other processes

system.time(rf.base <- randomForest(x = train_x, y = train_y, ntree = ntree, mtry = mtry, importance = TRUE))
token_importance <- round(importance(rf.base, 2), 2)
head(rownames(token_importance)[order(-token_importance)])

# print results
print(rf.base)

# The OOB error estimate is based on the predictions that you get 
# for each data point by averaging only those trees, 
# for which the record was not in the training data.

# If you have a low number of trees the OOB error might be 
# not a good estimate of the error rate that training an algorithm 
# like this has on new data, because each tree in RF tends to be 
# underfit and only once you combine enough trees the RF gets 
# better (so if there's only a small number of trees per record, 
# it may underestimate performance). On the other hand, 
# once you have a huge number of trees it becomes a pretty 
# good estimate like you get from a train-validation split 
# with a lot of data (or cross-validation).


# plot importance
# gini impurity = how "pure" is given node ~ class distribution
# = 0 if all instances the node applies to are of the same class
# upper bound depends on number of instances
varImpPlot(rf.base, n.var = 10, main = "Variable Importance")

# This is a fundamental outcome of the random forest and it shows, 
# for each variable, how important it is in classifying the data.
# The Mean Decrease Accuracy plot expresses how much accuracy 
# the model looses by excluding each variable. 
# The more the accuracy suffers, the more important the variable 
# is for the successful classification. The variables are presented 
# from descending importance. The mean decrease in Gini coefficient 
# is a measure of how each variable contributes to the 
# homogeneity of the nodes and leaves in the resulting random forest. The higher the value of mean decrease accuracy or mean decrease Gini score, the higher the importance of the variable in the model.


#?predict.randomForest

predict_test <- predict(rf.base, newdata = test_x)
confusionMatrix(data = predict_test, reference = test_y)

# to tune hyperparameters, use:
?tuneRF


#----------------------------------------
# 4. 5-Fold CV RandomForest Using Caret            ---
#----------------------------------------
# note that the RF model in caret calls randomForest, but it's wrapped in caret

# Documentation : https://topepo.github.io/caret/train-models-by-tag.html#random-forest

# View(modelLookup(model = "rf"))

trainControl <- trainControl(method = "cv", number = 5)
metric <- "Accuracy"
mtry <- sqrt(ncol(train_x))
ntree <- 51  
tunegrid <- expand.grid(.mtry = mtry)
system.time(rf.caret <- train(x = train_x, y = train_y, 
                              method = "rf", metric = metric, 
                              tuneGrid = tunegrid, trControl = trainControl,
                              ntree = ntree)
            )

# print results
print(rf.caret)

rf.caret$finalModel

rf_predict <- predict(rf.caret, newdata = test_x)
confusionMatrix(rf_predict, reference = test_y)

# plot importance
varImpPlot(rf.caret$finalModel, n.var = 10, main = "Variable Importance")


#----------------------------------------
# 4.5 Follow up from last time
#----------------------------------------
# how is the CV best model selected?

# function used to select the optimal tuning parameter

# selectionFunction can be used to supply a function to 
# algorithmically determine the final model.

# There are three existing functions in the package: 
# best
# oneSE
# tolerance

trainControl$selectionFunction
?best

#----------------------------------------
# 5. RandomForest Using Caret + tuning   ---
#----------------------------------------
# we are going to gridsearch over 1 parameter: mtry

trainControl <- trainControl(method = "cv", number = 5)
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry = c(0.5*mtry, mtry, 1.5*mtry))  
# at the moment caret only allows tuning of mtry 
# (partly b/c ntree is just a matter of computational constratints)
system.time(rf.grid <- train(x = train_x, y = train_y, method = "rf", 
                             metric = metric, tuneGrid = tunegrid, trControl = trainControl, 
                             ntree = ntree)
            )
# print grid search results
print(rf.grid)

# plot grid search results
plot(rf.grid)

#----------------------------------------
# 6. RandomForest Using Caret + manual tuning ---
#----------------------------------------
# we have one value for mtry and we will train 3 models with different values for ntree

tunegrid <- expand.grid(.mtry = mtry)

# ntree = 1
system.time(rf.man1 <- train(x = train_x, y = train_y, method = "rf", 
                             metric = metric, tuneGrid = tunegrid, trControl = trainControl, 
                             ntree = 1))

# ntree = 5
system.time(rf.man2 <- train(x = train_x, y = train_y, method = "rf", 
                             metric = metric, tuneGrid = tunegrid, trControl = trainControl, 
                             ntree = 5))

# ntree = 51
system.time(rf.man3 <- train(x = train_x, y = train_y, method = "rf", 
                             metric = metric, tuneGrid = tunegrid, trControl = trainControl, 
                             ntree = 51))

# collect results & summarize
results <- resamples(list(rf1 = rf.man1, rf5 = rf.man2, rf51 = rf.man3))
summary(results)

# test set accuracy
(cm <- confusionMatrix(predict(rf.man1, newdata = test_x), test_y))
# access the components of the results with the $ operator
cm$table
cm$overall

confusionMatrix(predict(rf.man2, newdata = test_x), test_y)
confusionMatrix(predict(rf.man3, newdata = test_x), test_y)

# box and whisker plots to compare models
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
bwplot(results, scales = scales)

# reminder: Kappa = Cohen's Kappa, compares observed accuracy 
# with expected accuracy (think: baseline accuracy)
