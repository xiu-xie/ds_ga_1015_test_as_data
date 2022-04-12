# TA: Susmita Karyakarte
# Course: Text as Data
# Date: 03/10/2022
# Lab adapted from: Kevin Munger, Patrick Chester, Leslie Huang, Pedro L. Rodriguez, Lucia Motolinia

#----------------------------------------
# Set up environment                     ---
#----------------------------------------
# clear global environment
rm(list = ls())

set.seed(1234)

# load required libraries
library(dplyr)
library(caret)
# EXCELLENT DOCUMENTATION https://topepo.github.io/caret/index.html
library(quanteda)

# The caret (Classification and Regression Training) package is a 
# set of functions that attempt to streamline the process for 
# creating predictive models. The package has tools for :

# data splitting
# pre-processing
# feature selection
# model tuning using resampling
# variable importance estimation

# set working directory
setwd("F:/R/textasdata2022/W7_03_10_22")

#----------------------------------------
# 1. Load, clean and inspect data        ---
#----------------------------------------
news_data <- readRDS("news_data.rds")
View(news_data)
table(news_data$category)

# let's work with 2 categories
news_samp <- news_data %>% filter(category %in% c("WEIRD NEWS", "GOOD NEWS")) %>% select(headline, category) %>% setNames(c("text", "class"))

View(news_samp)

# get a sense of how the text looks
dim(news_samp)
head(news_samp$text[news_samp$class == "WEIRD NEWS"])
head(news_samp$text[news_samp$class == "GOOD NEWS"])

# some pre-processing (the rest will let dfm do)
news_samp$text <- gsub(pattern = "'", "", news_samp$text)  # replace apostrophes
news_samp$class <- recode(news_samp$class,  "WEIRD NEWS" = "weird", "GOOD NEWS" = "good")

View(news_samp)

# what's the distribution of classes?
prop.table(table(news_samp$class))

# randomize order (notice how we split below)
news_samp <- news_samp %>% sample_n(nrow(news_samp))
rownames(news_samp) <- NULL
View(news_samp)


#----------------------------------------
# 2. Support Vector Machine (SVM) using Caret ---
#----------------------------------------

# create document feature matrix
news_dfm <- dfm(news_samp$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english")) %>% convert("matrix")

View(news_dfm)

# Caret functions are also documented on https://www.rdocumentation.org/
# and https://rdrr.io/rforge/caret/

# https://www.rdocumentation.org/packages/caret/versions/6.0-90/topics/createDataPartition

# A. the caret package has its own partitioning function

ids_train <- createDataPartition(1:nrow(news_dfm), p = 0.8, list = FALSE, times = 1)
View(ids_train)
train_x <- news_dfm[ids_train, ] %>% as.data.frame() # train set data
View(train_x)
train_y <- news_samp$class[ids_train] %>% as.factor()  # train set labels
train_y
test_x <- news_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- news_samp$class[-ids_train] %>% as.factor() # test set labels

# baseline
baseline_acc <- max(prop.table(table(test_y)))

# B. define training options
trctrl <- trainControl(method = "none") 
#none: only fits one model to the entire training set

# C. train model (caret gives us access to even more options)
# see: https://topepo.github.io/caret/available-models.html

# Code behind this model
# getModelInfo(method = "svmLinear")

# svm - linear
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trctrl)

svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y)

# svm - radial
# takes longer to run
svm_mod_radial <- train(x = train_x,
                        y = train_y,
                        method = "svmRadial",
                        trControl = trctrl)

svm_radial_pred <- predict(svm_mod_radial, newdata = test_x)
svm_radial_cmat <- confusionMatrix(svm_radial_pred, test_y)

cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  svm_linear_cmat$overall[["Accuracy"]], "\n",
  "SVM-Radial Accuracy:",  svm_radial_cmat$overall[["Accuracy"]]
)

# Use getModelInfo() to understand tuning parameters

svmmodel <- getModelInfo("svmRadial", regex = FALSE)[[1]]
svmmodel$grid(x = train_x, y = train_y,5)

sigmas = kernlab::sigest(as.matrix(train_x), na.action = na.omit, scaled = FALSE)
# from the code, you can see it takes the mean of the two extreme quantiles
sigmas[-2]
mean(sigmas[-2])

svm_mod_radial$bestTune

# To get default parameters
# Tuning parameters don't have default values
# They are based on different formulae, samples, number of records, 
# columns in a dataframe and so on.
# Look up code to understand how values are being calculated
View(modelLookup())
modelLookup(model = "svmLinear")

#----------------------------------------
# example with cross-validation
#----------------------------------------
# https://topepo.github.io/caret/model-training-and-tuning.html

# now we will have train / test / validation
val_x <- test_x
val_y <- test_y 
trctrl <- trainControl(method = "cv",
                       number = 5)

# Also available: Leave One Out CV
#trctrl <- trainControl(method = "LOOCV", p = 0.8)

# svm - linear
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trctrl)

# predict on heldout validation data
svm_linear_pred <- predict(svm_mod_linear, newdata = val_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, val_y)

# confusion matrix on predictions
svm_linear_cmat

#----------------------------------------

trctrl <- trainControl(method = "cv",
                       number = 3)

# svm - radial #this takes a long time to run!
svm_mod_radial <- train(x = train_x,
                        y = train_y,
                        method = "svmRadial",
                        trControl = trctrl)

svm_radial_pred <- predict(svm_mod_radial, newdata = val_x)
svm_radial_cmat <- confusionMatrix(svm_radial_pred, val_y)

cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  svm_linear_cmat$overall[["Accuracy"]], "\n",
  "SVM-Radial Accuracy:",  svm_radial_cmat$overall[["Accuracy"]]
)

