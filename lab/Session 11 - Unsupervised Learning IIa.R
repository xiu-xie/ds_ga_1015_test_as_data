# TA: Susmita Karyakarte
# Course: Text as Data
# Date: 04/14/2022
# Lab adapted from: Patrick Chester, Leslie Huang, Pedro L. Rodriguez, and Lucia Motolinia

# additional resources: 
# original paper introducing LDA: http://www.jmlr.org/papers/volume3/blei03a/blei03a.pdf
# https://www.tidytextmining.com/topicmodeling.html
# https://medium.com/nanonets/topic-modeling-with-lsa-psla-lda-and-lda2vec-555ff65b0b05
# most recent addition to topic modeling methods: https://multithreaded.stitchfix.com/blog/2016/05/27/lda2vec/#topic=38&lambda=1&term= 
# human validation of topic models: https://dl.acm.org/citation.cfm?id=2984126
# topic models use the frequency in which words appear to infer the 
# topics in the documents that produced those words

# basic intuition:
# a. documents are represented as random mixtures over latent topics.
# b. a topic is characterized by a distribution over words.
# we now propose a GENERATIVE MODEL OF THE DATA

# want to maximize the probability of a corpus as a 
# function of our parameters (of the dirichlets) and latent variables
# (doc topic mixtures and topic word distributions).

rm(list = ls())

setwd("F:/R/textasdata2022/W10_04_08_21")

set.seed(1234)

# Check for these packages, install them if you don't have them
# install.packages("tidytext")
# install.packages("topicmodels")
# install.packages("ldatuning")
# install.packages("stringi")
# install.packages("rjson")

# install.packages("lubridate")
# install.packages("parallel")
# install.packages("doParallel")
# install.packages("tidyr")

libraries <- c("ldatuning", "topicmodels", "ggplot2", "dplyr", "rjson", "quanteda", "lubridate", "parallel", "doParallel", "tidytext", "stringi", "tidyr")
lapply(libraries, require, character.only = TRUE)

## 1 Preprocessing

# Load data
blm_tweets <- read.csv("blm_samp.csv", stringsAsFactors = F)

View(blm_tweets)

# Create date vectors
blm_tweets$datetime <- as.POSIXct(strptime(blm_tweets$created_at, "%a %b %d %T %z %Y",tz = "GMT")) # full date/timestamp
blm_tweets$date <- mdy(paste(month(blm_tweets$datetime), day(blm_tweets$datetime), year(blm_tweets$datetime), sep = "-")) # date only

# Collapse tweets so we are looking at the total tweets at the day level
blm_tweets_sum <- blm_tweets %>% group_by(date) %>% summarise(text = paste(text, collapse = " "))

View(blm_tweets_sum)

# Remove non ASCII characters
blm_tweets_sum$text <- stringi::stri_trans_general(blm_tweets_sum$text, "latin-ascii")

# Removes solitary letters
blm_tweets_sum$text <- gsub(" [A-z] ", " ", blm_tweets_sum$text)

View(blm_tweets_sum)

# As always we begin with a DFM.
# Create DFM
blm_dfm <-dfm(blm_tweets_sum$text, stem = F, remove_punct = T, tolower = T, remove_numbers = TRUE, remove = c(stopwords("english"), "http","https","rt", "t.co"))

blm_dfm

## 2 LDA Topic models

# Selecting K

# Identify an appropriate number of topics 
# (FYI, this function takes a while)
k_optimize_blm <- FindTopicsNumber(
  blm_dfm,
  topics = seq(from = 2, to = 10, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1992),
  #libpath = "F:/R/R-4.1.2/library",
  mc.cores = detectCores(), # to us all cores available
  verbose = TRUE
)

View(k_optimize_blm)

# https://rdrr.io/cran/ldatuning/api/

# Arun2010: The measure is computed in terms of symmetric 
# KL-Divergence of salient distributions that are derived 
# from these matrix factor and is observed that the divergence 
# values are higher for non-optimal number of topics (maximize)
# http://doi.org/10.1007/978-3-642-13657-3_43

# CaoJuan2009: method of adaptively selecting the best LDA model 
# based on density.(minimize)
# http://doi.org/10.1016/j.neucom.2008.06.011

# Griffths2004: To evaluate the consequences of changing the number 
# of topics T, used the Gibbs sampling algorithm to obtain samples 
# from the posterior distribution over z at several choices of 
# T(minimize)
# http://doi.org/10.1073/pnas.0307752101

# Deveaud2014: Accurate and effective latent concept modeling for ad hoc 
# information retrieval.
# http://doi.org/10.3166/dn.17.1.61-84

# Anyone facing issues with running topicmodels
# https://github.com/nikita-moor/ldatuning/issues/3

# Find the path where libraries are installed with : .libPaths()
# Then use list.files(path) to see the packages installed

FindTopicsNumber_plot(k_optimize_blm)

# Where do these metrics come from?

# Go here for the citations (and another tutorial)
# https://cran.r-project.org/web/packages/ldatuning/ldatuning.pdf
# https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html

# What should you consider when choosing the number of topics you use in a topic model?

## 3 Visualizing Word weights

# Set number of topics
k <- 5

# Fit the topic model with the chosen k
system.time(
  blm_tm <- LDA(blm_dfm, k = k, method = "Gibbs",  control = list(seed = 1234)))

# Other parameters that we do not use here (because they increase the time the model takes) can be passed to the control parameter
?`LDAcontrol-class`
# iter : num iterations
# thin : every thin iteration is returned for iter iterations
# burnin : number of initial iterations discarded

## Letter soup

# gamma = posterior document distribution over topics
# what are the dimensions of gamma?

View(blm_tm)

blm_tm@control

dim(blm_tm@gamma)
blm_tm@gamma[1:5,1:5]
rowSums(blm_tm@gamma) # each row sums to?

# beta = topic distribution over words
dim(blm_dfm)  # how many features do we have?
dim(blm_tm@beta)
blm_tm@beta[1:5,1:5]

# Per topic per word proabilities matrix (beta)
blm_topics <- tidy(blm_tm, matrix = "beta") 
blm_topics

# Side note: You can pass objects between tidytext() and 
# topicmodels() functions because tidytext() implements topic 
# models from topicmodels()

# Generates a df of top terms
blm_top_terms <- blm_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

blm_top_terms

# Creates a plot of the weights and terms by topic
blm_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Creates a plot of features with greatest difference in word probabilities between two topics
blm_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  filter(topic %in% c("topic1", "topic2")) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1)) %>%
  arrange(-abs(log_ratio)) %>%
  slice(c(1:10,(nrow(.)-9):nrow(.))) %>%
  arrange(-log_ratio) %>%
  mutate(term = factor(term, levels = unique(term))) %>%
  ggplot(aes(as.factor(term), log_ratio)) +
  geom_col(show.legend = FALSE) +
  xlab("Terms") + ylab("Log-Ratio") +
  coord_flip()

## 4 Visualizing topic trends over time

# Store the results of the mixture of topics over documents 
doc_topics <- blm_tm@gamma

# To store the results of words over topics:
#words_topics <- blm_tm@beta

# Transpose the data so that the days are columns
doc_topics <- t(doc_topics)
dim(doc_topics)

# Arrange topics
# Find the top topic per column (day)
max <- apply(doc_topics, 2, which.max)

# Write a function that finds the second max
which.max2 <- function(x){
  which(x == sort(x,partial=(k-1))[k-1])
}

max2 <- apply(doc_topics, 2, which.max2)
max2 <- sapply(max2, max)

# Coding police shooting events
victim <- c("Freddie Gray", "Sandra Bland")
shootings <- mdy(c("04/12/2015","7/13/2015"))

# Combine data
top2 <- data.frame(top_topic = max, second_topic = max2, date = ymd(blm_tweets_sum$date))

# Plot
blm_plot <- ggplot(top2, aes(x=date, y=top_topic, pch="First")) 

blm_plot + geom_point(aes(x=date, y=second_topic, pch="Second") ) +theme_bw() + 
  ylab("Topic Number") + ggtitle("BLM-Related Tweets from 2014 to 2016 over Topics") + geom_point() + xlab(NULL) + 
  geom_vline(xintercept=as.numeric(shootings[1]), color = "blue", linetype=4) + # Freddie Gray (Topic)
  geom_vline(xintercept=as.numeric(shootings[2]), color = "black", linetype=4)  + # Sandra Bland
  scale_shape_manual(values=c(18, 1), name = "Topic Rank") 
