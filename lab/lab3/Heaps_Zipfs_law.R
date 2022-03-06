# TA: Susmita Karyakarte
# Course: Text as Data
# Date: 2/17/2022
# Lab adapted from: Kevin Munger, Patrick Chester, Leslie Huang, Pedro L. Rodriguez, and Lucia Motolinia

rm(list = ls())

library(dplyr)
library(quanteda)
library(quanteda.corpora)
library(quanteda.textstats)
library(ggplot2)
library(pbapply)

#-----------------------------
# HEAP'S LAW
#-----------------------------
# Token-type relationship in corpus
# How might pre-processing affect this relationship? 
# Think about reducing the dimensionality of the problem.

# Heaps law estimates vocabulary size as a function of collection size:
#     M = kT^b

# M = vocab size (num of types)
# T = number of tokens
# k, b are constants with typical values of:
# 30 <= k <= 100
# 0.4 <= b <= 0.6

# Example using data from the corpus of inaugural speeches
tokens <- tokens(data_corpus_inaugural, remove_punct = TRUE) 
num_tokens <- sum(lengths(tokens))

inaug_dfm <- dfm(data_corpus_inaugural)

M <- nfeat(inaug_dfm)  # number of types

# Let's check using parameter values from MRS Ch.5 for a corpus with more than 100,000 tokens

k <- 54
b <- 0.49

k * (num_tokens)^b

M

# Let's think about why (what types of texts are these?)
# The parameters are quite variables because vocabulary growth depends a lot on the nature of the collection and how the text is processed

# Lets try to model the relationship again with New parameters
k <- 44
b <- 0.455

k * (num_tokens)^b

M

# You can solve mathematically for k and b or fit a model to find k and b -- relationship between log(collection size) and log(vocab size) is linear

#-----------------------------
# ZIPF'S LAW
#-----------------------------
# Term frequency in corpus and rank

# x-axis: log of ranks 1 through 100
# y-axis log of frequency of top 100 terms from the DFM

plot(log10(1:100), log10(topfeatures(inaug_dfm, 100)),
     xlab = "log10(rank)", ylab = "log10(frequency)", main = "Top 100 Words in U.S. Presidential Inaugural Speech Corpus")

# Fits a linear regression to check if slope is approx -1.0
regression <- lm(log10(topfeatures(inaug_dfm, 100)) ~ log10(1:100))

# Adds the fitted line from regression to the plot
abline(regression, col = "red")

# Returns the 95% confidence intervals for the regression coefficients
confint(regression)

# Provides R-squared, F-test, and cofficient estimates from regression
summary(regression)

## Stopwords: do they affect Zipf's law?

inaug_dfm_nostop <- dfm(data_corpus_inaugural, remove=stopwords("english"))

plot(log10(1:100), log10(topfeatures(inaug_dfm_nostop, 100)),
     xlab = "log10(rank)", ylab = "log10(frequency)", main = "Top 100 Words in U.S. Presidential Inaugural Speech Corpus (w/o stopwords)")

# Regression to check if slope is approx -1.0
regression <- lm(log10(topfeatures(inaug_dfm_nostop, 100)) ~ log10(1:100))
abline(regression, col = "red")
confint(regression)
summary(regression)

# Zipf's law as a feature selection tool (e.g. http://www.jmlr.org/papers/volume3/forman03a/forman03a_full.pdf)

plot(1:100, topfeatures(inaug_dfm, 100),
     xlab = "rank", ylab = "frequency", main = "Top 100 Words in U.S. Presidential Inaugural Speech Corpus")

plot(1:100, topfeatures(inaug_dfm_nostop, 100),
     xlab = "rank", ylab = "frequency", main = "Top 100 Words in U.S. Presidential Inaugural Speech Corpus (w/o stopwords)")

