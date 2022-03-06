#-----------------------------
# STYLE
#-----------------------------

# 2.1 stylest package: estimate speaker (author) style distinctiveness (vis-a-vis other authors)
# see https://leslie-huang.github.io/stylest/

# source for this code: package vignette
library(stylest)
library(quanteda.corpora)
library(quanteda)

# data included in package
data(novels_excerpts)

# author list
unique(novels_excerpts$author)

# note how the data is organized
str(novels_excerpts)

stopwords_en <- stopwords("en")

# (1) select most informative (discriminative) features (subsets vocab by frequency percentile)
# Tokenization selections can optionally be passed as the filter argument
filter <- corpus::text_filter(drop_punct = TRUE, drop_number = TRUE, drop = stopwords_en)  # pre-processing choices
set.seed(1984L)  # why set seed? Replicability #Dont forget to seet seed in your HW!

# Explore the parameters: https://rdrr.io/cran/stylest/man/stylest_select_vocab.html

vocab_custom <- stylest_select_vocab(novels_excerpts$text, novels_excerpts$author,  # fits n-fold cross-validation
                                     filter = filter, smooth = 1, nfold = 5,
                                     cutoff_pcts = c(25, 50, 75, 99))
vocab_custom

vocab_custom$cutoff_pct_best  # percentile with best prediction rate
vocab_custom$miss_pct  # rate of incorrectly predicted speakers of held-out texts

# (2) subset features
vocab_subset <- stylest_terms(novels_excerpts$text, novels_excerpts$author, vocab_custom$cutoff_pct_best , filter = filter) # USE SAME FILTER

# (3) fit model with "optimal" percentile threshold (i.e. feature subset)
style_model <- stylest_fit(novels_excerpts$text, novels_excerpts$author, terms = vocab_subset, filter = filter)
summary(style_model)
# explore output
head(stylest_term_influence(style_model, novels_excerpts$text, novels_excerpts$author))  # influential terms

authors <- unique(novels_excerpts$author)
term_usage <- style_model$rate
lapply(authors, function(x) head(term_usage[x,][order(-term_usage[x,])])) %>% setNames(authors)

# odds for known texts
odds <- stylest_odds(style_model, novels_excerpts$text, novels_excerpts$author)
odds

# (4) predict speaker of a new text
new_text <- emma$text[30:75] %>% paste(., collapse = "") 
pred <- stylest_predict(style_model, new_text)
pred$predicted
pred$log_probs

# Project Gutenberg: http://www.gutenberg.org/wiki/Main_Page
# collection of (machine readable) novels and other texts + they have an R package!
#install.packages("gutenbergr")
# for more info refer to: https://cran.r-project.org/web/packages/gutenbergr/vignettes/intro.html
