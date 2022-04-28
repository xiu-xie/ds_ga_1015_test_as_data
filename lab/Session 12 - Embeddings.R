# TA: Susmita Karyakarte
# Course: Text as Data
# Date: 04/21/2022

# Lab adapted from: Leslie Huang, Pedro L. Rodriguez, and Lucia Motolinia

rm(list = ls())
libraries <- c("dplyr", "quanteda")
lapply(libraries, require, character.only = T)

setwd("F:/R/textasdata2022/W12_04_21_22")


# -----------------------------------------------
# Word Embeddings                                ---
# -----------------------------------------------
# KEY DIFFERENCE between embeddings and other distributional 
# semantic models we've seen: how we define context.
# Context in the case of word embeddings is defined by a window 
# (usually symmetric) around the target word.
# GloVe vs. Word2Vec
# cool/intuitive intro to W2V: http://mccormickml.com/2016/04/19/word2vec-tutorial-the-skip-gram-model/

library(text2vec)

# Tutorials : https://www.r-bloggers.com/2015/11/glove-vs-word2vec-revisited/amp/
# https://srdas.github.io/MLBook/Text2Vec.html

# see vignette: https://cran.r-project.org/web/packages/text2vec/vignettes/text-vectorization.html
# https://rdrr.io/cran/text2vec/
# http://text2vec.org/api.html
# https://rdrr.io/cran/text2vec/f/vignettes/glove.Rmd


# choice parameters
WINDOW_SIZE <- 6
RANK <- 300
ITERS <- 10
MIN_COUNT <- 10

# load data
corp <- readRDS("news_data.rds")
View(corp)
text <- tolower(corp$headline)
text
#rm(corp)

# shuffle text
set.seed(42L)
text <- sample(text)

# ================================
# create vocab
# ================================
tokens <- space_tokenizer(text)
tokens
#rm(text)
it <- itoken(tokens, progressbar = FALSE)
?itoken

# can be built in parallel using all your CPU cores
vocab <- create_vocabulary(it)
View(vocab)

# became slightly more efficient - it performs less unnecessary computations
vocab <- prune_vocabulary(vocab, term_count_min = MIN_COUNT) 
View(vocab)
# keep only words that meet count threshold

# ================================
# create term co-occurrence matrix
# ================================
# uses vocabulary to perfrom bag-of-ngrams vectorization
vectorizer <- vocab_vectorizer(vocab)
vectorizer
tcm <- create_tcm(it, vectorizer, skip_grams_window = WINDOW_SIZE, skip_grams_window_context = "symmetric")
View(tcm)

# ================================
# set model parameters
# ================================
# note that the nonstandard (for R) syntax here:
# GlobalVectors$new() is a method

glove <- GlobalVectors$new(
                           rank = RANK,
                           x_max = 100,
                           lambda = 1e-5)

# ================================
# fit model
# ================================
word_vectors_main <- glove$fit_transform(tcm, 
                                         n_iter = ITERS,
                                         convergence_tol = 1e-3, 
                                         n_check_convergence = 1L,
                                         n_threads = RcppParallel::defaultNumThreads())

# https://github.com/dselivanov/text2vec/issues/296

# ================================
# get output
# ================================
word_vectors_context <- glove$components
word_vectors <- word_vectors_main + t(word_vectors_context) # word vectors


# features?
head(rownames(word_vectors))


# pretrained GLoVE embeddings
pretrained <- readRDS("glove.rds") # GloVe pretrained (https://nlp.stanford.edu/projects/glove/)

View(pretrained)

# function to compute nearest neighbors
nearest_neighbors <- function(cue, embeds, N = 5, norm = "l2"){
  cos_sim <- sim2(x = embeds, y = embeds[cue, , drop = FALSE], method = "cosine", norm = norm)
  nn <- cos_sim <- cos_sim[order(-cos_sim),]
  return(names(nn)[2:(N + 1)])  # cue is always the nearest neighbor hence dropped
}

# e.g. 
nearest_neighbors("state", word_vectors, N = 10, norm = "l2")
nearest_neighbors("state", pretrained, N = 10, norm = "l2")

nearest_neighbors("welfare", word_vectors, N = 10, norm = "l2")
nearest_neighbors("welfare", pretrained, N = 10, norm = "l2")

