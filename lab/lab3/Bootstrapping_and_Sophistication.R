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

#------------------------------
# 1 BOOTSTRAPPING
#------------------------------
# there are packages in R that help with bootstrapping: e.g. https://cran.r-project.org/web/packages/boot/boot.pdf

# data prep: remove smaller parties (parties with only 1 document)
large_parties <- c("FF","FG","Green","SF")

irbudgetsCorpSub <- corpus_subset(data_corpus_irishbudgets, (party %in% large_parties))

irbudgetsCorpSub
View(irbudgetsCorpSub)
summary(irbudgetsCorpSub)
meta(irbudgetsCorpSub)
View(docvars(irbudgetsCorpSub))

# convert corpus to df 
irbudgets_df <- data.frame(texts = texts(irbudgetsCorpSub), 
                           party = docvars(irbudgetsCorpSub, "party"),
                           year = as.integer(docvars(irbudgetsCorpSub, "year")),
                           stringsAsFactors = FALSE)

# Let's filter out any NAs

#sapply(irbudgets_df, function(x) sum(is.na(x)))
irbudgets_df <- na.omit(irbudgets_df)

#View(irbudgets_df)
# Read more about textstat_readability:
# https://quanteda.io/reference/textstat_readability.html

# mean Flesch statistic per party
flesch_point <- irbudgets_df$texts %>% textstat_readability(measure = "Flesch") %>% 
  group_by(irbudgets_df$party) %>% 
  summarise(mean_flesch = mean(Flesch)) %>% 
  setNames(c("party", "mean")) %>% arrange(party) 

flesch_point

# ggplot point estimate
ggplot(flesch_point, aes(x = party, y = mean, colour = party)) +
  geom_point() +
  coord_flip() + theme_bw() + 
  scale_y_continuous(breaks=seq(floor(min(flesch_point$mean)), 
                                ceiling(max(flesch_point$mean)), by = 2)
  ) +
  xlab("") + ylab("Mean Flesch Score by Party") + theme(legend.position = "none")

# We will use a loop to bootstrap a sample of texts and subsequently calculate standard errors
iters <- 10

# HINT FOR YOUR HOMEWORK: In ?tokens look at the what options. The default is word, but other options are available.

# build function to be used in bootstrapping
boot_flesch <- function(party_data){
  N <- nrow(party_data)
  bootstrap_sample <- corpus_sample(corpus(c(party_data$text)), size = N, replace = TRUE)
  bootstrap_sample<- as.data.frame(as.matrix(bootstrap_sample))
  readability_results <- textstat_readability(bootstrap_sample$V1, measure = "Flesch")
  return(mean(readability_results$Flesch))
}

#textstat_readability takes only characters or corpus objects as a parameter

# apply function to each party
boot_flesch_by_party <- pblapply(large_parties, function(x){
  sub_data <- irbudgets_df %>% filter(party == x)
  output_flesch <- lapply(1:iters, function(i) boot_flesch(sub_data))
  return(unlist(output_flesch))
})

names(boot_flesch_by_party) <- large_parties
View(boot_flesch_by_party)

# compute mean and std.errors
party_means <- lapply(boot_flesch_by_party, mean) %>% unname() %>% unlist()
party_ses <- lapply(boot_flesch_by_party, sd) %>% unname() %>% unlist() # bootstrap standard error = sample standard deviation bootstrap distribution
party_means
party_ses
# Plot results--party
plot_dt <- tibble(party = large_parties, mean = party_means, ses = party_ses)

# A confidence interval is the probability that a value will fall between an upper and lower limits of a probability distribution
# confidence intervals

?qnorm
# qnorm: given an area, find the boundary value that determines this area
# A confidence interval is the probability that a value will fall between 
# an upper and lower limits of a probability distribution

# So 90% CI means you are 90% confident that the values of the results will 
# fall between the upper and lower limits

interval1 <- -qnorm((1-0.9)/2)   # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

interval1
interval2

# ggplot point estimate + variance
ggplot(plot_dt, aes(colour = party)) +
  geom_linerange(aes(x = party, ymin = mean - ses*interval1, ymax = mean + ses*interval1), 
                 lwd = 1, position = position_dodge(width = 1/2)
  ) +
  geom_pointrange(aes(x = party, y = mean, ymin = mean - ses*interval2, ymax = mean + ses*interval2), 
                  lwd = 1/2, position = position_dodge(width = 1/2), 
                  shape = 21, fill = "WHITE"
  ) +
  coord_flip() + theme_bw() + 
  xlab("") + ylab("Mean Fleisch Score by Party") + 
  ggtitle("Bootstrapped Irish Budget Fleisch Scores by Party") +
  theme(legend.position = "none")

#------------------------------
# 2 SOPHISTICATION
#------------------------------
#devtools::install_github("kbenoit/sophistication")
library("sophistication")
# HINT FOR YOUR HW!!! see vignette: https://github.com/kbenoit/sophistication
# We'll run through the example from https://github.com/kbenoit/sophistication

# Load data
data(data_corpus_sotu, package = "quanteda.corpora")

# Read more about : https://rdrr.io/github/kbenoit/sophistication/src/R/snippets_clean.R#sym-snippets_clean

# Make snippets of 1 sentence each, then clean them
snippetData <- snippets_make(data_corpus_sotu, nsentence = 1, minchar = 150, maxchar = 250)
snippetData <- snippets_clean(snippetData)
head(snippetData)

# Sample the snippets
testData <- sample_n(snippetData, 5)

View(testData)

# generate n-1 pairs from n test snippets for a minimum spanning tree
snippetPairsMST <- pairs_regular_make(testData)

pairs_regular_browse(snippetPairsMST)

# generate more pairs from a larger sample of data
snippetPairsAll <- pairs_regular_make(snippetData[sample(1:nrow(snippetData), 1000), ])

pairs_regular_browse(snippetPairsAll)

# Make some "Gold" questions -- for use with CrowdFlower workers 
# default reading level is Flesch and the default difference in readability of the two snippets in the pair is the 0.1 and 0.9 quintiles
# sample of snippet pairs, where the readability is the most different between pairs

gold_questions <- pairs_gold_make(snippetPairsAll, n.pairs = 10)

View(gold_questions)

# Read more here : https://rdrr.io/github/kbenoit/sophistication/man/pairs_gold_make.html
