# TA: Susmita Karyakarte
# Course: Text as Data
# Date: 02/24/2022
# Lab adapted from: Kevin Munger, Patrick Chester, Leslie Huang,  Pedro L. Rodriguez, Lucia Motolinia

#----------------------------------------
# 1 Set up environment                   ---
#----------------------------------------
# clear global environment
rm(list = ls())

# set path where our data is stored
setwd("F:/R/textasdata2022/W5_03_04_21")

# load required libraries
library(quanteda)
library(quanteda.corpora)
library(dplyr)

#----------------------------------------
# 2 Load data: conservative manifestos ---
#----------------------------------------
# read in the files : https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/list.files

# List the files in the directory
# files() produces a character vector of the names of the files
filenames <- list.files(path = "conservative_manifestos", full.names=TRUE)
filenames

# readLine : https://statisticsglobe.com/r-readlines-example
cons_manifestos <- lapply(filenames, readLines)

View(cons_manifestos)

# paste() : Concatenate vectors after converting to character
# collapse : an optional character string to separate the results
cons_manifestos <- unlist(lapply(cons_manifestos, function(x) paste(x, collapse = " "))) 
View(cons_manifestos)
# because readLines returns a vector with each elements = lines

# gregexpr : gregexpr can take a vector of strings for its 
# second argument. It therefore returns a numeric vector 
# for each string in the supplied character vector to represent the 
# starting point of the matches in each string. 
# It stores all of these vectors together in a list.
# More : http://www.endmemo.com/r/gregexpr.php

filenames[1]
gregexpr("[[:digit:]]+", filenames[1])

# get the date docvar from the filename
dates <- unlist(regmatches(unlist(filenames), gregexpr("[[:digit:]]+", unlist(filenames))))
dates

# construct tibble (a tibble is an "enhanced" data.frame)
# see ?tibble
manifestos_df <- tibble(year = dates, text = cons_manifestos)
View(manifestos_df)

#----------------------------------------
# 3 Regular expressions                  ---
#----------------------------------------
# Let us take a step back and have a refresher on grep that we will need for later
words <- c("Washington Post", "NYT", "Wall Street Journal", "Peer-2-Peer", "Red State", "Cheese", "222", ",")

words

# Exploring by character type
#?grep
# Elements that have alphanumeric characters
grep("\\w", words, value = T)  
# Elements that have words that are at least 7 characters long
grep("\\w{7}", words, value = T)  
# Elements that contain numbers
grep("\\d", words, value = T)  
# Elements that contain nonword characters (Including white space)
grep("\\W", words, value = T)  

# note that  grep returns the full element that matched the pattern
words2 <- c("voting", "votes", "devoted", "vote")

# Returns the index of matching items in the vector
grep("^vot", words2) 
# Returns the elements of the vector that matched the pattern
grep("^vot", words2, value = T) 
# Returns a logical vector indicating whether or not the component containes the expression
grepl("^vot", words2)  

# you can use the indices to select elements from the original vector that you want
words2[grepl("^vot", words2)]

presidents <- c("Roosevelt-33", "Roosevelt-37", "Obama-2003")

# Use gsub to replace patterns with a string
# Parentheses can identify components that can later be referenced by \\1 - \\2
gsub("(\\w+)-(\\d{2})", "\\1-19\\2", presidents) 
# We want to use the $ to indicate that the pattern should come at the end of the word, to avoid the mismatch in Obama-192003
gsub("(\\w+)-(\\d{2})$", "\\1-19\\2", presidents) 

# Note that regex expressions in R are similar to those in other languages but there are some key differences
# More on this : https://rstudio-pubs-static.s3.amazonaws.com/74603_76cd14d5983f47408fdf0b323550b846.html
# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/regex

# Resources:
# other packages to work with regular expressions: stringr, stringi
# cheatsheet for regex: https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
# https://rstudio-pubs-static.s3.amazonaws.com/74603_76cd14d5983f47408fdf0b323550b846.html
# http://r4ds.had.co.nz/strings.html#matching-patterns-with-regular-expressions

#----------------------------------------
# 4 Selecting Features from DFM using Regular Expressions ---
#----------------------------------------

# Using simple texts

testText <- "The quick brown fox named Seamus jumps over the lazy dog also named Seamus, with the newspaper from a a boy named Seamus, in his mouth."

print(dfm(testText, select = "^f", valuetype = "regex")) # keep only words starting with "f"

testTweets <- c("2 + 2 = 4 #1984",
                "I thought you said the park? Why are we at the vet? #QuestionsFromPets",
                "Holy freeway #flooding Batman! #californiastorms taking their toll.")

print(dfm(testTweets, select="^#", valuetype = "regex"))  # keep only hashtags i.e. expressions starting with a pound sign

# Selecting features from a corpus

data("data_corpus_irishbudgets")

irishbudgets_dfm <- dfm(data_corpus_irishbudgets, select=c("tax|budg|^auster"), 
                        valuetype = "regex") 
irishbudgets_dfm_noregex <- dfm(data_corpus_irishbudgets)

# valuetype = "regex" ensures that the select input will be interpreted as a regular expression

# You can pass a list of words to the "select" parameter in dfm, but using regular expressions can enable you to get all variants of a word
irishbudgets_dfm
irishbudgets_dfm_noregex

irishbudgets_dfm_df <- convert(irishbudgets_dfm, to = "data.frame")

View(irishbudgets_dfm_df)

irishbudgets_dfm_df_noregex <- convert(irishbudgets_dfm_noregex, to = "data.frame")

View(irishbudgets_dfm_df_noregex)

# More on select in dplyr : https://tidyselect.r-lib.org/reference/starts_with.html

#----------------------------------------
# 5 Dictionaries                         ---
#----------------------------------------
# Here, dictionary = list of words, not the data structure.
# Python users: there is no dictionary object in R :( (Note: you can create dictionary-like objects using lists)

mytexts <- c("The new law included a capital gains tax, and an inheritance tax.",
             "New York City has raised a taxes: an income tax and a sales tax.")

mydict <- c("tax", "income", "capital", "gains", "inheritance")

print(dfm(mytexts, select = mydict))

# Example: Laver Garry dictionary
# https://rdrr.io/github/kbenoit/quanteda.dictionaries/man/data_dictionary_LaverGarry.html
# https://provalisresearch.com/products/content-analysis-software/wordstat-dictionary/laver-garry-dictionary-of-policy-position/
# https://github.com/kbenoit/quanteda.dictionaries (other dictionaries such as Hu & Liu sentiment are available!)
lgdict <- dictionary(file = "LaverGarry.cat", format = "wordstat")

#Laver and Garry dictionary was developed to estimates the policy positions of political actors in the United Kingdom 
#by comparing their speeches and written documents to key words found in the British Conservative and Labour manifestos of 1992. 
#Note: Please remember that this dictionary was customized to reflect the policy positions of UK political parties.

# What's in this thing? 
lgdict
# The dictionary contains 415 words and word patterns. The dictionary has two levels of nesting with 7 main policy areas (level 1) divided up into 19 sub-categories (level 2).

# Run the conservative manifestos through this dictionary
manifestos_lg <- dfm(manifestos_df$text, dictionary = lgdict)

# how does this look
as.matrix(manifestos_lg)[1:5, 1:5]
featnames(manifestos_lg)

# plot it
plot(manifestos_df$year, 
     manifestos_lg[,"CULTURE.SPORT"],
     xlab="Year", ylab="SPORTS", type="b", pch=19)

# pch : plotting characters or symbols : http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
# More here : https://www.learnbyexample.org/r-plot-function/

plot(manifestos_df$year, 
     manifestos_lg[,"VALUES.CONSERVATIVE"],
     xlab="Year", ylab="Conservative values", type="b", pch=19)

plot(manifestos_df$year, 
     manifestos_lg[,"INSTITUTIONS.CONSERVATIVE"] - manifestos_lg[,"INSTITUTIONS.RADICAL"],
     xlab="Year", ylab="Net Conservative Institutions", type="b", pch=19)

# RID Dictionary--Regressive Imagery Dictionary
# https://www.kovcomp.co.uk/wordstat/RID.html 
# (multiple languages available!)
#The English Regressive Imagery Dictionary (RID) is composed of 
# about 3200 words and roots assigned to 29 categories of 
# primary process cognition, 7 categories of secondary process 
# cognition, and 7 categories of emotions.

rid_dict <- dictionary(file = "RID.cat", format = "wordstat")
rid_dict

data("data_corpus_sotu")

sotus_texts <- texts(data_corpus_sotu)

# Get the docvars from the corpus object
year <- (data_corpus_sotu$Date)

sotu_rid_dfm <- dfm(data_corpus_sotu, dictionary = rid_dict)

# Look at the categories
featnames(sotu_rid_dfm)

# Inspect the results graphically
plot(year, 
     sotu_rid_dfm[,"PRIMARY.REGR_KNOL.NARCISSISM"],
     xlab="Year", ylab="Narcissism", type="b", pch=19)

plot(year, 
     sotu_rid_dfm[,"PRIMARY.ICARIAN_IM.FIRE"] + sotu_rid_dfm[,"PRIMARY.ICARIAN_IM.ASCEND"] +sotu_rid_dfm[,"PRIMARY.ICARIAN_IM.DESCENT"] +
       sotu_rid_dfm[,"PRIMARY.ICARIAN_IM.DEPTH"] + sotu_rid_dfm[,"PRIMARY.ICARIAN_IM.HEIGHT"] + sotu_rid_dfm[,"PRIMARY.ICARIAN_IM.WATER"],
     xlab="Year", ylab="Icarian-ness", type="b", pch=19)

