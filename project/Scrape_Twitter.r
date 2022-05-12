library(tidyverse)
library(twitteR)
library("openssl")
library("httpuv")
library(base64enc)
library(rtweet)
library(httr)
library(jsonlite)
#devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
install.packages("base64enc")

API_Key <- "bYOtzuYCqdSWhZVkXDDL2lO5h"
API_secret <- "mZf15RhbDFP9ovYMSs86YADQOJC3rTu07faOtm1BNYgzb4tSyw"
Access_token <- "2991279104-oDOERHRrPdK4wP9yihR0PFHwRhepG84LmPdcrNS"
Access_secret <- "OxkJ7r7y0NYR0AQ2JlsahAYPaTrRsxnl3248SghI1GTj4"
Client_ID <- "U2xLTmIxZTRuX25MUm8ycWYxVWk6MTpjaQ"
Client_secret <- "ZFV9h9zDtBRovirGG_tiRq-5BWPdWz6H6ev7ygk-Xjt6OBJ-qg"

setup_twitter_oauth(API_Key,API_secret, Access_token, Access_secret)

my_authorization <- rtweet::create_token(app = "ScrapingTango",
                                         consumer_key = API_Key,
                                         consumer_secret = API_secret,
                                         access_token = Access_token,
                                         access_secret = Access_secret)

tangotiger <- rtweet::get_timeline(c("tangotiger"), n = 3500, parse=T, token=my_authorization)
tangotiger <- tangotiger %>% filter(is_retweet == FALSE)
quote_tweets <- tangotiger %>% filter(is_quote == TRUE)

write.csv(tangotiger$text, "tango_texts.csv")


lockout_tweets <- tangotiger %>%
  filter(is_retweet == FALSE,
         created_at < "2022-03-10",
         created_at >= "2021-12-02")

min(tangotiger$status_id)

tangotiger_2 <- rtweet::get_timeline(c("tangotiger"), n = 3000, parse = T,
                                     max_id = min(tangotiger$status_id),
                                     token = my_authorization)

labels <- read.csv("clean_counts.csv", header = TRUE, na = c("", "NA"))

labels <- labels %>% select(-X, -count) %>%
  filter(!is.na(label))

hockey_dict <- c(labels %>%
                   filter(label == "hockey") %>%
                   select(name))

baseball_dict <- c(labels %>%
                     filter(label == "baseball") %>%
                     select(name))

neither_dict <- c(labels %>%
                    filter(label == "neither") %>%
                    select(name))

library(stringr)
hockey_tweets <- tangotiger[str_detect(tangotiger$text, paste(hockey_dict$name, collapse = '|')),]
baseball_tweets <- tangotiger[str_detect(tangotiger$text, paste(baseball_dict$name, collapse = '|')),]
neither_tweets <- tangotiger[str_detect(tangotiger$text, paste(neither_dict$name, collapse = '|')),]

hockey_tweets <- hockey_tweets %>% mutate(label = "hockey")
baseball_tweets <- baseball_tweets %>% mutate(label = "baseball")
neither_tweets <- neither_tweets %>% mutate(label = "neither")

labeled_tweets <- rbind(baseball_tweets, hockey_tweets, neither_tweets)
labeled_tweets <- labeled_tweets %>%
  mutate(date = as.Date(created_at))

tweeted <- labeled_tweets %>%
  group_by(label, date) %>%
  summarize(count = n())

date_range <- seq(as.Date("2021-11-11"), as.Date("2022-05-09"), by="days")
label_group <- expand.grid(label=c("baseball","hockey","neither"), date=date_range)
label_group <- label_group %>%
  left_join(tweeted, by=c("label","date")) %>%
  mutate(count = ifelse(is.na(count), 0, count))

ggplot(tweeted, aes(x=date, y=count)) + geom_line(aes(color = label)) +
  labs(title = "Tom Tango's Tweets Count over the Last Six Months",
       subtitle = "Based on Classifications Predicted by the Model",
       x = "Date", y = "Count")

ggplot(tweeted, aes(x=date, y=count)) + geom_line(aes(color = label)) +
  xlim(as.Date(c("2021-12-02","2022-03-09"))) +
  labs(title = "Tom Tango's Tweets Count during The Lockout",
       subtitle = "Based on Classifications Predicted by the Model",
       x = "Date", y = "Count")

ggplot(label_group, aes(x=date, y=count)) + geom_line(aes(color = label))

ggplot(hockey_tweets, aes(created_at)) + geom_histogram(bins = 100)
ggplot(baseball_tweets, aes(created_at)) + geom_histogram(bins = 100)
ggplot(neither_tweets, aes(created_at)) + geom_histogram(bins = 100)

## --------------------------------------------

label_class <- labels %>% filter(!is.na(class))
active_dict <- c(label_class %>%
                   filter(class == "active") %>%
                   select(name))
retire_dict <- c(label_class %>%
                   filter(class == "retired") %>%
                   select(name))

active_tweets <- tangotiger[str_detect(tangotiger$text, paste(active_dict$name, collapse = '|')),]
retire_tweets <- tangotiger[str_detect(tangotiger$text, paste(retire_dict$name, collapse = '|')),]

active_tweets <- active_tweets %>% mutate(class = "active")
retire_tweets <- retire_tweets %>% mutate(class = "retire")

class_tweets <- rbind(active_tweets, retire_tweets)
class_tweets <- class_tweets %>% mutate(date = as.Date(created_at))

class_tweeted <- class_tweets %>%
  group_by(class, date) %>%
  summarize(count = n())

ggplot(class_tweeted, aes(x=date, y=count)) + geom_line(aes(color = class)) +
  labs(title = "Tom Tango's Baseball Tweets Count over the Last Six Months",
       subtitle = "Based on Classifications Predicted by the Model",
       x = "Date", y = "Count")

ggplot(class_tweeted, aes(x=date, y=count)) + geom_line(aes(color = class)) +
  xlim(as.Date(c("2021-12-02","2022-03-09"))) + ylim(0,12)

date_range <- seq(as.Date("2021-11-11"), as.Date("2022-05-09"), by="days")
date_group <- expand.grid(class=c("active","retire"), date=date_range)
date_group <- date_group %>%
  left_join(class_tweeted, by=c("class","date")) %>%
  mutate(count = ifelse(is.na(count), 0, count))

ggplot(date_group, aes(x=date, y=count)) + geom_line(aes(color = class)) +
  labs(title = "Tom Tango's Baseball Tweets Count over the Last Six Months",
       subtitle = "Based on Classifications Predicted by the Model",
       x = "Date", y = "Count")
ggplot(date_group, aes(x=date, y=count)) + geom_line(aes(color = class)) +
  xlim(as.Date(c("2021-12-02","2022-03-09"))) +
  labs(title = "Tom Tango's Baseball Tweets Count over The Lockout",
       subtitle = "Based on Classifications Predicted by the Model",
       x = "Date", y = "Count")

## --------------------------------------------

lockout_active <- class_tweets %>%
  filter(class == "active",
         date >= "2021-12-02",
         date < "2022-03-10")


## --------------------------------------------

mlb_path <- "https://www.mlb.com/news"
text <- readLines(mlb_path)
docs <- Corpus(VectorSource(text))
inspect(docs)

library("tm")


tangotiger_2 <- userTimeline(user = "tangotiger", n = 3500, includeRts = FALSE, excludeReplies = TRUE)

search_tweets("tangotiger")

rt <- search_tweets("#rstats", n = 3, include_rts = FALSE)

## ---------------------------------------------------

bearer_token <- Sys.getenv("AAAAAAAAAAAAAAAAAAAAAJhdcQEAAAAANhMgzOZFvoKLNCZDxeKQz8hqXg0%3D0Lo6vvUvB0rjTJAnUcdrafpS776aMeCYuHkucBa8RLqBBxf7f0")
headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))

params <- list(`user.fields` = 'description',
               `expansions` = 'pinned_tweet_id')

handle <- 'tangotiger'
url_handle <- sprintf('https://api.twitter.com/2/users/by?usernames=%s', handle)

url_handle

response <-
  httr::GET(url = url_handle,
            httr::add_headers(.headers = headers),
            query = params)
obj <- httr::content(response, as = "text")
print(obj)

json_data <- fromJSON(obj, flatten = TRUE) %>% as.data.frame
View(json_data)
