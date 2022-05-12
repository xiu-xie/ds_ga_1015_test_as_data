library(tidyverse)
library(rtweet)
library(stringr)

API_Key <- "xxxx"
API_secret <- "xxxx"
Access_token <- "xxxx"
Access_secret <- "xxxx"

my_authorization <- rtweet::create_token(app = "ScrapingTango",
                                         consumer_key = API_Key,
                                         consumer_secret = API_secret,
                                         access_token = Access_token,
                                         access_secret = Access_secret)

# Scrape Tom Tango's Twitter Timeline
tangotiger <- rtweet::get_timeline(c("tangotiger"), n = 3500,
                                   parse=T, token=my_authorization)
tangotiger <- tangotiger %>% filter(is_retweet == FALSE)

# Check Number of Quote Tweets
quote_tweets <- tangotiger %>% filter(is_quote == TRUE)

# Write out to CSV to work in Python
write.csv(tangotiger$text, "tango_texts.csv")

## -----------------------------------------------------

# Import manually labeled names
labels <- read.csv("clean_counts.csv", header = TRUE, na = c("", "NA"))
labels <- labels %>% select(-X, -count) %>% filter(!is.na(label))

# Create Dictionaries based on labels
hockey_dict <- c(labels %>%
                   filter(label == "hockey") %>%
                   select(name))
baseball_dict <- c(labels %>%
                     filter(label == "baseball") %>%
                     select(name))
neither_dict <- c(labels %>%
                    filter(label == "neither") %>%
                    select(name))

# Make Predictions using str_detect()
hockey_tweets <- tangotiger[str_detect(tangotiger$text, paste(hockey_dict$name, collapse = '|')),]
baseball_tweets <- tangotiger[str_detect(tangotiger$text, paste(baseball_dict$name, collapse = '|')),]
neither_tweets <- tangotiger[str_detect(tangotiger$text, paste(neither_dict$name, collapse = '|')),]

# Add label to data frame and combine
hockey_tweets <- hockey_tweets %>% mutate(label = "hockey")
baseball_tweets <- baseball_tweets %>% mutate(label = "baseball")
neither_tweets <- neither_tweets %>% mutate(label = "neither")

labeled_tweets <- rbind(baseball_tweets, hockey_tweets, neither_tweets)
labeled_tweets <- labeled_tweets %>% mutate(date = as.Date(created_at))

# Group by label and date for plotting
tweeted <- labeled_tweets %>%
  group_by(label, date) %>%
  summarize(count = n())

# Plot 1: All Tweets, by label
ggplot(tweeted, aes(x=date, y=count)) + geom_line(aes(color = label)) +
  labs(title = "Tom Tango's Tweets Count over the Last Six Months",
       subtitle = "Based on Classifications Predicted by the Model",
       x = "Date", y = "Count")

# Plot 2: Lockout Tweets, by label
ggplot(tweeted, aes(x=date, y=count)) + geom_line(aes(color = label)) +
  xlim(as.Date(c("2021-12-02","2022-03-09"))) +
  labs(title = "Tom Tango's Tweets Count during The Lockout",
       subtitle = "Based on Classifications Predicted by the Model",
       x = "Date", y = "Count")

## -----------------------------------------------------

# Subset labeled names by baseball class
label_class <- labels %>% filter(!is.na(class))

# Create Dictionaries based on class
active_dict <- c(label_class %>%
                   filter(class == "active") %>%
                   select(name))
retire_dict <- c(label_class %>%
                   filter(class == "retired") %>%
                   select(name))

# Make Predictions using str_detect()
active_tweets <- tangotiger[str_detect(tangotiger$text, paste(active_dict$name, collapse = '|')),]
retire_tweets <- tangotiger[str_detect(tangotiger$text, paste(retire_dict$name, collapse = '|')),]

# Add label to data frame and combine
active_tweets <- active_tweets %>% mutate(class = "active")
retire_tweets <- retire_tweets %>% mutate(class = "retire")

class_tweets <- rbind(active_tweets, retire_tweets)
class_tweets <- class_tweets %>% mutate(date = as.Date(created_at))

# Group by class and date for plotting
class_tweeted <- class_tweets %>%
  group_by(class, date) %>%
  summarize(count = n())

# Create full grid to fill in missing dates
date_range <- seq(as.Date("2021-11-11"), as.Date("2022-05-09"), by="days")
date_group <- expand.grid(class=c("active","retire"), date=date_range)
date_group <- date_group %>%
  left_join(class_tweeted, by=c("class","date")) %>%
  mutate(count = ifelse(is.na(count), 0, count))

# Plot 3: All Baseball Tweets, by class
ggplot(date_group, aes(x=date, y=count)) + geom_line(aes(color = class)) +
  labs(title = "Tom Tango's Baseball Tweets Count over the Last Six Months",
       subtitle = "Based on Classifications Predicted by the Model",
       x = "Date", y = "Count")

# Plot 4: Lockout Baseball Tweets, by class
ggplot(date_group, aes(x=date, y=count)) + geom_line(aes(color = class)) +
  xlim(as.Date(c("2021-12-02","2022-03-09"))) +
  labs(title = "Tom Tango's Baseball Tweets Count over The Lockout",
       subtitle = "Based on Classifications Predicted by the Model",
       x = "Date", y = "Count")

## --------------------------------------------

# Identify Lockout Tweets classified as "active"
lockout_active <- class_tweets %>%
  filter(class == "active",
         date >= "2021-12-02",
         date < "2022-03-10")
