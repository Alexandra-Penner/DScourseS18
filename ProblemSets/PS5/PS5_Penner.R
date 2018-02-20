library(rvest)
library(stringr)
library(twitteR)
library(ROAuth)
library(httr)
library(plyr)


bball <- read_html("http://www.espn.com/mens-college-basketball/team/_/id/201/")

scores <- bball %>%
  html_nodes(".club-schedule li a") %>%
  html_text()

# it is giving me combined strings, and I don't have time to relearn how to parse these strings right now.



# Set API Keys
api_key <- "wMFyv90ZfsfOkvHnyYOLF5Nx3"
api_secret <- "t5ZTQdPGfjmMoFI2rjhJrFPbNPAo8lgFGrO0vfV9jz6OzEqsp2"
access_token <- "2306075156-6HRFQPgmAnke5NXz5W5zFzcbqaVAcxAkxQeLnJV"
access_token_secret <- "1icGG0de2NS7TVOkGXEwwibXO6U3afnNt1Aplttih7pCI"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

tweets_bball <- searchTwitter('OU Basketball', n=10)

# table tweets
tweettable <- do.call("rbind", lapply(tweets_bball, as.data.frame))

