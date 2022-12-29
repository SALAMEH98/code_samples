## Advanced Web Scraping Code Example
## Sief Salameh

# The purpose of this section is to generalize a code that parses every 
# Refugee Brief article from the UNCHR. I do this by automatically web scraping 
# every report between the most recent article (June 24th, 2022) to the oldest
# article (April 8th, 2022). I then describe the sentiment of the combined 
# articles using built-in sentiment data bases and I also identify which 
# countries are discussed in the articles by using a built-in library in R.

## Libraries

path <- ("~/Downloads/work_samples/web_scraping/raw_data")

library(tidytext)
library(udpipe)
library(tidyverse)
library(ggplot2)
library(nametagger)
library(SnowballC)
library(maps)
library(rvest)
library(purrr)

## Section One

# I begin by reading-in one of the unhcr links

base_html <- read_html("https://www.unhcr.org/refugeebrief/the-refugee-brief-24-june-2022/")

# I use the base link to identify all the sub links written in the 
# article. I filter it down so that I only receive links that include 
# "refugeebrief" in the link name. This generalizes the code so it does not
# solely rely on the dates, but it does have to include "refugeebrief" in the
# link name in order for it to be attached.

nodes <- html_nodes(base_html,
                    xpath = '//*[contains(@href, "refugeebrief/the")]'
) %>%
  html_attr("href") %>%
  tail(-1)

# I then create a function that runs on all the links I captured and I read them 
# in using the html reader. I extract the text that is in the main body of the 
# article by specifying its xpath.

text_list <- c()

for (i in nodes) {
  html_reader <- read_html(i)
  text_body <- html_nodes(html_reader, xpath = "/html/body/div[1]/div/div/div/div/div[1]/article/div[2]") %>%
    html_text()
  text_list <- c(text_list, text_body)
}

# I take the bodies of text which are stored as values and convert them to a 
# data frame. This provides me one column with ten rows. 
# Each row represents a different body of text that corresponds with the text 
# found in each of the correlating articles. 

text_df <- as.data.frame(as.character(text_list))

names(text_df) <- "article_text"

# I save this data frame as a text file. This eliminates the columns and
# provides me each body of text all in one document. Each article is separated
# by a line of space.

write.table(text_df,
            file = "~/Downloads/work_samples/web_scraping/raw_data/combined_articles.txt",
            quote = FALSE,
            sep = " -> "
)

## Section Two

setwd("~/Downloads/work_samples/web_scraping/raw_data")

text_one <- read_file("combined_articles.txt")

unhcr_full <- udpipe(text_one, "english")

unhcr_full$stem <- wordStem(unhcr_full$token, language = "english")

# I clean the full unhcr data set by filtering out any punctuation and 
# coordinating conjunctions. This allows me to extract only the necessary words 
# in the article.

unhcr_full <- unhcr_full %>%
  filter(
    !upos %in% c("PUNCT", "CCONJ"),
    !lemma %in% stop_words$word
  ) %>%
  mutate_if(is.character, str_to_lower) %>%
  select(token, lemma, upos, head_token_id, stem)

# After I use the udpipe to pull out all the words in the article, I then
# find all the words that are attached together. So I use the unnest function 
# to extract the words using a bigram (2) method. 

text_two <- tibble(text = text_one)

combined_words <- unnest_tokens(text_two,
                                bigrams,
                                text,
                                token = "ngrams",
                                n = 2
)

combined_words <- separate(combined_words,
                           bigrams,
                           c("word1", "word2"),
                           sep = " "
)

combined_words <- combined_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# I use the count function to see the number of times each attached word
# is utilized in the article.

count(combined_words, word1, word2, sort = TRUE)

combined_words <- unite(combined_words, bigram, word1, word2, sep = " ")

# I only filter out the countries that are spelled using two words. I do not 
# pull out "asylum seekers" or "human rights" because I already know that the 
# sentiment functions are going to value them the same value whether they are 
# attached or not.

combined_words <- combined_words %>%
  filter(bigram %in% c("burkina faso", "côte d’ivoire"))

colnames(combined_words)[which(names(combined_words) == "bigram")] <- "lemma"

# I attach the countries to the full unhcr data set.

unhcr_full <- bind_rows(unhcr_full, combined_words)

# I then filter out the same two countries that existed in the full unhcr data 
# set so I do not have duplicate values. I do this because I attached 
# the same countries from the bigram method.

unhcr_full <- unhcr_full %>%
  filter(!lemma %in% c("côte", "faso", "burkina", "ivoire", "d'"))

sentiment_nrc <- get_sentiments("nrc") %>%
  rename(nrc = sentiment)

sentiment_afinn <- get_sentiments("afinn")

sentiment_bing <- get_sentiments("bing") %>%
  rename(bing = sentiment)

# I join the sentiment functions to my full unhcr data set. I join them using 
# the lemmas because the lemmas provide us real words that are 
# stripped of all word additions and/or word endings.  

sentiment_df <-
  left_join(unhcr_full, sentiment_nrc, by = c("lemma" = "word")) %>%
  select(lemma, nrc) %>%
  na.omit(nrc)

write.csv(sentiment_df,
          "~/Downloads/work_samples/web_scraping/raw_data/NRC_sentiments.csv",
          row.names = FALSE
)

sentiment_df %>%
  filter(!is.na(nrc)) %>%
  group_by(nrc) %>%
  count(nrc) %>%
  arrange(desc(n)) %>%
  head(10)

sentiment_df_two <- unhcr_full %>%
  left_join(sentiment_bing, by = c("lemma" = "word")) %>%
  select(lemma, bing) %>%
  na.omit(bing)

write.csv(sentiment_df_two,
          "~/Downloads/work_samples/web_scraping/raw_data/bing_sentiments.csv",
          row.names = FALSE
)

sentiment_df_two %>%
  filter(!is.na(bing)) %>%
  group_by(bing) %>%
  count(bing) %>%
  arrange(desc(n)) %>%
  head(2)

# The first ggplot identifies sentiments using the NRC method. To be aware, 
# this method duplicates lemmas if the word has more than one value within the 
# NRC.

ggplot(data = filter(sentiment_df, !is.na(nrc))) +
  geom_bar(aes(x = nrc, fill = factor(nrc)), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_y_continuous(breaks = c(50, 150, 250, 350, 450, 550, 650, 750)) +
  labs(title = "The Refugee Brief Total Sentiments (NRC Method)") +
  labs(fill = "Sentiments") +
  xlab("Sentiments/ Word Category") +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold.italic"),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "red", size = 9, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "red", size = 9, face = "bold"),
  )

# The second ggplot identifies sentiments using the bing method. This method
# does not duplicate values because they are either positive or negative within 
# bing. 

ggplot(data = filter(sentiment_df_two, !is.na(bing))) +
  geom_bar(aes(x = bing, fill = factor(bing)), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_y_continuous(breaks = c(50, 150, 250, 350, 450)) +
  labs(title = "The Refugee Brief Total Sentiments (Bing Method)") +
  labs(fill = "Sentiments") +
  xlab("Sentiments/ Binary Feelings") +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold.italic"),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "red", size = 9, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "red", size = 9, face = "bold"),
  )

## Section Three

# I use a built-in function in R to pull out a list of countries.

countries <- tibble(country = sort(unique(tolower(world.cities$country.etc))))

# I notice that the list does not include some countries identified in the unhcr
# full data set. So I manually input them into the countries data set myself.

missing_countries <- tibble(country = c(
  "côte d’ivoire",
  "eritrean",
  "american",
  "egyptian",
  "america"
))

countries <- bind_rows(countries, missing_countries)

# I then match the countries in the countries data set with any country
# mentioned in the full unhcr data set.

country_df <- unhcr_full %>%
  mutate(reply = lemma %in% c(countries$country)) %>%
  filter(reply == "TRUE") %>%
  select(lemma)

country_df %>%
  group_by(lemma) %>%
  count(lemma) %>%
  rename("country_mentions" = "n") %>%
  arrange(desc(country_mentions)) %>%
  head(10)

# I plot the number of times each country is mentioned in the full unhcr data 
# set.

ggplot(data = country_df) +
  geom_bar(aes(x = lemma, fill = factor(lemma)), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 55)) +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)) +
  labs(title = "The Refugee Brief Total Country Mentions") +
  labs(fill = "Countries in order") +
  xlab("Countries") +
  ylab("Total Mention Counts") +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold.italic"),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "red", size = 8, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "red", size = 9, face = "bold"),
  )

## END