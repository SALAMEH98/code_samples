## Web Scraping Code Example
## Sief Salameh

# The purpose of this section is to parse one article from the Refugee Brief 
# created by UNCHR, using natural language processing. I describe the sentiment 
# of the article using built-in sentiment data bases, while also identifying which 
# countries are discussed in the article by using a built-in library in R.

## Libraries

library(tidytext)
library(udpipe)
library(tidyverse)
library(ggplot2)
library(nametagger)
library(SnowballC)
library(maps)

## Section One

path <- ("~/Downloads/work_samples/web_scraping/raw_data")

text_one <- read_file("~/Downloads/work_samples/web_scraping/raw_data/unhcr_jan_28_2022.txt")

unhcr_df <- udpipe(text_one, "english")

unhcr_df$stem <- wordStem(unhcr_df$token, language = "english")

# I clean the unhcr data set by filtering out any punctuation and coordinating 
# conjunctions. This allows me to extract only the necessary words in the 
# article.  

unhcr_df <- unhcr_df %>%
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
                                token = "ngrams", n = 2
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

combined_words <- unite(combined_words,
                        bigram,
                        word1,
                        word2,
                        sep = " "
)

# I only filter out the countries that are spelled using two words. I do not 
# pull out "asylum seekers" or "human rights" because I already know that the 
# sentiment functions are going to value them the same whether they are 
# attached or not.

combined_words <- combined_words %>%
  filter(bigram %in% c("burkina faso", "côte d’ivoire"))

colnames(combined_words)[which(names(combined_words) == "bigram")] <- "lemma"

# I attach the countries to the unhcr data set. 

unhcr_df <- bind_rows(unhcr_df, combined_words)

# I then filter out the same two countries that existed in the unhcr data set 
# so I do not have duplicate values. I do this because I attached 
# the same countries from the bigram method. 

unhcr_df <- unhcr_df %>%
  filter(!lemma %in% c("côte", "faso", "burkina", "ivoire", "d'"))

sentiment_nrc <- get_sentiments("nrc") %>%
  rename(nrc = sentiment)

sentiment_afinn <- get_sentiments("afinn")

sentiment_bing <- get_sentiments("bing") %>%
  rename(bing = sentiment)

# I join the sentiment functions to my unhcr data set. I join them using the
# lemmas because the lemmas provide us real words that are stripped 
# of all word additions and/or word endings.  

sentiment_df <-
  left_join(unhcr_df, sentiment_nrc, by = c("lemma" = "word")) %>%
  select(lemma, nrc)

sentiment_df %>%
  filter(!is.na(nrc)) %>%
  group_by(nrc) %>%
  count(nrc) %>%
  arrange(desc(n)) %>% 
  head(10)

sentiment_df_two <- unhcr_df %>%
  left_join(sentiment_bing, by = c("lemma" = "word")) %>%
  select(lemma, bing)

sentiment_df_two %>%
  filter(!is.na(bing)) %>%
  group_by(bing) %>%
  count(bing) %>%
  arrange(desc(n)) %>%
  head(10)

# The first ggplot identifies sentiments using the NRC method. To be aware, 
# this method duplicates lemmas if the word has more than one value within the 
# NRC.

ggplot(data = filter(sentiment_df, !is.na(nrc))) +
  geom_bar(aes(x = nrc, fill = factor(nrc)), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
  labs(title = "The Refugee Brief Sentiments (NRC Method)") +
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
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50)) +
  labs(title = "The Refugee Brief Sentiments (Bing Method)") +
  labs(fill = "Sentiments") +
  xlab("Sentiments/ Binary Feelings") +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold.italic"),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "red", size = 9, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "red", size = 9, face = "bold"),
  )

## Section Two

# I use a built-in function in R to pull out a list of countries.

countries <- tibble(country = sort(unique(tolower(world.cities$country.etc))))

# I notice that the list does not include some countries identified in the unhcr
# data set. So I manually input them into the countries data set myself.

extra_country <- tibble(country = c(
  "côte d’ivoire",
  "eritrean",
  "american",
  "egyptian",
  "america"
))

countries <- bind_rows(countries, extra_country)

# I then match the countries in the countries data set with any country
# mentioned in the unhcr data set.

country_df <- unhcr_df %>%
  mutate(reply = lemma %in% c(countries$country)) %>%
  filter(reply == "TRUE") %>%
  select(lemma)

country_df %>%
  group_by(lemma) %>%
  count(lemma) %>%
  rename("country_mentions" = "n") %>%
  arrange(desc(country_mentions)) %>%
  head(10)

# I plot the number of times each country is mentioned in the unhcr data set.

ggplot(data = country_df) +
  geom_bar(aes(x = lemma, fill = factor(lemma)), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6)) +
  labs(title = "The Refugee Brief Country Mentions") +
  labs(fill = "Countries") +
  xlab("Country Mentions") +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold.italic"),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "red", size = 9, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "red", size = 9, face = "bold"),
  )

## END