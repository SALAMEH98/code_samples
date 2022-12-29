## Text Processing Code Example
## Sief Salameh

# The purpose of this section is to compare the amount of times renewable-
# energy terms are mentioned during the State of the State Address made by the 
# the states' governors. For the text processing section,
# I selected a total of four states that each had a change in the 
# governor's political party. Two states had a Democratic state legislature 
# (Maine and Minnesota) and the other two states had a Republican state 
# legislature (Missouri and Tennessee).

library(tidytext)
library(udpipe)
library(tidyverse)
library(ggplot2)
library(nametagger)
library(SnowballC)
library(maps)
library(rvest)
library(purrr)
library(ggpubr)
library(pdftools)

## Step One: Creating the Internal Energy Glossary

path <- setwd("~/Downloads/work_samples/text_processing/raw_data")

# I begin by creating an internal data base that includes all the terms and 
# vocabulary related to renewable energy. I do this by gathering all the
# renewable energy glossaries from governmental, non-profit, and private 
# entities and run a function that web scrapes only the terms that are bolded
# in each of those sites. I do this to avoid any words that are also part of
# the term definition.  

energy_links <- c(
  "https://www.epa.gov/green-power-markets/glossary",
  "https://www.eia.gov/tools/glossary/?id=renewable",
  "https://www.energy.gov/eere/fuelcells/glossary",
  "https://alternateenergycompany.com/renewable-energy-terminology/",
  "https://www.motherearthnews.com/sustainable-living/energy-glossary-zl0z1001zken/",
  "https://www.co.blaine.id.us/983/Solar-Energy-Glossary",
  "https://www.uesaz.com/glossary/"
)

key_words <- lapply(energy_links, function(x) {
  x %>%
    read_html() %>%
    html_nodes("strong") %>%
    html_text() %>%
    strsplit(split = " ")
})

energy_glossary <- as.data.frame(as.character(key_words)) %>%
  unlist_tokens() %>%
  rename("glossary_terms" = "token")

energy_glossary <- udpipe(energy_glossary$glossary_terms, "english")

# I create a general function that removes all the unnecessary and filler words 
# from the selected source. I run this function on the internal energy 
# glossary I created in the previous step. 

clean_words <- function(x) {
  x %>%
    filter(
      !upos %in% c("PUNCT", "CCONJ", "ADP", "DET", "AUX", "PART", "NUM", "sym"),
      !lemma %in% stop_words$word
    ) %>%
    mutate_if(is.character, str_to_lower) %>%
    filter(!lemma == "i")
}

energy_glossary <- clean_words(energy_glossary)

energy_glossary <- energy_glossary %>%
  distinct(lemma)

# After removing all the duplicate words, I create another column that 
# identifies the glossary words as energy related. This step allows me to 
# extract the matching words from the words mentioned in the governors' 
# speeches when I conduct the merge process. 

energy_glossary$energy_term <- c(rep("Yes", times = 604))

## Step Two: Extracting the Words from the Governors' Speeches

# The first state is Maine. I extract the words from the governor's speech 
# made in 2010 which was a Democratic governor, and then in 2011 which was a 
# Republican governor. The state legislature was Democratic.

maine_one <- pdf_text("~/Downloads/work_samples/text_processing/raw_data/Maine_2010.pdf")

maine_one <- udpipe(maine_one, "english")

maine_one <- clean_words(maine_one)

maine_2010 <-
  left_join(maine_one, energy_glossary, by = c("lemma" = "lemma")) %>%
  filter(energy_term == "Yes") %>%
  group_by(lemma) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(15)

maine_two <- pdf_text("~/Downloads/work_samples/text_processing/raw_data/Maine_2011.pdf")

maine_two <- udpipe(maine_two, "english")

maine_two <- clean_words(maine_two)

maine_2011 <-
  left_join(maine_two, energy_glossary, by = c("lemma" = "lemma")) %>%
  filter(energy_term == "Yes") %>%
  group_by(lemma) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(15)

graph_one <- ggplot(data = maine_2010) +
  geom_col(aes(y = lemma, x = n), fill = "blue") +
  scale_y_discrete(guide = guide_axis(angle = 0)) +
  xlim(0, 50) +
  labs(title = "Renewable-Energy Terms Mentioned During Maine's Governor Address") +
  ylab("Energy Terms") +
  xlab("Times Mentioned") +
  theme(
    plot.title = element_text(color = "black", size = 16, face = "bold.italic"),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 10, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 10, face = "bold"),
  )

graph_two <- ggplot(data = maine_2011) +
  geom_col(aes(y = lemma, x = n), fill = "red") +
  scale_y_discrete(guide = guide_axis(angle = 0)) +
  xlim(0, 50) +
  ylab("Energy Terms") +
  xlab("Times Mentioned") +
  theme(
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 10, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 10, face = "bold"),
  )

ggarrange(graph_one, graph_two,
          labels = c("2010", "2011"),
          ncol = 1, nrow = 2,
          vjust = c(1.5, .2),
          font.label = list(size = 14, face = "bold.italic", color = "purple")
)

# The second state is Minnesota. I extract the words from the governor's speech 
# made in 2010 which was a Republican governor, and then in 2011 which was a 
# Democratic governor. The state legislature was Democratic. 

minnesota_one <- pdf_text("~/Downloads/work_samples/text_processing/raw_data/Minnesota_2010.pdf")

minnesota_one <- udpipe(minnesota_one, "english")

minnesota_one <- clean_words(minnesota_one)

minnesota_2010 <-
  left_join(minnesota_one, energy_glossary, by = c("lemma" = "lemma")) %>%
  filter(energy_term == "Yes") %>%
  group_by(lemma) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(15)

minnesota_two <- pdf_text("~/Downloads/work_samples/text_processing/raw_data/Minnesota_2011.pdf")

minnesota_two <- udpipe(minnesota_two, "english")

minnesota_two <- clean_words(minnesota_two)

minnesota_2011 <-
  left_join(minnesota_two, energy_glossary, by = c("lemma" = "lemma")) %>%
  filter(energy_term == "Yes") %>%
  group_by(lemma) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(15)

graph_three <- ggplot(data = minnesota_2010) +
  geom_col(aes(y = lemma, x = n), fill = "red") +
  scale_y_discrete(guide = guide_axis(angle = 0)) +
  xlim(0, 15) +
  labs(title = "Renewable-Energy Terms Mentioned During Minnesota's Governor Address") +
  ylab("Energy Terms") +
  xlab("Times Mentioned") +
  theme(
    plot.title = element_text(color = "black", size = 16, face = "bold.italic"),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 10, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 10, face = "bold"),
  )

graph_four <- ggplot(data = minnesota_2011) +
  geom_col(aes(y = lemma, x = n), fill = "blue") +
  scale_y_discrete(guide = guide_axis(angle = 0)) +
  xlim(0, 15) +
  ylab("Energy Terms") +
  xlab("Times Mentioned") +
  theme(
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 10, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 10, face = "bold"),
  )

ggarrange(graph_three, graph_four,
          labels = c("2010", "2011"),
          ncol = 1, nrow = 2,
          vjust = c(1.5, .2),
          font.label = list(size = 14, face = "bold.italic", color = "purple")
)

# The third state is Tennessee. I extract the words from the governor's speech 
# made in 2010 which was a Democratic governor, and then in 2011 which was a 
# Republican governor. The state legislature was Republican. 

tennessee_one <- pdf_text("~/Downloads/work_samples/text_processing/raw_data/Tennessee_2010.pdf")

tennessee_one <- udpipe(tennessee_one, "english")

tennessee_one <- clean_words(tennessee_one)

tennessee_2010 <-
  left_join(tennessee_one, energy_glossary, by = c("lemma" = "lemma")) %>%
  filter(energy_term == "Yes") %>%
  group_by(lemma) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(15)

tennessee_two <- pdf_text("~/Downloads/work_samples/text_processing/raw_data/Tennessee_2011.pdf")

tennessee_two <- udpipe(tennessee_two, "english")

tennessee_two <- clean_words(tennessee_two)

tennessee_2011 <-
  left_join(tennessee_two, energy_glossary, by = c("lemma" = "lemma")) %>%
  filter(energy_term == "Yes") %>%
  group_by(lemma) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(15)

graph_five <- ggplot(data = tennessee_2010) +
  geom_col(aes(y = lemma, x = n), fill = "blue") +
  scale_y_discrete(guide = guide_axis(angle = 0)) +
  xlim(0, 20) +
  labs(title = "Renewable-Energy Terms Mentioned During Tennessee's Governor Address") +
  ylab("Energy Terms") +
  xlab("Times Mentioned") +
  theme(
    plot.title = element_text(color = "black", size = 16, face = "bold.italic"),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 10, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 10, face = "bold"),
  )

graph_six <- ggplot(data = tennessee_2011) +
  geom_col(aes(y = lemma, x = n), fill = "red") +
  scale_y_discrete(guide = guide_axis(angle = 0)) +
  xlim(0, 20) +
  ylab("Energy Terms") +
  xlab("Times Mentioned") +
  theme(
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 10, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 10, face = "bold"),
  )

ggarrange(graph_five, graph_six,
          labels = c("2010", "2011"),
          ncol = 1, nrow = 2,
          vjust = c(1.5, .2),
          font.label = list(size = 14, face = "bold.italic", color = "purple")
)

# The fourth state is Missouri. I extract the words from the governor's speech 
# made in 2008 which was a Republican governor, and then in 2009 which was a 
# Democratic governor. The state legislature was Republican. 

missouri_one <- pdf_text("~/Downloads/work_samples/text_processing/raw_data/Missouri_2008.pdf")

missouri_one <- udpipe(missouri_one, "english")

missouri_one <- clean_words(missouri_one)

missouri_2008 <-
  left_join(missouri_one, energy_glossary, by = c("lemma" = "lemma")) %>%
  filter(energy_term == "Yes") %>%
  group_by(lemma) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(15)

missouri_two <- pdf_text("~/Downloads/work_samples/text_processing/raw_data/Missouri_2009.pdf")

missouri_two <- udpipe(missouri_two, "english")

missouri_two <- clean_words(missouri_two)

missouri_2009 <-
  left_join(missouri_two, energy_glossary, by = c("lemma" = "lemma")) %>%
  filter(energy_term == "Yes") %>%
  group_by(lemma) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(15)

graph_seven <- ggplot(data = missouri_2008) +
  geom_col(aes(y = lemma, x = n), fill = "red") +
  scale_y_discrete(guide = guide_axis(angle = 0)) +
  xlim(0, 15) +
  labs(title = "Renewable-Energy Terms Mentioned During Missouri's Governor Address") +
  ylab("Energy Terms") +
  xlab("Times Mentioned") +
  theme(
    plot.title = element_text(color = "black", size = 16, face = "bold.italic"),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 10, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 10, face = "bold"),
  )

graph_eight <- ggplot(data = missouri_2009) +
  geom_col(aes(y = lemma, x = n), fill = "blue") +
  scale_y_discrete(guide = guide_axis(angle = 0)) +
  xlim(0, 15) +
  ylab("Energy Terms") +
  xlab("Times Mentioned") +
  theme(
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 10, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 10, face = "bold"),
  )

ggarrange(graph_seven, graph_eight,
          labels = c("2008", "2009"),
          ncol = 1, nrow = 2,
          vjust = c(1.5, .2),
          font.label = list(size = 14, face = "bold.italic", color = "purple")
)

## Step Three: Analysis of the Text Processing Graphs

# The results show that Maine, Minnesota, and Tennessee all saw an increase in 
# the mentions of energy-related terms when Democratic governors took office. 
# However, unlike the other states, Missouri saw a higher count of 
# energy-related terms when a Republican governor took office. The results 
# suggest that Democratic governors are more likely to mention energy-related 
# terms during their speeches, compared to Republican governors - regardless of 
# which political party controls the state legislature. However, because 
# Missouri is an outlier, we can not firmly conclude that this correlation 
# exists until we run the same test on multiple periods.

## END