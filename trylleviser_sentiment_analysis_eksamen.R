dir.create("data")
dir.create("figures")
dir.create("output_data")

install.packages("tidyverse")
install.packages("tidytext")
library(tidyverse)
library(tidytext)

#installing sentida
install.packages("remotes")
remotes::install_github("Guscode/Sentida")
remotes::install_github("Guscode/Sentida", force = TRUE)

1
library(Sentida)

#tidy data chec
library(dplyr)
library(stringr)

#loading data


#importing fonts for graphics
install.packages("extrafont")
library(extrafont)

font_import(prompt = FALSE)
fonttable()

#custom themes
custom_theme1 <- theme_minimal() +
  theme(
    panel.grid.major.y = element_line(linewidth = 2)
  )

custom_theme2 <- theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )

custom_theme3 <- theme(
  text = element_text(family = "AU Passata", size = 12),
  axis.text.x = element_text(angle = 45, hjust = 1)
)

library(ggplot2)



#trylleviser
#loading data

trylleviser <- read.csv("/Users/annapraest/Desktop/DM_førsteprojekt/DM_07_eksamen_tidy/data/trylleviser_eksamen.csv", header = TRUE, stringsAsFactors = FALSE)
trylleviser <- separate(trylleviser, "songtitle.lyrics", into = c("songtitle", "lyrics"), sep = ";")

#tidying data

trylleviser <- trylleviser %>%
  mutate(lyrics_clean = trylleviser$lyrics %>%
           str_replace_all("\n", " ") %>%
           str_replace_all("\\s{2,}", " ")
  )

#sentiment score trylleviser
sentiment_scores_trylleviser <- lapply(trylleviser$lyrics_clean, sentida, output = "total")
class(sentiment_scores_trylleviser)

numeric_scores <- sapply(sentiment_scores_trylleviser, as.numeric)
class(numeric_scores)

trylleviser$sentiment_scores <- numeric_scores

#visualizing it 
ggplot(trylleviser, aes(x = songtitle, y = sentiment_scores)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(title = "Trylleviser", x = "Folkeviser", y = "Sentiment score") +
  custom_theme1 + custom_theme2 + custom_theme3 +
  scale_y_continuous(limits = c(-50, 150 ), breaks = seq(-50, 150, by = 10))

#tokenization
trylleviser_token <- read.csv("/Users/annapraest/Desktop/DM_førsteprojekt/DM_07_eksamen_tidy/data/trylleviser_eksamen.csv", header = TRUE, stringsAsFactors = FALSE)
trylleviser_token <- separate(trylleviser_token, "songtitle.lyrics", into = c("songtitle", "lyrics"), sep = ";")


trylleviser_token <- trylleviser_token %>%
  mutate(lyrics_clean = trylleviser_token$lyrics %>%
           str_replace_all("\n", " ") %>%
           str_replace_all("\\s{2,}", " ")
  ) %>%
  unnest_tokens(word, lyrics_clean)

#sentiment analysis

sentiment_scores_trylleviser_token <- lapply(trylleviser_token$word, sentida, output = "total")
token_numeric_scores <- sapply(sentiment_scores_trylleviser_token, as.numeric)
trylleviser_token$sentiment_scores <- token_numeric_scores


trylleviser_token_grouped <- trylleviser_token %>%
  group_by(songtitle) %>%
  summarize(sentiment_scores = sum(sentiment_scores))


#visualizing it

ggplot(trylleviser_token_grouped, aes(x = songtitle, y = sentiment_scores)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(title = "Trylleviser tokenized", x = "Folkeviser", y = "Sentiment score") +
  custom_theme1 + custom_theme2 + custom_theme3 +
  scale_y_continuous(limits = c(-50, 150), breaks = seq(-50, 150, by = 10))


#combining dataframes into one sorting by common identifier

trylleviser_combined <- merge(trylleviser_token_grouped, trylleviser, by = "songtitle")

str(trylleviser_token_grouped)
str(trylleviser)


trylleviser_combined <- trylleviser_combined %>%
  rename(sentiment_scores_token = sentiment_scores.x, sentiment_scores = sentiment_scores.y)

str(trylleviser_combined)

class(trylleviser_combined$sentiment_scores)
class(trylleviser_combined$sentiment_scores_token)

names(trylleviser_combined)

#comparing results

ggplot(trylleviser_combined, aes(x = songtitle)) +
  geom_point(aes(y = sentiment_scores.x, color = "trylleviser"), size = 2.5) +
  geom_point(aes(y = sentiment_scores.y, color = "trylleviser_tokenized"), size = 2.5) +
  scale_color_manual(values = c("trylleviser" = "darkblue", "trylleviser_tokenized" = "darkred"),
                     name = "",
                     labels = c("trylleviser", "trylleviser tokenized")) +
  labs(title = "Sentiment scores comparison for trylleviser",
       x = "Trylleviser", y = "Sentiment score") +
  custom_theme1 + custom_theme2 + custom_theme3 +
  scale_y_continuous(limits = c(-50, 150), breaks = seq(-50, 150, by = 10))



#count how many words were not included in the analysis = 0.0000000
trylleviser_token_number_of_words <- trylleviser_token %>%
  group_by(songtitle) %>%
  summarise(words_with_score_0 = sum(sentiment_scores == 0.0000000),
            words_not_score_0 = sum(sentiment_scores != 0.0000000))


#count total number of words
trylleviser_token_number_of_words$combined_word_count <- trylleviser_token_number_of_words$words_with_score_0 + trylleviser_token_number_of_words$words_not_score_0


#removing stopwords
stopwords_19thcentury <- read.csv("/Users/annapraest/Desktop/DM_førsteprojekt/DM_07_eksamen_tidy/data/19th_century_stopwords_DA_light.csv")

#checking data compatability
print(head((trylleviser_token$word)))

print(head(stopwords_19thcentury))

class(trylleviser_token$word)
class(stopwords_19thcentury)

#extracting word column from stopwords_19thcentury
stopwords_list <- stopwords_19thcentury$word

class(stopwords_list)

#filtering out the stopwords from trylleviser_token
trylleviser_token_no_stopwords <- trylleviser_token[!trylleviser_token$word %in% stopwords_list, ]


#count how many words were not included in the analysis = 0.0000000 when stopwords not included
trylleviser_token_number_of_words_no_stopwords <- trylleviser_token_no_stopwords %>%
  group_by(songtitle) %>%
  summarise(words_with_score_0 = sum(sentiment_scores == 0.0000000),
            words_not_score_0 = sum(sentiment_scores != 0.0000000))


#count total number of words
trylleviser_token_number_of_words_no_stopwords$combined_word_count <- trylleviser_token_number_of_words_no_stopwords$words_with_score_0 + trylleviser_token_number_of_words$words_not_score_0





#problems with RMarkdown script, choosing CRANmirror
chooseCRANmirror(graphics = FALSE)


install.packages("rmarkdown")

install.packages("remotes")
install.packages("kableExtra")


