## Assignment #2: Text Mining
library(qdap) #replace_contraction
library(data.table) #fread
library(tm) # text minint (Corpus)
library(dplyr) #select
library(tidytext) #stop_words
library(tibble)
library(tidyverse)
library(stringr)
library(SnowballC) #stemming

if (!require("rlang")) install.packages("rlang")
if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/textstem")
pacman::p_load(textstem)
# ----------------------------------------------------------------------------
# FUNCTIONS
# ----------------------------------------------------------------------------
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)


# ----------------------------------------------------------------------------
# ANDROID
# Load file
reviews_android <- fread("android_review.csv", encoding = 'UTF-8')
# Drop column, keep only columns shared between two files
reviews_android <- select(reviews_android, c(Date, AppID, Author, Rating, Title, Review))
# convert integers to characters
reviews_android$AppID <- as.character(reviews_android$AppID)
reviews_android$Rating <- as.character(reviews_android$Rating)
reviews_android$Type = "android"

#str(reviews_android)
#dim(reviews_android); min(reviews_android$Date); max(reviews_android$Date)

# APPLE
# Load file
reviews_apple   <- fread("itunes_review.csv", encoding = 'UTF-8')
reviews_apple <- select(reviews_apple, c(Date, AppID, Author, Rating, Title, Review))
# convert integers to characters
reviews_apple$AppID <- as.character(reviews_apple$AppID)
reviews_apple$Rating <- as.character(reviews_apple$Rating)
reviews_apple$Type = "apple"
#str(reviews_apple)
#dim(reviews_apple); min(reviews_apple$Date); max(reviews_apple$Date)

## merge reviews
library(gtools)
reviews_all = smartbind(reviews_android, reviews_apple)
#Stem and Lemmatize reviews_all$Review
stem_words(reviews_all$Review)
lemmatize_words(reviews_all$Review)
#dim(reviews_all); min(reviews_all$Date); max(reviews_all$Date)
#head(reviews_all)
#str(reviews_all)

# Transformations, including changing letters to lowercase, remove
# punctuations, numbers and stop words.
# convert contractions back to their base words isn't is not
reviews_all$Review <- replace_contraction(reviews_all$Review)
# convert to lowercase
reviews_all$Review <- tolower(reviews_all$Review)
# remove punctuation
reviews_all$Review <- removePunctuation(reviews_all$Review)
reviews_all$Review <- gsub("[[:punct:]]", "",reviews_all$Review )
# remove numbers
reviews_all$Review <- removeNumbers(reviews_all$Review)
# remove anything other than English letters or space
reviews_all$Review <- removeNumPunct(reviews_all$Review)
#
# -----------------------------------------------------------------------------

##### TURN DATA INTO RData FILE HERE FOR SHINY APP


# Turn dataframe to tibble, number the reviews
tbl_reviews <- as_tibble(reviews_all) %>%
  mutate(review_no = row_number())
class(tbl_reviews)
# Print data
tbl_reviews


# one-token-per-row format + stemming
tidy_reviews <- tbl_reviews %>%
  unnest_tokens(word, Review) %>%
  mutate(word=wordStem(word))

tidy_reviews
# remove stop words
data("stop_words")
tidy_reviews <- tidy_reviews %>%
  anti_join(stop_words)
#count words
tidy_reviews %>%
  count(word, sort = TRUE)


# graph words
install.packages("ggplot2")
library(ggplot2)
tidy_reviews %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


tidy_reviews%>%
  count(word, sort = TRUE)


# word frequencies
library(tidyr)
library(stringr)
frequency <- tidy_reviews %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(Type, word) %>%
  group_by(Type) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(Type, proportion) %>%
  gather(Type, proportion)


library(scales)
# # expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = Type )) +#,
                      #color = abs(`android` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~Type, ncol = 2) +
  theme(legend.position = "none") #+
  #labs(y = `android`, x = NULL)

# cor.test(data = frequency[frequency$app == "android", ],
#          ~ proportion + 'apple')



# Sentiment Analysis
install.packages("tidytext")
library(tidytext)
sentiments
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


library(dplyr)
library(stringr)

nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

tidy_reviews %>%
  # filter(AppID == "1") %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)


library(tidyr)

androidsentiment <- tidy_reviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(Type, index = review_no %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

library(ggplot2)

ggplot(androidsentiment, aes(index, sentiment, fill = Type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Type, ncol = 2, scales = "free_x")


# comparing 3 sentiment dictionaries
afinn <- tidy_reviews %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = review_no %/% 80) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")


bing_and_nrc <- bind_rows(
  tidy_reviews %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  tidy_reviews %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive",
                                         "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, index = review_no %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


bind_rows(afinn,
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")


get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive",
                          "negative")) %>%
  count(sentiment)


get_sentiments("bing") %>%
  count(sentiment)


bing_word_counts <- tidy_reviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts


bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


# word clouds
library(wordcloud)

tidy_reviews %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


library(reshape2)

tidy_reviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)


tidy_reviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, Type, sort = TRUE) %>%
  acast(word ~ Type, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)


# beyond just words # bigrams / trigrams
bingnegative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

wordcounts <- tidy_reviews %>%
  group_by(review_no) %>%
  summarize(words = n())

tidy_reviews %>%
  semi_join(bingnegative) %>%
  group_by(review_no) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("review_no")) %>%
  mutate(ratio = negativewords/words) %>%
  #filter(review_no != 0) %>%
  top_n(1) %>%
  ungroup()


library(tidytext)
# term frequency

book_words <- reviews_all %>%
  unnest_tokens(word, Review) %>%
  count(Type, word, sort = TRUE) %>%
  ungroup()

total_words <- book_words %>%
  group_by(Type) %>%
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words

library(ggplot2)

ggplot(book_words, aes(n/total, fill = Type)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~Type, ncol = 2, scales = "free_y")


freq_by_rank <- book_words %>%
  group_by(Type) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

freq_by_rank

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = Type)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>%
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)


freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = Type)) +
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()


book_words <- book_words %>%
  bind_tf_idf(word, Type, n)
book_words

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(Type) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = Type)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~Type, ncol = 2, scales = "free") +
  coord_flip()


# Tokenizing by N-gram
android_bigrams <- reviews_all %>%
  unnest_tokens(bigram, Review, token = "ngrams", n = 2)

android_bigrams

android_bigrams %>%
  count(bigram, sort = TRUE)


library(tidyr)

bigrams_separated <- android_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")


bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts


bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

# Tri-grams
reviews_all %>%
  unnest_tokens(trigram, Review, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)


# Analyzing Bigrams
bigrams_filtered %>%
  filter(word2 == "app") %>%
  count(Type, word1, sort = TRUE)


bigram_tf_idf <- bigrams_united %>%
  count(Type, bigram) %>%
  bind_tf_idf(bigram, Type, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

# Using Bigrams to Provide Context in Sentiment Analysis
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")

AFINN
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words

not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()


negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()


library(igraph)

# original counts
bigram_counts

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  filter(word1 != "NA") %>%
  graph_from_data_frame()

bigram_graph

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

###############################################
##################BIGRAMS######################
###############################################

#Note: need to remove "NA"

review_bigrams <- reviews_all %>%
  unnest_tokens(bigram, Review, token = "ngrams", n = 2)
bigrams_separated <- review_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united %>%
  count(bigram, sort = TRUE)

#Trigrams
review_trigrams <- reviews_all %>%
  unnest_tokens(trigram, Review, token = "ngrams", n = 3)

trigrams_separated <- review_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

trigram_counts <- trigrams_filtered %>% 
  count(word1, word2, word3, sort = TRUE)

trigrams_united <- trigrams_filtered %>%
  unite(trigram, word1, word2, word3, sep = " ")

trigrams_united %>%
  count(trigram, sort = TRUE)









#Vizualize Bigrams usings word network


theme_set(theme_minimal())#set chart 

library(dplyr)
review_subject <- reviews_all %>% 
  unnest_tokens(word, Review) %>% 
  anti_join(stop_words)
my_stopwords <- data_frame(word = c(as.character(1:10)))
review_subject <- review_subject %>% 
  anti_join(my_stopwords)
title_word_pairs <- review_subject %>% 
  pairwise_count(word, ID, sort = TRUE, upper = FALSE)
set.seed(1234)
title_word_pairs %>%
  filter(n >= 1000) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle('Word network in TripAdvisor reviews')
theme_void()
