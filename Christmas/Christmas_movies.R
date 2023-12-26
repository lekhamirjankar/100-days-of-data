# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(tm)
library(wordcloud)

# Read the CSV file into a data frame
file_path <- "/cloud/project/christmas_movies.csv"
movies <- read.csv(file_path)

head(movies)
summary(movies)
str(movies)

# 1. Trends Over Years

# Check nulls
print(movies[is.na(movies$release_year), c('title', 'release_year', 'director', 'stars')])
print(min(na.omit(movies$release_year)))
print(max(na.omit(movies$release_year)))
yearly_counts <- table(movies$release_year)

# Data frame for plotting
trends_data <- data.frame(
  year = as.numeric(names(yearly_counts)),
  count_movies = as.numeric(yearly_counts)
)

trends_palette <- c("#ff0000", "#00ff00", "#0000ff", "#ffcc00")

# Visualize
ggplot(trends_data, aes(x = year, y = count_movies)) +
  geom_line() +
  geom_point(color = rep(trends_palette, length.out = nrow(trends_data)), size = 2, shape = 16) +
  labs(
    title = "Number of Christmas Movies Released Each Year (2016-2022)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Junicode"),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_continuous(breaks = seq(min(trends_data$year), max(trends_data$year), by = 5)) +
  scale_y_continuous(breaks = seq(min(trends_data$count_movies), max(trends_data$count_movies), by = 9))

# 2. Genre distribution

# Check nulls
print(movies[is.na(movies$genre), c('title', 'genre')])

# Split genres into individual rows
genres_data <- movies %>%
  separate_rows(genre, sep = ", ")

# Count the occurrences of each genre and store in dataframe
genre_counts <- genres_data %>%
  group_by(genre) %>%
  summarize(count_movies = n()) %>%
  arrange(desc(count_movies))

# Custom color palette
genre_palette <- c("#741102", "#042D29")

# Visualize
ggplot(genre_counts, aes(x = reorder(genre, -count_movies), y = count_movies)) +
  geom_bar(stat = "identity", fill = rep(genre_palette, length.out = nrow(genre_counts))) +
  labs(
    title = "Most Common Genres for Christmas Movies",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Junicode"),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 3. Top & Lowest Rated/Voted Movies

# Convert votes to numeric
movies$votes_numeric <- as.numeric(gsub(",", "", movies$votes))

# Top rated
# Top movies based on ratings
top_movies <- movies %>%
  arrange(-imdb_rating, -votes_numeric) %>%
  head(30)

# Filter for movies released in the year 2000 and later
filtered_movies <- movies %>%
  filter(release_year >= 2000)

# Top movies based on ratings (2000 and later)
top_movies <- filtered_movies %>%
  arrange(-imdb_rating, -votes_numeric) %>%
  head(50)

# Visualize
ggplot(top_movies, aes(x = reorder(title, imdb_rating), y = imdb_rating)) +
  geom_bar(stat = "identity", fill = '#042D29', width = 0.7) +
  geom_text(aes(label = imdb_rating), hjust = -0.2, size = 3) +  # Add text labels 
  coord_flip() +
  labs(
    title = "Top-Rated Christmas Movies",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Junicode"),
    legend.position = "none",
    axis.text.x = element_blank()  # Remove x-axis labels
  )

# Top voted
# Top movies based on voting

top_voted_movies <- movies %>%
  arrange(-votes_numeric) %>%
  head(15)

# Visualize

ggplot(top_voted_movies, aes(x = reorder(title, votes_numeric), y = votes)) +
  geom_bar(stat = "identity", fill = '#042D29', width = 0.7) +
  geom_text(aes(label = imdb_rating), hjust = -0.2, size = 3) +  # Add text labels 
  coord_flip() +
  labs(
    title = "Top-Voted Christmas Movies",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Junicode"),
    legend.position = "none",
    axis.text.x = element_blank()  # Remove x-axis labels
  )

# lowest rated
# Lowest movies based on ratings
lowest_movies <- movies %>%
  arrange(imdb_rating, votes_numeric) %>%
  head(20)

# Visualize
ggplot(lowest_movies, aes(x = reorder(title, -imdb_rating), y = imdb_rating)) +
  geom_bar(stat = "identity", width = 0.7, fill = '#741102') +
  geom_text(aes(label = imdb_rating), hjust = -0.2, size = 3) +  # Add text labels 
  labs(
    title = "Lowest-Rated Christmas Movies",
    x = NULL,
    y = NULL
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    text = element_text(family = "Junicode"),
    legend.position = "none",
    axis.text.x = element_blank()  # Remove x-axis labels
  )

# 4. Word cloud

# Create a text corpus
corpus <- Corpus(VectorSource(movies$description))

# Preprocess the corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Create a tdm
tdm <- TermDocumentMatrix(corpus)

# Convert the tdm to a matrix
m <- as.matrix(tdm)

# Get word frequencies
word_freqs <- sort(rowSums(m), decreasing=TRUE)

# Get top 100 word frequencies
top_word_freqs <- head(word_freqs, 100)

# Define a custom color palette
my_color_palette <- brewer.pal(8, "Dark2")

# Create a word cloud with top 100 words
wordcloud(
  words = names(top_word_freqs),
  freq = top_word_freqs,
  scale = c(3, 0.5),
  min.freq = 1,
  colors = my_color_palette
)

wordcloud(words = names(word_freqs), freq = word_freqs, scale = c(3, 0.5), min.freq = 1, colors = brewer.pal(8, "Dark2"))