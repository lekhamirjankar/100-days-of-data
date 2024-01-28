install.packages(c("httr", "jsonlite", "magrittr", "openxlsx"))
library(httr)
library(jsonlite)
library(magrittr)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(grid)

# Data scraping & manipulation

# This is to see all the available lists
res <- GET("https://api.nytimes.com/svc/books/v3/lists/names.json?api-key=<add your api key here>")
res
all_lists <- fromJSON(rawToChar(res$content))
all_lists

# Function to get data from the list 'combined-print-and-e-book-fiction' 
get_nyt_data <- function(api_key, date) {
  url <- paste0("https://api.nytimes.com/svc/books/v3/lists/", date, "/combined-print-and-e-book-fiction.json?api-key=", api_key)
  res <- GET(url)

  # Introduce 12 sec delay between each request to avoid overwhelming the site 
  Sys.sleep(12)
  
  # Process Response
  content <- fromJSON(rawToChar(res$content), flatten = TRUE)
  
  # Accessing list elements
  list_name <- content$results$list_name
  bestsellers_date <- content$results$bestsellers_date
  published_date <- content$results$published_date
  display_name <- content$results$display_name
  normal_list_ends_at <- content$results$normal_list_ends_at
  
  # Accessing information about books
  books <- content$results$books
  rank <- books$rank
  title <- books$title
  author <- books$author
 
  # Combining data into a data frame 
  book_data <- data.frame(
    Rank = rank,
    Title = title,
    Author = author,
    BestsellerDate = bestsellers_date,
    PublishedDate = published_date
  )
  
  return(book_data)
}

# Get data for each week in 2023
api_key <- "<add your api key here>"
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-12-31")
date_range <- seq(start_date, end_date, by = "weeks")
all_data <- lapply(date_range, function(date) get_nyt_data(api_key, format(date, "%Y-%m-%d")))

# Combine all data frames into a single data frame
combined_data <- do.call(rbind, all_data)

#print(tail(combined_data, 5))

# Set the max print option to display all rows
#options(max.print = 99999)
#combined_data

# Specify the file path for the Excel file
#excel_file_path <- "nyt_bestsellers.xlsx"

# Write the data frame to an Excel file
#write.xlsx(combined_data, file = excel_file_path, sheetName = "Bestsellers 2023", rowNames = TRUE)

# Function to scrape genre information from goodreads
scrapeGenres <- function(title) {
  title <- gsub(" ", "%20", title, fixed = TRUE)
  
  # Construct the url
  search_query <- paste0(title)
  
 search_url <- paste0("https://www.goodreads.com/search?q=", URLencode(search_query))
  
  # Perform google search to get the goodreads link
  search_results <- read_html(search_url, encoding = "latin1")
  
  # Extract the link to the first result
  first_result_link <- search_results %>%
    html_nodes("a.bookTitle") %>%
    html_attr("href") %>%
    head(1)
  
  # Construct the full URL for the first result
  full_url <- paste0("https://www.goodreads.com", first_result_link)
  
  # Scrape the genre information
  genres <- full_url %>%  
    read_html() %>%  
    html_nodes(".BookPageMetadataSection__genres a") %>%  
    html_text()
 
  return(genres)
}

# Test if the function is working 
# Extract the first title from the NYT dataframe
# first_title <- combined_data$Title[1]
# Run the script for the first title
# genres <- scrapeGenres(first_title)
# Print the genres
# print(genres)

# Iterating through titles in the NYT dataframe
combined_data_with_genres <- combined_data
combined_data_with_genres$Genre1 <- NA
combined_data_with_genres$Genre2 <- NA
combined_data_with_genres$Genre3 <- NA
combined_data_with_genres$Genre4 <- NA

for (i in seq_len(nrow(combined_data_with_genres))) {
  title <- combined_data_with_genres$Title[i]
  genres <- scrapeGenres(title)
  
  # Assign top 4 genres
  combined_data_with_genres$Genre1[i] <- genres[1]
  combined_data_with_genres$Genre2[i] <- genres[2]
  combined_data_with_genres$Genre3[i] <- genres[3]
  combined_data_with_genres$Genre4[i] <- genres[4]
  
  # Delay to avoid overwhelming the site
  Sys.sleep(10)
}

# Create a final genre column based on conditions
NYT_df_combined_genre <- combined_data_with_genres %>% 
  mutate(Genre = case_when(
    !(Genre1 %in% c('Fiction', 'Audiobook', 'Contemporary')) ~ Genre1,
    !(Genre2 %in% c('Fiction', 'Audiobook', 'Contemporary')) ~ Genre2,
    !(Genre3 %in% c('Fiction', 'Audiobook', 'Contemporary')) ~ Genre3,
    !(Genre4 %in% c('Fiction', 'Audiobook', 'Contemporary')) ~ Genre4,
    TRUE ~ 'Unknown'
  ))

# Analysis 

# 1. Top Genres

# Calculate the count of genres
top_genres <- NYT_df_combined_genre %>%
  group_by(Genre = Genre) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))

# Visualize
ggplot(top_genres, aes(x = "", y = Count, fill = Genre)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Top Genres",
       fill = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "Junicode"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        legend.position = "right")

# 2. Temporal Analysis

# Round down the dates to the start of each month
NYT_df_combined_genre$YearMonth <- floor_date(NYT_df_combined_genre$PublishedDate, "month")

# Group data by month and summarize the rankings
monthly_data <- NYT_df_combined_genre %>%
  group_by(YearMonth, Genre) %>%
  summarise(AverageRank = mean(Rank))

# Visualize
ggplot(monthly_data, aes(x = YearMonth, fill = Genre)) +
  geom_bar() +
  labs(title = "Genre Distribution Over Time",
       x = NULL,
       y = "Frequency",
       fill = "Genre") +
  scale_color_brewer(palette = "Set3") +
  theme_minimal() +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme(
    text = element_text(family = "Junicode"),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 0.5, size = 11)
  )

# 3. Prolific Authors

# Convert data type
NYT_df_combined_genre$Author <- as.character(NYT_df_combined_genre$Author)

# Identify most prolific authors based on the number of appearances
prolific_authors <- NYT_df_combined_genre %>%
  group_by(Author) %>%
  summarize(Appearances = n_distinct(Title)) %>%
  arrange(desc(Appearances))

# Filter for authors with at least 3 appearances
prolific_authors_filtered <- prolific_authors %>%
  filter(Appearances >= 3)

# Visualize
ggplot(prolific_authors_filtered, aes(x = reorder(Author, -Appearances), y = Appearances)) +
  geom_bar(stat = "identity", fill = "#AC66CC") +
  labs(title = "Most Prolific Authors",
       x = NULL,
       y = "Number of Appearances") +
  theme_minimal() +
  theme(
    text = element_text(family = "Junicode"),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 0.5, size = 11)
  )

# 4. Bestseller Rankings Over Time

# Convert dates to Date type
NYT_df_combined_genre$PublishedDate <- as.Date(NYT_df_combined_genre$PublishedDate)

# Identify top 10 most frequent books
top_10_books <- NYT_df_combined_genre %>%
  group_by(Title) %>%
  summarize(Frequency = n()) %>%
  top_n(10, Frequency) %>%
  pull(Title)

# Filter dataframe for top 10 books
filtered_data <- NYT_df_combined_genre %>%
  filter(Title %in% top_10_books)

# Define your custom colors
custom_colors <- c("#333C83", "#FFB830", "#9ADCFF", "#C70A80", "#176B87", "#F24C4C", "#0B666A", "#8B104E", "#004721", "#563761")

# Visualize
ggplot(filtered_data, aes(x = PublishedDate, y = Rank, color = Title)) +
  geom_line(linewidth = 0.7) +
  labs(title = "Bestseller Rankings Over Time",
       x = NULL,
       y = "Rank") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Junicode"),
        axis.text.x = element_text(hjust = 0.5, size = 11)) +
  scale_color_manual(values = custom_colors) +
  guides(color = guide_legend(title = NULL)) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month")

# 5. Top 10 Books

# Create a vector of unique titles
unique_titles <- unique(filtered_data$Title)

# Create dataset containing only top 10 books
top_10_data <- filtered_data %>%
  filter(Title %in% unique_titles) %>%
  distinct(Title, .keep_all = TRUE) %>%
  slice_head(n = 10)

# Display book covers for each unique title
plots <- lapply(seq_len(nrow(top_10_data)), function(i) {
  
  img <- top_10_data$Image[i] |>
    readBin("raw", 1e6) |>
    jpeg::readJPEG() 
  img <- rasterGrob(img, interpolate = TRUE)
  
  text <- textGrob(paste("Rank: ", top_10_data$Rank[i]), gp = gpar(fontsize = 12, col = "white"))
  
  # Combine image and text
  grob <- arrangeGrob(img, text, ncol = 1, heights = unit.c(unit(0.8, "npc"), unit(0.2, "npc")))
  
  # Return the combined grob
  return(grob)
})

# Arrange the plots in a grid with 2 rows and 5 columns
grid.arrange(grobs = plots, ncol = 5, nrow = 2)