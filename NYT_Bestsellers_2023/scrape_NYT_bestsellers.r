install.packages(c("httr", "jsonlite", "magrittr", "openxlsx"))
library(httr)
library(jsonlite)
library(magrittr)
library(openxlsx)

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