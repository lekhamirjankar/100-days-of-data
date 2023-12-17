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
excel_file_path <- "nyt_bestsellers.xlsx"

# Write the data frame to an Excel file
write.xlsx(combined_data, file = excel_file_path, sheetName = "Bestsellers 2023", rowNames = TRUE)