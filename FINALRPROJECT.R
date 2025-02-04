#TASK 1: Install and Load Required Libraries

# Install required packages if not already installed
if (!require("rvest")) install.packages("rvest")  # For web scraping
if (!require("httr")) install.packages("httr")    # For HTTP requests
if (!require("xml2")) install.packages("xml2")    # For parsing XML/HTML
if (!require("dplyr")) install.packages("dplyr")  # For data manipulation
if (!require("stringr")) install.packages("stringr")  # For string processing
if (!require("ggplot2")) install.packages("ggplot2")  # For data visualization
if (!require("tidyr")) install.packages("tidyr")  # For data tidying
if (!require("lubridate")) install.packages("lubridate")  # For handling dates


# Load libraries
library(rvest)
library(httr)
library(xml2)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(lubridate)


#TASK 2: Take User Input and Scrape Articles

# Prompt the user for the year of interest
year_input <- as.integer(readline(prompt = "Enter the year for which you want to scrape articles: "))


# Function to scrape articles for the given year
scrape_articles <- function(year) {
  base_url <- "https://molecular-cancer.biomedcentral.com/articles"  # Base URL for the website
  articles <- list()  # Initialize an empty list to store articles
  
  for (page in 1:10) {  # Loop through the first 10 pages; can adjust as needed
    url <- paste0(base_url, "?year=", year, "&page=", page)  # Construct the URL with year and page
    webpage <- GET(url)  # Fetch the webpage
    
    # Check if the webpage is successfully retrieved
    if (status_code(webpage) == 200) {
      content <- read_html(webpage)  # Parse the HTML content
      article_nodes <- content %>% html_nodes(".c-listing__title a")  # Extract article links
      
      for (node in article_nodes) {  # Loop through each article link
        article_url <- paste0("https://molecular-cancer.biomedcentral.com", html_attr(node, "href"))  # Full article URL
        article_page <- read_html(GET(article_url))  # Fetch the article page
        
        # Extract article details using appropriate selectors
        title <- tryCatch(article_page %>% html_node("h1.c-article-title") %>% html_text(trim = TRUE), error = function(e) NA)
        authors <- tryCatch(article_page %>% html_nodes(".c-article-author-list__item, .c-author-list__author-name") %>% html_text(trim = TRUE) %>% paste(collapse = ", "), error = function(e) NA)
        
        corr_author <- tryCatch({
          # Extract all `a` tags inside the `#corresponding-author-list` element
          article_page %>%
            html_nodes("#corresponding-author-list a") %>% # Select all `a` elements in the list
            html_text(trim = TRUE) %>%                     # Get the text content of each `a` tag
            paste(collapse = ", ")                         # Combine all names into a single string
        }, error = function(e) NA) 
        
        #corr_author <- tryCatch(article_page %>% html_node("#corresp-c1") %>% html_text(trim = TRUE), error = function(e) NA)
        corr_email <- tryCatch(article_page %>% html_node("#corresp-c1") %>% html_attr("href") %>% str_replace("mailto:", ""), error = function(e) NA)
        
        # Extract and format the publication date
        publish_date <- tryCatch({
          article_page %>% 
            html_nodes(".c-bibliographic-information__list-item") %>%   
            html_nodes(xpath = ".//p[contains(., 'Published')]") %>%    
            html_node(".c-bibliographic-information__value time") %>%   
            html_text(trim = TRUE)                                      
        }, error = function(e) NA)
        
        # Extract abstract and keywords
        abstract <- tryCatch(article_page %>% html_node(".c-article-section__content") %>% html_text(trim = TRUE), error = function(e) NA)
        keywords <- tryCatch(
          article_page %>%
            html_nodes(".c-article-subject-list__subject") %>%
            html_text(trim = TRUE) %>%
            str_c(., collapse = ", "),  
          error = function(e) NA
        )
        # Add commas between keywords for readability
        keywords <- gsub("([a-z]) ([A-Z])", "\\1, \\2", keywords) 
        
        # Append the extracted data as a list
        articles <- append(articles, list(list(
          Title = title,
          Authors = authors,
          Correspondence_Author = corr_author,
          Correspondence_Email = corr_email,
          Publish_Date = publish_date,
          Abstract = abstract,
          Keywords = keywords
        )))
      }
    } else {
      message("Failed to retrieve page ", page)  # Log if a page fails to load
    }
  }
  
  return(bind_rows(lapply(articles, as.data.frame)))  # Combine all article lists into a data frame
}

# Call the function with user-provided input and save results
articles <- scrape_articles(year_input)

# Save scraped data to a CSV file
output_filename <- paste0("articles_", year_input, ".csv")
write.csv(articles, output_filename, row.names = FALSE)

cat("Data saved to", output_filename, "\n")

#TASK 3: Data Cleaning and Preprocessing

# Load the scraped data
articles <- read.csv(output_filename, stringsAsFactors = FALSE, colClasses = "character")

# Replace empty strings with NA for better processing
articles <- articles %>% mutate(across(everything(), ~na_if(.x, "")))

# Remove rows with missing titles OR abstracts
articles <- articles %>% filter(!is.na(Title), !is.na(Abstract))

# Replace abstracts starting with "Correction" with "None"
articles <- articles %>%
  mutate(Abstract = ifelse(grepl("^Correction", Abstract, ignore.case = TRUE), "None", Abstract))


# Remove duplicate articles based on Title and Authors
articles <- articles %>% distinct(Title, Authors, .keep_all = TRUE)

# Drop the Correspondence_Email column as it's sensitive information
articles <- articles %>% select(-Correspondence_Email)

# Fill missing keywords with "None"
articles <- articles %>% mutate(Keywords = ifelse(is.na(Keywords), "None", Keywords))

# Remove rows where both Abstract and Keywords are "None"
articles <- articles %>% filter(!(Abstract == "None" & Keywords == "None"))

articles <- articles %>%
  mutate(Authors = gsub("[^a-zA-Z\\s,]", "", Authors)) %>%  # Remove non-alphanumeric characters
  mutate(Authors = gsub("\\s+", " ", Authors)) %>%         # Replace multiple spaces with a single space
  mutate(Authors = gsub(",+", ",", Authors)) %>%           # Replace consecutive commas with a single comma
  mutate(Authors = gsub(",\\s*,", ", ", Authors)) %>%      # Handle stray commas
  mutate(Authors = gsub("([a-z])([A-Z])", "\\1 \\2", Authors)) %>%  # Add space between lowercase and uppercase
  mutate(Authors = gsub("([A-Z])([A-Z])", "\\1 \\2", Authors)) %>%  # Add space between consecutive uppercase letters
  mutate(Authors = gsub("\\b(?i)na\\b", "", Authors, perl = TRUE)) %>%  # Remove standalone 'na'
  mutate(Authors = gsub("\\s*O\\s*RC\\s*IDorcidorg\\s*", "", Authors, ignore.case = TRUE)) %>%  # Remove "O RC IDorcidorg"
  mutate(Authors = trimws(Authors)) %>%                   # Trim spaces
  mutate(Authors = lapply(strsplit(Authors, ",\\s*"), function(x) {  # Split names by commas
    x <- trimws(x)  # Trim spaces around each name
    if (length(x) > 1) {
      formatted_authors <- paste(x[-length(x)], collapse = ", ")  # Join all but the last name
      last_author <- paste0(x[length(x)], ".")  # Append period to the last name
      paste0(formatted_authors, ", ", last_author)
    } else {
      paste0(x, ".")  # Single author case
    }
  })) %>%
  mutate(Authors = unlist(Authors))  # Flatten list to a character vector





# Save cleaned data
write.csv(articles, "cleaned_articles.csv", row.names = FALSE)





#TASK4




# Load cleaned data
articles_data <- read.csv("cleaned_articles.csv")

# Convert the Publish_Date column to Date format
articles_data$Publish_Date <- as.Date(articles_data$Publish_Date, format = "%d %B %Y")

# Visualization 1: Publication Timeline
# Group data by publication date to count the number of articles published each day
publication_timeline <- articles_data %>%
  group_by(Publish_Date) %>%
  summarise(Articles_Published = n())

# Plot a line graph showing the trend of articles published over time
ggplot(publication_timeline, aes(x = Publish_Date, y = Articles_Published)) +
  geom_line(color = "blue") +
  labs(title = "Articles Published Over Time", 
       x = "Date", 
       y = "Number of Articles") +
  theme_minimal()

# This visualization shows the daily trend of article publication.
# Peaks indicate dates with high publication activity.
# Useful for identifying specific periods of increased research output.


# Visualization 2: Top Keywords
# Clean and split the Keywords column to count keyword frequency
# Clean and split the Keywords column to count keyword frequency
top_keywords <- articles_data %>%
  mutate(Keywords = str_replace_all(Keywords, "[\r\n]", " "),  # Remove newlines and extra spaces
         Keywords = str_replace_all(Keywords, "\\s*,\\s*", ",")) %>%
  separate_rows(Keywords, sep = ",") %>%
  mutate(Keywords = str_trim(Keywords)) %>%
  filter(!is.na(Keywords), Keywords != "none", Keywords != "None") %>%  # Exclude "none" in any case
  mutate(Keywords = str_to_lower(Keywords)) %>%  # Treat keywords as case-insensitive
  group_by(Keywords) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  head(10)  # Select the top 10 keywords

# Plot a bar graph of the top 10 keywords
ggplot(top_keywords, aes(x = reorder(Keywords, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 10 Keywords", 
       x = "Keywords", 
       y = "Frequency") +
  theme_minimal()



# This visualization highlights the most frequently used keywords in the articles.
# It helps understand the dominant research themes for the selected year.
# Keywords are ranked based on their frequency of occurrence.


# Visualization 3: Word Count in Abstracts
# Count the number of words in each article's abstract
articles_data$Abstract_Word_Count <- str_count(articles_data$Abstract, "\\w+")

# Plot a histogram showing the distribution of abstract word counts
ggplot(articles_data, aes(x = Abstract_Word_Count)) +
  geom_histogram(binwidth = 50, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Word Counts in Abstracts", 
       x = "Word Count", 
       y = "Number of Articles") +
  theme_minimal()

# This histogram illustrates the distribution of word counts in abstracts.
# It helps analyze the typical length of abstracts and identify outliers.
# For instance, articles with very short or very long abstracts may stand out.


# Visualization 4: Monthly Publication Trend
# Create a new column for the month of publication
articles_data$Publish_Month <- floor_date(articles_data$Publish_Date, "month")

# Group data by month to count the number of articles published each month
monthly_publication_trend <- articles_data %>%
  group_by(Publish_Month) %>%
  summarise(Articles_Published = n())

# Plot a bar graph of monthly publication trends
ggplot(monthly_publication_trend, aes(x = Publish_Month, y = Articles_Published)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Monthly Publication Trend", 
       x = "Month", 
       y = "Number of Articles") +
  theme_minimal()

# This bar chart shows the distribution of articles published per month.
# It helps identify seasonal or periodic trends in publication activity.
# For example, peaks in certain months may reflect deadlines or conference periods.


# Visualization 5: Distribution of Number of Authors per Article
# Count the number of authors for each article based on commas in the Authors field
articles_data$Num_Authors <- str_count(articles_data$Authors, ",") + 1

# Plot a histogram of the number of authors per article
ggplot(articles_data, aes(x = Num_Authors)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  labs(title = "Distribution of Number of Authors per Article", 
       x = "Number of Authors", 
       y = "Number of Articles") +
  theme_minimal()

# This histogram illustrates how many authors are typically involved in an article.
# It helps understand collaboration trends in research.
# A spike at lower numbers indicates that most articles have fewer authors, while tails show highly collaborative works.