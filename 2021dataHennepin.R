# Load required libraries
library(rvest)
library(dplyr)

# Function to scrape data for a given link
scrape_data <- function(link) {
  # Send a GET request to the link
  page <- read_html(link)
  
  # Extract year
  year <- 2021
  
  # Extract questions
  questions <- page %>% html_nodes(xpath = "/html/body/main/div/div[3]/div/main/div[2]/table[1]/caption/div/span[1]") %>%
    html_text(trim = TRUE)
  
  num_precincts <- page %>% html_nodes(xpath = "/html/body/main/div/div[3]/div/main/div[2]/table[1]/tbody/tr") %>%
    length()
  
  # Initialize data frame to store results
  results <- data.frame(Precinct = character(0), Question = character(0), Response = character(0), Number_Votes = numeric(0), Election_Date = character(0), stringsAsFactors = FALSE)
  
  # Loop through questions tables
  num_tables <- num_precincts
  
  for (table_index in 1:num_tables) {
    # Construct XPath for the current question table
    table_xpath <- paste0("/html/body/main/div/div[3]/div/main/div[2]/table[", table_index, "]")

    
    # Extract questions for the current table
    current_questions <- page %>% html_nodes(xpath = paste0(table_xpath, "/caption/div/span[1]")) %>%
      html_text(trim = TRUE)
    
    # Construct XPath for "Yes" and "No" for the current table
    yes_xpath <- paste0(table_xpath, "/tbody/tr[1]/td[3]")
    no_xpath <- paste0(table_xpath, "/tbody/tr[2]/td[3]")
    
    # Check if "Yes" is present for the current table
    yes_nodes <- page %>% html_nodes(xpath = yes_xpath)
    
    if (length(yes_nodes) > 0) {
      yes_present <- yes_nodes %>% html_text(trim = TRUE) == "YES"
    } else {
      yes_present <- FALSE
    }
    
    # Check if "No" is present for the current table
    no_nodes <- page %>% html_nodes(xpath = no_xpath)
    
    # Extract precinct name
    precinct_name <- page %>% html_node(xpath = "/html/body/main/div/div[3]/div/main/div[2]/div[2]/table/caption/text()") %>%
      html_text(trim = TRUE)
    
    if (length(no_nodes) > 0) {
      print(no_nodes %>% html_text(trim = TRUE))
      no_present <- no_nodes %>% html_text(trim = TRUE) == "NO"
    } else {
      no_present <- FALSE
    }
    
    if (yes_present) {
      yes_xpath_votes <- paste0(table_xpath, "/tbody/tr[1]/td[4]")
      yes_votes_str <- page %>% html_nodes(xpath = yes_xpath_votes) %>%
        html_text(trim = TRUE)
      # Remove commas and convert to numeric
      yes_votes <- as.numeric(gsub(",", "", yes_votes_str))
      results <- rbind(results, data.frame(Precinct = precinct_name, Question = current_questions, Response = "Yes", Number_Votes = yes_votes, Election_Date = year, stringsAsFactors = FALSE))
    }
    
    if (no_present) {
      print("NO PRESENT")
      no_xpath_votes <- paste0(table_xpath, "/tbody/tr[2]/td[4]")
      no_votes_str <- page %>% html_nodes(xpath = no_xpath_votes) %>%
        html_text(trim = TRUE)
      # Remove commas and convert to numeric
      no_votes <- as.numeric(gsub(",", "", no_votes_str))
      results <- rbind(results, data.frame(Precinct = precinct_name, Question = current_questions, Response = "No", Number_Votes = no_votes, Election_Date = year, stringsAsFactors = FALSE))
    }
  }
  
  return(results)
}

# Initialize a list to store data for all links
all_data <- list()

# URL for selecting all precincts
precinct_selection_url <- "https://electionresults.sos.state.mn.us/Select/Precinct/Index?ErsElectionId=142&Id=county&CountyId=27&show=Go"

# Send a GET request to the precinct selection URL
precinct_page <- read_html(precinct_selection_url)

# Extract all precinct links from the dropdown list
precinct_links <- precinct_page %>%
  html_nodes(xpath = "/html/body/main/div/div[2]/main/div[2]/form/div[2]/div/select/option") %>%
  html_attr("value")

# Loop through each precinct link
for (link in precinct_links) {
  # Create the complete URL for the precinct
  precinct_url <- paste0("https://electionresults.sos.state.mn.us/results/Index?ErsElectionId=142&CountyId=27&DistrictId=&Scenario=Precincts&selectprecincts=", link, "&show=Show+Selected+Precincts")
  # Scrape data for the current precinct link
  data <- scrape_data(precinct_url)
  
  # Append the data to the list
  all_data[[link]] <- data
}

# Combine all the data into a single data frame
final_data <- do.call(rbind, all_data)

# Save the final data as a CSV file
write.csv(final_data, file = "precinct_data.csv", row.names = FALSE)
