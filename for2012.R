# Load required libraries
library(rvest)
library(dplyr)

# Define a function to scrape data for a given precinct and URL
scrape_precinct_data <- function(precinct_num, election_year) {
  # Determine the base URL based on the election year
  base_url <- if (election_year == "2018") {
    "https://enr.electionsfl.org/BRO/1985/"
  } else if (election_year == "2016") {
    "https://enr.electionsfl.org/BRO/1642/"
  } else if (election_year == "2014") {
    "https://enr.electionsfl.org/BRO/1396/"
  } else if (election_year == "2012") {
    "https://enr.electionsfl.org/BRO/1376/"
  }
  
  # Construct the URL for the given precinct and election year
  if (election_year == "2016" && precinct_num >= 12841) {
    url <- glue::glue("{base_url}Precincts/{precinct_num}/")
  } else if (election_year == "2018" && precinct_num >= 21169) {
    url <- glue::glue("{base_url}Precincts/{precinct_num}")
  } else if (election_year == "2014" && precinct_num >= 8317) {
    url <- glue::glue("{base_url}Precincts/{precinct_num}/")
  } 
    else if (election_year == "2012"){
    url <- glue::glue("{base_url}Precincts/{precinct_num}/")
  }
    else {
    url <- glue::glue("{base_url}Precincts/{precinct_num}/0/576/")
  }
  
  # Send a GET request to the URL
  page <- read_html(url)
  
  # Find the total number of questions dynamically
  num_questions <- length(page %>% html_nodes(xpath = "/html/body/div[1]/div[2]/section/div"))
  
  # Initialize an empty data frame to store the data
  precinct_data <- data.frame()
  
  # Loop through each question
  for (question_num in 2:num_questions) {
    if (election_year == "2012") 
    {
      yes_xpath <- glue::glue("/html/body/div[1]/div[2]/section/div[{question_num}]/div[3]/div[2]/div[1]/div[4]")
      no_xpath <- glue::glue("/html/body/div[1]/div[2]/section/div[{question_num}]/div[3]/div[2]/div[2]/div[4]")
    } 
    else 
    {
    yes_xpath <- glue::glue("/html/body/div[1]/div[2]/section/div[{question_num}]/table/tbody/tr[1]/td[6]")
    no_xpath <- glue::glue("/html/body/div[1]/div[2]/section/div[{question_num}]/table/tbody/tr[2]/td[6]")
    }
    
    # Extract the Yes and No vote counts
    yes_count <- page %>% html_node(xpath = yes_xpath) %>% html_text(trim = TRUE)
    no_count <- page %>% html_node(xpath = no_xpath) %>% html_text(trim = TRUE)
    
    # Construct XPath to get the question title
    question_title_xpath <- if (election_year == "2018") {
      glue::glue("/html/body/div[1]/div[2]/section/fieldset/div[1]/div[2]/select/option[{precinct_num - 21071}]")
    } else if (election_year == "2016") {
      glue::glue("/html/body/div[1]/div[2]/section/fieldset/div[1]/div[2]/select/option[{precinct_num -12755}]")
    } else if (election_year == "2014") {
      glue::glue("/html/body/div[1]/div[2]/section/fieldset/div[1]/div[2]/select/option[{precinct_num - 8256}]")
    } else if (election_year == "2012") {
      glue::glue("/html/body/div[1]/div[2]/section/fieldset/div[1]/div[2]/select/option[{precinct_num - 8146}]")
    }
    
    # Extract the question title
    question_title <- page %>% html_node(xpath = question_title_xpath) %>% html_text(trim = TRUE)
    
    # Construct XPath to get the precinct name
    precinct_name_xpath <- glue::glue("/html/body/div[1]/div[2]/section/div[{question_num}]/div[1]/span[1]")
    
    # Extract the precinct name
    precinct_name <- page %>% html_node(xpath = precinct_name_xpath) %>% html_text(trim = TRUE)
    
    # Create data frames for Yes and No responses
    yes_data <- data.frame(
      Precinct = precinct_name,
      Question = question_title,
      Response = "Yes",
      Votes = paste(yes_count, "votes"),
      Election_Date = election_year,
      stringsAsFactors = FALSE
    )
    
    no_data <- data.frame(
      Precinct = precinct_name,
      Question = question_title,
      Response = "No",
      Votes = paste(no_count, "votes"),
      Election_Date = election_year,
      stringsAsFactors = FALSE
    )
    
    # Append this question's data to the overall data frame
    precinct_data <- rbind(precinct_data, yes_data, no_data)
  }
  
  return(precinct_data)
}

# Initialize a list to store data for all precincts and years
all_data <- list()

# Loop through precinct numbers for 2018 (from 21145 to 21201)
for (precinct_num in 21145:21201) {
  # Scrape data for the current precinct for 2018
  precinct_data_2018 <- scrape_precinct_data(precinct_num, "2018")
  
  # Append the 2018 data to the list
  all_data[[as.character(precinct_num)]] <- precinct_data_2018
}

# Loop through precinct numbers for 2016 (from 12833 to 12886)
for (precinct_num in 12833:12886) {
  # Scrape data for the current precinct for 2016
  precinct_data_2016 <- scrape_precinct_data(precinct_num, "2016")
  
  # Append the 2016 data to the list
  all_data[[as.character(precinct_num)]] <- precinct_data_2016
}

# Loop through precinct numbers for 2014 (from 8312 to 8346)
for (precinct_num in 8312:8346) {
  # Scrape data for the current precinct for 2014
  precinct_data_2014 <- scrape_precinct_data(precinct_num, "2014")
  
  # Append the 2014 data to the list
  all_data[[as.character(precinct_num)]] <- precinct_data_2014
}

# Loop through precinct numbers for 2012 (from 8219 to 8257)
for (precinct_num in 8219:8257) {
  # Scrape data for the current precinct for 2012
  precinct_data_2012 <- scrape_precinct_data(precinct_num, "2012")
  
  # Append the 2012 data to the list
  all_data[[as.character(precinct_num)]] <- precinct_data_2012
}

# Combine all the data into a single data frame
final_data <- do.call(rbind, all_data)

# Save the final data as a CSV file
write.csv(final_data, file = "precinct_data_2018_2016_2014_2012.csv", row.names = FALSE)