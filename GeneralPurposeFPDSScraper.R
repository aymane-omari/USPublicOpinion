library(rvest)
library(dplyr)

# Function to clean the text
clean_text <- function(text) {
  cleaned_text <- gsub("[\t\n$]", "", text)
  return(trimws(cleaned_text))
}

# Function to scrape data from the URL
scrape_fpds_data <- function(url) {
  webpage <- read_html(url)
  
  # Initialize data frame to store results
  results <- data.frame(Contracting_Agency = character(0), Action_Obligation = character(0),
                        Legal_Business_Name = character(0), PSC_Code = character(0), 
                        NAICS_Code = character(0), Entity_State = character(0),
                        Unique_Entity_ID = character(0), Award_ID = character(0),
                        stringsAsFactors = FALSE)
  
  # Extract the number of tables
  num_tables <- length(webpage %>% html_nodes(xpath = "//table[4]//td[2]//table//tr[2]//td//table//tr//td//table"))
  
  # Loop through tables
  for (i in 1:num_tables) {
    # Extract data from each table
    contracting_agency <- webpage %>%
      html_nodes(xpath = paste0("//table[4]//td[2]//table//tr[2]//td//table//tr//td//table[", i, "]//tr[2]//td[4]//span/a")) %>%
      html_text() %>%
      as.character()
    
    action_obligation <- webpage %>%
      html_nodes(xpath = paste0("//table[4]//td[2]//table//tr[2]//td//table//tr//td//table[", i, "]//tr[3]//td[4]")) %>%
      html_text() %>%
      as.character() %>%
      clean_text()
    
    legal_business_name <- webpage %>%
      html_nodes(xpath = paste0("//table[4]//td[2]//table//tr[2]//td//table//tr//td//table[", i, "]//tr[8]//td[4]//span/a")) %>%
      html_text() %>%
      as.character()
    
    psc_code <- webpage %>%
      html_nodes(xpath = paste0("//table[4]//td[2]//table//tr[2]//td//table//tr//td//table[", i, "]//tr[5]//td[4]//span/a")) %>%
      html_text() %>%
      as.character()
    
    naics_code <- webpage %>%
      html_nodes(xpath = paste0("//table[4]//td[2]//table//tr[2]//td//table//tr//td//table[", i, "]//tr[5]//td[2]//span/a")) %>%
      html_text() %>%
      as.character()
    
    entity_state <- webpage %>%
      html_nodes(xpath = paste0("//table[4]//td[2]//table//tr[2]//td//table//tr//td//table[", i, "]//tr[7]//td[2]")) %>%
      html_text() %>%
      as.character() %>%
      clean_text()
    
    unique_entity_id <- webpage %>%
      html_nodes(xpath = paste0("//table[4]//td[2]//table//tr[2]//td//table//tr//td//table[", i, "]//tr[6]//td[4]//span/a")) %>%
      html_text() %>%
      as.character()
    
    award_id <- webpage %>%
      html_nodes(xpath = paste0("//table[4]//td[2]//table//tr[2]//td//table//tr//td//table[", i, "]//tr[1]//td[2]//span/a[1]")) %>%
      html_text() %>%
      as.character()
    
    # Create a data frame for the current table
    row_data <- data.frame(Contracting_Agency = contracting_agency, Action_Obligation = action_obligation,
                           Legal_Business_Name = legal_business_name, PSC_Code = psc_code, 
                           NAICS_Code = naics_code, Entity_State = entity_state,
                           Unique_Entity_ID = unique_entity_id, Award_ID = award_id,
                           stringsAsFactors = FALSE)
    
    # Append the row data to the results data frame
    results <- bind_rows(results, row_data)
  }
  
  return(results)
}

# Define the search term
search_term <- "tote+bag"  # Replace with your desired search term

# Get the total number of pages
base_url <- "https://www.fpds.gov/ezsearch/search.do?indexName=awardfull&templateName=1.5.3&s=FPDS.GOV&q="
url <- paste0(base_url, search_term)
webpage <- read_html(url)
num_elements <- as.numeric(webpage %>% html_nodes(xpath = "//table[4]//td[2]//table//tr[1]//td[2]/span/b[3]") %>% html_text())
num_pages <- ceiling(num_elements / 30)

# Scrape data from all pages
all_results <- data.frame()
for (page in 1:num_pages) {
  if (page == 1) {
    current_url <- url
  } else {
    current_url <- paste0(base_url, search_term, "&s=FPDS.GOV&templateName=1.5.3&indexName=awardfull&start=", (page - 1) * 30)
  }
  page_results <- scrape_fpds_data(current_url)
  all_results <- bind_rows(all_results, page_results)
}

# Save the data as a CSV file
write.csv(all_results, file = "fpds_data_all_pages.csv", row.names = FALSE)
