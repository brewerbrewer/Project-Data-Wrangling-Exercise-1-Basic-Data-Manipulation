install.packages("tidyverse")
library(tidyverse)
setwd("/Users/mikebrewer/Downloads")
my_data = read.csv("refine_original.csv")
View(my_data)

#begin to clean data
clean_data <- my_data %>%

#1. Clean up the 'company' column so all of the misspellings of the brand names are standardized.
  my_data <- tolower(my_data$company) %>%
  ### not sure which format is correct:
  rename(company, replace = c(ends_with("lips") = "Philips")) %>%
  ### or this one
  rename(my_data$company, starts_with(van) = "Van Houten") %>%
  ### or this one
  sub(pattern = "^ak.*", replacement = "Akzo", x = company) %>%
  ### or this one
  sub(pattern = "^u.*", replacement = "Unilever", x = my_data$company) %>%
  
  
# 2: Separate the product code and product number into separate columns i.e. add two new columns called product_code and product_number
  separate(col = "Product code...number", 
           into = c("product_code", "product_number"), 
           sep = "-") %>%

# 3: Add product categories
  my_data$product_category <- sub(pattern = p, replacement = "Smartphone", x = 
                              sub(pattern = v, replacement = "TV", x = 
                              sub(pattern = x, replacement = "Laptop", x = 
                              sub(pattern = q, replacement = "Tablet", x = my_data$product_code)))) %>%

# 4: Add full address for geocoding
  unite(address, city, country, col = "full_address", sep = "") %>%

# 5: Create dummy variables for company and product category
  mutate(my_data, company_philips = ifelse(company == "Philips", 1, 0), 
  mutate(my_data, company_akzo = ifelse(company == "Akzo", 1, 0), 
  mutate(my_data, company_van_houten = ifelse(company == "Van Houten", 1, 0), 
  mutate(my_data, company_unilever = ifelse(company == "Unilever", 1, 0) 
         %>%
  mutate(my_data, product_smartphone = ifelse(product code == "Smartphone", 1, 0), 
  mutate(my_data, product_tv = ifelse(product code == "TV", 1, 0), 
  mutate(my_data, product_laptop = ifelse(product code == "Laptop", 1, 0), 
  mutate(my_data, product_tablet = ifelse(product code == "Tablet", 1, 0), 

                    
# 6: Submit the project on Github. Include your code, 
# the original data as a CSV file refine_original.csv, 
# and the cleaned up data as a CSV file called refine_clean.csv.
write.csv(clean_data, "refine_clean.csv")
   