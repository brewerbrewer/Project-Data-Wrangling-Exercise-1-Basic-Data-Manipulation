library(tidyverse)
setwd("/Users/mikebrewer/Downloads")
my_data = read_csv("refine_original.csv")
##when loading csv, can use of of these: name repair: read_csv("name", .name_repair = "unique"), setNames: my_data %>% setNames(make.names(colnames(.)))

View(my_data)

#begin to clean data
clean_data <- my_data %>%
  
#1. Clean up the 'company' column so all of the misspellings of the brand names are standardized.   # below, the '=' vs a ',' yields wildly different results
  mutate(company = tolower(company)) %>%
  mutate(company = gsub(".*ps$", "philips", company)) %>% 
  mutate(company = gsub("^ak.*", "akzo", company)) %>% 
  mutate(company = gsub("^va.*", "van houten", company)) %>% 
  mutate(company = gsub("^un.*", "unilever", company))  
  
# 2: Separate the product code and product number into separate columns i.e. add two new columns called product_code and product_number
clean_data <- separate(clean_data, col = "Product code / number", 
           into = c("product_code", "product_number"), 
           sep = "-")
 

# 3: Add product categories. I set a named vector, products, and added it to clean_data with mutate and then indexed product_code with products.  
#this works and is written much more concisely than using ifelse but I'd like to understand more about
products <- c(p = "smartphone", v = "tv", x = "laptop", q = "tablet")
clean_data <- mutate(clean_data, product_category = products[clean_data$product_code])

# 4: Add full address for geocoding
# first tried this line, which didnt work: clean_data <- unite(address, city, country, col = "full_address", sep = "")
clean_data <- unite(clean_data, "full_address", c(address, city, country))


# 5: Create dummy variables for company and product category
##this seems like a lot of code, is there a more concise way? I couldn't figure out model.matrix

#set dummy variable columns for companies
company_philips <- ifelse(clean_data$company == "philips", 1, 0) 
company_akzo <- ifelse(clean_data$company == "akzo", 1, 0) 
company_van_houten <- ifelse(clean_data$company == "van houten", 1, 0) 
company_unilever <- ifelse(clean_data$company == "unilever", 1, 0) 
#set dummy variable columns for products
product_smartphone <- ifelse(clean_data$product_category == "smartphone", 1, 0)  
product_tv <- ifelse(clean_data$product_category == "tv", 1, 0)
product_laptop <- ifelse(clean_data$product_category == "laptop", 1, 0)
product_tablet <- ifelse(clean_data$product_category == "tablet", 1, 0)
#add dummy variable columns to clean_data
clean_data <- mutate(clean_data, company_philips, company_akzo, company_van_houten, company_unilever, 
                                   product_tablet, product_laptop, product_tv, product_smartphone)
  

view(clean_data)
                    
# 6: Submit the project on Github. Include your code, original data as a CSV file refine_original.csv, and the cleaned up data as a CSV file called refine_clean.csv.
write.csv(clean_data, "refine_clean.csv")
   