
#install.packages("data.table")
#install.packages("dplyr")
#install.packages("tidyr")
library(dplyr)
library(tidyr)
library(data.table)
setwd("Downloads")
my_data = read.csv("refine_original.csv")

View(my_data)
my_data <- my_data %>%
  tolower($company) %>%
  separate(col = "Product code / number", 
           into = "product code", "product number", 
           sep = "-") %>%
  mutate(p = "Smartphone", v = "TV", x = "Laptop", q = "Tablet") %>%
  mutate(full_address = address, city, country) %>%
  mutate(company_philips = (company = "philips"), 
         company_akzo = (company = "akzo"),  
         company_van_houten = (company = "van_houten"), 
         company_unilever = (company = "unilevers") $>$
  mutate(product_smartphone = (Product code = "Smartphone"), 
         product_tv = (Product code = "TV"),  
         product_laptop = (Product code = "Laptop"), 
         product_tablet = (Product code = "Tablet") $>$
           
  
  View(my_data)

  