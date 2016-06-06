# Springboard Data Wrangling Exercise 1 - John Roberts

#install.packages("dplyr")
#install.packages("tidyr")
library(dplyr)
library(tidyr)


#read file
products <- read.csv("refine_original.csv", stringsAsFactors = FALSE)

#clean company names and factor column
products <- products %>% 
  mutate(company=ifelse(grepl("^phil|^fil|^phl", company, ignore.case=TRUE), "phillips", company)) %>%
  mutate(company=ifelse(grepl("^ak", company, ignore.case=TRUE), "akzo", company)) %>%
  mutate(company=ifelse(grepl("^van", company, ignore.case=TRUE), "van_houten", company)) %>%
  mutate(company=ifelse(grepl("^Uni", company, ignore.case=TRUE), "unilever", company)) %>%
  mutate(company = factor(company))

#Separate the product code and product number into separate columns 
products <- products %>%
  separate(Product.code...number, into=c("product_code", "product_number"), sep = "-")

#Add product categories (p = Smartphone, v = TV, x = Laptop, q = Tablet)
products <- products %>%
  mutate(product_category=ifelse(product_code == "p", "Smartphone", "")) %>%
  mutate(product_category=ifelse(product_code == "v", "TV", product_category)) %>%
  mutate(product_category=ifelse(product_code == "x", "Laptop", product_category)) %>%  
  mutate(product_category=ifelse(product_code == "q", "Tablet", product_category)) %>%
  mutate(product_category = factor(product_category))
  
#4: Add full address for geocoding
products <- products %>%
  unite("full_address", address:country, sep=", ", remove=TRUE)


#5: Create dummy binary variables for company and product category

products <- products %>% 
  mutate(company_phillips = ifelse(company == "phillips", 1, 0)) %>% 
  mutate(company_akzo = ifelse(company == "akzo", 1, 0)) %>% 
  mutate(company_van_houten = ifelse(company == "van_houten", 1, 0)) %>% 
  mutate(company_unilever = ifelse(company == "unilever", 1, 0))


products <- products %>% 
  mutate(category_laptop = ifelse(product_category == "Laptop", 1, 0)) %>% 
  mutate(category_smartphone = ifelse(product_category == "Smartphone", 1, 0)) %>% 
  mutate(category_tablet = ifelse(product_category == "Tablet", 1, 0)) %>% 
  mutate(category_tv = ifelse(product_category == "TV", 1, 0))


