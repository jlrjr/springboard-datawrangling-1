# Springboard Data Wrangling Exercise 1 - John Roberts

install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)


#read file
products <- read.csv("refine_original.csv", stringsAsFactors = FALSE)

#clean company names and factor column
products <- products %>% 
  mutate(company=ifelse(grepl("^phil|^fil|^phl", company, ignore.case=TRUE), "phillips", company)) %>%
  mutate(company=ifelse(grepl("^ak", company, ignore.case=TRUE), "akzo", company)) %>%
  mutate(company=ifelse(grepl("^van", company, ignore.case=TRUE), "van houten", company)) %>%
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


#5: Create dummy variables for company and product category
#Both the company name and product category are categorical variables i.e. they take only a fixed set of values. In order to use them in further analysis you need to create dummy variables. Create dummy binary variables for each of them with the prefix company_ and product_ i.e.
#Add four binary (1 or 0) columns for company: company_philips, company_akzo, company_van_houten and company_unilever
#Add four binary (1 or 0) columns for product category: product_smartphone, product_tv, product_laptop and product_tablet
