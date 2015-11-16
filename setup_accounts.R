data = read.table(file="/Users/reedn/DataChallenge/companies.csv", 
                  col.names = c("row_id", "duns", "business_name", "physical_city",
                                "physical_state_abbr", "emp_total", "sales_volume", "primary_sic",
                                "primary_sic_desc", "viability_score", "naics_desc", "naics", 
                                "market_segmentation_cluster"),
                  colClasses=c("character",        # row id
                               "character",        # Duns
                               "character",        # Business Name
                               "character",        # Physical City
                               "factor",           # State
                               "numeric",          # Emp Total 
                               "numeric",          # Sales Volume
                               "factor",           # Primary Sic
                               "character",        # Primary Sic Description
                               "factor",           # Viability Score
                               "character",        # NAICS DESC
                               "factor",           # NAICS
                               "factor"           # marketing segmentation cluster
                  ), 
                  skip=2, quote="\"'", sep=",")

# Setup accounts data
library(dplyr)
n <- 1 : nrow(data);
mn_sales <- mean(data$sales_volume)
epsilon <- mn_sales / nrow(data) * 1000
accounts <- data %>%
  mutate(expected_deal_size = sales_volume * .20, 
         noise = runif(length(sales_volume),                           
                       -epsilon, 
                       epsilon),
         deal_size = as.integer(ifelse(expected_deal_size + noise < 0,
                                0,
                                expected_deal_size + noise)))

# Add a few outliers
outliers <- filter(accounts, deal_size > 100000)
outliers <- mutate(outliers[1,], deal_size = deal_size + 500000)
outliers <- mutate(outliers[2,], deal_size = deal_size / 2)
outliers <- mutate(outliers[3,], deal_size = deal_size * 2)

deal_size <- accounts[5,c('deal_size')]
accounts[5,c('deal_size')] = deal_size * 2

deal_size <- accounts[2,c('deal_size')]
accounts[2,c('deal_size')] = deal_size / 2

deal_size <- accounts[7,c('deal_size')]
accounts[7,c('deal_size')] = deal_size * 2

# Days to close
accounts <- mutate(accounts, 
                   days_to_close = as.integer((sales_volume >= 18225000) * 90 + 
                     (sales_volume <= 18225000 & sales_volume >= 700000) * 60 + 
                     (sales_volume >= 110000 & sales_volume <= 700000) * 30 + 
                     (sales_volume <= 110000) * 20 + 
                     runif(length(sales_volume), -15, 15)))

# Days in Pipeline
accounts <- mutate(accounts,
                   days_in_pipeline = as.integer(runif(length(emp_total), 0, 120)))

accounts <- select(accounts, duns, business_name, days_to_close, days_in_pipeline, deal_size, emp_total, sales_volume, physical_state_abbr)

write.csv(accounts, file="accounts.csv")

