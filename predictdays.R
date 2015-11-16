library(dplyr)

today <- Sys.Date()

accounts <- read.csv("accounts.csv")

fit1 <- lm(data=accounts, deal_size ~ sales_volume)
deal_size_data <- data.frame(accounts, predict(fit1, interval="prediction"))
deal_size_data <- mutate(deal_size_data, predicted_deal_size=fit) %>% 
  select(duns, predicted_deal_size)
                         
fit2 <- lm(data=accounts, days_to_close ~ emp_total)
days_to_close_data <- data.frame(accounts, predict(fit2, interval="prediction"))
days_to_close_data <- mutate(days_to_close_data, predicted_days_to_close=as.integer(fit))
days_to_close_data = select(days_to_close_data, duns, business_name, days_to_close, days_in_pipeline, predicted_days_to_close)

# Group into buckets by days to close
days_to_close_data <- days_to_close_data %>%
  mutate(predicted_group=ifelse(predicted_days_to_close < 15, 
                                "1: Less than 15", 
                                ifelse(predicted_days_to_close < 30, 
                                       "2: Between 15 and 30",
                                       ifelse(predicted_days_to_close < 60,
                                              "3: Between 30 and 60",
                                              "4: 60 Days or More")))) %>%
  mutate(days_to_predicted_close=predicted_days_to_close - days_in_pipeline) %>%
  mutate(sales_pipeline_group=ifelse(days_to_predicted_close < 0,
                                     "1: Past predicted Closing Date",
                                     ifelse(days_to_predicted_close < 15,
                                            "2: Within 15 Days of Predicted Closing Date",
                                            ifelse(days_to_predicted_close < 30,
                                                   "3: Within 30 Days of Predicted Closing Date",
                                                   "4: 30 Days or More from Predicted Closing Date")))) %>%
  mutate(closing_date = today + days_to_predicted_close)

# Group into buckets by deal size
deal_size_data <- deal_size_data %>% mutate(deal_size_group = ifelse(predicted_deal_size < 7.5*10^6,
                                                                     "1: Less than $7.5M",
                                                                     ifelse(predicted_deal_size < 25*10^6,
                                                                            "2: Between $7.5M and $25M",
                                                                            ifelse(predicted_deal_size < 38*10^6,
                                                                                   "3: Between $25M and $38M",
                                                                                   ifelse(predicted_deal_size<67*10^6,
                                                                                          "4: Between $38M and $67M",
                                                                                          ifelse(predicted_deal_size < 85*10^6,
                                                                                                 "5: Between $67M and $85M",
                                                                                                 "6: Over $85M"))))))

d <- merge(deal_size_data, days_to_close_data, by="duns")
write.csv(d, file="days-to-close-predictions.csv")

