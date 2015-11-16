accounts <- read.csv("accounts.csv")
fit <- lm(data=accounts, deal_size ~ sales_volume)
d = data.frame(accounts, predict(fit, interval="prediction"))
write.csv(d, file="predictions.csv")

