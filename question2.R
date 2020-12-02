## What is the average time, in days, from the posting on shopify to sale on shopify, by month? 
## This is of those items that have sold. Should we do of those that are currently listed? 

sold_orders <- subset(big_df, sold_status == "Sold")

date2 <- str_split_fixed(sold_orders$created_at, " ", 2)
sold_orders$date2 <- date2[,1]
sold_orders$date2 <- strptime(sold_orders$date2, "%Y-%m-%d")
sold_orders$date_diff2 <- (sold_orders$date2-sold_orders$date_listed_shopify)
units(sold_orders$date_diff2)  <- "days"
sold_orders$date_diff2 <- round(sold_orders$date_diff2, digits = 0)
sold_orders$date_diff2 <- ifelse(sold_orders$date_diff2<0, NA, sold_orders$date_diff2)

## Why are there sold date differences of less than zero? 
selling_time <- aggregate(date_diff2 ~ type, sold_orders, FUN = mean)
selling_time <- data.table(table(sold_orders$date_diff2))
selling_time$V1 <- as.numeric(selling_time$V1)

n <- table(!is.na(sold_orders$date_diff2))

ggplot(selling_time, aes(x=V1, y=N)) +
  geom_bar(stat = "identity") +
  #geom_ribbon(stat= "smooth", aes(ymin=0, ymax = ..y..)) +
  ggtitle(glue("Number of days from posting to selling on Isella, ", "N={n[2]}")) +
  xlab("Days") +
  ylab(expression("Frequency"))

average_days <- round(mean(sold_orders$date_diff2, na.rm = TRUE), digits = 0)
average_days

plot(selling_time$V1, selling_time$N)