## Number of items currently for sale and how long they have been posted for

active_orders <- subset(big_df, status == "active")

active_orders$todays_date <- strptime((Sys.Date()), "%Y-%m-%d")

active_orders$date_diff3 <- (active_orders$todays_date-active_orders$date_listed_shopify)

posted_time <- data.table(table(active_orders$date_diff3))
posted_time$V1 <- as.numeric(posted_time$V1)

n_active <- table(!is.na(active_orders$date_diff3))[2]
items_posted <- nrow(active_orders)

ggplot(posted_time, aes(x=V1, y=N)) +
  geom_bar(stat = "identity") +
  #geom_ribbon(stat= "smooth", aes(ymin=0, ymax = ..y..)) +
  ggtitle(glue("Number of days from posting to selling on Isella, ", "N={n[2]}")) +
  xlab("Days") +
  ylab(expression("Frequency"))

average_days <- round(mean(active_orders$date_diff3, na.rm = TRUE), digits = 0)
average_days

greater_14days <- filter(active_orders, date_diff3>14)
greater_14days <- tibble(subset(greater_14days[c(1, 2, 3, 5, 7, 8, 18, 42)]))
ggplot()

n_greater_14days <- nrow(greater_14days)

glue("Of {items_posted} items currently active on Shopify, we have date data for {n_active} posts. Of those {n_active} posts, 
     the average amount of time those have been posted is {average_days} days. The number of posts that have been active for more than 14 days is: {n_greater_14days} posts. 
     The details for posts listed for more than 14 days are as follows:")

greater_14days