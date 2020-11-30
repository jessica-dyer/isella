## What is the average time, in days, from the seller listing with Isella to Isella posting on shopify? 

## Variables come from the google sheet listings (now). There are many missing from the first few months. 
date1 <- str_split_fixed(big_df$date_sent_seller, " ", 2)
big_df$date1 <- date1[,1]
big_df$date1 <- strptime(big_df$date1, "%m/%d/%Y")
big_df$date_diff <- (big_df$date_listed_shopify-big_df$date1)
units(big_df$date_diff)  <- "days"
big_df$date_diff <- round(big_df$date_diff, digits = 0)


posting_time <- data.table(table(big_df$date_diff))
posting_time$V1 <- as.numeric(posting_time$V1)

n <- table(!is.na(big_df$date_diff))
  
ggplot(posting_time, aes(x=V1, y=N)) +
  geom_bar(stat = "identity") +
  #geom_ribbon(stat= "smooth", aes(ymin=0, ymax = ..y..)) +
  ggtitle(glue("Number of days from listing to posting on Isella, ", "N={n[2]}")) +
  xlab("Days") +
  ylab(expression("Frequency")) +
  scale_colour_discrete(name = "Type of sources")

units(big_df$date_diff)  <- "days"

average_days <- round(mean(big_df$date_diff, na.rm = TRUE), digits = 0)
average_days