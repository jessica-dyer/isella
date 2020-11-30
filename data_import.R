###Data Import###
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(glue)

Isella_Gear_Listing_Form_All_Responses_LIST_BY_10_30_20 <- read_csv("Data/Isella Gear Listing Form - All Responses - LIST BY 10_30_20.csv")
Isella_Gear_Listing_Form_All_Responses_LIST_BY_11_06_20 <- read_csv("Data/Isella Gear Listing Form - All Responses - LIST BY 11_06_20.csv")
Isella_Gear_Listing_Form_All_Responses_LIST_BY_11_13_20 <- read_csv("Data/Isella Gear Listing Form - All Responses - LIST BY 11_13_20.csv")
Isella_Gear_Listing_Form_All_Responses_LIST_BY_11_20_20 <- read_csv("Data/Isella Gear Listing Form - All Responses - LIST BY 11_20_20.csv")
Isella_Gear_Listing_Form_All_Responses_LIST_BY_11_27_20 <- read_csv("Data/Isella Gear Listing Form - All Responses - LIST BY 11_27_20.csv")
Isella_Gear_Listing_Form_All_Responses_LIST_BY_12_04_20 <- read_csv("Data/Isella Gear Listing Form - All Responses - LIST BY 12_04_20.csv")
Isella_Gear_Listing_Form_Responses_Automagical_Sheetifier_V2_0 <- read_csv("Data/Isella Gear Listing Form (Responses) - Automagical Sheetifier V2.0.csv")
products_export_1 <- read_csv("Data/products_export_1.csv")
orders_export_1 <- read_csv("Data/orders_export_1.csv")

## Combine new listings into one DF

listing_df <- rbind(Isella_Gear_Listing_Form_All_Responses_LIST_BY_10_30_20, 
                    Isella_Gear_Listing_Form_All_Responses_LIST_BY_11_06_20, 
                    Isella_Gear_Listing_Form_All_Responses_LIST_BY_11_13_20, 
                    Isella_Gear_Listing_Form_All_Responses_LIST_BY_11_20_20) 
                   
listing_df$'Would you like to prioritize your item for an Instagram feature?' <- NA

listing_df <- rbind(listing_df,
                    Isella_Gear_Listing_Form_All_Responses_LIST_BY_11_27_20, 
                    Isella_Gear_Listing_Form_All_Responses_LIST_BY_12_04_20)

listing_df <- subset(listing_df[-c(6, 8, 9, 12, 15, 16, 17)])

col_names <- c("sku", "date_listed_shopify", "url", "date_sent_seller", "email", "name", "item_brand", 
               "item_style", "item_gender", "item_size", "donation", "price_asking", "price_new", "instagram_feature")

colnames(listing_df) <- col_names


## clean up old listing sheet to merge with new listing sheet 
old_listing_df <- Isella_Gear_Listing_Form_Responses_Automagical_Sheetifier_V2_0

old_listing_df <- subset(old_listing_df[c(1, 6, 14, 47, 48, 49, 50, 51)])
col_names_old <- c("item_style", "donation", "sku", "price_asking", "date_sent_seller", "date_listed_shopify", "email", "name" ) 
colnames(old_listing_df) <-col_names_old 

old_listing_df$url <- NA 
old_listing_df$item_brand <- NA 
old_listing_df$instagram_feature <- NA 
old_listing_df$url <- NA 
old_listing_df$item_size <- NA 
old_listing_df$item_gender <- NA 
old_listing_df$price_new <- NA 

old_listing_df <- filter(old_listing_df, !is.na(old_listing_df$item_style))

## Merge the new and the old lists into one big list 
final_listing_df <- rbind(listing_df, old_listing_df)

## Clean up each variable
final_listing_df$date_listed_shopify <- ifelse(final_listing_df$date_listed_shopify=="1/1/1900", NA, final_listing_df$date_listed_shopify)
final_listing_df$date_listed_shopify <- ifelse(final_listing_df$date_listed_shopify=="NA", NA, final_listing_df$date_listed_shopify)
final_listing_df$date_listed_shopify <- ifelse(final_listing_df$date_listed_shopify=="N/A", NA, final_listing_df$date_listed_shopify)
final_listing_df$date_listed_shopify <- ifelse(final_listing_df$date_listed_shopify=="10/24/20", "10/24/2020", final_listing_df$date_listed_shopify)
final_listing_df$date_listed_shopify <- ifelse(final_listing_df$date_listed_shopify=="11/25/20", "11/25/2020", final_listing_df$date_listed_shopify)
final_listing_df$date_listed_shopify <- ifelse(final_listing_df$date_listed_shopify=="10/27/20", "10/27/2020", final_listing_df$date_listed_shopify)
final_listing_df$date_listed_shopify <- ifelse(final_listing_df$date_listed_shopify=="11/11/20", "11/11/2020", final_listing_df$date_listed_shopify)
final_listing_df$date_listed_shopify <- ifelse(final_listing_df$date_listed_shopify=="11/09/20", "11/09/2020", final_listing_df$date_listed_shopify)

final_listing_df$date_listed_shopify <- ifelse(final_listing_df$date_listed_shopify=="earlier", NA, final_listing_df$date_listed_shopify)

final_listing_df$date_listed_shopify <- strptime(final_listing_df$date_listed_shopify, "%m/%d/%Y")
final_listing_df$date_sent_seller_new <- strptime(final_listing_df$date_sent_seller, "%m/%d/%Y %H:%M:%S")       

## Look at missing SKUs
missing_sku <- subset(final_listing_df, sku == "-" | sku == "N/A")

## Reassign those with strange SKUs to NA 
final_listing_df$sku[final_listing_df$sku=="N/A"] <- NA 
final_listing_df$sku[final_listing_df$sku=="-"] <- NA 
final_listing_df$sku <- str_replace_all(final_listing_df$sku, "[[:punct:]]", "")
table(is.na(final_listing_df$sku))

## Factor out all interesting variables 
final_listing_df$item_brand <- as.factor(final_listing_df$item_brand)

## Map from 3 different values to only two levels:
(final_listing_df$instagram_feature <- factor(final_listing_df$instagram_feature, levels = c("No thanks, I'll just wait for my gear to sell on the site.", "No thanks", "Make me a $5 Instagram ad!", "Yes"),
              labels = c("No", "No", "Yes", "Yes")))
#> [1] Male   Male   Male   Female Female
#> Levels: Male Female

## Filter out all NAs of 'final_listing'
final_listing_df <- filter(final_listing_df, !is.na(final_listing_df$sku))


## Look at all products listed on Isella (active and archived)
products <- filter(products_export_1, !is.na(products_export_1$Title))
products <- subset(products[c(2, 4, 5, 7, 14, 17, 20, 21, 47, 48)])
col_names_products <- c("title", "vendor", "type", "published", "sku", "inventory", "price_isella_listed", "price_compare", "price_seller", "status" ) 
colnames(products) <-col_names_products

## Currently 91 products without an SKU 
table(is.na(products$sku))

products$type <- as.factor(products$type)

products$sku <- str_replace_all(products$sku, "[[:punct:]]", "")
products <- filter(products, !is.na(products$sku))


## Look at all orders executed on Isella
orders <- orders_export_1
orders <- subset(orders[c(1, 2, 3, 4, 5, 6, 9, 10, 11, 12, 16,  21, 30, 31, 32, 47)])
col_names_orders <- c("order_number", "buyer_email", "financial_status", "date_purchased", "fulfillment_status", "date_fulfilled", "subtotal", "shipping", "taxes", "total", 
                      "created_at", "sku", "city", "zip", "state", "cancelled")
colnames(orders) <- col_names_orders

orders$date_purchased <- strptime(orders$date_purchased, "%Y-%m-%d %H:%M:%S %z") 
orders$date_fulfilled <- strptime(orders$date_fulfilled, "%Y-%m-%d %H:%M:%S %z")
orders$created_at <- strptime(orders$created_at, "%Y-%m-%d %H:%M:%S %z")  

orders$cancelled <- strptime(orders$cancelled, "%Y-%m-%d %H:%M:%S %z") 
orders$sku <- str_replace_all(orders$sku, "[[:punct:]]", "")
orders$sold_status <- ifelse(is.na(orders$cancelled), "Sold", "Cancelled")
orders$zip <- str_replace_all(orders$zip, "'", "")

orders$sold_status <- as.factor(orders$sold_status)
orders$state <- as.factor(orders$state)
orders$zip <- as.factor(orders$zip)

orders <- filter(orders, !is.na(orders$sku))


## Merge listings, products, and orders based on SKU 
big_df = merge(final_listing_df, products, by.x = "sku", by.y = "sku", all = TRUE)
big_df = merge(big_df, orders, by.x = "sku", by.y = "sku", all = TRUE)



