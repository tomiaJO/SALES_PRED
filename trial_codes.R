library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)

options(digits = 4)
options(scipen = 999)

data <- fread("./../Data/Sales Prediction/training.csv", stringsAsFactors = F)

glimpse(data)

data <- data %>%
          filter(quantity > 0 & sales_amount > 0) %>% ##this could be explored furter. EXPLAIN!
          mutate(purchase_date = as.Date(purchase_date, "%Y-%m-%d")) %>%
          arrange(contact_id, purchase_date) %>%
          select(contact_id, order_id, purchase_date, product_id, quantity, sales_amount)

data %>%
  summarize(row_count = n(),
    unique_customers = n_distinct(contact_id),
    transaction_count = n_distinct(order_id),
    unique_products = n_distinct(product_id),
    avg_quantity = mean(quantity),
    median_quantity = median(quantity),
    avg_sales_amount = mean(sales_amount),
    median_sales_amount = median(sales_amount)
    )

##TODO: avg&median / customer (not line item)! --> two-step aggr.
per_contact_avgs <- data %>%
                      group_by(contact_id) %>%
                      summarize(total_line_items = n(),
                                transaction_count = n_distinct(order_id),
                                unique_products = n_distinct(product_id),
                                avg_quantity = mean(quantity),
                                median_quantity = median(quantity),
                                avg_sales_amount = mean(sales_amount),
                                median_sales_amount = median(sales_amount),
                                total_sales_amount = sum(sales_amount))
per_contact_avgs %>%
  arrange(desc(transaction_count)) %>%
  head()

##visualize
per_contact_avgs %>%
  ggplot() + 
  geom_histogram(aes(x= transaction_count)) + ##log so at least we can read something...
  facet_wrap(~transaction_count > 25, scales = "free") 

#total_line_items * avg_sales_amount --> total sales. show?



##visualize contact_id groups and their contribution to sales --> "calibration" check
##TODO_validate results. groups should be decreasing. ratio?
total_rows <- nrow(data)
data %>%
  group_by(contact_id) %>%
  summarize(total_sales_amount = sum(sales_amount)) %>%
  ungroup() %>%
  arrange(desc(total_sales_amount)) %>%
  mutate(rowno = row_number()) %>%
  mutate(category = cut(rowno,
                        seq(-1, total_rows, total_rows/20),
                            include.lowest = T)) %>%
  group_by(category) %>%
  summarize(sum_total_sales = sum(total_sales_amount)) %>%
  mutate(cumsum_total_sales = cumsum(sum_total_sales)) %>%
  ggplot(aes(x=category, y=cumsum_total_sales)) + 
    geom_point() + 
    geom_line(group=1)


##TODO: monthly volumes
##visualize
ordered_year_month <- data %>%
                        mutate(purchase_year = year(purchase_date)) %>%
                        mutate(purchase_month = month(purchase_date)) %>%
                        distinct(purchase_year, purchase_month) %>%
                        mutate(purchase_year_month = paste(as.character(purchase_year),
                                                           as.character(purchase_month), sep = "-")) %>%
                        arrange(purchase_year, purchase_month) %>%
                        select(purchase_year_month)

data %>%
  mutate(purchase_year = year(purchase_date)) %>%
  mutate(purchase_month = month(purchase_date)) %>%
  mutate(purchase_year_month = paste(as.character(purchase_year),
                                     as.character(purchase_month), sep = "-")) %>%
  mutate(purchase_year_month = factor(purchase_year_month, 
                                      levels = ordered_year_month[["purchase_year_month"]])) %>%
  group_by(purchase_year_month) %>%
  summarize(sales = sum(sales_amount)) %>%
  ggplot(aes(x= purchase_year_month, y = sales)) + 
    geom_point() +
    geom_line(group=1)

head(data)
?cumsum

data %>%
  mutate(purchase_date_minus_2wks = purchase_date - 14) %>%
  group_by(contact_id, purchase_date) %>%
  mutate(cum_sales = cumsum(sales_amount)) %>%
  head(15)
