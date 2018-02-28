library(data.table)
library(dplyr)
library(ggplot2)

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
                                median_sales_amount = median(sales_amount))
per_contact_avgs %>%
  arrange(desc(transaction_count)) %>%
  head()

##visualize
per_contact_avgs %>%
  ggplot() + 
  geom_histogram(aes(x= transaction_count)) + ##log so at least we can read something...
  facet_wrap(~transaction_count > 25, scales = "free") 

#total_line_items * avg_sales_amount --> total sales. show?


##TODO: visualize top 10 customers (volume & $ amount)

##TODO: monthly volumes
##visualize

data %>%
  filter(sales_amount <= 0)


head(data, 15)
