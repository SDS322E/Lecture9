## Data Transformation Lecture Code

## Collaborative Filtering Demonstration

library(tidyverse)

## Try using this much smaller file to run the code shown in class
amazon <- read_tsv("amazon_reviews_us_Pet_Products_v1_00.tsv.bz2",
                   col_types = "cccccccdddccccD")

amazon

## Quick look
glimpse(amazon)

## Select a subset of columns
dat <- amazon |> 
    select(customer_id, 
           product_id:product_title,
           star_rating,
           starts_with("review")) |> 
    rename(rating = star_rating)
dat

## Remove some columns that we don't need
dat <- dat |> 
    select(-review_body, -review_id)
dat |> 
    glimpse()

## Look at the distribution of star ratings
dat |> 
    group_by(rating) |> 
    summarize(n = n())

## Add a new variable that represents the percentage of each rating
dat |> 
    group_by(rating) |> 
    summarize(n = n()) |> 
    mutate(pct = 100 * n / sum(n))

## Sort the ratings in ascending order by percentage
dat |> 
    filter(!is.na(rating)) |> 
    group_by(rating) |> 
    summarize(n = n()) |> 
    mutate(pct = 100 * n / sum(n)) |> 
    arrange(pct)

## Sort the ratings in descending order by percentage
dat |> 
    filter(!is.na(rating)) |> 
    group_by(rating) |> 
    summarize(n = n()) |> 
    mutate(pct = 100 * n / sum(n)) |> 
    arrange(desc(pct))


## Sample one review
dat |> 
    sample_n(1)

## NOTE: Need to change this to whatever product_id is shown above
dat |> 
    filter(product_id == "0763004227")

## Show a data frame of the number of reviews there are for each product
dat |> 
    group_by(product_id) |> 
    summarize(n = n())

## Summarize the distribution of the number of reviews per product
dat |> 
    group_by(product_id) |> 
    summarize(n = n()) |> 
    select(n) |> 
    summary()

## Show a data frame of the products with more than 2 reviews
dat |> 
    group_by(product_id) |> 
    summarize(n = n()) |> 
    filter(n > 2)

## Store a vector containing the product IDs of those products with more than 2
## reviews
prodlist <- dat |> 
    group_by(product_id) |> 
    summarize(n = n()) |> 
    filter(n > 2) |> 
    pull(product_id)

glimpse(prodlist)

## Show the distribution of reviews per customer ID
dat |> 
    group_by(customer_id) |> 
    summarize(n = n()) |> 
    select(n) |> 
    summary()

## Store a vector containing customer IDs that have done more than 1 review
userlist <- dat |> 
    group_by(customer_id) |> 
    summarize(n = n()) |> 
    filter(n > 1) |> 
    pull(customer_id)

glimpse(userlist)

## Subset the data to those customer IDs and product IDs that have multiple
## reviews
ratings <- dat |> 
    filter(customer_id %in% userlist 
           & product_id %in% prodlist)

ratings |> 
    glimpse()

## Sample a single review (after setting the random number generator seed)
set.seed(2023)
ratings |> 
    sample_n(1) |> 
    select(customer_id, product_id, 
           rating, product_title)

## See who else has reviewed this product (NOTE: You will need to change the
## product_id here)
ratings |>
    filter(product_id == "B001U4M9I6") |>
    select(customer_id, rating,
           review_headline, review_date)

## Find a (different) customer who has reviewed the same product (NOTE: You will
## need to change the customer_id here)
ratings |>
    filter(customer_id == "50994974") |>
    select(product_id, product_title)

## Look at how a few customers give ratings on average (NOTE: You will need to
## change the customer_id's here)
ratings |> 
    filter(customer_id %in% c("45464833", "50994974", 
                              "41861197", "12485248")) |> 
    group_by(customer_id) |> 
    summarize(mean_rating = mean(rating),
              num_rating = n())

## Show a data frame of products that these customers have reviewed (NOTE: You
## will need to change the customer_id's here)
ratings |> 
    filter(customer_id %in% c("50994974", "41861197", "12485248")) |> 
    filter(rating >= 4) |> 
    arrange(customer_id) |> 
    select(customer_id, product_id, 
           rating, product_title) 


