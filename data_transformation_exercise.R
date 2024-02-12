## Data Transformation Exercise

## Amazon Digital Software Reviews Data

library(tidyverse)

## Population Exploration Questions:
##
## How many customers are there?
## What kinds of customers are there?
## How many products do people review, on average?
## What are the most popular products?
## How many reviews have helpful votes?
## How have reviews of Digital Software changed over time?
    
    
## Read in data file for Digital Software products
amazon <- read_tsv("amazon_reviews_us_Digital_Software_v1_00.tsv.gz",
                   col_types = "cccccccdddccccD")

amazon

## Quick look
glimpse(amazon)


## Look at the distribution of star ratings
amazon |> 
    group_by(star_rating) |> 
    summarize(n = n())

################################################################################
## How many customers are there?

## Number of unique customers
amazon |> 
    select(customer_id) |> 
    distinct()


################################################################################
## What kinds of customers are there?

## Who has reviewed the most digital software?
amazon |> 
    group_by(customer_id) |> 
    summarize(n = n()) |> 
    arrange(desc(n))

## Look at the distribution of products reviewed count
amazon |> 
    group_by(customer_id) |> 
    summarize(n = n()) |> 
    ggplot(aes(x = n)) +
    geom_bar() +
    labs(x = "Number of products reviewed",
         y = "Number of customers")

## How many products do people review, on average?
amazon |> 
    group_by(customer_id) |> 
    summarize(n = n()) |> 
    summarize(avg = mean(n))
    

## Who reviewed the most products?
amazon |> 
    group_by(customer_id) |> 
    summarize(n = n()) |> 
    arrange(desc(n))

## Look at reviews from #1 reviewer
amazon |> 
    filter(customer_id == "17957446") 

amazon |> 
    filter(customer_id == "17957446") |> 
    select(product_title, review_date) |> 
    arrange(review_date)



## Select fewer columns
amazon |> 
    filter(customer_id == "17957446") |> 
    select(product_id, rating, product_title, review_date)


## What are the most popular products?
amazon |> 
    group_by(product_title) |> 
    summarize(n = n()) |> 
    arrange(desc(n))


################################################################################
## How many reviews have votes? How many are helpful?

glimpse(amazon)

amazon |> 
    filter(!is.na(total_votes)) |> 
    summarize(
        total = sum(total_votes > 0) / n(),
        helpful = sum(helpful_votes > 0, na.rm=T) / n()
    )

amazon |> 
    ggplot(aes(x = total_votes)) + 
    geom_histogram(bins = 10)


################################################################################
## How have reviews of Digital Software changed over time?
##
## H1: Star ratings have increased over time (due to better management)
## H2: Star ratings are flat over time

## Star ratings over time
amazon |> 
    ggplot(aes(x = review_date, y = star_rating)) + 
    geom_point()

## Average daily star rating
amazon |> 
    group_by(review_date) |> 
    summarize(rating = mean(star_rating)) |> 
    ggplot(aes(x = review_date, y = rating)) + 
    geom_point()

## Number of reviews by day
amazon |> 
    group_by(review_date) |> 
    summarize(rating = mean(star_rating),
              n = n()) |> 
    ggplot(aes(x = review_date, y = n)) + 
    geom_point()


