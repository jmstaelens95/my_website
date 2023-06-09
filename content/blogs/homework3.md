---
title: "Money in UK politics"
date: "2022-06-08"
description: Introduction to data wrangling at scale
draft: no
image: markrutte.jpg
keywords: ''
slug: homework3
categories: null
---





# Money in UK politics

[The Westminster Accounts](https://news.sky.com/story/the-westminster-accounts-12786091), a recent collaboration between Sky News and Tortoise Media, examines the flow of money through UK politics. It does so by combining data from three key sources:

1.  [Register of Members' Financial Interests](https://www.parliament.uk/mps-lords-and-offices/standards-and-financial-interests/parliamentary-commissioner-for-standards/registers-of-interests/register-of-members-financial-interests/),
2.  [Electoral Commission records of donations to parties](http://search.electoralcommission.org.uk/English/Search/Donations), and
3.  [Register of All-Party Parliamentary Groups](https://www.parliament.uk/mps-lords-and-offices/standards-and-financial-interests/parliamentary-commissioner-for-standards/registers-of-interests/register-of-all-party-party-parliamentary-groups/).

You can [search and explore the results](https://news.sky.com/story/westminster-accounts-search-for-your-mp-or-enter-your-full-postcode-12771627) through the collaboration's interactive database. Simon Willison [has extracted a database](https://til.simonwillison.net/shot-scraper/scraping-flourish) and this is what we will be working with. If you want to read more about [the project's methodology](https://www.tortoisemedia.com/2023/01/08/the-westminster-accounts-methodology/).

## Open a connection to the database

The database made available by Simon Willison is an `SQLite` database


```r
sky_westminster <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = here::here("data", "sky-westminster-files.db")
)
```

How many tables does the database have?


```r
DBI::dbListTables(sky_westminster)
```

```
## [1] "appg_donations"  "appgs"           "member_appgs"    "members"        
## [5] "parties"         "party_donations" "payments"
```

```r
# 7 tables
```

# the database has 7 tables

## Which MP has received the most amount of money?


```r
# read payments table from SQL into R
payments_db <-  dplyr::tbl(sky_westminster, "payments")
# read members table from SQL into R
members_db <- dplyr::tbl(sky_westminster, "members")

# left join payments to members by matching member_id to id
left_join(payments_db, members_db, by=c("member_id" = "id")) %>% 
    # group by member id and name
    group_by(member_id, name) %>%
    # calculate sum of value per member_id and name
    summarize(total_amount_received = sum(value, na.rm = TRUE)) %>%
    # sort in descending order by total_amount_received
    arrange(desc(total_amount_received)) %>%
    # return top row only
    head(1)
```

```
## # Source:     SQL [1 x 3]
## # Database:   sqlite 3.41.2 [C:\Users\stael\OneDrive\Documenten\my_website\data\sky-westminster-files.db]
## # Groups:     member_id
## # Ordered by: desc(total_amount_received)
##   member_id name        total_amount_received
##   <chr>     <chr>                       <dbl>
## 1 m8        Theresa May              2809765.
```

```r
# Theresa May
```

You need to work with the `payments` and `members` tables and for now we just want the total among all years. To insert a new, blank chunk of code where you can write your beautiful code (and comments!), please use the following shortcut: `Ctrl + Alt + I` (Windows) or `cmd + option + I` (mac)

## Any `entity` that accounts for more than 5% of all donations?

Is there any `entity` whose donations account for more than 5% of the total payments given to MPs over the 2020-2022 interval? Who are they and who did they give money to?


```r
# left join payments to members by matching member_id to id
left_join(payments_db, members_db, by=c("member_id" = "id")) %>% 
    # group by entity
    group_by(entity, na.rm = TRUE) %>%
    # calculate sum of value per member_id and name
    summarize(total_amount_received = sum(value, na.rm = TRUE)) %>%
    # calculate prop of value donated by entity
    mutate(
        prop = total_amount_received / sum(total_amount_received), 
        na.rm = TRUE
      ) %>%
    # filter on entities with prop > 5%
    filter(prop > 0.05)
```

```
## # Source:   SQL [?? x 4]
## # Database: sqlite 3.41.2 [C:\Users\stael\OneDrive\Documenten\my_website\data\sky-westminster-files.db]
## # Groups:   entity
##    entity                                      na.rm total_amount_received  prop
##    <chr>                                       <int>                 <dbl> <dbl>
##  1 12 Property FE                                  1                 10000     1
##  2 1912 Club                                       1                  1800     1
##  3 39th Street Strategies LLC                      1                  2600     1
##  4 3V International                                1                  2000     1
##  5 5 Oceans Partnership                            1                  5000     1
##  6 5x15                                            1                   500     1
##  7 79 Borough Road (trading as 'The Ministry')     1                  3500     1
##  8 89UP                                            1                 10645     1
##  9 8hwe                                            1                  6000     1
## 10 97 Dining Club                                  1                 10000     1
## # ℹ more rows
```

```r
# withers LLP has donated more than 5% of all donations with a total of 1,81M donated
```

## Do `entity` donors give to a single party or not?

-   How many distinct entities who paid money to MPS are there?


```r
payments_db %>%
  # filter on payments > 0
  filter(value > 0) %>%
  # return all distinct entity names
  distinct(entity)
```

```
## # Source:   SQL [?? x 1]
## # Database: sqlite 3.41.2 [C:\Users\stael\OneDrive\Documenten\my_website\data\sky-westminster-files.db]
##    entity                                                                       
##    <chr>                                                                        
##  1 GUBA Foundation                                                              
##  2 Mahir Kilic                                                                  
##  3 People's National Party (PNP) Women's Movement                               
##  4 National Union of Rail, Maritime and Transport Workers (RMT)                 
##  5 The Football Association Premier League                                      
##  6 1912 Club                                                                    
##  7 97 Dining Club                                                               
##  8 Ministry of Foreign Affairs, Qatar                                           
##  9 Qatar Racing and Equestrian Club (Qatar Ministry of Sports and Culture) and …
## 10 Catalyst Presents Foundation                                                 
## # ℹ more rows
```

```r
# there are 1000 distinct entities
```

-   How many (as a number and %) donated to MPs belonging to a single party only?


```r
# determine list of MPs belonging to a single party only
members_db_sp <- members_db %>%
  # count number of rows (party_id's) found per member id
  count(id) %>%
  # only keep rows where count == 1
  filter(n == 1)
 
### we find that all MPs belong to a single party only. Consequently, all payments or 100% is donated to an MP belonging to a single party only.

# left join payments to members by matching member_id to id
left_join(payments_db, members_db, by=c("member_id" = "id")) %>% 
    # group by entity, party_id
    group_by(entity, party_id) %>%
    # count number of parties an entity donated to
    count(entity) %>%
    # create column that indicates if party has only donated to one party
    mutate(
        loyal_TF = ifelse(n==1, TRUE, FALSE)
      ) %>%
    # ungroup table
    ungroup() %>%
    # count observations per loyal_TF
    count(loyal_TF) %>%
    # calculate proportion
    mutate(prop = as.numeric(n) / sum(as.numeric(n)))
```

```
## # Source:   SQL [2 x 3]
## # Database: sqlite 3.41.2 [C:\Users\stael\OneDrive\Documenten\my_website\data\sky-westminster-files.db]
##   loyal_TF     n  prop
##      <int> <int> <dbl>
## 1        0   934 0.378
## 2        1  1535 0.622
```

```r
# 62% of entities is loyal to a single party
```

## Which party has raised the greatest amount of money in each of the years 2020-2022?

I would like you to write code that generates the following table.

<img src="../../images/total_donations_table.png" width="80%" style="display: block; margin: auto;" />


```r
library(stringr)

# left join payments to members by matching member_id to id
left_join(payments_db, members_db, by=c("member_id" = "id")) %>%
    mutate(year = sql("REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(SUBSTR(date, 1, 4), ' ', ''), '-', ''), '.', ''), '/', ''), ':', ''), '+', ''), '(', ''), ')', ''), '[', ''), ']', '')")) %>%
    group_by(year, party_id) %>%
    summarize(
      total_year_donations = sum(value, na.rm = TRUE)
    )
```

```
## # Source:   SQL [?? x 3]
## # Database: sqlite 3.41.2 [C:\Users\stael\OneDrive\Documenten\my_website\data\sky-westminster-files.db]
## # Groups:   year
##    year  party_id total_year_donations
##    <chr> <chr>                   <dbl>
##  1 Rece  p1                       394 
##  2 Rece  p1034                   1570 
##  3 Rece  p15                   977802.
##  4 Rece  p17                    11469.
##  5 Rece  p22                     1326.
##  6 Rece  p29                    86353.
##  7 Rece  p31                      320 
##  8 Rece  p4                   7945900.
##  9 Rece  p44                     9346.
## 10 Rece  p7                      3775 
## # ℹ more rows
```

... and then, based on this data, plot the following graph.

<img src="../../images/total_donations_graph.png" width="80%" style="display: block; margin: auto;" />

This uses the default ggplot colour pallete, as I dont want you to worry about using the [official colours for each party](https://en.wikipedia.org/wiki/Wikipedia:Index_of_United_Kingdom_political_parties_meta_attributes). However, I would like you to ensure the parties are sorted according to total donations and not alphabetically. You may even want to remove some of the smaller parties that hardly register on the graph. Would facetting help you?

Finally, when you are done working with the databse, make sure you close the connection, or disconnect from the database.


```r
dbDisconnect(sky_westminster)
```
