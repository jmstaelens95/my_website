---
categories:  
- ""    #the front matter should be like the one found in, e.g., blog2.md. It cannot be like the normal Rmd we used
- ""
date: "2021-09-30"
description: Risk-Return of DJIA stocks # the title that will show up once someone gets to this page
draft: false
image: spices.jpg # save picture in \static\img\blogs. Acceptable formats= jpg, jpeg, or png . Your iPhone pics wont work

keywords: ""
slug: risk_return # slug is the shorthand URL address... no spaces plz
title: Risk-Return of DJIA stocks
---






```r
# add a nice tulip picture
knitr::include_graphics(here::here("static/img/pic1.jpg"))
```

<img src="../../static/img/pic1.jpg" style="display: block; margin: auto;" />

```r
# read and show brexit results
brexit_results <- read_csv(here::here("data","brexit_results.csv"))
glimpse(brexit_results)
```

```
## Rows: 632
## Columns: 11
## $ Seat        <chr> "Aldershot", "Aldridge-Brownhills", "Altrincham and Sale W…
## $ con_2015    <dbl> 50.6, 52.0, 53.0, 44.0, 60.8, 22.4, 52.5, 22.1, 50.7, 53.0…
## $ lab_2015    <dbl> 18.3, 22.4, 26.7, 34.8, 11.2, 41.0, 18.4, 49.8, 15.1, 21.3…
## $ ld_2015     <dbl> 8.82, 3.37, 8.38, 2.98, 7.19, 14.83, 5.98, 2.42, 10.62, 5.…
## $ ukip_2015   <dbl> 17.87, 19.62, 8.01, 15.89, 14.44, 21.41, 18.82, 21.76, 19.…
## $ leave_share <dbl> 57.9, 67.8, 38.6, 65.3, 49.7, 70.5, 59.9, 61.8, 51.8, 50.3…
## $ born_in_uk  <dbl> 83.1, 96.1, 90.5, 97.3, 93.3, 97.0, 90.5, 90.7, 87.0, 88.8…
## $ male        <dbl> 49.9, 48.9, 48.9, 49.2, 48.0, 49.2, 48.5, 49.2, 49.5, 49.5…
## $ unemployed  <dbl> 3.64, 4.55, 3.04, 4.26, 2.47, 4.74, 3.69, 5.11, 3.39, 2.93…
## $ degree      <dbl> 13.87, 9.97, 28.60, 9.34, 18.78, 6.09, 13.12, 7.90, 17.80,…
## $ age_18to24  <dbl> 9.41, 7.33, 6.44, 7.75, 5.73, 8.21, 7.82, 8.94, 7.56, 7.61…
```

# Returns of financial stocks

Next, let's choose the [Dow Jones Industrial Average (DJIA)](https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average) stocks and their ticker symbols and download some data. Besides the thirty stocks that make up the DJIA, we will also add `SPY` which is an SP500 ETF (Exchange Traded Fund).

We will use the `rvest` package to scrape the Wikipedia page for the constituents of DJIA


```r
djia_url <- "https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average"


#get tables that exist on URL
tables <- djia_url %>% 
  read_html() %>% 
  html_nodes(css="table")


# parse HTML tables into a dataframe called djia. 
# Use purr::map() to create a list of all tables in URL
djia <- map(tables, . %>% 
               html_table(fill=TRUE)%>% 
               clean_names())


# constituents
table1 <- djia[[2]] %>% # the second table on the page contains the ticker symbols
  mutate(date_added = ymd(date_added),
         
         # if a stock is listed on NYSE, its symbol is, e.g., NYSE: MMM
         # We will get prices from yahoo finance which requires just the ticker
         
         # if symbol contains "NYSE*", the * being a wildcard
         # then we jsut drop the first 6 characters in that string
         ticker = ifelse(str_detect(symbol, "NYSE*"),
                          str_sub(symbol,7,11),
                          symbol)
         )

# we need a vector of strings with just the 30 tickers + SPY  + VIX
tickers <- table1 %>% 
  select(ticker) %>% 
  pull() %>% # pull() gets them as a sting of characters
  c("SPY", "^VIX") # and lets us add SPY, the SP500 ETF, and the VIX index
```


```r
# Notice the cache=TRUE argument in the chunk options. Because getting data is time consuming, # cache=TRUE means that once it downloads data, the chunk will not run again next time you knit your Rmd

myStocks <- tickers %>% 
  tq_get(get  = "stock.prices",
         from = "2000-01-01") %>%
  group_by(symbol) 

glimpse(myStocks) # examine the structure of the resulting data frame
```

```
## Rows: 180,777
## Columns: 8
## Groups: symbol [32]
## $ symbol   <chr> "MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM", "MMM"…
## $ date     <date> 2000-01-03, 2000-01-04, 2000-01-05, 2000-01-06, 2000-01-07, …
## $ open     <dbl> 48.0, 46.4, 45.6, 47.2, 50.6, 50.2, 50.4, 51.0, 50.7, 50.4, 4…
## $ high     <dbl> 48.2, 47.4, 48.1, 51.2, 51.9, 51.8, 51.2, 51.8, 50.9, 50.5, 4…
## $ low      <dbl> 47.0, 45.3, 45.6, 47.2, 50.0, 50.0, 50.2, 50.4, 50.2, 49.5, 4…
## $ close    <dbl> 47.2, 45.3, 46.6, 50.4, 51.4, 51.1, 50.2, 50.4, 50.4, 49.7, 4…
## $ volume   <dbl> 2173400, 2713800, 3699400, 5975800, 4101200, 3863800, 2357600…
## $ adjusted <dbl> 25.1, 24.1, 24.8, 26.8, 27.4, 27.2, 26.8, 26.8, 26.8, 26.5, 2…
```

Financial performance analysis depend on returns; If I buy a stock today for 100 and I sell it tomorrow for 101.75, my one-day return, assuming no transaction costs, is 1.75%. So given the adjusted closing prices, our first step is to calculate daily and monthly returns.


```r
#calculate daily returns
myStocks_returns_daily <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "daily_returns",
               cols = c(nested.col))  

#calculate monthly  returns
myStocks_returns_monthly <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "monthly_returns",
               cols = c(nested.col)) 
```


```r
#visualise monthly returns since 2010, for each of the 30 DJIA stocks
myStocks_returns_monthly %>% 
  filter(symbol != "^VIX", symbol != "SPY") %>% 
  filter(date >= "2010-01-01") %>% 
  ggplot(aes(x = monthly_returns)) +
  geom_density(aes(colour = symbol), alpha = 1) +
  geom_histogram(aes(fill = symbol), alpha = 0.4, binwidth = 0.005)+
  facet_wrap(~symbol, nrow=7)+
  theme_bw(8)+
  theme(legend.position = "none") +
  scale_x_continuous(labels = scales::percent) +
  labs(
    title = "Distribution of monthly returns for DJIA stocks",
    subtitle = "Jan 2010 - now",
    x = "Monthly returns (%)",
    y = "" )+
  NULL
```

<img src="/blogs/risk_return_files/figure-html/unnamed-chunk-2-1.png" width="648" style="display: block; margin: auto;" />

<img src="/blogs/risk_return_files/figure-html/risk_return-1.png" width="648" style="display: block; margin: auto;" />
