---
categories:
- ""
- ""
date: "2022-06-08"
description: Introduction to data manipulation
draft: false
image: baby.webp
keywords: ""
slug: babynames
title: Babynames
---




```r
library(babynames)
library(mosaic)
library(tidyverse)
```

Each year, the US Social Security Administration publishes a list of the most popular names given to babies.  In 2018, [the list](http://www.ssa.gov/oact/babynames/#ht=2) shows Emma and Olivia leading for girls, Liam and Noah for boys.

The `babynames` data table in the `babynames` package comes from the Social Security Administration's listing of the names givens to babies in each year, and the number of babies of each sex given that name. (Only names with 5 or more babies are published by the SSA.)


## 1. How many babies are represented?


```r
babynames %>%
  summarise(total = sum(n)) # a reduction verb
```

```
## # A tibble: 1 × 1
##       total
##       <int>
## 1 348120517
```

## 2. How many babies are there in each year?


```r
babynames %>% 
  group_by(year) %>% 
  summarise(total= sum(n))
```

```
## # A tibble: 138 × 2
##     year  total
##    <dbl>  <int>
##  1  1880 201484
##  2  1881 192696
##  3  1882 221533
##  4  1883 216946
##  5  1884 243462
##  6  1885 240854
##  7  1886 255317
##  8  1887 247394
##  9  1888 299473
## 10  1889 288946
## # ℹ 128 more rows
```
Can you plot the total number of names (and hence births) by year?

## 3. How many distinct names in each year?


```r
babynames %>%
  group_by(year) %>%
  summarise(name_count = n_distinct(name))
```

```
## # A tibble: 138 × 2
##     year name_count
##    <dbl>      <int>
##  1  1880       1889
##  2  1881       1830
##  3  1882       2012
##  4  1883       1962
##  5  1884       2158
##  6  1885       2139
##  7  1886       2225
##  8  1887       2215
##  9  1888       2454
## 10  1889       2390
## # ℹ 128 more rows
```

## 4. How many distinct names of each sex in each year?


```r
babynames %>%
  group_by(year, sex) %>%
  summarise(name_count = n_distinct(name)) 
```

```
## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 276 × 3
## # Groups:   year [138]
##     year sex   name_count
##    <dbl> <chr>      <int>
##  1  1880 F            942
##  2  1880 M           1058
##  3  1881 F            938
##  4  1881 M            997
##  5  1882 F           1028
##  6  1882 M           1099
##  7  1883 F           1054
##  8  1883 M           1030
##  9  1884 F           1172
## 10  1884 M           1125
## # ℹ 266 more rows
```

Do boys or girls have similar number of distinct names? Plot `name_count` by `year`, coloured by `sex`

# Popularity of Jane and Mary

## 5. Track the yearly number of Janes and Marys over the years.


```r
Result <-
  babynames %>%
  filter(name %in% c("Jane", "Mary")) %>% # just the Janes and Marys
  group_by(year, name) %>% # for each year for each name
  summarise(count = sum(n))
```

```
## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.
```

### 6. Plot out the result

Put `year` on the x-axis and the count of each name on the y-axis.  
* *Map* the name (Mary or Jane) to the aesthetic of color.  Remember that mapping to aesthetics is always done inside the `aes()` function.
* Instead of using dots as the glyph, use a line that connects consecutive values: `geom_line()`.
* Change the y-axis label to "Yearly Births": `+ ylab("Yearly Births")`
* *Set* the line thickness to `size=2`. Remember that "setting" refers to adjusting the value of an aesthetic to a constant.  Thus, it's *outside* the `aes()` function.
* Add a vertical line to mark a year in which something happened that might relate to the increase or decrease the popularity of the name.  Example: The movie [*Whatever Happened to Baby Jane*](http://en.wikipedia.org/wiki/What_Ever_Happened_to_Baby_Jane%3F_%281962_film%29) came out in 1962.  The glyph is a vertical line: `geom_vline(xintercept=1962)`.


```r
ggplot(Result, aes(x= year, y=count,colour = name)) +
  geom_line(size = 2) +
  labs(
    title = "Names over time", 
    x="Year", 
    y= "Yearly Births") +
  geom_vline(xintercept=1962)
```

```
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

<img src="/blogs/dplyr_babynames_files/figure-html/unnamed-chunk-7-1.png" width="672" />



## 7. Look at the *proportion* of births rather than the count


```r
Result2 <-
  babynames %>%
  group_by(year) %>%
  mutate(total = sum(n)) %>%
  filter(name %in% c("Mary", "Jane")) %>%
  mutate(proportion = n / total)

head(Result2)
```

```
## # A tibble: 6 × 7
## # Groups:   year [2]
##    year sex   name      n     prop  total proportion
##   <dbl> <chr> <chr> <int>    <dbl>  <int>      <dbl>
## 1  1880 F     Mary   7065 0.0724   201484   0.0351  
## 2  1880 F     Jane    215 0.00220  201484   0.00107 
## 3  1880 M     Mary     27 0.000228 201484   0.000134
## 4  1881 F     Mary   6919 0.0700   192696   0.0359  
## 5  1881 F     Jane    216 0.00219  192696   0.00112 
## 6  1881 M     Mary     29 0.000268 192696   0.000150
```

Why is sex a variable in Result2? Eliminate it, keeping just the girls.



```r
Result2 <-
  babynames %>%
  filter(sex == "F") %>%
  group_by(year) %>%
  mutate(total = sum(n)) %>%
  filter(name %in% c("Mary", "Jane")) %>%
  mutate(proportion = n / total)
```


Just as you did with count vs year, graph proportion vs year.

```r
ggplot(Result2, aes(x= year, y=proportion,colour = name)) +
   geom_line(size = 2) +
  labs(title = "Names over time", x="Year", y= "Yearly Births")+
  geom_vline(xintercept=1962)
```

  
## 8. Pick out name(s) of interest to you 

Plot out their popularity over time.
