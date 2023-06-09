---
title: "Credit card fraud"
date: "2022-06-08"
description: Predicting credit card fraud
draft: no
image: kindersdijk.jpeg
keywords: ''
slug: groupwork
categories: null
---



# The problem: predicting credit card fraud

The goal of the project is to predict fraudulent credit card transactions.

We will be using a dataset with credit card transactions containing legitimate and fraud transactions. Fraud is typically well below 1% of all transactions, so a naive model that predicts that all transactions are legitimate and not fraudulent would have an accuracy of well over 99%-- pretty good, no?

You can read more on credit card fraud on [Credit Card Fraud Detection Using Weighted Support Vector Machine](https://www.scirp.org/journal/paperinformation.aspx?paperid=105944)

The dataset we will use consists of credit card transactions and it includes information about each transaction including customer details, the merchant and category of purchase, and whether or not the transaction was a fraud.

## Obtain the data

The dataset is too large to be hosted on Canvas or Github, so please download it from dropbox <https://www.dropbox.com/sh/q1yk8mmnbbrzavl/AAAxzRtIhag9Nc_hODafGV2ka?dl=0> and save it in your `dsb` repo, under the `data` folder.

As we will be building a classifier model using tidymodels, there's two things we need to do:

1.  Define the outcome variable `is_fraud` as a factor, or categorical, variable, instead of the numerical 0-1 varaibles.
2.  In tidymodels, the first level is the event of interest. If we leave our data as is, `0` is the first level, but we want to find out when we actually did (`1`) have a fraudulent transaction


```
## Rows: 671,028
## Columns: 14
## $ trans_date_trans_time <dttm> 2019-02-22 07:32:58, 2019-02-16 15:07:20, 2019-…
## $ trans_year            <dbl> 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2020, …
## $ category              <chr> "entertainment", "kids_pets", "personal_care", "…
## $ amt                   <dbl> 7.79, 3.89, 8.43, 40.00, 54.04, 95.61, 64.95, 3.…
## $ city                  <chr> "Veedersburg", "Holloway", "Arnold", "Apison", "…
## $ state                 <chr> "IN", "OH", "MO", "TN", "CO", "GA", "MN", "AL", …
## $ lat                   <dbl> 40.1, 40.0, 38.4, 35.0, 39.5, 32.8, 48.6, 34.6, …
## $ long                  <dbl> -87.3, -81.0, -90.4, -85.0, -106.4, -83.2, -93.3…
## $ city_pop              <dbl> 4049, 128, 35439, 3730, 277, 1841, 136, 190178, …
## $ job                   <chr> "Development worker, community", "Child psychoth…
## $ dob                   <date> 1959-10-19, 1946-04-03, 1985-03-31, 1991-01-28,…
## $ merch_lat             <dbl> 39.4, 39.7, 37.7, 34.5, 40.0, 32.9, 48.6, 35.1, …
## $ merch_long            <dbl> -87.5, -81.5, -91.4, -84.1, -106.9, -82.2, -94.1…
## $ is_fraud              <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
```

The data dictionary is as follows

| column(variable)      | description                                 |
|-----------------------|---------------------------------------------|
| trans_date_trans_time | Transaction DateTime                        |
| trans_year            | Transaction year                            |
| category              | category of merchant                        |
| amt                   | amount of transaction                       |
| city                  | City of card holder                         |
| state                 | State of card holder                        |
| lat                   | Latitude location of purchase               |
| long                  | Longitude location of purchase              |
| city_pop              | card holder's city population               |
| job                   | job of card holder                          |
| dob                   | date of birth of card holder                |
| merch_lat             | Latitude Location of Merchant               |
| merch_long            | Longitude Location of Merchant              |
| is_fraud              | Whether Transaction is Fraud (1) or Not (0) |

We also add some of the variables we considered in our EDA for this dataset during homework 2.


```r
library(lubridate)

card_fraud <- card_fraud %>% 
  mutate( hour = hour(trans_date_trans_time),
          wday = wday(trans_date_trans_time, label = TRUE),
          month_name = month(trans_date_trans_time, label = TRUE),
          age = interval(dob, trans_date_trans_time) / years(1)
) %>% 
  rename(year = trans_year) %>% 
  
  mutate(
    
    # convert latitude/longitude to radians
    lat1_radians = lat / 57.29577951,
    lat2_radians = merch_lat / 57.29577951,
    long1_radians = long / 57.29577951,
    long2_radians = merch_long / 57.29577951,
    
    # calculate distance in miles
    distance_miles = 3963.0 * acos((sin(lat1_radians) * sin(lat2_radians)) + cos(lat1_radians) * cos(lat2_radians) * cos(long2_radians - long1_radians)),

    # calculate distance in km
    distance_km = 6377.830272 * acos((sin(lat1_radians) * sin(lat2_radians)) + cos(lat1_radians) * cos(lat2_radians) * cos(long2_radians - long1_radians))

  )
```

## Exploratory Data Analysis (EDA)

You have done some EDA and you can pool together your group's expertise in which variables to use as features. You can reuse your EDA from earlier, but we expect at least a few visualisations and/or tables to explore the dataset and identify any useful features.


```r
card_fraud %>%
  # only keep fraudulent transactions
  filter(is_fraud==1) %>%
  # count number of (fraudulent) transactions per category
  count(category, sort = TRUE) %>%
  mutate(
    # calculate prop as number of fraudulent transactions per catogory divided by total number of fraudulent transactions
    prop = n/sum(n),
    # order categories by number of fraudulent transactionss
    category = fct_reorder(category, prop)
    ) %>%
  # plot col chart
  ggplot(aes(x=prop,y=category)) +
    geom_col() +
    scale_x_continuous(labels = scales::percent)
```

<img src="/blogs/final_group_project_17_sodedit_files/figure-html/unnamed-chunk-4-1.png" width="648" style="display: block; margin: auto;" />


```r
# when is fraud more prevalent?
card_fraud %>%
  # count number of obs per weekday
  count(wday, sort=TRUE) %>%
  # calculate prop as number of obs per group / total number obs
  mutate(
    prop = n/sum(n)
    ) %>%
  ggplot(aes(x=wday,y=prop)) +
    geom_col()
```

<img src="/blogs/final_group_project_17_sodedit_files/figure-html/unnamed-chunk-5-1.png" width="648" style="display: block; margin: auto;" />


```r
card_fraud %>%
  # count number of observations per month_name
  count(month_name, sort=TRUE) %>%
  # calculate prop as number of obs per group / total number obs
  mutate(
    prop = n/sum(n)
    ) %>%
  # plot column chart
  ggplot(aes(x=month_name,y=prop)) +
    geom_col()
```

<img src="/blogs/final_group_project_17_sodedit_files/figure-html/unnamed-chunk-6-1.png" width="648" style="display: block; margin: auto;" />


```r
card_fraud %>%
  # remove transactions with no hour
  filter(is.na(hour)==FALSE) %>%
  # count number of obs per hour value
  count(hour, sort=TRUE) %>%
  # calculate prop as number of obs per group / total number obs
  mutate(
    prop = n/sum(n)
    ) %>%
  # plot column chart
  ggplot(aes(x=hour,y=prop)) +
    geom_col()
```

<img src="/blogs/final_group_project_17_sodedit_files/figure-html/unnamed-chunk-7-1.png" width="648" style="display: block; margin: auto;" />


```r
# are elders more prone to credit card fraud?
card_fraud %>%
  # remove NA values for is_fraud
  filter(is.na(is_fraud)==0) %>%
  mutate(
   age = interval(dob, trans_date_trans_time) / years(1),
    ) %>%
  # plot geom historgram wrapped by is_fraud value, free y scale
  ggplot(aes(x=age)) +
    geom_histogram() +
    facet_wrap(~is_fraud, scales="free_y")
```

<img src="/blogs/final_group_project_17_sodedit_files/figure-html/unnamed-chunk-8-1.png" width="648" style="display: block; margin: auto;" />


```r
# examine relationship distance_km and probability of fraud
card_fraud %>%
  ggplot(aes(
    y=distance_km, 
    x = is_fraud, 
    fill= as.factor(is_fraud), 
    group = as.factor(is_fraud))
    ) +
    geom_boxplot()
```

<img src="/blogs/final_group_project_17_sodedit_files/figure-html/unnamed-chunk-9-1.png" width="648" style="display: block; margin: auto;" />

Group all variables by type and examine each variable class by class. The dataset has the following types of variables:

1.  Strings
2.  Geospatial Data
3.  Dates
4.  Date/Times
5.  Numerical

Strings are usually not a useful format for classification problems. The strings should be converted to factors, dropped, or otherwise transformed.

***Strings to Factors***

-   `category`, Category of Merchant
-   `job`, Job of Credit Card Holder

***Strings to Geospatial Data***

We have plenty of geospatial data as lat/long pairs, so I want to convert city/state to lat/long so I can compare to the other geospatial variables. This will also make it easier to compute new variables like the distance the transaction is from the home location.

-   `city`, City of Credit Card Holder
-   `state`, State of Credit Card Holder

## Exploring factors: how is the compactness of categories?

-   Do we have excessive number of categories? Do we want to combine some?


```r
card_fraud %>% 
  count(category, sort=TRUE)%>% 
  mutate(perc = n/sum(n))
```

```
## # A tibble: 14 × 3
##    category           n   perc
##    <chr>          <int>  <dbl>
##  1 gas_transport  68046 0.101 
##  2 grocery_pos    63791 0.0951
##  3 home           63597 0.0948
##  4 shopping_pos   60416 0.0900
##  5 kids_pets      58772 0.0876
##  6 shopping_net   50743 0.0756
##  7 entertainment  48521 0.0723
##  8 food_dining    47527 0.0708
##  9 personal_care  46843 0.0698
## 10 health_fitness 44341 0.0661
## 11 misc_pos       41244 0.0615
## 12 misc_net       32829 0.0489
## 13 grocery_net    23485 0.0350
## 14 travel         20873 0.0311
```

```r
card_fraud %>% 
  count(job, sort=TRUE) %>% 
  mutate(perc = n/sum(n))
```

```
## # A tibble: 494 × 3
##    job                            n    perc
##    <chr>                      <int>   <dbl>
##  1 Film/video editor           5106 0.00761
##  2 Exhibition designer         4728 0.00705
##  3 Naval architect             4546 0.00677
##  4 Surveyor, land/geomatics    4448 0.00663
##  5 Materials engineer          4292 0.00640
##  6 Designer, ceramics/pottery  4262 0.00635
##  7 IT trainer                  4014 0.00598
##  8 Financial adviser           3959 0.00590
##  9 Systems developer           3948 0.00588
## 10 Environmental consultant    3831 0.00571
## # ℹ 484 more rows
```

The predictors `category` and `job` are transformed into factors.


```r
card_fraud <- card_fraud %>% 
  mutate(category = factor(category),
         job = factor(job))
```

`category` has 14 unique values, and `job` has 494 unique values. The dataset is quite large, with over 670K records, so these variables don't have an excessive number of levels at first glance. However, it is worth seeing if we can compact the levels to a smaller number.

### Why do we care about the number of categories and whether they are "excessive"?

Consider the extreme case where a dataset had categories that only contained one record each. There is simply insufficient data to make correct predictions using category as a predictor on new data with that category label. Additionally, if your modeling uses dummy variables, having an extremely large number of categories will lead to the production of a huge number of predictors, which can slow down the fitting. This is fine if all the predictors are useful, but if they aren't useful (as in the case of having only one record for a category), trimming them will improve the speed and quality of the data fitting.

If I had subject matter expertise, I could manually combine categories. If you don't have subject matter expertise, or if performing this task would be too labor intensive, then you can use cutoffs based on the amount of data in a category. If the majority of the data exists in only a few categories, then it might be reasonable to keep those categories and lump everything else in an "other" category or perhaps even drop the data points in smaller categories.

## Do all variables have sensible types?

Consider each variable and decide whether to keep, transform, or drop it. This is a mixture of Exploratory Data Analysis and Feature Engineering, but it's helpful to do some simple feature engineering as you explore the data. In this project, we have all data to begin with, so any transformations will be performed on the entire dataset. Ideally, do the transformations as a `recipe_step()` in the tidymodels framework. Then the transformations would be applied to any data the recipe was used on as part of the modeling workflow. There is less chance of data leakage or missing a step when you perform the feature engineering in the recipe.

## Which variables to keep in your model?

You have a number of variables and you have to decide which ones to use in your model. For instance, you have the latitude/lognitude of the customer, that of the merchant, the same data in radians, as well as the `distance_km` and `distance_miles`. Do you need them all?

## Fit your workflows in smaller sample

You will be running a series of different models, along the lines of the California housing example we have seen in class. However, this dataset has 670K rows and if you try various models and run cross validation on them, your computer may slow down or crash.

Thus, we will work with a smaller sample of 10% of the values the original dataset to identify the best model, and once we have the best model we can use the full dataset to train- test our best model.


```r
# select a smaller subset
my_card_fraud <- card_fraud %>% 
  # select a smaller subset, 10% of the entire dataframe 
  slice_sample(prop = 0.10) 
```

## Split the data in training - testing


```r
# **Split the data**

library(rsample)

set.seed(123)

data_split <- initial_split(my_card_fraud, # updated data
                           prop = 0.8, 
                           strata = is_fraud)

card_fraud_train <- training(data_split) 
card_fraud_test <- testing(data_split)
```

## Cross Validation

Start with 3 CV folds to quickly get an estimate for the best model and you can increase the number of folds to 5 or 10 later.


```r
set.seed(123)
cv_folds <- vfold_cv(data = card_fraud_train, 
                          v = 3, 
                          strata = is_fraud)
cv_folds 
```

```
## #  3-fold cross-validation using stratification 
## # A tibble: 3 × 2
##   splits                id   
##   <list>                <chr>
## 1 <split [35787/17894]> Fold1
## 2 <split [35787/17894]> Fold2
## 3 <split [35788/17893]> Fold3
```

## Define a tidymodels `recipe`

What steps are you going to add to your recipe? Do you need to do any log transformations?


```r
library(tidyverse)
library(tidymodels)
library(GGally)
library(sf)
library(leaflet)
library(janitor)
library(rpart.plot)
library(gt)
library(DataExplorer)

fraud_rec <- recipe(
    
    # define outcome variable
    is_fraud ~ 
      
    # define predictor variables
    distance_km + age + hour + wday + month_name + category + amt, 
    
    # define dataset
    data = card_fraud_train
  ) %>%
  
  #update_role(long, lat, 
  #            new_role = "ID") %>% 
  
  # log transform variables which are highly skewed
  step_log(
    amt,
    distance_km
    ) %>% 
  
  # apply tidying steps to the recipe
  step_naomit(everything(), skip = TRUE) %>% 
  step_novel(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_numeric(), -all_outcomes()) %>%
  step_corr(all_predictors(), threshold = 0.7, method = "spearman") 
```

Once you have your recipe, you can check the pre-processed dataframe


```r
prepped_data <- 
  fraud_rec %>% # use the recipe object
  prep() %>% # perform the recipe on training data
  juice() # extract only the preprocessed dataframe 

glimpse(prepped_data)
```

```
## Rows: 53,681
## Columns: 37
## $ distance_km             <dbl> -0.299, -0.327, 1.035, -0.315, 0.914, 0.540, -…
## $ age                     <dbl> 0.18926, -0.88508, -0.90448, -0.19386, 1.57208…
## $ hour                    <dbl> -1.422, -0.106, 1.502, 1.502, -1.130, -0.545, …
## $ amt                     <dbl> -0.966, 0.579, -0.750, -2.397, 0.726, -0.192, …
## $ is_fraud                <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0…
## $ wday_1                  <dbl> -0.5401, -0.5401, -0.2315, -0.5401, -0.2315, 0…
## $ wday_2                  <dbl> 0.5401, 0.5401, -0.2315, 0.5401, -0.2315, 0.07…
## $ wday_3                  <dbl> -0.431, -0.431, 0.431, -0.431, 0.431, -0.308, …
## $ wday_4                  <dbl> 0.282, 0.282, -0.121, 0.282, -0.121, -0.524, -…
## $ wday_5                  <dbl> -0.150, -0.150, -0.364, -0.150, -0.364, -0.492…
## $ wday_6                  <dbl> 0.0615, 0.0615, 0.5539, 0.0615, 0.5539, -0.307…
## $ wday_7                  <dbl> -0.0171, -0.0171, -0.3585, -0.0171, -0.3585, -…
## $ month_name_01           <dbl> 7.41e-02, -1.48e-01, -1.48e-01, -2.22e-01, 2.9…
## $ month_name_02           <dbl> -0.2905, -0.2235, -0.2235, -0.1117, 0.0447, 0.…
## $ month_name_03           <dbl> -1.67e-01, 2.93e-01, 2.93e-01, 3.34e-01, -2.51…
## $ month_name_04           <dbl> 0.2453, 0.0422, 0.0422, -0.2070, -0.3680, -0.3…
## $ month_name_05           <dbl> 2.54e-01, -3.31e-01, -3.31e-01, -1.40e-01, -2.…
## $ month_name_06           <dbl> -0.1678, 0.1845, 0.1845, 0.3607, 0.0671, 0.067…
## $ month_name_07           <dbl> -3.29e-01, 2.14e-01, 2.14e-01, -2.47e-01, 3.39…
## $ month_name_08           <dbl> 0.0513, -0.3640, -0.3640, -0.0974, 0.4563, 0.4…
## $ month_name_09           <dbl> 3.81e-01, 7.63e-02, 7.63e-02, 3.94e-01, 4.07e-…
## $ month_name_10           <dbl> 0.1255, 0.3379, 0.3379, -0.4585, 0.2671, 0.267…
## $ month_name_11           <dbl> -3.85e-01, -4.81e-01, -4.81e-01, 3.21e-01, 1.2…
## $ month_name_12           <dbl> -0.4816, 0.3010, 0.3010, -0.1338, 0.0401, 0.04…
## $ category_food_dining    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ category_gas_transport  <dbl> 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0…
## $ category_grocery_net    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ category_grocery_pos    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1…
## $ category_health_fitness <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ category_home           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ category_kids_pets      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0…
## $ category_misc_net       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0…
## $ category_misc_pos       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0…
## $ category_personal_care  <dbl> 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ category_shopping_net   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0…
## $ category_shopping_pos   <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0…
## $ category_travel         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
```

## Define various models

You should define the following classification models:

1.  Logistic regression, using the `glm` engine
2.  Decision tree, using the `C5.0` engine
3.  Random Forest, using the `ranger` engine and setting `importance = "impurity"`)
4.  A boosted tree using Extreme Gradient Boosting, and the `xgboost` engine
5.  A k-nearest neighbours, using 4 nearest_neighbors and the `kknn` engine


```r
## Model Building 

# 1. Pick a `model type`
# 2. set the `engine`
# 3. Set the `mode`:  classification

# brackets aroud the assignment assigns and prints

# Logistic regression
(
  log_spec <-
    logistic_reg() %>%  # model type
    set_engine(engine = "glm") %>%  # model engine
    set_mode("classification") # model mode
)
```

```
## Logistic Regression Model Specification (classification)
## 
## Computational engine: glm
```

```r
# Decision Tree
(
  tree_spec <-
    decision_tree() %>%
    set_engine(engine = "C5.0") %>%
    set_mode("classification")
)
```

```
## Decision Tree Model Specification (classification)
## 
## Computational engine: C5.0
```

```r
# Random Forest
library(ranger)

(
  rf_spec <-
    rand_forest() %>%
    set_engine("ranger", importance = "impurity") %>%
    set_mode("classification")
)
```

```
## Random Forest Model Specification (classification)
## 
## Engine-Specific Arguments:
##   importance = impurity
## 
## Computational engine: ranger
```

```r
# Boosted tree (XGBoost)
library(xgboost)

(
  xgb_spec <-
    boost_tree() %>%
    set_engine("xgboost") %>%
    set_mode("classification") 
)
```

```
## Boosted Tree Model Specification (classification)
## 
## Computational engine: xgboost
```

```r
# K-nearest neighbour (k-NN)
(
  knn_spec <-
    nearest_neighbor(neighbors = 4) %>% # we can adjust the number of neighbors 
    set_engine("kknn") %>%
    set_mode("classification") 
)
```

```
## K-Nearest Neighbor Model Specification (classification)
## 
## Main Arguments:
##   neighbors = 4
## 
## Computational engine: kknn
```

## Bundle recipe and model with `workflows`


```r
## Bundle recipe and model with `workflows`

(
log_wflow <- # log workflow object
 workflow() %>% # use workflow function
 add_recipe(fraud_rec) %>%   # use the new recipe
 add_model(log_spec) # add model spec
)
```

```
## ══ Workflow ════════════════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: logistic_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 7 Recipe Steps
## 
## • step_log()
## • step_naomit()
## • step_novel()
## • step_normalize()
## • step_dummy()
## • step_zv()
## • step_corr()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Logistic Regression Model Specification (classification)
## 
## Computational engine: glm
```

```r
(
tree_wflow <- # log workflow object
 workflow() %>% # use workflow function
 add_recipe(fraud_rec) %>%   # use the new recipe
 add_model(tree_spec) # add model spec
)
```

```
## ══ Workflow ════════════════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: decision_tree()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 7 Recipe Steps
## 
## • step_log()
## • step_naomit()
## • step_novel()
## • step_normalize()
## • step_dummy()
## • step_zv()
## • step_corr()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Decision Tree Model Specification (classification)
## 
## Computational engine: C5.0
```

```r
(
rf_wflow <- # log workflow object
 workflow() %>% # use workflow function
 add_recipe(fraud_rec) %>%   # use the new recipe
 add_model(rf_spec) # add model spec
)
```

```
## ══ Workflow ════════════════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: rand_forest()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 7 Recipe Steps
## 
## • step_log()
## • step_naomit()
## • step_novel()
## • step_normalize()
## • step_dummy()
## • step_zv()
## • step_corr()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Random Forest Model Specification (classification)
## 
## Engine-Specific Arguments:
##   importance = impurity
## 
## Computational engine: ranger
```

```r
(
xgb_wflow <- # log workflow object
 workflow() %>% # use workflow function
 add_recipe(fraud_rec) %>%   # use the new recipe
 add_model(xgb_spec) # add model spec
)
```

```
## ══ Workflow ════════════════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: boost_tree()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 7 Recipe Steps
## 
## • step_log()
## • step_naomit()
## • step_novel()
## • step_normalize()
## • step_dummy()
## • step_zv()
## • step_corr()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Boosted Tree Model Specification (classification)
## 
## Computational engine: xgboost
```

```r
(
knn_wflow <- # log workflow object
 workflow() %>% # use workflow function
 add_recipe(fraud_rec) %>%   # use the new recipe
 add_model(knn_spec) # add model spec
)
```

```
## ══ Workflow ════════════════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: nearest_neighbor()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 7 Recipe Steps
## 
## • step_log()
## • step_naomit()
## • step_novel()
## • step_normalize()
## • step_dummy()
## • step_zv()
## • step_corr()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## K-Nearest Neighbor Model Specification (classification)
## 
## Main Arguments:
##   neighbors = 4
## 
## Computational engine: kknn
```

## Fit models

You may want to compare the time it takes to fit each model. `tic()` starts a simple timer and `toc()` stops it


```r
library(tictoc)

tic()
log_res <- log_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, accuracy,
      kap, roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)) 
time <- toc()
```

```
## 21.29 sec elapsed
```

```r
log_time <- time[[4]]

tic()
tree_res <- tree_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, accuracy,
      kap, roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)) 
time <- toc()
```

```
## 34.89 sec elapsed
```

```r
tree_time <- time[[4]]

tic()
rf_res <- rf_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, accuracy,
      kap, roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE))
time <- toc()
```

```
## 47.11 sec elapsed
```

```r
rf_time <- time[[4]]

tic()
xgb_res <- xgb_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, accuracy,
      kap, roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE))
time <- toc()
```

```
## 20 sec elapsed
```

```r
xgb_time <- time[[4]]

tic()
knn_res <- knn_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, accuracy,
      kap, roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE))
time <- toc()
```

```
## 196.69 sec elapsed
```

```r
knn_time <- time[[4]]
```

## Compare models


```r
## Model Comparison

log_metrics <- 
  log_res %>% 
    collect_metrics(summarise = TRUE) %>%
    
    # add the name of the model to every row
    mutate(
      model = "Logistic Regression",
      time = log_time
      )

# add mode models here
tree_metrics <- 
  tree_res %>% 
    collect_metrics(summarise = TRUE) %>%
    mutate(
      model = "Decision tree",
      time = tree_time
      )

rf_metrics <- 
  rf_res %>% 
    collect_metrics(summarise = TRUE) %>%
    mutate(
      model = "Random Forest",
      time = rf_time
      )

xgb_metrics <- 
  xgb_res %>% 
    collect_metrics(summarise = TRUE) %>%
    mutate(
      model = "Extreme Gradient Boosting",
      time = xgb_time
      )

knn_metrics <- 
  knn_res %>%
    collect_metrics(summarise = TRUE) %>%
    mutate(
      model = "K-nearest neighbour",
      time = knn_time
    )

# create dataframe with all models
model_compare <- bind_rows(log_metrics,
                            tree_metrics,
                            rf_metrics,
                           xgb_metrics,
                           knn_metrics
                      ) %>% 
  # get rid of 'sec elapsed' and turn it into a number
  mutate(
    time = as.double( str_sub(time, end = -13) )
  )
```

## Which metric to use

This is a highly imbalanced data set, as roughly 99.5% of all transactions are ok, and it's only 0.5% of transactions that are fraudulent. A `naive` model, which classifies everything as ok and not-fraud, would have an accuracy of 99.5%, but what about the sensitivity, specificity, the AUC, etc?

## `last_fit()`


```r
#Pivot wider to create barplot
  model_comp <- model_compare %>% 
    select(model, .metric, mean, std_err) %>% 
    pivot_wider(names_from = .metric, values_from = c(mean, std_err)) 

# show mean are under the curve (ROC-AUC) for every model
model_comp %>% 
  arrange(mean_roc_auc) %>% 
  mutate(model = fct_reorder(model, mean_roc_auc)) %>% # order results
  ggplot(aes(model, mean_roc_auc, fill=model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
   geom_text(
     size = 3,
     aes(label = round(mean_roc_auc, 2), 
         y = mean_roc_auc + 0.08),
     vjust = 1
  )+
  theme_light()+
  theme(legend.position = "none")+
  labs(y = NULL)
```

<img src="/blogs/final_group_project_17_sodedit_files/figure-html/unnamed-chunk-16-1.png" width="648" style="display: block; margin: auto;" />

## Best Model

We conclude that the Random Forest model and the Extreme Gradient Boosting model work best! We will use the Random Forest model to fit on our test set.


```r
# Use`last_fit() to fit a model to the whole training data and evaluate it on the test set

last_fit_rf <- last_fit(rf_wflow, 
                        split = data_split,
                        metrics = metric_set(
                          accuracy, f_meas, kap, precision,
                          recall, roc_auc, sens, spec))

last_fit_rf %>% collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 4
##   .metric   .estimator .estimate .config             
##   <chr>     <chr>          <dbl> <chr>               
## 1 accuracy  binary         0.997 Preprocessor1_Model1
## 2 f_meas    binary         0.628 Preprocessor1_Model1
## 3 kap       binary         0.627 Preprocessor1_Model1
## 4 precision binary         1     Preprocessor1_Model1
## 5 recall    binary         0.458 Preprocessor1_Model1
## 6 sens      binary         0.458 Preprocessor1_Model1
## 7 spec      binary         1     Preprocessor1_Model1
## 8 roc_auc   binary         0.973 Preprocessor1_Model1
```

```r
#Compare to training
rf_res %>% collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n   std_err .config             
##   <chr>     <chr>      <dbl> <int>     <dbl> <chr>               
## 1 accuracy  binary     0.996     3 0.000261  Preprocessor1_Model1
## 2 f_meas    binary     0.511     3 0.0145    Preprocessor1_Model1
## 3 kap       binary     0.510     3 0.0146    Preprocessor1_Model1
## 4 precision binary     0.964     3 0.0360    Preprocessor1_Model1
## 5 recall    binary     0.348     3 0.0128    Preprocessor1_Model1
## 6 roc_auc   binary     0.972     3 0.00275   Preprocessor1_Model1
## 7 sens      binary     0.348     3 0.0128    Preprocessor1_Model1
## 8 spec      binary     1.00      3 0.0000749 Preprocessor1_Model1
```

## Get variable importance using `vip` package


```r
library(vip)

last_fit_rf %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 10) +
  theme_light()
```

<img src="/blogs/final_group_project_17_sodedit_files/figure-html/unnamed-chunk-18-1.png" width="648" style="display: block; margin: auto;" />

## Plot Final Confusion matrix and ROC curve


```r
## Final Confusion Matrix

last_fit_rf %>%
  collect_predictions() %>% 
  conf_mat(is_fraud, .pred_class) %>% 
  autoplot(type = "heatmap")
```

<img src="/blogs/final_group_project_17_sodedit_files/figure-html/unnamed-chunk-19-1.png" width="648" style="display: block; margin: auto;" />

```r
## Final ROC curve
last_fit_rf %>% 
  collect_predictions() %>% 
  roc_curve(is_fraud, .pred_1) %>% 
  autoplot()
```

<img src="/blogs/final_group_project_17_sodedit_files/figure-html/unnamed-chunk-19-2.png" width="648" style="display: block; margin: auto;" />

## Calculating the cost of fraud to the company

-   How much money (in US\$ terms) are fraudulent transactions costing the company? Generate a table that summarizes the total amount of legitimate and fraudulent transactions per year and calculate the % of fraudulent transactions, in US\$ terms. Compare your model vs the naive classification that we do not have any fraudulent transactions.


```r
best_model_wflow <- rf_wflow

best_model_preds <- 
  best_model_wflow %>% 
  fit(data = card_fraud_train) %>%  
  
  ## Use `augment()` to get predictions for entire data set
  augment(new_data = card_fraud)

best_model_preds %>% 
  conf_mat(truth = is_fraud, estimate = .pred_class)
```

```
##           Truth
## Prediction      1      0
##          1   1854     37
##          0   2082 667055
```

```r
cost <- best_model_preds %>%
  select(is_fraud, amt, pred = .pred_class) 

cost <- cost %>%
  mutate(
  
  # naive false-- we think every single transaction is ok and not fraud
  false_naives = ifelse(is_fraud==1,amt,0),

  # false negatives-- we thought they were not fraud, but they were
  false_negatives = ifelse(pred==0&is_fraud==1,amt,0),
  
  # false positives-- we thought they were fraud, but they were not
  false_positives = ifelse(pred==1&is_fraud==0,amt,0),
  
  # true positives-- we thought they were fraud, and they were 
  true_positives = ifelse(pred==1&is_fraud==1,amt,0),

  
  # true negatives-- we thought they were ok, and they were
  true_negatives = ifelse(pred==0&is_fraud==0,amt,0)
)
  
# Summarising

cost_summary <- cost %>% 
  summarise(across(starts_with(c("false","true", "amt")), 
            ~ sum(.x, na.rm = TRUE)))

cost_summary
```

```
## # A tibble: 1 × 6
##   false_naives false_negatives false_positives true_positives true_negatives
##          <dbl>           <dbl>           <dbl>          <dbl>          <dbl>
## 1     2075089.         834778.           36227       1240311.      45072588.
## # ℹ 1 more variable: amt <dbl>
```

-   If we use a naive classifier thinking that all transactions are legitimate and not fraudulent, the cost to the company is $2,075,089.

-   With our best model, the total cost of false negatives, namely transactions our classifier thinks are legitimate but which turned out to be fraud, is $834,778.

-   Our classifier also has some false positives, $36,227, namely flagging transactions as fraudulent, but which were legitimate. Assuming the card company makes around 2% for each transaction (source: <https://startups.co.uk/payment-processing/credit-card-processing-fees/>), the amount of money lost due to these false positives is $724.54

-   The \$ improvement over the naive policy is $1,239,586.
