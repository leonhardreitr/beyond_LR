# Chapter 1


``` r
library(knitr) 
library(gridExtra)
library(GGally)
```

    Loading required package: ggplot2

    Registered S3 method overwritten by 'GGally':
      method from   
      +.gg   ggplot2

``` r
library(kableExtra)
library(jtools)
library(rsample)
library(broom)
```

    Registered S3 methods overwritten by 'broom':
      method            from  
      tidy.glht         jtools
      tidy.summary.glht jtools

``` r
library(tidyverse)   
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ✔ purrr     1.0.2     ✔ tidyr     1.3.1

    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::combine()    masks gridExtra::combine()
    ✖ dplyr::filter()     masks stats::filter()
    ✖ dplyr::group_rows() masks kableExtra::group_rows()
    ✖ dplyr::lag()        masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggdist)

theme_set(
theme_ggdist()
)
```

This book aims to provide us with ways to deal with non-normal and/or
nested data: GLM and (G)MLM

Assumptions of the OLS

- Linear Relationship between X and Y
- Independent Errors
- Normally distributed responses at each level of X
- Equal Variance

# Horse Racing

``` r
setwd("~/Desktop/R stuff/beyond_LR")
d <- read_csv("data/derbyplus.csv")
```

    Rows: 122 Columns: 5
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (2): winner, condition
    dbl (3): year, speed, starters

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
glimpse(d)
```

    Rows: 122
    Columns: 5
    $ year      <dbl> 1896, 1897, 1898, 1899, 1900, 1901, 1902, 1903, 1904, 1905, …
    $ winner    <chr> "Ben Brush", "Typhoon II", "Plaudit", "Manuel", "Lieut. Gibs…
    $ condition <chr> "good", "slow", "good", "fast", "fast", "fast", "fast", "fas…
    $ speed     <dbl> 51.66, 49.81, 51.16, 50.00, 52.28, 51.66, 51.26, 51.16, 51.3…
    $ starters  <dbl> 8, 6, 4, 5, 7, 5, 4, 6, 5, 3, 6, 6, 8, 10, 7, 7, 7, 8, 7, 16…

``` r
d <- d %>%
  mutate( fast = ifelse(condition=="fast",1,0), 
          good = ifelse(condition=="good",1,0),
          yearnew = year - 1896,
          fastfactor = ifelse(fast == 0, "not fast", "fast"))
```

ALWAYS TRY TO UNDERSTAND YOUR DATA FIRST

``` r
range(d$year)
```

    [1] 1896 2017

``` r
count(d, winner)
```

    # A tibble: 122 × 2
       winner               n
       <chr>            <int>
     1 *Omar Khayyam        1
     2 *Tomy Lee            1
     3 Affirmed             1
     4 Agile                1
     5 Alan-a-Dale          1
     6 Always Dreaming      1
     7 Alysheba             1
     8 American Pharoah     1
     9 Animal Kingdom       1
    10 Assault              1
    # ℹ 112 more rows

``` r
d |> 
  summarise(
    speed_mean = mean(speed),
    speed_sd = sd(speed)
  )
```

    # A tibble: 1 × 2
      speed_mean speed_sd
           <dbl>    <dbl>
    1       53.2     1.29

``` r
d |> 
  count(condition)
```

    # A tibble: 3 × 2
      condition     n
      <chr>     <int>
    1 fast         88
    2 good         10
    3 slow         24

``` r
d |> 
  ggplot(
    aes(
      speed
    )
  ) +
  stat_halfeye() +
  labs(
    x = "winning speed",,
    y = "Freq"
  )
```

![](Chapter1_files/figure-commonmark/unnamed-chunk-4-1.png)

``` r
d |> 
  ggplot(
    aes(
      x = year,
      y = speed
    )
  ) +
  geom_smooth(
    method = "lm", se = F,
      aes(col = fastfactor,
          lty = fastfactor)
  ) +
  ggthemes::scale_color_colorblind()
```

    `geom_smooth()` using formula = 'y ~ x'

![](Chapter1_files/figure-commonmark/unnamed-chunk-4-2.png)

``` r
library(correlation)

d |> correlation()
```

    # Correlation Matrix (pearson-method)

    Parameter1 | Parameter2 |     r |         95% CI | t(120) |         p
    ---------------------------------------------------------------------
    year       |      speed |  0.72 | [ 0.62,  0.79] |  11.25 | < .001***
    year       |   starters |  0.65 | [ 0.53,  0.74] |   9.38 | < .001***
    year       |       fast |  0.23 | [ 0.05,  0.39] |   2.55 | 0.084    
    year       |       good | -0.13 | [-0.30,  0.05] |  -1.39 | 0.841    
    year       |    yearnew |  1.00 | [ 1.00,  1.00] |    Inf | < .001***
    speed      |   starters |  0.42 | [ 0.26,  0.56] |   5.11 | < .001***
    speed      |       fast |  0.57 | [ 0.44,  0.68] |   7.58 | < .001***
    speed      |       good | -0.10 | [-0.28,  0.08] |  -1.14 | 0.841    
    speed      |    yearnew |  0.72 | [ 0.62,  0.79] |  11.25 | < .001***
    starters   |       fast |  0.07 | [-0.11,  0.24] |   0.77 | 0.889    
    starters   |       good | -0.04 | [-0.22,  0.14] |  -0.45 | 0.889    
    starters   |    yearnew |  0.65 | [ 0.53,  0.74] |   9.38 | < .001***
    fast       |       good | -0.48 | [-0.61, -0.33] |  -6.01 | < .001***
    fast       |    yearnew |  0.23 | [ 0.05,  0.39] |   2.55 | 0.084    
    good       |    yearnew | -0.13 | [-0.30,  0.05] |  -1.39 | 0.841    

    p-value adjustment method: Holm (1979)
    Observations: 122

# Fit it

Time to fit a simple linear regression using a non-disrecte predictor

``` r
mod1 <- lm(
  speed ~ year, data = d
)

summary(mod1)
```


    Call:
    lm(formula = speed ~ year, data = d)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -3.08190 -0.50026  0.07387  0.67367  1.68720 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept) 2.053473   4.543754   0.452    0.652    
    year        0.026126   0.002322  11.251   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.9032 on 120 degrees of freedom
    Multiple R-squared:  0.5134,    Adjusted R-squared:  0.5093 
    F-statistic: 126.6 on 1 and 120 DF,  p-value: < 2.2e-16

``` r
confint(mod1)
```

                      2.5 %      97.5 %
    (Intercept) -6.94284394 11.04979019
    year         0.02152859  0.03072344

``` r
predict(mod1)
```

           1        2        3        4        5        6        7        8 
    51.58839 51.61452 51.64064 51.66677 51.69290 51.71902 51.74515 51.77127 
           9       10       11       12       13       14       15       16 
    51.79740 51.82353 51.84965 51.87578 51.90190 51.92803 51.95416 51.98028 
          17       18       19       20       21       22       23       24 
    52.00641 52.03253 52.05866 52.08479 52.11091 52.13704 52.16316 52.18929 
          25       26       27       28       29       30       31       32 
    52.21542 52.24154 52.26767 52.29379 52.31992 52.34605 52.37217 52.39830 
          33       34       35       36       37       38       39       40 
    52.42443 52.45055 52.47668 52.50280 52.52893 52.55506 52.58118 52.60731 
          41       42       43       44       45       46       47       48 
    52.63343 52.65956 52.68569 52.71181 52.73794 52.76406 52.79019 52.81632 
          49       50       51       52       53       54       55       56 
    52.84244 52.86857 52.89469 52.92082 52.94695 52.97307 52.99920 53.02532 
          57       58       59       60       61       62       63       64 
    53.05145 53.07758 53.10370 53.12983 53.15595 53.18208 53.20821 53.23433 
          65       66       67       68       69       70       71       72 
    53.26046 53.28658 53.31271 53.33884 53.36496 53.39109 53.41721 53.44334 
          73       74       75       76       77       78       79       80 
    53.46947 53.49559 53.52172 53.54784 53.57397 53.60010 53.62622 53.65235 
          81       82       83       84       85       86       87       88 
    53.67847 53.70460 53.73073 53.75685 53.78298 53.80910 53.83523 53.86136 
          89       90       91       92       93       94       95       96 
    53.88748 53.91361 53.93973 53.96586 53.99199 54.01811 54.04424 54.07036 
          97       98       99      100      101      102      103      104 
    54.09649 54.12262 54.14874 54.17487 54.20099 54.22712 54.25325 54.27937 
         105      106      107      108      109      110      111      112 
    54.30550 54.33162 54.35775 54.38388 54.41000 54.43613 54.46225 54.48838 
         113      114      115      116      117      118      119      120 
    54.51451 54.54063 54.56676 54.59288 54.61901 54.64514 54.67126 54.69739 
         121      122 
    54.72351 54.74964 

In this model the intercept tells us the change effect of year on speed
if year is 0. Not really useful. We can thus center the predictor. Then
the intercept represents the expected value of Y when X is at its mean

``` r
d <- 
  d |> 
  mutate(year_cent = year - 1896)

# yields the same result
mod2 <- lm(
  speed ~ year, data = d
)

summary(mod2)
```


    Call:
    lm(formula = speed ~ year, data = d)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -3.08190 -0.50026  0.07387  0.67367  1.68720 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept) 2.053473   4.543754   0.452    0.652    
    year        0.026126   0.002322  11.251   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.9032 on 120 degrees of freedom
    Multiple R-squared:  0.5134,    Adjusted R-squared:  0.5093 
    F-statistic: 126.6 on 1 and 120 DF,  p-value: < 2.2e-16

``` r
confint(mod2)
```

                      2.5 %      97.5 %
    (Intercept) -6.94284394 11.04979019
    year         0.02152859  0.03072344

``` r
predict(mod2)
```

           1        2        3        4        5        6        7        8 
    51.58839 51.61452 51.64064 51.66677 51.69290 51.71902 51.74515 51.77127 
           9       10       11       12       13       14       15       16 
    51.79740 51.82353 51.84965 51.87578 51.90190 51.92803 51.95416 51.98028 
          17       18       19       20       21       22       23       24 
    52.00641 52.03253 52.05866 52.08479 52.11091 52.13704 52.16316 52.18929 
          25       26       27       28       29       30       31       32 
    52.21542 52.24154 52.26767 52.29379 52.31992 52.34605 52.37217 52.39830 
          33       34       35       36       37       38       39       40 
    52.42443 52.45055 52.47668 52.50280 52.52893 52.55506 52.58118 52.60731 
          41       42       43       44       45       46       47       48 
    52.63343 52.65956 52.68569 52.71181 52.73794 52.76406 52.79019 52.81632 
          49       50       51       52       53       54       55       56 
    52.84244 52.86857 52.89469 52.92082 52.94695 52.97307 52.99920 53.02532 
          57       58       59       60       61       62       63       64 
    53.05145 53.07758 53.10370 53.12983 53.15595 53.18208 53.20821 53.23433 
          65       66       67       68       69       70       71       72 
    53.26046 53.28658 53.31271 53.33884 53.36496 53.39109 53.41721 53.44334 
          73       74       75       76       77       78       79       80 
    53.46947 53.49559 53.52172 53.54784 53.57397 53.60010 53.62622 53.65235 
          81       82       83       84       85       86       87       88 
    53.67847 53.70460 53.73073 53.75685 53.78298 53.80910 53.83523 53.86136 
          89       90       91       92       93       94       95       96 
    53.88748 53.91361 53.93973 53.96586 53.99199 54.01811 54.04424 54.07036 
          97       98       99      100      101      102      103      104 
    54.09649 54.12262 54.14874 54.17487 54.20099 54.22712 54.25325 54.27937 
         105      106      107      108      109      110      111      112 
    54.30550 54.33162 54.35775 54.38388 54.41000 54.43613 54.46225 54.48838 
         113      114      115      116      117      118      119      120 
    54.51451 54.54063 54.56676 54.59288 54.61901 54.64514 54.67126 54.69739 
         121      122 
    54.72351 54.74964 

Furthermore both models have identical model perfomances

``` r
performance::compare_performance(mod1,mod2)
```

    Some of the nested models seem to be identical

    # Comparison of Model Performance Indices

    Name | Model | AIC (weights) | AICc (weights) | BIC (weights) |    R2 | R2 (adj.) |  RMSE | Sigma
    -------------------------------------------------------------------------------------------------
    mod1 |    lm | 325.4 (0.500) |  325.6 (0.500) | 333.8 (0.500) | 0.513 |     0.509 | 0.896 | 0.903
    mod2 |    lm | 325.4 (0.500) |  325.6 (0.500) | 333.8 (0.500) | 0.513 |     0.509 | 0.896 | 0.903
