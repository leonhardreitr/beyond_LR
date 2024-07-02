# Chapter 4


``` r
# Packages required for Chapter 4
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ✔ purrr     1.0.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(easystats)
```

    # Attaching packages: easystats 0.7.2
    ✔ bayestestR  0.13.2   ✔ correlation 0.8.5 
    ✔ datawizard  0.11.0   ✔ effectsize  0.8.8 
    ✔ insight     0.20.1   ✔ modelbased  0.8.8 
    ✔ performance 0.12.0   ✔ parameters  0.22.0
    ✔ report      0.5.8    ✔ see         0.8.4 

``` r
theme_set(
  theme_lucid()
)
```

This chapter explores the Poisson Regression. It can be used for count
data appearing in a fixed interval. The main parameter is $\lambda$, the
average count of appearances per unit of time and space. In practice we
need to use the log, so we can do a linear regression, such as:
$log(\lambda_i) = \beta_0 + \beta_1x_1$. Note that in a poisson
distribution the mean and the variance are assumed to be identical.
$E(Y) = VAR(Y) = \lambda$. Furhter assumption included Y to be a count,
a linearity of $log(\lambda)$ on x and , as with all non-hieracial
regressions, independent observations

``` r
url_string <- "https://raw.githubusercontent.com/proback/BeyondMLR/master/data/fHH1.csv"
d <- read.csv(url(url_string))
glimpse(d) 
```

    Rows: 1,500
    Columns: 6
    $ X        <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18…
    $ location <chr> "CentralLuzon", "MetroManila", "DavaoRegion", "Visayas", "Met…
    $ age      <int> 65, 75, 54, 49, 74, 59, 54, 41, 50, 59, 72, 36, 42, 39, 65, 5…
    $ total    <int> 0, 3, 4, 3, 3, 6, 5, 5, 6, 4, 2, 3, 7, 4, 5, 4, 2, 2, 2, 5, 1…
    $ numLT5   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0…
    $ roof     <chr> "Predominantly Strong Material", "Predominantly Strong Materi…

``` r
d |>
  summarise(
    age = mean(age),
    max_ppl = max(total),
    min_ppl = min(total),
    mean_ppl = mean(total),
    sd_ppl = sd(total),
    .by = location
  )
```

          location      age max_ppl min_ppl mean_ppl   sd_ppl
    1 CentralLuzon 54.47321      12       0 3.401786 2.037583
    2  MetroManila 51.52189      11       0 3.707071 2.205273
    3  DavaoRegion 52.08556      10       0 3.390374 2.173277
    4      Visayas 52.30948      16       0 3.901830 2.569438
    5 IlocosRegion 54.40314      16       0 3.586387 2.324158

``` r
d |>
  ggplot(
    aes(
      x = total
    )
  ) +
  geom_bar(
    width = .4,
    fill = "grey",
    col = "black"
  ) +
  scale_y_continuous(
    expand = expansion(0, 0)
  ) +
  labs(
    x = "Number of people in household beside head of household",
    y = "Count"
  )
```

![](chap4_files/figure-commonmark/unnamed-chunk-4-1.png)

``` r
d |>
  ggplot(
    aes(
      x = total
    )
  ) +
  geom_bar(
    width = .4,
    fill = "grey",
    col = "black"
  ) +
  scale_y_continuous(
    expand = expansion(0, 0)
  ) +
  labs(
    x = "Number of people in household beside head of household",
    y = "Count"
  ) +
  facet_wrap(~location, )
```

![](chap4_files/figure-commonmark/unnamed-chunk-4-2.png)

We need to check if mean = variance. an easy way is to just compare
means and variances

``` r
d |>
  summarise(
    mean = mean(total),
    var = var(total),
    .by = age
  )
```

       age     mean        var
    1   65 3.729730  8.0915916
    2   75 2.666667  4.5882353
    3   54 3.903226  4.9569892
    4   49 5.048780  7.8475610
    5   74 2.157895  2.2514620
    6   59 3.500000  5.5909091
    7   41 4.137931  3.0517241
    8   50 3.568182  6.9952431
    9   72 3.437500 10.9291667
    10  36 4.000000  3.3333333
    11  42 3.545455  3.6931818
    12  39 3.685714  4.1630252
    13  56 4.535714 10.7764550
    14  57 2.909091  4.3722944
    15  66 3.086957  7.6284585
    16  47 4.878788  5.1723485
    17  53 4.745098  6.3937255
    18  63 3.520000  5.8433333
    19  60 4.068966  6.7093596
    20  76 2.857143  7.9780220
    21  37 3.464286  3.7394180
    22  61 3.666667  6.6923077
    23  24 1.666667  1.0666667
    24  70 2.705882  6.5955882
    25  55 3.585366  4.6487805
    26  62 4.218750  5.3377016
    27  29 3.166667  0.6969697
    28  43 4.393939  5.4962121
    29  38 3.735294  4.0793226
    30  48 4.872340  5.9398705
    31  77 2.533333  5.9809524
    32  67 4.000000  8.1000000
    33  51 4.285714  5.7394958
    34  44 4.447368  4.9025605
    35  45 4.523810  4.6457607
    36  40 4.277778  2.6063492
    37  30 3.210526  1.3976608
    38  32 3.000000  1.2173913
    39  69 3.037037  5.8831909
    40  84 1.500000  1.9000000
    41  64 3.343750  5.0715726
    42  52 3.133333  2.4643678
    43  83 2.000000  4.0000000
    44  34 3.562500  2.7958333
    45  68 3.740741 11.5071225
    46  78 2.500000  4.0000000
    47  35 3.961538  2.5984615
    48  71 3.625000 15.4500000
    49  81 2.000000  4.6666667
    50  58 3.875000  4.7580645
    51  19 2.000000         NA
    52  31 3.100000  2.5157895
    53  73 2.136364  2.7900433
    54  93 0.000000         NA
    55  33 3.545455  1.7835498
    56  79 2.875000  4.1250000
    57  87 1.000000  0.0000000
    58  27 2.777778  0.4444444
    59  88 2.600000  7.3000000
    60  26 3.000000  2.0000000
    61  23 1.833333  0.9666667
    62  25 3.000000  2.5000000
    63  46 4.034483  4.0344828
    64  20 1.750000  0.9166667
    65  80 1.916667  2.4469697
    66  28 1.400000  2.8000000
    67  94 2.000000         NA
    68  82 2.400000  2.9333333
    69  18 1.000000         NA
    70  85 3.500000  3.6666667
    71  86 1.000000         NA
    72  98 0.000000         NA
    73  95 2.000000         NA
    74  22 3.000000         NA
    75  90 7.000000         NA

``` r
d |>
  summarise(
    mntotal = mean(total),
    logmntotal = log(mntotal), n = n(),
    .by = age
  ) |>
  ggplot(aes(x = age, y = logmntotal)) +
  geom_point(col = "black") +
  geom_smooth(method = "loess", size = 1.5,
              col = "firebrick", fill = "grey70") +
  xlab("Age of head of the household") +
  ylab("Log of the empirical mean number in the house")
```

    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.

    `geom_smooth()` using formula = 'y ~ x'

    Warning: Removed 2 rows containing non-finite outside the scale range
    (`stat_smooth()`).

![](chap4_files/figure-commonmark/unnamed-chunk-6-1.png)

``` r
d |>
  mutate(
    mntotal = mean(total),
    logmntotal = log(mntotal), n = n(),
    .by = age
  ) |>
  ggplot(aes(x = age, y = logmntotal,
             lty = location, shape = location,
              color = location)) +
geom_point() +
  geom_smooth(method = "loess", size = 1.5, se = F) +
  xlab("Age of head of the household") +
  ylab("Log of the empirical mean number in the house") + 
    scale_color_bluebrown()
```

    `geom_smooth()` using formula = 'y ~ x'

    Warning: Removed 2 rows containing non-finite outside the scale range
    (`stat_smooth()`).

![](chap4_files/figure-commonmark/unnamed-chunk-6-2.png)

We want to estimate this model

$$
log(\hat{\lambda_Age}) = \beta_0 + \beta_1Age
$$

``` r
mod1 <- glm(
  total ~ age,
  family = poisson, 
  data = d
)

check_model(mod1)
```

![](chap4_files/figure-commonmark/unnamed-chunk-7-1.png)

``` r
parameters(mod1)
```

    Parameter   |  Log-Mean |       SE |         95% CI |     z |      p
    --------------------------------------------------------------------
    (Intercept) |      1.55 |     0.05 | [ 1.45,  1.65] | 30.83 | < .001
    age         | -4.71e-03 | 9.36e-04 | [-0.01,  0.00] | -5.03 | < .001


    Uncertainty intervals (profile-likelihood) and p-values (two-tailed)
      computed using a Wald z-distribution approximation.


    The model has a log- or logit-link. Consider using `exponentiate =
      TRUE` to interpret coefficients as ratios.

``` r
effectsize(mod1)
```

    # Standardization method: refit

    Parameter   | Std. Coef. |         95% CI
    -----------------------------------------
    (Intercept) |       1.30 | [ 1.28,  1.33]
    age         |      -0.07 | [-0.09, -0.04]

    - Response is unstandardized.

``` r
mod2 <- glm(
  total ~ age^2,
  family = poisson, 
  data = d
)
```
