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
