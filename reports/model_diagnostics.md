---
title: "Model Diagnostics"
author: "Noam Ross"
date: "2022-08-03"
output: 
  html_document:
    keep_md: true
---

<style type="text/css">
.main-container {
  max-width: 1800px !important;
  margin-left: auto;
  margin-right: auto;
}
</style>

This document summarizes model diagnostics and checks.

First, we set up by loading packages and generated R objects.


```r
knitr::opts_chunk$set(echo = TRUE)
library(targets)
library(mgcv)
options(width = 250)
```


```r
tar_load(c(
  multinomial_model,
  gam_posterior,
  posterior_stats
))
```

## Model Summary


```r
summary(multinomial_model)
```

```
## 
## Family: multinom 
## Link function: 
## 
## Formula:
## outcome ~ s(sample_type, bs = "re", by = dummy_rectal) + s(day, 
##     bs = "tp", k = 5, by = sample_type, m = 2) + s(day, gender_age, 
##     bs = "fs", k = 5, xt = list(bs = "tp"), by = dummy_rectal, 
##     m = 2) + s(day_of_year, bs = "cc", by = sample_type, k = 5, 
##     m = 2) + s(day_of_year, gender_age, bs = "fs", xt = list(bs = "cc"), 
##     k = 5, by = dummy_rectal, m = 2) + s(fmi_kg_m2, k = 5, bs = "tp", 
##     by = dummy_rectal) + s(reproductive_condition, bs = "re", 
##     by = dummy_repro) + s(frac_subadult, bs = "tp", k = 5, by = dummy_any_rectal) + 
##     s(frac_subadult, sample_type, bs = "fs", k = 5, xt = list(bs = "tp"), 
##         by = dummy_any_rectal, m = 2)
## <environment: 0x55c031b935b0>
## ~s(sample_type, bs = "re", by = dummy_rectal) + s(day, bs = "tp", 
##     k = 5, by = sample_type, m = 2) + s(day, gender_age, bs = "fs", 
##     k = 5, xt = list(bs = "tp"), by = dummy_rectal, m = 2) + 
##     s(day_of_year, bs = "cc", by = sample_type, k = 5, m = 2) + 
##     s(day_of_year, gender_age, bs = "fs", xt = list(bs = "cc"), 
##         k = 5, by = dummy_rectal, m = 2) + s(fmi_kg_m2, k = 5, 
##     bs = "tp", by = dummy_rectal) + s(reproductive_condition, 
##     bs = "re", by = dummy_repro) + s(frac_subadult, bs = "tp", 
##     k = 5, by = dummy_any_rectal) + s(frac_subadult, sample_type, 
##     bs = "fs", k = 5, xt = list(bs = "tp"), by = dummy_any_rectal, 
##     m = 2)
## <environment: 0x55c031b935b0>
## ~s(sample_type, bs = "re", by = dummy_rectal) + s(day, bs = "tp", 
##     k = 5, by = sample_type, m = 2) + s(day_of_year, bs = "cc", 
##     by = sample_type, k = 5, m = 2)
## <environment: 0x55c031b935b0>
## 
## Parametric coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)    -2.6628     0.4240  -6.280 3.38e-10 ***
## (Intercept).1  -3.4124     0.4004  -8.522  < 2e-16 ***
## (Intercept).2  -6.4496     0.8494  -7.593 3.12e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##                                                       edf Ref.df Chi.sq  p-value    
## s(sample_type):dummy_rectal1                    2.899e-05  1.000  0.000 0.789330    
## s(day):sample_typeFecal                         1.000e+00  1.000  0.724 0.394727    
## s(day):sample_typeRectal                        1.000e+00  1.000  0.394 0.530193    
## s(day,gender_age):dummy_rectal1                 2.987e-04 19.000  0.000 0.635107    
## s(day_of_year):sample_typeFecal                 1.965e+00  3.000 14.163 0.000225 ***
## s(day_of_year):sample_typeRectal                9.786e-01  3.000  2.132 0.098737 .  
## s(day_of_year,gender_age):dummy_rectal1         9.337e-05 16.000  0.000 0.838837    
## s(fmi_kg_m2):dummy_rectal1                      1.000e+00  1.000  2.517 0.112670    
## s(reproductive_condition):dummy_repro1          2.314e-05  3.000  0.000 0.815053    
## s(frac_subadult):dummy_any_rectal               3.948e+00  4.428 10.460 0.031024 *  
## s(frac_subadult,sample_type):dummy_any_rectal   1.442e-01  8.000  0.150 0.273131    
## s.1(sample_type):dummy_rectal1                  3.110e-05  1.000  0.000 0.645994    
## s.1(day):sample_typeFecal                       1.956e+00  2.275 29.559 1.16e-06 ***
## s.1(day):sample_typeRectal                      3.106e+00  3.503 30.700 7.16e-06 ***
## s.1(day,gender_age):dummy_rectal1               2.364e-04 19.000  0.000 0.858819    
## s.1(day_of_year):sample_typeFecal               2.191e+00  3.000 18.299 1.35e-05 ***
## s.1(day_of_year):sample_typeRectal              6.413e-04  3.000  0.001 0.214107    
## s.1(day_of_year,gender_age):dummy_rectal1       7.618e-05 16.000  0.000 0.812105    
## s.1(fmi_kg_m2):dummy_rectal1                    1.000e+00  1.001  5.909 0.015083 *  
## s.1(reproductive_condition):dummy_repro1        8.728e-01  3.000  1.223 0.235590    
## s.1(frac_subadult):dummy_any_rectal             4.773e+00  4.957 33.281 3.70e-06 ***
## s.1(frac_subadult,sample_type):dummy_any_rectal 1.628e-04  8.000  0.000 0.625941    
## s.2(sample_type):dummy_rectal1                  1.479e-05  1.000  0.000 0.606146    
## s.2(day):sample_typeFecal                       1.000e+00  1.000  0.029 0.864617    
## s.2(day):sample_typeRectal                      1.001e+00  1.001  0.246 0.620572    
## s.2(day_of_year):sample_typeFecal               2.192e+00  3.000 17.114 9.72e-05 ***
## s.2(day_of_year):sample_typeRectal              1.753e+00  3.000  5.547 0.032784 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Deviance explained = 16.9%
## -REML = -587.48  Scale est. = 1         n = 1430
```

## GAM Checks

`gam.check()` provides convergence diagnostics. Basis dimension (k) results should all be above 0.05 to indicate
that there are not systematic patterns in the model residuals.  Four diagnostic
plot are provided. In the first (Q-Q) plot, we should see residuals and quantiles
line up on the 1-1 line.  Other plots are difficult to interpret in the multinomial
case. 


```r
gam.check(multinomial_model)
```

![](model_diagnostics_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```
## 
## Method: REML   Optimizer: outer newton
## full convergence after 17 iterations.
## Gradient range [-0.0002321921,0.0001830839]
## (score -587.481 & scale 1).
## eigenvalue range [-1.650752e-05,1.092461].
## Model rank =  187 / 187 
## 
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
## 
##                                                       k'      edf k-index p-value
## s(sample_type):dummy_rectal1                    2.00e+00 2.90e-05      NA      NA
## s(day):sample_typeFecal                         4.00e+00 1.00e+00    0.96    0.21
## s(day):sample_typeRectal                        4.00e+00 1.00e+00    0.96    0.28
## s(day,gender_age):dummy_rectal1                 2.50e+01 2.99e-04    0.96    0.20
## s(day_of_year):sample_typeFecal                 3.00e+00 1.96e+00    0.96    0.26
## s(day_of_year):sample_typeRectal                3.00e+00 9.79e-01    0.96    0.20
## s(day_of_year,gender_age):dummy_rectal1         2.00e+01 9.34e-05    0.96    0.26
## s(fmi_kg_m2):dummy_rectal1                      4.00e+00 1.00e+00    0.98    0.61
## s(reproductive_condition):dummy_repro1          4.00e+00 2.31e-05      NA      NA
## s(frac_subadult):dummy_any_rectal               5.00e+00 3.95e+00    0.96    0.30
## s(frac_subadult,sample_type):dummy_any_rectal   1.00e+01 1.44e-01    0.96    0.21
## s.1(sample_type):dummy_rectal1                  2.00e+00 3.11e-05      NA      NA
## s.1(day):sample_typeFecal                       4.00e+00 1.96e+00    0.96    0.27
## s.1(day):sample_typeRectal                      4.00e+00 3.11e+00    0.96    0.26
## s.1(day,gender_age):dummy_rectal1               2.50e+01 2.36e-04    0.96    0.28
## s.1(day_of_year):sample_typeFecal               3.00e+00 2.19e+00    0.96    0.26
## s.1(day_of_year):sample_typeRectal              3.00e+00 6.41e-04    0.96    0.27
## s.1(day_of_year,gender_age):dummy_rectal1       2.00e+01 7.62e-05    0.96    0.26
## s.1(fmi_kg_m2):dummy_rectal1                    4.00e+00 1.00e+00    0.98    0.59
## s.1(reproductive_condition):dummy_repro1        4.00e+00 8.73e-01      NA      NA
## s.1(frac_subadult):dummy_any_rectal             5.00e+00 4.77e+00    0.96    0.29
## s.1(frac_subadult,sample_type):dummy_any_rectal 1.00e+01 1.63e-04    0.96    0.25
## s.2(sample_type):dummy_rectal1                  2.00e+00 1.48e-05      NA      NA
## s.2(day):sample_typeFecal                       4.00e+00 1.00e+00    0.96    0.23
## s.2(day):sample_typeRectal                      4.00e+00 1.00e+00    0.96    0.23
## s.2(day_of_year):sample_typeFecal               3.00e+00 2.19e+00    0.96    0.26
## s.2(day_of_year):sample_typeRectal              3.00e+00 1.75e+00    0.96    0.29
```

## Individual Smooth terms

These are plots of the indivudal smooths in the model on the linear scale.
Inspect for strange behavior and check that their shape corresponds with
significance terms in in the model summary.


```r
for (i in seq_len(length(multinomial_model$smooth))) {
  plot(multinomial_model, scale = 0, select = i)
}
```

<img src="model_diagnostics_files/figure-html/unnamed-chunk-3-1.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-2.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-3.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-4.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-5.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-6.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-7.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-8.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-9.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-10.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-11.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-12.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-13.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-14.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-15.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-16.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-17.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-18.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-19.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-20.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-21.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-22.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-23.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-24.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-25.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-26.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-3-27.png" width="33%" />

## MCMC diagnostics

These are acceptance ratios from the 4 MCMC chains
run.  

From `?gam.mh`:

> The function reports the acceptance rate of the two types of step. If the 
> random walk acceptance probability (`$rw.accept`) is higher than a quarter then `rw.step` 
> should probably be increased. Similarly if the acceptance rate (`$accept`) is too low,
> it should be decreased. The random walk steps can be turned off altogether
> (see above), but it is important to check the chains for stuck sections if
> this is done.



```r
attributes(gam_posterior)[c("accept", "rw.accept")]
```

```
## $accept
## [1] 0.1227273 0.1431818 0.1317818 0.1309818
## 
## $rw.accept
## [1] 0.3313091 0.3324000 0.3333455 0.3337636
```

Here is a summary of the dimensions of the MCMC chain output

```r
tibble(
  dimension = names(dimnames(gam_posterior)),
  value = dim(gam_posterior)
)
```

```
## # A tibble: 3 × 2
##   dimension value
##   <chr>     <int>
## 1 Iteration   550
## 2 Chain         4
## 3 Parameter   187
```


Here are Stan-type per-parameter chain diagnostics.  `Rhat` should be at or near
1 to indicate that the multiple MCMC chains are well-mixed.  `ess_bulk` and 
`ess_tail` are the effective sample sizes for posterior samples from the parameter
distributions and the tails of those distributions, respectively. From the Stan
manual:

> We recommend running at least four chains by default and only using the sample
> if R-hat is less than 1.05....
>
> Both bulk-ESS and tail-ESS should be at least 100 (approximately) per
> Markov Chain in order to be reliable and indicate that estimates of respective
> posterior quantiles are reliable


```r
print(posterior_stats, n = Inf)
```

```
## # A tibble: 187 × 4
##     parameter                                           Rhat ess_bulk ess_tail
##     <chr>                                              <dbl>    <dbl>    <dbl>
##   1 (Intercept)                                        1.00     1840.    1933.
##   2 s(sample_type):dummy_rectal1.1                     1.00     1696.    1628.
##   3 s(sample_type):dummy_rectal1.2                     0.999    1787.    1988.
##   4 s(day):sample_typeFecal.1                          1.00     1798.    1780.
##   5 s(day):sample_typeFecal.2                          0.999    1693.    2018.
##   6 s(day):sample_typeFecal.3                          1.00     1826.    2076.
##   7 s(day):sample_typeFecal.4                          1.00     1805.    1959.
##   8 s(day):sample_typeRectal.1                         1.00     1641.    1744.
##   9 s(day):sample_typeRectal.2                         1.00     1830.    1820.
##  10 s(day):sample_typeRectal.3                         1.00     1950.    1914.
##  11 s(day):sample_typeRectal.4                         1.00     1650.    1768.
##  12 s(day,gender_age):dummy_rectal1.1                  1.00     1911.    1983.
##  13 s(day,gender_age):dummy_rectal1.2                  1.00     2039.    2075.
##  14 s(day,gender_age):dummy_rectal1.3                  1.00     2085.    2115.
##  15 s(day,gender_age):dummy_rectal1.4                  1.00     1630.    2113.
##  16 s(day,gender_age):dummy_rectal1.5                  0.999    1689.    1712.
##  17 s(day,gender_age):dummy_rectal1.6                  1.00     1660.    1966.
##  18 s(day,gender_age):dummy_rectal1.7                  1.00     1610.    1837.
##  19 s(day,gender_age):dummy_rectal1.8                  1.00     2001.    1934.
##  20 s(day,gender_age):dummy_rectal1.9                  1.00     1980.    2027.
##  21 s(day,gender_age):dummy_rectal1.10                 1.00     1794.    1978.
##  22 s(day,gender_age):dummy_rectal1.11                 1.00     1845.    2026.
##  23 s(day,gender_age):dummy_rectal1.12                 1.00     1895.    1861.
##  24 s(day,gender_age):dummy_rectal1.13                 1.00     1748.    1851.
##  25 s(day,gender_age):dummy_rectal1.14                 1.00     1786.    2037.
##  26 s(day,gender_age):dummy_rectal1.15                 1.00     1755.    2146.
##  27 s(day,gender_age):dummy_rectal1.16                 1.00     1813.    1738.
##  28 s(day,gender_age):dummy_rectal1.17                 1.00     1774.    1750.
##  29 s(day,gender_age):dummy_rectal1.18                 1.00     1813.    2076.
##  30 s(day,gender_age):dummy_rectal1.19                 1.00     1836.    1937.
##  31 s(day,gender_age):dummy_rectal1.20                 1.00     2004.    1998.
##  32 s(day,gender_age):dummy_rectal1.21                 1.00     1712.    1602.
##  33 s(day,gender_age):dummy_rectal1.22                 1.00     1713.    1853.
##  34 s(day,gender_age):dummy_rectal1.23                 1.00     1947.    1869.
##  35 s(day,gender_age):dummy_rectal1.24                 1.00     1953.    1782.
##  36 s(day,gender_age):dummy_rectal1.25                 1.00     1757.    1617.
##  37 s(day_of_year):sample_typeFecal.1                  1.00     1906.    1960.
##  38 s(day_of_year):sample_typeFecal.2                  1.00     1778.    1782.
##  39 s(day_of_year):sample_typeFecal.3                  1.00     1822.    1695.
##  40 s(day_of_year):sample_typeRectal.1                 1.00     1713.    1776.
##  41 s(day_of_year):sample_typeRectal.2                 1.00     1688.    1830.
##  42 s(day_of_year):sample_typeRectal.3                 1.00     1725.    2067.
##  43 s(day_of_year,gender_age):dummy_rectal1.1          1.00     1835.    1957.
##  44 s(day_of_year,gender_age):dummy_rectal1.2          1.00     1760.    2070.
##  45 s(day_of_year,gender_age):dummy_rectal1.3          1.00     1815.    2026.
##  46 s(day_of_year,gender_age):dummy_rectal1.4          1.00     1692.    1645.
##  47 s(day_of_year,gender_age):dummy_rectal1.5          0.999    2042.    2152.
##  48 s(day_of_year,gender_age):dummy_rectal1.6          1.00     1488.    1867.
##  49 s(day_of_year,gender_age):dummy_rectal1.7          1.00     1908.    1707.
##  50 s(day_of_year,gender_age):dummy_rectal1.8          0.999    1500.    1853.
##  51 s(day_of_year,gender_age):dummy_rectal1.9          1.00     1391.    1586.
##  52 s(day_of_year,gender_age):dummy_rectal1.10         1.00     1838.    1717.
##  53 s(day_of_year,gender_age):dummy_rectal1.11         1.00     1513.    1470.
##  54 s(day_of_year,gender_age):dummy_rectal1.12         1.00     2004.    1839.
##  55 s(day_of_year,gender_age):dummy_rectal1.13         1.00     1907.    1973.
##  56 s(day_of_year,gender_age):dummy_rectal1.14         1.00     1706.    1644.
##  57 s(day_of_year,gender_age):dummy_rectal1.15         1.00     2048.    2113.
##  58 s(day_of_year,gender_age):dummy_rectal1.16         1.00     1889.    1904.
##  59 s(day_of_year,gender_age):dummy_rectal1.17         1.00     1615.    1654.
##  60 s(day_of_year,gender_age):dummy_rectal1.18         1.00     1609.    1868.
##  61 s(day_of_year,gender_age):dummy_rectal1.19         1.00     1728.    1934.
##  62 s(day_of_year,gender_age):dummy_rectal1.20         1.00     1671.    1695.
##  63 s(fmi_kg_m2):dummy_rectal1.1                       1.00     2096.    2084.
##  64 s(fmi_kg_m2):dummy_rectal1.2                       1.00     1688.    1868.
##  65 s(fmi_kg_m2):dummy_rectal1.3                       1.00     1817.    1749.
##  66 s(fmi_kg_m2):dummy_rectal1.4                       1.00     1737.    1995.
##  67 s(reproductive_condition):dummy_repro1.1           1.00     1989.    2109.
##  68 s(reproductive_condition):dummy_repro1.2           1.00     1751.    1869.
##  69 s(reproductive_condition):dummy_repro1.3           1.00     1705.    1965.
##  70 s(reproductive_condition):dummy_repro1.4           1.00     1655.    2034.
##  71 s(frac_subadult):dummy_any_rectal.1                1.00     1724.    1827.
##  72 s(frac_subadult):dummy_any_rectal.2                1.00     1571.    1734.
##  73 s(frac_subadult):dummy_any_rectal.3                1.00     1832.    2059.
##  74 s(frac_subadult):dummy_any_rectal.4                1.00     1679.    1770.
##  75 s(frac_subadult):dummy_any_rectal.5                1.00     1463.    1356.
##  76 s(frac_subadult,sample_type):dummy_any_rectal.1    1.00     1835.    1582.
##  77 s(frac_subadult,sample_type):dummy_any_rectal.2    1.00     2039.    1836.
##  78 s(frac_subadult,sample_type):dummy_any_rectal.3    1.00     1946.    1941.
##  79 s(frac_subadult,sample_type):dummy_any_rectal.4    1.00     1888.    1772.
##  80 s(frac_subadult,sample_type):dummy_any_rectal.5    1.00     1604.    1850.
##  81 s(frac_subadult,sample_type):dummy_any_rectal.6    0.999    2129.    1944.
##  82 s(frac_subadult,sample_type):dummy_any_rectal.7    1.00     1688.    1984.
##  83 s(frac_subadult,sample_type):dummy_any_rectal.8    1.00     1840.    1959.
##  84 s(frac_subadult,sample_type):dummy_any_rectal.9    1.00     1688.    1806.
##  85 s(frac_subadult,sample_type):dummy_any_rectal.10   1.00     2025.    1683.
##  86 (Intercept).1                                      1.00     1520.    2008.
##  87 s.1(sample_type):dummy_rectal1.1                   1.00     1498.    1813.
##  88 s.1(sample_type):dummy_rectal1.2                   1.00     1820.    1775.
##  89 s.1(day):sample_typeFecal.1                        1.00     1520.    1969.
##  90 s.1(day):sample_typeFecal.2                        1.00     1548.    1989.
##  91 s.1(day):sample_typeFecal.3                        1.00     1622.    1858.
##  92 s.1(day):sample_typeFecal.4                        1.00     1337.    1841.
##  93 s.1(day):sample_typeRectal.1                       1.00     1916.    2055.
##  94 s.1(day):sample_typeRectal.2                       1.00     1906.    1838.
##  95 s.1(day):sample_typeRectal.3                       1.00     1888.    2112.
##  96 s.1(day):sample_typeRectal.4                       1.00     1885.    1954.
##  97 s.1(day,gender_age):dummy_rectal1.1                1.00     1876.    1968.
##  98 s.1(day,gender_age):dummy_rectal1.2                1.00     1801.    2049.
##  99 s.1(day,gender_age):dummy_rectal1.3                1.00     1711.    1942.
## 100 s.1(day,gender_age):dummy_rectal1.4                1.00     1716.    1788.
## 101 s.1(day,gender_age):dummy_rectal1.5                1.00     1786.    1870.
## 102 s.1(day,gender_age):dummy_rectal1.6                1.00     1899.    1752.
## 103 s.1(day,gender_age):dummy_rectal1.7                1.00     2126.    1611.
## 104 s.1(day,gender_age):dummy_rectal1.8                1.00     2060.    2167.
## 105 s.1(day,gender_age):dummy_rectal1.9                1.00     1906.    1949.
## 106 s.1(day,gender_age):dummy_rectal1.10               1.00     1876.    2113.
## 107 s.1(day,gender_age):dummy_rectal1.11               1.00     1880.    2097.
## 108 s.1(day,gender_age):dummy_rectal1.12               1.00     1774.    1853.
## 109 s.1(day,gender_age):dummy_rectal1.13               1.00     1810.    2112.
## 110 s.1(day,gender_age):dummy_rectal1.14               1.00     2044.    2094.
## 111 s.1(day,gender_age):dummy_rectal1.15               1.00     1959.    1866.
## 112 s.1(day,gender_age):dummy_rectal1.16               1.00     1725.    1778.
## 113 s.1(day,gender_age):dummy_rectal1.17               1.00     1692.    1822.
## 114 s.1(day,gender_age):dummy_rectal1.18               1.00     1773.    1830.
## 115 s.1(day,gender_age):dummy_rectal1.19               1.00     1889.    2045.
## 116 s.1(day,gender_age):dummy_rectal1.20               0.999    1902.    2000.
## 117 s.1(day,gender_age):dummy_rectal1.21               1.00     1999.    2022.
## 118 s.1(day,gender_age):dummy_rectal1.22               1.00     2056.    2172.
## 119 s.1(day,gender_age):dummy_rectal1.23               1.00     1663.    1851.
## 120 s.1(day,gender_age):dummy_rectal1.24               1.00     1863.    2012.
## 121 s.1(day,gender_age):dummy_rectal1.25               1.00     1858.    1894.
## 122 s.1(day_of_year):sample_typeFecal.1                1.00     1676.    1952.
## 123 s.1(day_of_year):sample_typeFecal.2                1.00     1742.    1646.
## 124 s.1(day_of_year):sample_typeFecal.3                1.00     1795.    1956.
## 125 s.1(day_of_year):sample_typeRectal.1               1.00     1815.    1619.
## 126 s.1(day_of_year):sample_typeRectal.2               1.00     1719.    1969.
## 127 s.1(day_of_year):sample_typeRectal.3               1.00     1817.    1944.
## 128 s.1(day_of_year,gender_age):dummy_rectal1.1        1.00     1916.    1950.
## 129 s.1(day_of_year,gender_age):dummy_rectal1.2        1.00     1988.    1952.
## 130 s.1(day_of_year,gender_age):dummy_rectal1.3        1.00     1983.    1877.
## 131 s.1(day_of_year,gender_age):dummy_rectal1.4        1.00     1926.    1751.
## 132 s.1(day_of_year,gender_age):dummy_rectal1.5        1.00     1888.    1966.
## 133 s.1(day_of_year,gender_age):dummy_rectal1.6        0.999    1671.    1814.
## 134 s.1(day_of_year,gender_age):dummy_rectal1.7        1.00     1772.    1594.
## 135 s.1(day_of_year,gender_age):dummy_rectal1.8        1.00     1906.    2151.
## 136 s.1(day_of_year,gender_age):dummy_rectal1.9        1.00     2069.    2194.
## 137 s.1(day_of_year,gender_age):dummy_rectal1.10       1.00     1424.    1812.
## 138 s.1(day_of_year,gender_age):dummy_rectal1.11       1.00     1868.    1980.
## 139 s.1(day_of_year,gender_age):dummy_rectal1.12       1.00     1928.    2076.
## 140 s.1(day_of_year,gender_age):dummy_rectal1.13       1.00     1942.    2036.
## 141 s.1(day_of_year,gender_age):dummy_rectal1.14       1.00     1832.    1651.
## 142 s.1(day_of_year,gender_age):dummy_rectal1.15       1.00     1668.    1478.
## 143 s.1(day_of_year,gender_age):dummy_rectal1.16       1.00     1969.    1920.
## 144 s.1(day_of_year,gender_age):dummy_rectal1.17       1.00     1681.    1967.
## 145 s.1(day_of_year,gender_age):dummy_rectal1.18       1.00     1713.    1362.
## 146 s.1(day_of_year,gender_age):dummy_rectal1.19       1.00     1917.    1888.
## 147 s.1(day_of_year,gender_age):dummy_rectal1.20       1.00     1945.    1579.
## 148 s.1(fmi_kg_m2):dummy_rectal1.1                     1.00     1831.    1935.
## 149 s.1(fmi_kg_m2):dummy_rectal1.2                     1.00     1962.    2003.
## 150 s.1(fmi_kg_m2):dummy_rectal1.3                     1.00     1955.    1823.
## 151 s.1(fmi_kg_m2):dummy_rectal1.4                     1.00     1766.    1719.
## 152 s.1(reproductive_condition):dummy_repro1.1         1.00     1947.    2111.
## 153 s.1(reproductive_condition):dummy_repro1.2         0.999    1772.    1434.
## 154 s.1(reproductive_condition):dummy_repro1.3         1.00     1701.    1737.
## 155 s.1(reproductive_condition):dummy_repro1.4         1.00     1617.    1935.
## 156 s.1(frac_subadult):dummy_any_rectal.1              1.00     1709.    1858.
## 157 s.1(frac_subadult):dummy_any_rectal.2              1.00     1892.    1888.
## 158 s.1(frac_subadult):dummy_any_rectal.3              1.00     1682.    1954.
## 159 s.1(frac_subadult):dummy_any_rectal.4              1.00     1687.    2002.
## 160 s.1(frac_subadult):dummy_any_rectal.5              1.00     1921.    2015.
## 161 s.1(frac_subadult,sample_type):dummy_any_rectal.1  0.999    1974.    2019.
## 162 s.1(frac_subadult,sample_type):dummy_any_rectal.2  1.00     1925.    2013.
## 163 s.1(frac_subadult,sample_type):dummy_any_rectal.3  1.00     1896.    2015.
## 164 s.1(frac_subadult,sample_type):dummy_any_rectal.4  1.00     1724.    1805.
## 165 s.1(frac_subadult,sample_type):dummy_any_rectal.5  0.999    1883.    2058.
## 166 s.1(frac_subadult,sample_type):dummy_any_rectal.6  0.999    1971.    1740.
## 167 s.1(frac_subadult,sample_type):dummy_any_rectal.7  1.00     1972.    1752.
## 168 s.1(frac_subadult,sample_type):dummy_any_rectal.8  1.00     1808.    2031.
## 169 s.1(frac_subadult,sample_type):dummy_any_rectal.9  1.00     1717.    2036.
## 170 s.1(frac_subadult,sample_type):dummy_any_rectal.10 1.00     1830.    1856.
## 171 (Intercept).2                                      1.00     1355.     811.
## 172 s.2(sample_type):dummy_rectal1.1                   1.00     1502.    2072.
## 173 s.2(sample_type):dummy_rectal1.2                   1.00     1676.    1837.
## 174 s.2(day):sample_typeFecal.1                        1.00     1735.    1934.
## 175 s.2(day):sample_typeFecal.2                        1.00     1796.    2001.
## 176 s.2(day):sample_typeFecal.3                        1.00     1698.    1967.
## 177 s.2(day):sample_typeFecal.4                        1.00     1754.    1715.
## 178 s.2(day):sample_typeRectal.1                       1.00     1659.    1558.
## 179 s.2(day):sample_typeRectal.2                       1.00     1829.    1785.
## 180 s.2(day):sample_typeRectal.3                       1.00     1690.    1848.
## 181 s.2(day):sample_typeRectal.4                       1.00     1441.    1479.
## 182 s.2(day_of_year):sample_typeFecal.1                1.00     1751.    2074.
## 183 s.2(day_of_year):sample_typeFecal.2                1.00     1426.     928.
## 184 s.2(day_of_year):sample_typeFecal.3                1.00     1458.     880.
## 185 s.2(day_of_year):sample_typeRectal.1               1.00     1508.    1799.
## 186 s.2(day_of_year):sample_typeRectal.2               1.00     1571.    1324.
## 187 s.2(day_of_year):sample_typeRectal.3               1.00     1292.    1180.
```

