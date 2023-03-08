---
title: "Model Diagnostics"
author: "Noam Ross"
date: "2023-03-08"
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
  posterior_stats,
  dat_prepped
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
## <environment: 0x7f7f2a73d608>
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
## <environment: 0x7f7f2a73d608>
## ~s(sample_type, bs = "re", by = dummy_rectal) + s(day, bs = "tp", 
##     k = 5, by = sample_type, m = 2) + s(day_of_year, bs = "cc", 
##     by = sample_type, k = 5, m = 2)
## <environment: 0x7f7f2a73d608>
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
## s(sample_type):dummy_rectal1                    2.899e-05  2.000  0.000 0.789330    
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
## Gradient range [-0.0002321923,0.0001830841]
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
for (i in seq_len(length(multinomial_model$smooth) + sum(multinomial_model$nsdf) - 3)) {
  plot(multinomial_model, scale = 0, select = i, all.terms = TRUE)
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
## [1] 0.1363636 0.1418909 0.1297455 0.1259636
## 
## $rw.accept
## [1] 0.03040000 0.03345455 0.03280000 0.03047273
```

Here is a summary of the dimensions of the MCMC chain output

```r
tibble::tibble(
  dimension = names(dimnames(gam_posterior)),
  value = dim(gam_posterior)
)
```

```
## # A tibble: 3 × 2
##   dimension value
##   <chr>     <int>
## 1 Iteration   138
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
##   1 (Intercept)                                        1.01     250.      109.
##   2 s(sample_type):dummy_rectal1.1                     1.00     518.      506.
##   3 s(sample_type):dummy_rectal1.2                     1.01     380.      472.
##   4 s(day):sample_typeFecal.1                          1.00     448.      531.
##   5 s(day):sample_typeFecal.2                          1.00     449.      467.
##   6 s(day):sample_typeFecal.3                          1.01     422.      446.
##   7 s(day):sample_typeFecal.4                          1.01     432.      503.
##   8 s(day):sample_typeRectal.1                         1.00     483.      474.
##   9 s(day):sample_typeRectal.2                         1.01     369.      440.
##  10 s(day):sample_typeRectal.3                         1.02     356.      487.
##  11 s(day):sample_typeRectal.4                         1.00     433.      401.
##  12 s(day,gender_age):dummy_rectal1.1                  1.01     451.      526.
##  13 s(day,gender_age):dummy_rectal1.2                  1.00     364.      390.
##  14 s(day,gender_age):dummy_rectal1.3                  0.999    513.      541.
##  15 s(day,gender_age):dummy_rectal1.4                  1.01     432.      428.
##  16 s(day,gender_age):dummy_rectal1.5                  1.00     334.      431.
##  17 s(day,gender_age):dummy_rectal1.6                  0.999    455.      467.
##  18 s(day,gender_age):dummy_rectal1.7                  1.00     466.      478.
##  19 s(day,gender_age):dummy_rectal1.8                  1.01     403.      471.
##  20 s(day,gender_age):dummy_rectal1.9                  1.01     511.      493.
##  21 s(day,gender_age):dummy_rectal1.10                 1.01     494.      469.
##  22 s(day,gender_age):dummy_rectal1.11                 1.01     434.      538.
##  23 s(day,gender_age):dummy_rectal1.12                 1.01     405.      468.
##  24 s(day,gender_age):dummy_rectal1.13                 1.01     546.      582.
##  25 s(day,gender_age):dummy_rectal1.14                 1.00     438.      409.
##  26 s(day,gender_age):dummy_rectal1.15                 0.996    536.      540.
##  27 s(day,gender_age):dummy_rectal1.16                 1.00     429.      356.
##  28 s(day,gender_age):dummy_rectal1.17                 1.00     448.      407.
##  29 s(day,gender_age):dummy_rectal1.18                 1.01     385.      280.
##  30 s(day,gender_age):dummy_rectal1.19                 1.01     479.      503.
##  31 s(day,gender_age):dummy_rectal1.20                 1.00     437.      405.
##  32 s(day,gender_age):dummy_rectal1.21                 1.00     532.      499.
##  33 s(day,gender_age):dummy_rectal1.22                 1.01     349.      404.
##  34 s(day,gender_age):dummy_rectal1.23                 1.01     439.      544.
##  35 s(day,gender_age):dummy_rectal1.24                 1.00     564.      509.
##  36 s(day,gender_age):dummy_rectal1.25                 1.01     486.      405.
##  37 s(day_of_year):sample_typeFecal.1                  1.01     347.      441.
##  38 s(day_of_year):sample_typeFecal.2                  1.01     253.      101.
##  39 s(day_of_year):sample_typeFecal.3                  1.01     345.      416.
##  40 s(day_of_year):sample_typeRectal.1                 0.998    439.      470.
##  41 s(day_of_year):sample_typeRectal.2                 1.01     438.      478.
##  42 s(day_of_year):sample_typeRectal.3                 0.998    484.      407.
##  43 s(day_of_year,gender_age):dummy_rectal1.1          1.00     457.      242.
##  44 s(day_of_year,gender_age):dummy_rectal1.2          1.00     490.      464.
##  45 s(day_of_year,gender_age):dummy_rectal1.3          1.01     446.      506.
##  46 s(day_of_year,gender_age):dummy_rectal1.4          0.999    368.      404.
##  47 s(day_of_year,gender_age):dummy_rectal1.5          1.01     458.      541.
##  48 s(day_of_year,gender_age):dummy_rectal1.6          1.00     494.      507.
##  49 s(day_of_year,gender_age):dummy_rectal1.7          1.00     373.      240.
##  50 s(day_of_year,gender_age):dummy_rectal1.8          1.01     368.      467.
##  51 s(day_of_year,gender_age):dummy_rectal1.9          1.00     546.      504.
##  52 s(day_of_year,gender_age):dummy_rectal1.10         1.01     450.      498.
##  53 s(day_of_year,gender_age):dummy_rectal1.11         1.01     533.      635.
##  54 s(day_of_year,gender_age):dummy_rectal1.12         1.01     443.      385.
##  55 s(day_of_year,gender_age):dummy_rectal1.13         1.00     448.      573.
##  56 s(day_of_year,gender_age):dummy_rectal1.14         1.01     420.      392.
##  57 s(day_of_year,gender_age):dummy_rectal1.15         1.00     425.      353.
##  58 s(day_of_year,gender_age):dummy_rectal1.16         1.00     290.      442.
##  59 s(day_of_year,gender_age):dummy_rectal1.17         1.00     368.      470.
##  60 s(day_of_year,gender_age):dummy_rectal1.18         1.01     397.      378.
##  61 s(day_of_year,gender_age):dummy_rectal1.19         1.00     457.      396.
##  62 s(day_of_year,gender_age):dummy_rectal1.20         0.998    466.      539.
##  63 s(fmi_kg_m2):dummy_rectal1.1                       0.999    492.      447.
##  64 s(fmi_kg_m2):dummy_rectal1.2                       1.00     449.      470.
##  65 s(fmi_kg_m2):dummy_rectal1.3                       1.00     366.      386.
##  66 s(fmi_kg_m2):dummy_rectal1.4                       1.00     479.      499.
##  67 s(reproductive_condition):dummy_repro1.1           1.00     477.      426.
##  68 s(reproductive_condition):dummy_repro1.2           0.998    444.      411.
##  69 s(reproductive_condition):dummy_repro1.3           1.00     378.      441.
##  70 s(reproductive_condition):dummy_repro1.4           1.01     526.      345.
##  71 s(frac_subadult):dummy_any_rectal.1                1.00     433.      353.
##  72 s(frac_subadult):dummy_any_rectal.2                1.00     310.      252.
##  73 s(frac_subadult):dummy_any_rectal.3                1.00     484.      500.
##  74 s(frac_subadult):dummy_any_rectal.4                1.00     412.      193.
##  75 s(frac_subadult):dummy_any_rectal.5                1.01     359.      308.
##  76 s(frac_subadult,sample_type):dummy_any_rectal.1    1.00     400.      501.
##  77 s(frac_subadult,sample_type):dummy_any_rectal.2    1.01     476.      491.
##  78 s(frac_subadult,sample_type):dummy_any_rectal.3    1.00     390.      507.
##  79 s(frac_subadult,sample_type):dummy_any_rectal.4    1.00     391.      440.
##  80 s(frac_subadult,sample_type):dummy_any_rectal.5    1.02     432.      461.
##  81 s(frac_subadult,sample_type):dummy_any_rectal.6    1.01     439.      497.
##  82 s(frac_subadult,sample_type):dummy_any_rectal.7    1.00     536.      506.
##  83 s(frac_subadult,sample_type):dummy_any_rectal.8    1.00     475.      532.
##  84 s(frac_subadult,sample_type):dummy_any_rectal.9    1.01     445.      351.
##  85 s(frac_subadult,sample_type):dummy_any_rectal.10   0.995    491.      507.
##  86 (Intercept).1                                      1.01     396.      458.
##  87 s.1(sample_type):dummy_rectal1.1                   1.00     449.      468.
##  88 s.1(sample_type):dummy_rectal1.2                   1.01     292.      341.
##  89 s.1(day):sample_typeFecal.1                        1.00     496.      504.
##  90 s.1(day):sample_typeFecal.2                        1.01     423.      447.
##  91 s.1(day):sample_typeFecal.3                        1.02     428.      361.
##  92 s.1(day):sample_typeFecal.4                        0.999    475.      370.
##  93 s.1(day):sample_typeRectal.1                       1.00     487.      493.
##  94 s.1(day):sample_typeRectal.2                       1.01     583.      543.
##  95 s.1(day):sample_typeRectal.3                       1.01     580.      585.
##  96 s.1(day):sample_typeRectal.4                       1.01     496.      578.
##  97 s.1(day,gender_age):dummy_rectal1.1                0.999    410.      473.
##  98 s.1(day,gender_age):dummy_rectal1.2                0.998    503.      500.
##  99 s.1(day,gender_age):dummy_rectal1.3                1.00     421.      464.
## 100 s.1(day,gender_age):dummy_rectal1.4                1.01     347.      205.
## 101 s.1(day,gender_age):dummy_rectal1.5                1.01     504.      507.
## 102 s.1(day,gender_age):dummy_rectal1.6                1.01     329.      230.
## 103 s.1(day,gender_age):dummy_rectal1.7                1.01     365.      439.
## 104 s.1(day,gender_age):dummy_rectal1.8                0.999    518.      330.
## 105 s.1(day,gender_age):dummy_rectal1.9                1.00     464.      464.
## 106 s.1(day,gender_age):dummy_rectal1.10               1.01     378.      420.
## 107 s.1(day,gender_age):dummy_rectal1.11               0.999    448.      345.
## 108 s.1(day,gender_age):dummy_rectal1.12               1.00     497.      437.
## 109 s.1(day,gender_age):dummy_rectal1.13               1.00     388.      505.
## 110 s.1(day,gender_age):dummy_rectal1.14               1.01     405.      271.
## 111 s.1(day,gender_age):dummy_rectal1.15               1.01     463.      490.
## 112 s.1(day,gender_age):dummy_rectal1.16               1.00     468.      414.
## 113 s.1(day,gender_age):dummy_rectal1.17               1.00     511.      468.
## 114 s.1(day,gender_age):dummy_rectal1.18               1.00     442.      507.
## 115 s.1(day,gender_age):dummy_rectal1.19               1.01     431.      336.
## 116 s.1(day,gender_age):dummy_rectal1.20               1.00     380.      381.
## 117 s.1(day,gender_age):dummy_rectal1.21               1.00     427.      427.
## 118 s.1(day,gender_age):dummy_rectal1.22               1.00     593.      505.
## 119 s.1(day,gender_age):dummy_rectal1.23               1.01     424.      204.
## 120 s.1(day,gender_age):dummy_rectal1.24               1.01     460.      406.
## 121 s.1(day,gender_age):dummy_rectal1.25               1.00     425.      501.
## 122 s.1(day_of_year):sample_typeFecal.1                1.00     411.      338.
## 123 s.1(day_of_year):sample_typeFecal.2                1.01     473.      393.
## 124 s.1(day_of_year):sample_typeFecal.3                1.00     463.      522.
## 125 s.1(day_of_year):sample_typeRectal.1               0.998    381.      503.
## 126 s.1(day_of_year):sample_typeRectal.2               1.01     322.      345.
## 127 s.1(day_of_year):sample_typeRectal.3               0.999    416.      368.
## 128 s.1(day_of_year,gender_age):dummy_rectal1.1        1.00     411.      505.
## 129 s.1(day_of_year,gender_age):dummy_rectal1.2        1.00     447.      471.
## 130 s.1(day_of_year,gender_age):dummy_rectal1.3        1.01     533.      544.
## 131 s.1(day_of_year,gender_age):dummy_rectal1.4        1.00     379.      581.
## 132 s.1(day_of_year,gender_age):dummy_rectal1.5        1.00     378.      345.
## 133 s.1(day_of_year,gender_age):dummy_rectal1.6        1.00     405.      363.
## 134 s.1(day_of_year,gender_age):dummy_rectal1.7        1.00     482.      480.
## 135 s.1(day_of_year,gender_age):dummy_rectal1.8        1.01     519.      544.
## 136 s.1(day_of_year,gender_age):dummy_rectal1.9        1.01     324.      465.
## 137 s.1(day_of_year,gender_age):dummy_rectal1.10       1.00     498.      494.
## 138 s.1(day_of_year,gender_age):dummy_rectal1.11       1.00     438.      487.
## 139 s.1(day_of_year,gender_age):dummy_rectal1.12       1.01     504.      525.
## 140 s.1(day_of_year,gender_age):dummy_rectal1.13       0.998    576.      578.
## 141 s.1(day_of_year,gender_age):dummy_rectal1.14       1.00     507.      393.
## 142 s.1(day_of_year,gender_age):dummy_rectal1.15       1.00     377.      253.
## 143 s.1(day_of_year,gender_age):dummy_rectal1.16       1.01     519.      470.
## 144 s.1(day_of_year,gender_age):dummy_rectal1.17       1.00     414.      440.
## 145 s.1(day_of_year,gender_age):dummy_rectal1.18       1.00     532.      474.
## 146 s.1(day_of_year,gender_age):dummy_rectal1.19       1.01     377.      392.
## 147 s.1(day_of_year,gender_age):dummy_rectal1.20       1.00     516.      502.
## 148 s.1(fmi_kg_m2):dummy_rectal1.1                     1.00     471.      444.
## 149 s.1(fmi_kg_m2):dummy_rectal1.2                     1.00     504.      473.
## 150 s.1(fmi_kg_m2):dummy_rectal1.3                     1.00     417.      443.
## 151 s.1(fmi_kg_m2):dummy_rectal1.4                     1.00     557.      543.
## 152 s.1(reproductive_condition):dummy_repro1.1         1.00     391.      336.
## 153 s.1(reproductive_condition):dummy_repro1.2         1.00     470.      385.
## 154 s.1(reproductive_condition):dummy_repro1.3         1.00     450.      540.
## 155 s.1(reproductive_condition):dummy_repro1.4         1.01     356.      389.
## 156 s.1(frac_subadult):dummy_any_rectal.1              1.01     445.      264.
## 157 s.1(frac_subadult):dummy_any_rectal.2              1.01     361.      310.
## 158 s.1(frac_subadult):dummy_any_rectal.3              1.00     450.      271.
## 159 s.1(frac_subadult):dummy_any_rectal.4              1.01     461.      313.
## 160 s.1(frac_subadult):dummy_any_rectal.5              1.01     410.      383.
## 161 s.1(frac_subadult,sample_type):dummy_any_rectal.1  1.00     490.      430.
## 162 s.1(frac_subadult,sample_type):dummy_any_rectal.2  1.01     435.      344.
## 163 s.1(frac_subadult,sample_type):dummy_any_rectal.3  1.01     440.      400.
## 164 s.1(frac_subadult,sample_type):dummy_any_rectal.4  1.01     401.      468.
## 165 s.1(frac_subadult,sample_type):dummy_any_rectal.5  1.00     491.      424.
## 166 s.1(frac_subadult,sample_type):dummy_any_rectal.6  1.00     431.      502.
## 167 s.1(frac_subadult,sample_type):dummy_any_rectal.7  1.00     452.      402.
## 168 s.1(frac_subadult,sample_type):dummy_any_rectal.8  0.998    504.      558.
## 169 s.1(frac_subadult,sample_type):dummy_any_rectal.9  1.01     478.      470.
## 170 s.1(frac_subadult,sample_type):dummy_any_rectal.10 0.999    485.      385.
## 171 (Intercept).2                                      1.00     334.      352.
## 172 s.2(sample_type):dummy_rectal1.1                   1.01     466.      494.
## 173 s.2(sample_type):dummy_rectal1.2                   1.01     427.      477.
## 174 s.2(day):sample_typeFecal.1                        1.00     450.      417.
## 175 s.2(day):sample_typeFecal.2                        1.01     398.      531.
## 176 s.2(day):sample_typeFecal.3                        1.01     422.      537.
## 177 s.2(day):sample_typeFecal.4                        1.01     373.      418.
## 178 s.2(day):sample_typeRectal.1                       1.00     468.      544.
## 179 s.2(day):sample_typeRectal.2                       1.01     383.      421.
## 180 s.2(day):sample_typeRectal.3                       1.01     386.      280.
## 181 s.2(day):sample_typeRectal.4                       1.04      79.2     133.
## 182 s.2(day_of_year):sample_typeFecal.1                1.01     458.      437.
## 183 s.2(day_of_year):sample_typeFecal.2                1.01     303.      350.
## 184 s.2(day_of_year):sample_typeFecal.3                1.00     456.      358.
## 185 s.2(day_of_year):sample_typeRectal.1               1.01     446.      382.
## 186 s.2(day_of_year):sample_typeRectal.2               1.01     360.      356.
## 187 s.2(day_of_year):sample_typeRectal.3               1.01     346.      328.
```

