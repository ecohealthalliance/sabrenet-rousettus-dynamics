---
title: "Model Diagnostics"
author: "Noam Ross"
date: "2023-05-02"
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
library(gt)
options(width = 250)
```


```r
tar_load(c(
  multinomial_model,
  multinomial_model_alt,
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
##     m = 1) + s(day_of_year, bs = "cc", by = sample_type, k = 5, 
##     m = 2) + s(day_of_year, gender_age, bs = "fs", xt = list(bs = "cc"), 
##     k = 5, by = dummy_rectal, m = 1) + s(fmi_normalized, k = 5, 
##     bs = "tp", by = dummy_rectal) + s(reproductive_condition, 
##     bs = "re", by = dummy_repro)
## <environment: 0x7fd39e46f590>
## ~s(sample_type, bs = "re", by = dummy_rectal) + s(day, bs = "tp", 
##     k = 5, by = sample_type, m = 2) + s(day, gender_age, bs = "fs", 
##     k = 5, xt = list(bs = "tp"), by = dummy_rectal, m = 1) + 
##     s(day_of_year, bs = "cc", by = sample_type, k = 5, m = 2) + 
##     s(day_of_year, gender_age, bs = "fs", xt = list(bs = "cc"), 
##         k = 5, by = dummy_rectal, m = 1) + s(fmi_normalized, 
##     k = 5, bs = "tp", by = dummy_rectal) + s(reproductive_condition, 
##     bs = "re", by = dummy_repro)
## <environment: 0x7fd39e46f590>
## ~s(sample_type, bs = "re", by = dummy_rectal) + s(day, bs = "tp", 
##     k = 5, by = sample_type, m = 2) + s(day_of_year, bs = "cc", 
##     by = sample_type, k = 5, m = 2)
## <environment: 0x7fd39e46f590>
## 
## Parametric coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)    -3.8526     0.2441 -15.784   <2e-16 ***
## (Intercept).1  -2.4051     0.1467 -16.391   <2e-16 ***
## (Intercept).2  -6.0192     0.6724  -8.953   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##                                                 edf Ref.df Chi.sq  p-value    
## s(sample_type):dummy_rectal1              1.807e-04  1.000  0.000 0.515589    
## s(day):sample_typeFecal                   2.574e+00  2.885  4.874 0.173261    
## s(day):sample_typeRectal                  1.000e+00  1.000  0.161 0.688685    
## s(day,gender_age):dummy_rectal1           1.219e-04 22.000  0.000 0.905557    
## s(day_of_year):sample_typeFecal           2.247e+00  3.000 24.721 7.52e-07 ***
## s(day_of_year):sample_typeRectal          1.351e+00  3.000  3.923 0.048421 *  
## s(day_of_year,gender_age):dummy_rectal1   1.902e-04 16.000  0.000 0.900047    
## s(fmi_normalized):dummy_rectal1           1.000e+00  1.000  3.184 0.074388 .  
## s(reproductive_condition):dummy_repro1    1.019e-04  3.000  0.000 0.911317    
## s.1(sample_type):dummy_rectal1            7.222e-05  1.000  0.000 0.458759    
## s.1(day):sample_typeFecal                 2.339e+00  2.689 13.628 0.001508 ** 
## s.1(day):sample_typeRectal                2.629e+00  2.873  6.911 0.055820 .  
## s.1(day,gender_age):dummy_rectal1         2.112e-04 20.000  0.000 0.866381    
## s.1(day_of_year):sample_typeFecal         2.412e+00  3.000 38.195  < 2e-16 ***
## s.1(day_of_year):sample_typeRectal        2.074e+00  3.000  9.862 0.000529 ***
## s.1(day_of_year,gender_age):dummy_rectal1 5.080e-04 16.000  0.000 0.802830    
## s.1(fmi_normalized):dummy_rectal1         1.000e+00  1.000  1.927 0.165134    
## s.1(reproductive_condition):dummy_repro1  1.833e+00  3.000  5.754 0.025429 *  
## s.2(sample_type):dummy_rectal1            1.475e-05  1.000  0.000 0.758139    
## s.2(day):sample_typeFecal                 1.000e+00  1.000  0.009 0.923636    
## s.2(day):sample_typeRectal                1.000e+00  1.000  0.344 0.557249    
## s.2(day_of_year):sample_typeFecal         2.073e+00  3.000 18.059 4.35e-05 ***
## s.2(day_of_year):sample_typeRectal        1.478e+00  3.000  4.624 0.036269 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Deviance explained = 14.6%
## -REML = -571.32  Scale est. = 1         n = 1430
```

### Alterate Model Summary

This is an alternate model fomulation with simple singel random effects of age/sex category
rather than time-varying ones.  This is used to check if there age/sex effects are
just insignificant because of the high complexity of the time-varying smooths.


```r
summary(multinomial_model_alt)
```

```
## 
## Family: multinom 
## Link function: 
## 
## Formula:
## outcome ~ s(sample_type, bs = "re", by = dummy_rectal) + s(day, 
##     bs = "tp", k = 5, by = sample_type, m = 2) + s(day_of_year, 
##     bs = "cc", by = sample_type, k = 5, m = 2) + s(gender_age, 
##     bs = "re", by = dummy_rectal) + s(fmi_normalized, k = 5, 
##     bs = "tp", by = dummy_rectal) + s(reproductive_condition, 
##     bs = "re", by = dummy_repro)
## <environment: 0x7fd39e3233c0>
## ~s(sample_type, bs = "re", by = dummy_rectal) + s(day, bs = "tp", 
##     k = 5, by = sample_type, m = 2) + s(day_of_year, bs = "cc", 
##     by = sample_type, k = 5, m = 2) + s(gender_age, bs = "re", 
##     by = dummy_rectal) + s(fmi_normalized, k = 5, bs = "tp", 
##     by = dummy_rectal) + s(reproductive_condition, bs = "re", 
##     by = dummy_repro)
## <environment: 0x7fd39e3233c0>
## ~s(sample_type, bs = "re", by = dummy_rectal) + s(day, bs = "tp", 
##     k = 5, by = sample_type, m = 2) + s(day_of_year, bs = "cc", 
##     by = sample_type, k = 5, m = 2)
## <environment: 0x7fd39e3233c0>
## 
## Parametric coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)    -3.8528     0.2441 -15.782   <2e-16 ***
## (Intercept).1  -2.4051     0.1467 -16.391   <2e-16 ***
## (Intercept).2  -6.0192     0.6724  -8.953   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##                                                edf Ref.df Chi.sq  p-value    
## s(sample_type):dummy_rectal1             2.109e-04  2.000  0.000 0.516073    
## s(day):sample_typeFecal                  2.575e+00  2.886  4.878 0.172907    
## s(day):sample_typeRectal                 1.000e+00  1.000  0.160 0.688800    
## s(day_of_year):sample_typeFecal          2.247e+00  3.000 24.725 7.50e-07 ***
## s(day_of_year):sample_typeRectal         1.351e+00  3.000  3.923 0.048417 *  
## s(gender_age):dummy_rectal1              1.742e-04  4.000  0.000 0.976698    
## s(fmi_normalized):dummy_rectal1          1.000e+00  1.000  3.184 0.074371 .  
## s(reproductive_condition):dummy_repro1   9.877e-05  3.000  0.000 0.911349    
## s.1(sample_type):dummy_rectal1           2.896e-04  1.000  0.000 0.458759    
## s.1(day):sample_typeFecal                2.339e+00  2.689 13.628 0.001508 ** 
## s.1(day):sample_typeRectal               2.629e+00  2.873  6.909 0.055847 .  
## s.1(day_of_year):sample_typeFecal        2.412e+00  3.000 38.196  < 2e-16 ***
## s.1(day_of_year):sample_typeRectal       2.075e+00  3.000  9.863 0.000529 ***
## s.1(gender_age):dummy_rectal1            7.434e-04  4.000  0.000 0.659272    
## s.1(fmi_normalized):dummy_rectal1        1.000e+00  1.000  1.927 0.165131    
## s.1(reproductive_condition):dummy_repro1 1.833e+00  3.000  5.754 0.025432 *  
## s.2(sample_type):dummy_rectal1           1.858e-05  1.000  0.000 0.758138    
## s.2(day):sample_typeFecal                1.000e+00  1.000  0.009 0.923646    
## s.2(day):sample_typeRectal               1.000e+00  1.000  0.345 0.557247    
## s.2(day_of_year):sample_typeFecal        2.073e+00  3.000 18.059 4.35e-05 ***
## s.2(day_of_year):sample_typeRectal       1.478e+00  3.000  4.624 0.036270 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Deviance explained = 14.6%
## -REML = -571.32  Scale est. = 1         n = 1430
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

![](model_diagnostics_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```
## 
## Method: REML   Optimizer: efs
## $iter
## [1] 51
## 
## $score.hist
##  [1] -544.2658 -557.4188 -562.9083 -565.1258 -566.6785 -567.9323 -568.8746 -569.5572 -570.0543 -570.4158 -570.6714 -570.8419 -570.9473 -571.0080 -571.0413 -571.0592 -571.0687 -571.0738 -571.0768 -571.0786 -571.0799 -571.0811 -571.0822 -571.0835
## [25] -571.0850 -571.0868 -571.0888 -571.0913 -571.0942 -571.0977 -571.1019 -571.1070 -571.1131 -571.1206 -571.1296 -571.1405 -571.1536 -571.1693 -571.1874 -571.2077 -571.2293 -571.2507 -571.2699 -571.2855 -571.2971 -571.3049 -571.3098 -571.3127
## [49] -571.3144 -571.3152 -571.3160
## 
## $conv
## [1] "full convergence"
## 
## Model rank =  157 / 157 
## 
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
## 
##                                                 k'      edf k-index p-value
## s(sample_type):dummy_rectal1              2.00e+00 1.81e-04      NA      NA
## s(day):sample_typeFecal                   4.00e+00 2.57e+00    0.95    0.15
## s(day):sample_typeRectal                  4.00e+00 1.00e+00    0.95    0.25
## s(day,gender_age):dummy_rectal1           2.50e+01 1.22e-04    0.95    0.14
## s(day_of_year):sample_typeFecal           3.00e+00 2.25e+00    0.95    0.21
## s(day_of_year):sample_typeRectal          3.00e+00 1.35e+00    0.95    0.18
## s(day_of_year,gender_age):dummy_rectal1   2.00e+01 1.90e-04    0.95    0.20
## s(fmi_normalized):dummy_rectal1           4.00e+00 1.00e+00    0.97    0.39
## s(reproductive_condition):dummy_repro1    4.00e+00 1.02e-04      NA      NA
## s.1(sample_type):dummy_rectal1            2.00e+00 7.22e-05      NA      NA
## s.1(day):sample_typeFecal                 4.00e+00 2.34e+00    0.95    0.22
## s.1(day):sample_typeRectal                4.00e+00 2.63e+00    0.95    0.18
## s.1(day,gender_age):dummy_rectal1         2.50e+01 2.11e-04    0.95    0.21
## s.1(day_of_year):sample_typeFecal         3.00e+00 2.41e+00    0.95    0.18
## s.1(day_of_year):sample_typeRectal        3.00e+00 2.07e+00    0.95    0.21
## s.1(day_of_year,gender_age):dummy_rectal1 2.00e+01 5.08e-04    0.95    0.21
## s.1(fmi_normalized):dummy_rectal1         4.00e+00 1.00e+00    0.97    0.43
## s.1(reproductive_condition):dummy_repro1  4.00e+00 1.83e+00      NA      NA
## s.2(sample_type):dummy_rectal1            2.00e+00 1.48e-05      NA      NA
## s.2(day):sample_typeFecal                 4.00e+00 1.00e+00    0.95    0.16
## s.2(day):sample_typeRectal                4.00e+00 1.00e+00    0.95    0.15
## s.2(day_of_year):sample_typeFecal         3.00e+00 2.07e+00    0.95    0.24
## s.2(day_of_year):sample_typeRectal        3.00e+00 1.48e+00    0.95    0.20
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

<img src="model_diagnostics_files/figure-html/unnamed-chunk-4-1.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-2.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-3.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-4.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-5.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-6.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-7.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-8.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-9.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-10.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-11.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-12.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-13.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-14.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-15.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-16.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-17.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-18.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-19.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-20.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-21.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-22.png" width="33%" /><img src="model_diagnostics_files/figure-html/unnamed-chunk-4-23.png" width="33%" />

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
## [1] 0.15905 0.16605 0.17300 0.18320
## 
## $rw.accept
## [1] 0.16190 0.16190 0.16505 0.16145
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
## 1 Iteration   100
## 2 Chain         4
## 3 Parameter   157
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

Note that for coefficient values < 1e-6, mixing isn't very relevant.


```r
posterior_stats |> 
  gt() |> 
  fmt_number(columns = c("ess_bulk", "ess_tail"), decimals = 0) |> 
  fmt_number(2:7, n_sigfig = 2) |> 
  fmt_number("rhat", n_sigfig = 3) |>  
  cols_align_decimal(2:8) 
```

```{=html}
<div id="zajimgnezv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zajimgnezv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#zajimgnezv thead, #zajimgnezv tbody, #zajimgnezv tfoot, #zajimgnezv tr, #zajimgnezv td, #zajimgnezv th {
  border-style: none;
}

#zajimgnezv p {
  margin: 0;
  padding: 0;
}

#zajimgnezv .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#zajimgnezv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#zajimgnezv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#zajimgnezv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#zajimgnezv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zajimgnezv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zajimgnezv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zajimgnezv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#zajimgnezv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#zajimgnezv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zajimgnezv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zajimgnezv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#zajimgnezv .gt_spanner_row {
  border-bottom-style: hidden;
}

#zajimgnezv .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#zajimgnezv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#zajimgnezv .gt_from_md > :first-child {
  margin-top: 0;
}

#zajimgnezv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zajimgnezv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#zajimgnezv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#zajimgnezv .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#zajimgnezv .gt_row_group_first td {
  border-top-width: 2px;
}

#zajimgnezv .gt_row_group_first th {
  border-top-width: 2px;
}

#zajimgnezv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zajimgnezv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#zajimgnezv .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#zajimgnezv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zajimgnezv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zajimgnezv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zajimgnezv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#zajimgnezv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zajimgnezv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zajimgnezv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zajimgnezv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zajimgnezv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zajimgnezv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zajimgnezv .gt_left {
  text-align: left;
}

#zajimgnezv .gt_center {
  text-align: center;
}

#zajimgnezv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zajimgnezv .gt_font_normal {
  font-weight: normal;
}

#zajimgnezv .gt_font_bold {
  font-weight: bold;
}

#zajimgnezv .gt_font_italic {
  font-style: italic;
}

#zajimgnezv .gt_super {
  font-size: 65%;
}

#zajimgnezv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#zajimgnezv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#zajimgnezv .gt_indent_1 {
  text-indent: 5px;
}

#zajimgnezv .gt_indent_2 {
  text-indent: 10px;
}

#zajimgnezv .gt_indent_3 {
  text-indent: 15px;
}

#zajimgnezv .gt_indent_4 {
  text-indent: 20px;
}

#zajimgnezv .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="variable">variable</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="mean">mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="median">median</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sd">sd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="mad">mad</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="q5">q5</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="q95">q95</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="rhat">rhat</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="ess_bulk">ess_bulk</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="ess_tail">ess_tail</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="variable" class="gt_row gt_left">(Intercept)</td>
<td headers="mean" class="gt_row gt_right">−4.0       </td>
<td headers="median" class="gt_row gt_right">−4.0      </td>
<td headers="sd" class="gt_row gt_right">0.25   </td>
<td headers="mad" class="gt_row gt_right">0.24   </td>
<td headers="q5" class="gt_row gt_right">−4.4    </td>
<td headers="q95" class="gt_row gt_right">−3.6   </td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">407</td>
<td headers="ess_tail" class="gt_row gt_right">304</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(sample_type):dummy_rectal1.1</td>
<td headers="mean" class="gt_row gt_right">−0.00011   </td>
<td headers="median" class="gt_row gt_right">−0.00032  </td>
<td headers="sd" class="gt_row gt_right">0.0070 </td>
<td headers="mad" class="gt_row gt_right">0.0066 </td>
<td headers="q5" class="gt_row gt_right">−0.012  </td>
<td headers="q95" class="gt_row gt_right"> 0.011 </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">385</td>
<td headers="ess_tail" class="gt_row gt_right">348</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(sample_type):dummy_rectal1.2</td>
<td headers="mean" class="gt_row gt_right">−0.00019   </td>
<td headers="median" class="gt_row gt_right">−0.00067  </td>
<td headers="sd" class="gt_row gt_right">0.0067 </td>
<td headers="mad" class="gt_row gt_right">0.0069 </td>
<td headers="q5" class="gt_row gt_right">−0.011  </td>
<td headers="q95" class="gt_row gt_right"> 0.011 </td>
<td headers="rhat" class="gt_row gt_right">0.997</td>
<td headers="ess_bulk" class="gt_row gt_right">375</td>
<td headers="ess_tail" class="gt_row gt_right">412</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day):sample_typeFecal.1</td>
<td headers="mean" class="gt_row gt_right"> 2.2       </td>
<td headers="median" class="gt_row gt_right"> 2.2      </td>
<td headers="sd" class="gt_row gt_right">1.1    </td>
<td headers="mad" class="gt_row gt_right">1.2    </td>
<td headers="q5" class="gt_row gt_right"> 0.23   </td>
<td headers="q95" class="gt_row gt_right"> 3.9   </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">408</td>
<td headers="ess_tail" class="gt_row gt_right">369</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day):sample_typeFecal.2</td>
<td headers="mean" class="gt_row gt_right"> 0.46      </td>
<td headers="median" class="gt_row gt_right"> 0.41     </td>
<td headers="sd" class="gt_row gt_right">2.9    </td>
<td headers="mad" class="gt_row gt_right">3.1    </td>
<td headers="q5" class="gt_row gt_right">−4.2    </td>
<td headers="q95" class="gt_row gt_right"> 5.2   </td>
<td headers="rhat" class="gt_row gt_right">0.996</td>
<td headers="ess_bulk" class="gt_row gt_right">362</td>
<td headers="ess_tail" class="gt_row gt_right">413</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day):sample_typeFecal.3</td>
<td headers="mean" class="gt_row gt_right">−0.92      </td>
<td headers="median" class="gt_row gt_right">−1.0      </td>
<td headers="sd" class="gt_row gt_right">3.7    </td>
<td headers="mad" class="gt_row gt_right">4.0    </td>
<td headers="q5" class="gt_row gt_right">−7.0    </td>
<td headers="q95" class="gt_row gt_right"> 5.0   </td>
<td headers="rhat" class="gt_row gt_right">0.998</td>
<td headers="ess_bulk" class="gt_row gt_right">362</td>
<td headers="ess_tail" class="gt_row gt_right">403</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day):sample_typeFecal.4</td>
<td headers="mean" class="gt_row gt_right">−2.6       </td>
<td headers="median" class="gt_row gt_right">−2.7      </td>
<td headers="sd" class="gt_row gt_right">1.3    </td>
<td headers="mad" class="gt_row gt_right">1.3    </td>
<td headers="q5" class="gt_row gt_right">−4.6    </td>
<td headers="q95" class="gt_row gt_right">−0.44  </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">401</td>
<td headers="ess_tail" class="gt_row gt_right">416</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day):sample_typeRectal.1</td>
<td headers="mean" class="gt_row gt_right">−0.00020   </td>
<td headers="median" class="gt_row gt_right">−0.00033  </td>
<td headers="sd" class="gt_row gt_right">0.0035 </td>
<td headers="mad" class="gt_row gt_right">0.0038 </td>
<td headers="q5" class="gt_row gt_right">−0.0061 </td>
<td headers="q95" class="gt_row gt_right"> 0.0052</td>
<td headers="rhat" class="gt_row gt_right">0.999</td>
<td headers="ess_bulk" class="gt_row gt_right">392</td>
<td headers="ess_tail" class="gt_row gt_right">336</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day):sample_typeRectal.2</td>
<td headers="mean" class="gt_row gt_right"> 0.000094  </td>
<td headers="median" class="gt_row gt_right"> 0.00032  </td>
<td headers="sd" class="gt_row gt_right">0.0056 </td>
<td headers="mad" class="gt_row gt_right">0.0057 </td>
<td headers="q5" class="gt_row gt_right">−0.0090 </td>
<td headers="q95" class="gt_row gt_right"> 0.0084</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">422</td>
<td headers="ess_tail" class="gt_row gt_right">334</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day):sample_typeRectal.3</td>
<td headers="mean" class="gt_row gt_right"> 0.00034   </td>
<td headers="median" class="gt_row gt_right"> 0.00061  </td>
<td headers="sd" class="gt_row gt_right">0.010  </td>
<td headers="mad" class="gt_row gt_right">0.010  </td>
<td headers="q5" class="gt_row gt_right">−0.017  </td>
<td headers="q95" class="gt_row gt_right"> 0.017 </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">399</td>
<td headers="ess_tail" class="gt_row gt_right">413</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day):sample_typeRectal.4</td>
<td headers="mean" class="gt_row gt_right">−0.10      </td>
<td headers="median" class="gt_row gt_right">−0.13     </td>
<td headers="sd" class="gt_row gt_right">0.35   </td>
<td headers="mad" class="gt_row gt_right">0.34   </td>
<td headers="q5" class="gt_row gt_right">−0.66   </td>
<td headers="q95" class="gt_row gt_right"> 0.48  </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">423</td>
<td headers="ess_tail" class="gt_row gt_right">392</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.1</td>
<td headers="mean" class="gt_row gt_right">−0.0000069 </td>
<td headers="median" class="gt_row gt_right"> 0.0000071</td>
<td headers="sd" class="gt_row gt_right">0.0015 </td>
<td headers="mad" class="gt_row gt_right">0.0016 </td>
<td headers="q5" class="gt_row gt_right">−0.0026 </td>
<td headers="q95" class="gt_row gt_right"> 0.0025</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">423</td>
<td headers="ess_tail" class="gt_row gt_right">439</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.2</td>
<td headers="mean" class="gt_row gt_right"> 0.000056  </td>
<td headers="median" class="gt_row gt_right"> 0.000079 </td>
<td headers="sd" class="gt_row gt_right">0.0016 </td>
<td headers="mad" class="gt_row gt_right">0.0015 </td>
<td headers="q5" class="gt_row gt_right">−0.0026 </td>
<td headers="q95" class="gt_row gt_right"> 0.0026</td>
<td headers="rhat" class="gt_row gt_right">0.998</td>
<td headers="ess_bulk" class="gt_row gt_right">456</td>
<td headers="ess_tail" class="gt_row gt_right">414</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.3</td>
<td headers="mean" class="gt_row gt_right"> 0.00018   </td>
<td headers="median" class="gt_row gt_right"> 0.00034  </td>
<td headers="sd" class="gt_row gt_right">0.0016 </td>
<td headers="mad" class="gt_row gt_right">0.0016 </td>
<td headers="q5" class="gt_row gt_right">−0.0025 </td>
<td headers="q95" class="gt_row gt_right"> 0.0027</td>
<td headers="rhat" class="gt_row gt_right">0.999</td>
<td headers="ess_bulk" class="gt_row gt_right">425</td>
<td headers="ess_tail" class="gt_row gt_right">414</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.4</td>
<td headers="mean" class="gt_row gt_right">−0.000044  </td>
<td headers="median" class="gt_row gt_right">−0.00010  </td>
<td headers="sd" class="gt_row gt_right">0.0015 </td>
<td headers="mad" class="gt_row gt_right">0.0015 </td>
<td headers="q5" class="gt_row gt_right">−0.0024 </td>
<td headers="q95" class="gt_row gt_right"> 0.0024</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">572</td>
<td headers="ess_tail" class="gt_row gt_right">370</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.5</td>
<td headers="mean" class="gt_row gt_right"> 0.000037  </td>
<td headers="median" class="gt_row gt_right"> 0.00012  </td>
<td headers="sd" class="gt_row gt_right">0.0021 </td>
<td headers="mad" class="gt_row gt_right">0.0020 </td>
<td headers="q5" class="gt_row gt_right">−0.0035 </td>
<td headers="q95" class="gt_row gt_right"> 0.0035</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">412</td>
<td headers="ess_tail" class="gt_row gt_right">375</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.6</td>
<td headers="mean" class="gt_row gt_right"> 0.000058  </td>
<td headers="median" class="gt_row gt_right"> 0.000050 </td>
<td headers="sd" class="gt_row gt_right">0.0015 </td>
<td headers="mad" class="gt_row gt_right">0.0014 </td>
<td headers="q5" class="gt_row gt_right">−0.0025 </td>
<td headers="q95" class="gt_row gt_right"> 0.0024</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">362</td>
<td headers="ess_tail" class="gt_row gt_right">373</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.7</td>
<td headers="mean" class="gt_row gt_right"> 0.00011   </td>
<td headers="median" class="gt_row gt_right"> 0.00024  </td>
<td headers="sd" class="gt_row gt_right">0.0015 </td>
<td headers="mad" class="gt_row gt_right">0.0016 </td>
<td headers="q5" class="gt_row gt_right">−0.0024 </td>
<td headers="q95" class="gt_row gt_right"> 0.0025</td>
<td headers="rhat" class="gt_row gt_right">0.999</td>
<td headers="ess_bulk" class="gt_row gt_right">414</td>
<td headers="ess_tail" class="gt_row gt_right">415</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.8</td>
<td headers="mean" class="gt_row gt_right"> 0.000049  </td>
<td headers="median" class="gt_row gt_right"> 0.000046 </td>
<td headers="sd" class="gt_row gt_right">0.0015 </td>
<td headers="mad" class="gt_row gt_right">0.0017 </td>
<td headers="q5" class="gt_row gt_right">−0.0025 </td>
<td headers="q95" class="gt_row gt_right"> 0.0025</td>
<td headers="rhat" class="gt_row gt_right">0.997</td>
<td headers="ess_bulk" class="gt_row gt_right">415</td>
<td headers="ess_tail" class="gt_row gt_right">349</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.9</td>
<td headers="mean" class="gt_row gt_right"> 0.000036  </td>
<td headers="median" class="gt_row gt_right"> 0.000074 </td>
<td headers="sd" class="gt_row gt_right">0.0015 </td>
<td headers="mad" class="gt_row gt_right">0.0015 </td>
<td headers="q5" class="gt_row gt_right">−0.0026 </td>
<td headers="q95" class="gt_row gt_right"> 0.0022</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">353</td>
<td headers="ess_tail" class="gt_row gt_right">372</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.10</td>
<td headers="mean" class="gt_row gt_right">−0.0000075 </td>
<td headers="median" class="gt_row gt_right"> 0.000034 </td>
<td headers="sd" class="gt_row gt_right">0.0021 </td>
<td headers="mad" class="gt_row gt_right">0.0023 </td>
<td headers="q5" class="gt_row gt_right">−0.0034 </td>
<td headers="q95" class="gt_row gt_right"> 0.0034</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">383</td>
<td headers="ess_tail" class="gt_row gt_right">461</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.11</td>
<td headers="mean" class="gt_row gt_right"> 0.000068  </td>
<td headers="median" class="gt_row gt_right"> 0.000036 </td>
<td headers="sd" class="gt_row gt_right">0.0015 </td>
<td headers="mad" class="gt_row gt_right">0.0015 </td>
<td headers="q5" class="gt_row gt_right">−0.0023 </td>
<td headers="q95" class="gt_row gt_right"> 0.0025</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">340</td>
<td headers="ess_tail" class="gt_row gt_right">374</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.12</td>
<td headers="mean" class="gt_row gt_right"> 0.000024  </td>
<td headers="median" class="gt_row gt_right"> 0.000013 </td>
<td headers="sd" class="gt_row gt_right">0.0015 </td>
<td headers="mad" class="gt_row gt_right">0.0015 </td>
<td headers="q5" class="gt_row gt_right">−0.0023 </td>
<td headers="q95" class="gt_row gt_right"> 0.0025</td>
<td headers="rhat" class="gt_row gt_right">0.999</td>
<td headers="ess_bulk" class="gt_row gt_right">401</td>
<td headers="ess_tail" class="gt_row gt_right">373</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.13</td>
<td headers="mean" class="gt_row gt_right"> 0.000037  </td>
<td headers="median" class="gt_row gt_right"> 0.000041 </td>
<td headers="sd" class="gt_row gt_right">0.0017 </td>
<td headers="mad" class="gt_row gt_right">0.0017 </td>
<td headers="q5" class="gt_row gt_right">−0.0026 </td>
<td headers="q95" class="gt_row gt_right"> 0.0028</td>
<td headers="rhat" class="gt_row gt_right">0.996</td>
<td headers="ess_bulk" class="gt_row gt_right">332</td>
<td headers="ess_tail" class="gt_row gt_right">406</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.14</td>
<td headers="mean" class="gt_row gt_right"> 0.000041  </td>
<td headers="median" class="gt_row gt_right"> 0.000072 </td>
<td headers="sd" class="gt_row gt_right">0.0015 </td>
<td headers="mad" class="gt_row gt_right">0.0015 </td>
<td headers="q5" class="gt_row gt_right">−0.0024 </td>
<td headers="q95" class="gt_row gt_right"> 0.0025</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">327</td>
<td headers="ess_tail" class="gt_row gt_right">282</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.15</td>
<td headers="mean" class="gt_row gt_right"> 0.000057  </td>
<td headers="median" class="gt_row gt_right">−0.00010  </td>
<td headers="sd" class="gt_row gt_right">0.0021 </td>
<td headers="mad" class="gt_row gt_right">0.0021 </td>
<td headers="q5" class="gt_row gt_right">−0.0033 </td>
<td headers="q95" class="gt_row gt_right"> 0.0035</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">366</td>
<td headers="ess_tail" class="gt_row gt_right">373</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.16</td>
<td headers="mean" class="gt_row gt_right">−0.000021  </td>
<td headers="median" class="gt_row gt_right">−0.000013 </td>
<td headers="sd" class="gt_row gt_right">0.0015 </td>
<td headers="mad" class="gt_row gt_right">0.0014 </td>
<td headers="q5" class="gt_row gt_right">−0.0025 </td>
<td headers="q95" class="gt_row gt_right"> 0.0024</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">451</td>
<td headers="ess_tail" class="gt_row gt_right">297</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.17</td>
<td headers="mean" class="gt_row gt_right"> 0.000073  </td>
<td headers="median" class="gt_row gt_right"> 0.000034 </td>
<td headers="sd" class="gt_row gt_right">0.0016 </td>
<td headers="mad" class="gt_row gt_right">0.0016 </td>
<td headers="q5" class="gt_row gt_right">−0.0024 </td>
<td headers="q95" class="gt_row gt_right"> 0.0027</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">363</td>
<td headers="ess_tail" class="gt_row gt_right">446</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.18</td>
<td headers="mean" class="gt_row gt_right">−0.000089  </td>
<td headers="median" class="gt_row gt_right"> 0.000041 </td>
<td headers="sd" class="gt_row gt_right">0.0015 </td>
<td headers="mad" class="gt_row gt_right">0.0014 </td>
<td headers="q5" class="gt_row gt_right">−0.0027 </td>
<td headers="q95" class="gt_row gt_right"> 0.0021</td>
<td headers="rhat" class="gt_row gt_right">0.997</td>
<td headers="ess_bulk" class="gt_row gt_right">395</td>
<td headers="ess_tail" class="gt_row gt_right">375</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.19</td>
<td headers="mean" class="gt_row gt_right"> 0.0000071 </td>
<td headers="median" class="gt_row gt_right"> 0.000025 </td>
<td headers="sd" class="gt_row gt_right">0.0015 </td>
<td headers="mad" class="gt_row gt_right">0.0016 </td>
<td headers="q5" class="gt_row gt_right">−0.0024 </td>
<td headers="q95" class="gt_row gt_right"> 0.0025</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">432</td>
<td headers="ess_tail" class="gt_row gt_right">414</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.20</td>
<td headers="mean" class="gt_row gt_right"> 0.00019   </td>
<td headers="median" class="gt_row gt_right"> 0.000074 </td>
<td headers="sd" class="gt_row gt_right">0.0022 </td>
<td headers="mad" class="gt_row gt_right">0.0023 </td>
<td headers="q5" class="gt_row gt_right">−0.0033 </td>
<td headers="q95" class="gt_row gt_right"> 0.0040</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">432</td>
<td headers="ess_tail" class="gt_row gt_right">414</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.21</td>
<td headers="mean" class="gt_row gt_right"> 0.000046  </td>
<td headers="median" class="gt_row gt_right"> 0.00015  </td>
<td headers="sd" class="gt_row gt_right">0.0015 </td>
<td headers="mad" class="gt_row gt_right">0.0015 </td>
<td headers="q5" class="gt_row gt_right">−0.0024 </td>
<td headers="q95" class="gt_row gt_right"> 0.0022</td>
<td headers="rhat" class="gt_row gt_right">0.996</td>
<td headers="ess_bulk" class="gt_row gt_right">356</td>
<td headers="ess_tail" class="gt_row gt_right">447</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.22</td>
<td headers="mean" class="gt_row gt_right"> 0.000045  </td>
<td headers="median" class="gt_row gt_right"> 0.000081 </td>
<td headers="sd" class="gt_row gt_right">0.0015 </td>
<td headers="mad" class="gt_row gt_right">0.0015 </td>
<td headers="q5" class="gt_row gt_right">−0.0026 </td>
<td headers="q95" class="gt_row gt_right"> 0.0024</td>
<td headers="rhat" class="gt_row gt_right">0.997</td>
<td headers="ess_bulk" class="gt_row gt_right">394</td>
<td headers="ess_tail" class="gt_row gt_right">394</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.23</td>
<td headers="mean" class="gt_row gt_right"> 0.00015   </td>
<td headers="median" class="gt_row gt_right"> 0.00016  </td>
<td headers="sd" class="gt_row gt_right">0.0016 </td>
<td headers="mad" class="gt_row gt_right">0.0017 </td>
<td headers="q5" class="gt_row gt_right">−0.0025 </td>
<td headers="q95" class="gt_row gt_right"> 0.0026</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">368</td>
<td headers="ess_tail" class="gt_row gt_right">361</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.24</td>
<td headers="mean" class="gt_row gt_right"> 0.000095  </td>
<td headers="median" class="gt_row gt_right">−0.000024 </td>
<td headers="sd" class="gt_row gt_right">0.0016 </td>
<td headers="mad" class="gt_row gt_right">0.0017 </td>
<td headers="q5" class="gt_row gt_right">−0.0025 </td>
<td headers="q95" class="gt_row gt_right"> 0.0028</td>
<td headers="rhat" class="gt_row gt_right">0.998</td>
<td headers="ess_bulk" class="gt_row gt_right">430</td>
<td headers="ess_tail" class="gt_row gt_right">372</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day,gender_age):dummy_rectal1.25</td>
<td headers="mean" class="gt_row gt_right"> 0.000089  </td>
<td headers="median" class="gt_row gt_right"> 0.00012  </td>
<td headers="sd" class="gt_row gt_right">0.0022 </td>
<td headers="mad" class="gt_row gt_right">0.0022 </td>
<td headers="q5" class="gt_row gt_right">−0.0035 </td>
<td headers="q95" class="gt_row gt_right"> 0.0038</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">482</td>
<td headers="ess_tail" class="gt_row gt_right">371</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year):sample_typeFecal.1</td>
<td headers="mean" class="gt_row gt_right"> 0.66      </td>
<td headers="median" class="gt_row gt_right"> 0.66     </td>
<td headers="sd" class="gt_row gt_right">0.53   </td>
<td headers="mad" class="gt_row gt_right">0.51   </td>
<td headers="q5" class="gt_row gt_right">−0.20   </td>
<td headers="q95" class="gt_row gt_right"> 1.7   </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">403</td>
<td headers="ess_tail" class="gt_row gt_right">332</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year):sample_typeFecal.2</td>
<td headers="mean" class="gt_row gt_right">−2.2       </td>
<td headers="median" class="gt_row gt_right">−2.2      </td>
<td headers="sd" class="gt_row gt_right">0.81   </td>
<td headers="mad" class="gt_row gt_right">0.79   </td>
<td headers="q5" class="gt_row gt_right">−3.6    </td>
<td headers="q95" class="gt_row gt_right">−0.89  </td>
<td headers="rhat" class="gt_row gt_right">0.999</td>
<td headers="ess_bulk" class="gt_row gt_right">438</td>
<td headers="ess_tail" class="gt_row gt_right">412</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year):sample_typeFecal.3</td>
<td headers="mean" class="gt_row gt_right">−2.0       </td>
<td headers="median" class="gt_row gt_right">−2.0      </td>
<td headers="sd" class="gt_row gt_right">0.59   </td>
<td headers="mad" class="gt_row gt_right">0.57   </td>
<td headers="q5" class="gt_row gt_right">−3.0    </td>
<td headers="q95" class="gt_row gt_right">−1.0   </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">405</td>
<td headers="ess_tail" class="gt_row gt_right">414</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year):sample_typeRectal.1</td>
<td headers="mean" class="gt_row gt_right"> 0.22      </td>
<td headers="median" class="gt_row gt_right"> 0.22     </td>
<td headers="sd" class="gt_row gt_right">0.40   </td>
<td headers="mad" class="gt_row gt_right">0.42   </td>
<td headers="q5" class="gt_row gt_right">−0.38   </td>
<td headers="q95" class="gt_row gt_right"> 0.82  </td>
<td headers="rhat" class="gt_row gt_right">0.999</td>
<td headers="ess_bulk" class="gt_row gt_right">466</td>
<td headers="ess_tail" class="gt_row gt_right">416</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year):sample_typeRectal.2</td>
<td headers="mean" class="gt_row gt_right">−0.50      </td>
<td headers="median" class="gt_row gt_right">−0.51     </td>
<td headers="sd" class="gt_row gt_right">0.45   </td>
<td headers="mad" class="gt_row gt_right">0.44   </td>
<td headers="q5" class="gt_row gt_right">−1.2    </td>
<td headers="q95" class="gt_row gt_right"> 0.24  </td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">360</td>
<td headers="ess_tail" class="gt_row gt_right">388</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year):sample_typeRectal.3</td>
<td headers="mean" class="gt_row gt_right">−0.69      </td>
<td headers="median" class="gt_row gt_right">−0.72     </td>
<td headers="sd" class="gt_row gt_right">0.44   </td>
<td headers="mad" class="gt_row gt_right">0.47   </td>
<td headers="q5" class="gt_row gt_right">−1.4    </td>
<td headers="q95" class="gt_row gt_right"> 0.043 </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">444</td>
<td headers="ess_tail" class="gt_row gt_right">374</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.1</td>
<td headers="mean" class="gt_row gt_right"> 0.000049  </td>
<td headers="median" class="gt_row gt_right"> 0.00019  </td>
<td headers="sd" class="gt_row gt_right">0.0022 </td>
<td headers="mad" class="gt_row gt_right">0.0021 </td>
<td headers="q5" class="gt_row gt_right">−0.0036 </td>
<td headers="q95" class="gt_row gt_right"> 0.0036</td>
<td headers="rhat" class="gt_row gt_right">0.996</td>
<td headers="ess_bulk" class="gt_row gt_right">394</td>
<td headers="ess_tail" class="gt_row gt_right">394</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.2</td>
<td headers="mean" class="gt_row gt_right">−0.00040   </td>
<td headers="median" class="gt_row gt_right">−0.00050  </td>
<td headers="sd" class="gt_row gt_right">0.0023 </td>
<td headers="mad" class="gt_row gt_right">0.0025 </td>
<td headers="q5" class="gt_row gt_right">−0.0039 </td>
<td headers="q95" class="gt_row gt_right"> 0.0033</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">375</td>
<td headers="ess_tail" class="gt_row gt_right">407</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.3</td>
<td headers="mean" class="gt_row gt_right"> 0.000070  </td>
<td headers="median" class="gt_row gt_right"> 0.000046 </td>
<td headers="sd" class="gt_row gt_right">0.0022 </td>
<td headers="mad" class="gt_row gt_right">0.0020 </td>
<td headers="q5" class="gt_row gt_right">−0.0032 </td>
<td headers="q95" class="gt_row gt_right"> 0.0036</td>
<td headers="rhat" class="gt_row gt_right">0.999</td>
<td headers="ess_bulk" class="gt_row gt_right">404</td>
<td headers="ess_tail" class="gt_row gt_right">413</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.4</td>
<td headers="mean" class="gt_row gt_right"> 0.00014   </td>
<td headers="median" class="gt_row gt_right"> 0.00013  </td>
<td headers="sd" class="gt_row gt_right">0.0022 </td>
<td headers="mad" class="gt_row gt_right">0.0021 </td>
<td headers="q5" class="gt_row gt_right">−0.0032 </td>
<td headers="q95" class="gt_row gt_right"> 0.0041</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">377</td>
<td headers="ess_tail" class="gt_row gt_right">364</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.5</td>
<td headers="mean" class="gt_row gt_right">−0.000071  </td>
<td headers="median" class="gt_row gt_right">−0.00025  </td>
<td headers="sd" class="gt_row gt_right">0.0023 </td>
<td headers="mad" class="gt_row gt_right">0.0022 </td>
<td headers="q5" class="gt_row gt_right">−0.0038 </td>
<td headers="q95" class="gt_row gt_right"> 0.0038</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">398</td>
<td headers="ess_tail" class="gt_row gt_right">394</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.6</td>
<td headers="mean" class="gt_row gt_right"> 0.000080  </td>
<td headers="median" class="gt_row gt_right"> 0.00013  </td>
<td headers="sd" class="gt_row gt_right">0.0022 </td>
<td headers="mad" class="gt_row gt_right">0.0022 </td>
<td headers="q5" class="gt_row gt_right">−0.0037 </td>
<td headers="q95" class="gt_row gt_right"> 0.0036</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">419</td>
<td headers="ess_tail" class="gt_row gt_right">458</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.7</td>
<td headers="mean" class="gt_row gt_right">−0.00012   </td>
<td headers="median" class="gt_row gt_right">−0.00014  </td>
<td headers="sd" class="gt_row gt_right">0.0022 </td>
<td headers="mad" class="gt_row gt_right">0.0022 </td>
<td headers="q5" class="gt_row gt_right">−0.0039 </td>
<td headers="q95" class="gt_row gt_right"> 0.0036</td>
<td headers="rhat" class="gt_row gt_right">0.998</td>
<td headers="ess_bulk" class="gt_row gt_right">320</td>
<td headers="ess_tail" class="gt_row gt_right">248</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.8</td>
<td headers="mean" class="gt_row gt_right"> 0.00018   </td>
<td headers="median" class="gt_row gt_right"> 0.00018  </td>
<td headers="sd" class="gt_row gt_right">0.0022 </td>
<td headers="mad" class="gt_row gt_right">0.0021 </td>
<td headers="q5" class="gt_row gt_right">−0.0033 </td>
<td headers="q95" class="gt_row gt_right"> 0.0038</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">460</td>
<td headers="ess_tail" class="gt_row gt_right">333</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.9</td>
<td headers="mean" class="gt_row gt_right">−0.000089  </td>
<td headers="median" class="gt_row gt_right"> 0.0000028</td>
<td headers="sd" class="gt_row gt_right">0.0020 </td>
<td headers="mad" class="gt_row gt_right">0.0020 </td>
<td headers="q5" class="gt_row gt_right">−0.0035 </td>
<td headers="q95" class="gt_row gt_right"> 0.0030</td>
<td headers="rhat" class="gt_row gt_right">0.995</td>
<td headers="ess_bulk" class="gt_row gt_right">441</td>
<td headers="ess_tail" class="gt_row gt_right">416</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.10</td>
<td headers="mean" class="gt_row gt_right">−0.00014   </td>
<td headers="median" class="gt_row gt_right">−0.00011  </td>
<td headers="sd" class="gt_row gt_right">0.0021 </td>
<td headers="mad" class="gt_row gt_right">0.0020 </td>
<td headers="q5" class="gt_row gt_right">−0.0037 </td>
<td headers="q95" class="gt_row gt_right"> 0.0034</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">468</td>
<td headers="ess_tail" class="gt_row gt_right">291</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.11</td>
<td headers="mean" class="gt_row gt_right"> 0.00014   </td>
<td headers="median" class="gt_row gt_right"> 0.000038 </td>
<td headers="sd" class="gt_row gt_right">0.0021 </td>
<td headers="mad" class="gt_row gt_right">0.0019 </td>
<td headers="q5" class="gt_row gt_right">−0.0032 </td>
<td headers="q95" class="gt_row gt_right"> 0.0037</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">447</td>
<td headers="ess_tail" class="gt_row gt_right">413</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.12</td>
<td headers="mean" class="gt_row gt_right"> 0.000097  </td>
<td headers="median" class="gt_row gt_right"> 0.000073 </td>
<td headers="sd" class="gt_row gt_right">0.0022 </td>
<td headers="mad" class="gt_row gt_right">0.0021 </td>
<td headers="q5" class="gt_row gt_right">−0.0036 </td>
<td headers="q95" class="gt_row gt_right"> 0.0035</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">314</td>
<td headers="ess_tail" class="gt_row gt_right">339</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.13</td>
<td headers="mean" class="gt_row gt_right"> 0.000081  </td>
<td headers="median" class="gt_row gt_right"> 0.000014 </td>
<td headers="sd" class="gt_row gt_right">0.0021 </td>
<td headers="mad" class="gt_row gt_right">0.0021 </td>
<td headers="q5" class="gt_row gt_right">−0.0033 </td>
<td headers="q95" class="gt_row gt_right"> 0.0038</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">342</td>
<td headers="ess_tail" class="gt_row gt_right">376</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.14</td>
<td headers="mean" class="gt_row gt_right">−0.00018   </td>
<td headers="median" class="gt_row gt_right">−0.000085 </td>
<td headers="sd" class="gt_row gt_right">0.0022 </td>
<td headers="mad" class="gt_row gt_right">0.0022 </td>
<td headers="q5" class="gt_row gt_right">−0.0038 </td>
<td headers="q95" class="gt_row gt_right"> 0.0034</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">331</td>
<td headers="ess_tail" class="gt_row gt_right">312</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.15</td>
<td headers="mean" class="gt_row gt_right"> 0.000018  </td>
<td headers="median" class="gt_row gt_right"> 0.00016  </td>
<td headers="sd" class="gt_row gt_right">0.0022 </td>
<td headers="mad" class="gt_row gt_right">0.0021 </td>
<td headers="q5" class="gt_row gt_right">−0.0038 </td>
<td headers="q95" class="gt_row gt_right"> 0.0036</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">360</td>
<td headers="ess_tail" class="gt_row gt_right">327</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.16</td>
<td headers="mean" class="gt_row gt_right">−0.0000021 </td>
<td headers="median" class="gt_row gt_right">−0.0000038</td>
<td headers="sd" class="gt_row gt_right">0.0021 </td>
<td headers="mad" class="gt_row gt_right">0.0023 </td>
<td headers="q5" class="gt_row gt_right">−0.0034 </td>
<td headers="q95" class="gt_row gt_right"> 0.0032</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">483</td>
<td headers="ess_tail" class="gt_row gt_right">402</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.17</td>
<td headers="mean" class="gt_row gt_right"> 0.0000036 </td>
<td headers="median" class="gt_row gt_right">−0.00010  </td>
<td headers="sd" class="gt_row gt_right">0.0023 </td>
<td headers="mad" class="gt_row gt_right">0.0022 </td>
<td headers="q5" class="gt_row gt_right">−0.0038 </td>
<td headers="q95" class="gt_row gt_right"> 0.0042</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">316</td>
<td headers="ess_tail" class="gt_row gt_right">371</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.18</td>
<td headers="mean" class="gt_row gt_right">−0.000043  </td>
<td headers="median" class="gt_row gt_right">−0.00017  </td>
<td headers="sd" class="gt_row gt_right">0.0022 </td>
<td headers="mad" class="gt_row gt_right">0.0024 </td>
<td headers="q5" class="gt_row gt_right">−0.0035 </td>
<td headers="q95" class="gt_row gt_right"> 0.0034</td>
<td headers="rhat" class="gt_row gt_right">0.998</td>
<td headers="ess_bulk" class="gt_row gt_right">407</td>
<td headers="ess_tail" class="gt_row gt_right">457</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.19</td>
<td headers="mean" class="gt_row gt_right"> 0.00014   </td>
<td headers="median" class="gt_row gt_right"> 0.00013  </td>
<td headers="sd" class="gt_row gt_right">0.0021 </td>
<td headers="mad" class="gt_row gt_right">0.0023 </td>
<td headers="q5" class="gt_row gt_right">−0.0033 </td>
<td headers="q95" class="gt_row gt_right"> 0.0036</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">364</td>
<td headers="ess_tail" class="gt_row gt_right">389</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(day_of_year,gender_age):dummy_rectal1.20</td>
<td headers="mean" class="gt_row gt_right"> 0.00011   </td>
<td headers="median" class="gt_row gt_right">−0.000011 </td>
<td headers="sd" class="gt_row gt_right">0.0022 </td>
<td headers="mad" class="gt_row gt_right">0.0023 </td>
<td headers="q5" class="gt_row gt_right">−0.0033 </td>
<td headers="q95" class="gt_row gt_right"> 0.0039</td>
<td headers="rhat" class="gt_row gt_right">0.998</td>
<td headers="ess_bulk" class="gt_row gt_right">406</td>
<td headers="ess_tail" class="gt_row gt_right">384</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(fmi_normalized):dummy_rectal1.1</td>
<td headers="mean" class="gt_row gt_right">−0.000031  </td>
<td headers="median" class="gt_row gt_right">−0.00012  </td>
<td headers="sd" class="gt_row gt_right">0.0014 </td>
<td headers="mad" class="gt_row gt_right">0.0013 </td>
<td headers="q5" class="gt_row gt_right">−0.0022 </td>
<td headers="q95" class="gt_row gt_right"> 0.0023</td>
<td headers="rhat" class="gt_row gt_right">0.999</td>
<td headers="ess_bulk" class="gt_row gt_right">453</td>
<td headers="ess_tail" class="gt_row gt_right">372</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(fmi_normalized):dummy_rectal1.2</td>
<td headers="mean" class="gt_row gt_right">−0.000029  </td>
<td headers="median" class="gt_row gt_right">−0.000061 </td>
<td headers="sd" class="gt_row gt_right">0.00068</td>
<td headers="mad" class="gt_row gt_right">0.00066</td>
<td headers="q5" class="gt_row gt_right">−0.0011 </td>
<td headers="q95" class="gt_row gt_right"> 0.0011</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">457</td>
<td headers="ess_tail" class="gt_row gt_right">462</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(fmi_normalized):dummy_rectal1.3</td>
<td headers="mean" class="gt_row gt_right"> 0.00030   </td>
<td headers="median" class="gt_row gt_right"> 0.00037  </td>
<td headers="sd" class="gt_row gt_right">0.0086 </td>
<td headers="mad" class="gt_row gt_right">0.0083 </td>
<td headers="q5" class="gt_row gt_right">−0.014  </td>
<td headers="q95" class="gt_row gt_right"> 0.014 </td>
<td headers="rhat" class="gt_row gt_right">0.995</td>
<td headers="ess_bulk" class="gt_row gt_right">404</td>
<td headers="ess_tail" class="gt_row gt_right">341</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(fmi_normalized):dummy_rectal1.4</td>
<td headers="mean" class="gt_row gt_right">−0.29      </td>
<td headers="median" class="gt_row gt_right">−0.30     </td>
<td headers="sd" class="gt_row gt_right">0.16   </td>
<td headers="mad" class="gt_row gt_right">0.16   </td>
<td headers="q5" class="gt_row gt_right">−0.55   </td>
<td headers="q95" class="gt_row gt_right">−0.0083</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">338</td>
<td headers="ess_tail" class="gt_row gt_right">283</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(reproductive_condition):dummy_repro1.1</td>
<td headers="mean" class="gt_row gt_right">−0.000033  </td>
<td headers="median" class="gt_row gt_right">−0.00039  </td>
<td headers="sd" class="gt_row gt_right">0.0066 </td>
<td headers="mad" class="gt_row gt_right">0.0066 </td>
<td headers="q5" class="gt_row gt_right">−0.011  </td>
<td headers="q95" class="gt_row gt_right"> 0.012 </td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">358</td>
<td headers="ess_tail" class="gt_row gt_right">330</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(reproductive_condition):dummy_repro1.2</td>
<td headers="mean" class="gt_row gt_right"> 0.00029   </td>
<td headers="median" class="gt_row gt_right"> 0.00033  </td>
<td headers="sd" class="gt_row gt_right">0.0072 </td>
<td headers="mad" class="gt_row gt_right">0.0068 </td>
<td headers="q5" class="gt_row gt_right">−0.011  </td>
<td headers="q95" class="gt_row gt_right"> 0.012 </td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">370</td>
<td headers="ess_tail" class="gt_row gt_right">285</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(reproductive_condition):dummy_repro1.3</td>
<td headers="mean" class="gt_row gt_right"> 0.00014   </td>
<td headers="median" class="gt_row gt_right"> 0.00048  </td>
<td headers="sd" class="gt_row gt_right">0.0072 </td>
<td headers="mad" class="gt_row gt_right">0.0076 </td>
<td headers="q5" class="gt_row gt_right">−0.012  </td>
<td headers="q95" class="gt_row gt_right"> 0.012 </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">347</td>
<td headers="ess_tail" class="gt_row gt_right">416</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s(reproductive_condition):dummy_repro1.4</td>
<td headers="mean" class="gt_row gt_right">−0.00059   </td>
<td headers="median" class="gt_row gt_right">−0.00069  </td>
<td headers="sd" class="gt_row gt_right">0.0070 </td>
<td headers="mad" class="gt_row gt_right">0.0073 </td>
<td headers="q5" class="gt_row gt_right">−0.011  </td>
<td headers="q95" class="gt_row gt_right"> 0.011 </td>
<td headers="rhat" class="gt_row gt_right">0.998</td>
<td headers="ess_bulk" class="gt_row gt_right">438</td>
<td headers="ess_tail" class="gt_row gt_right">339</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">(Intercept).1</td>
<td headers="mean" class="gt_row gt_right">−2.5       </td>
<td headers="median" class="gt_row gt_right">−2.5      </td>
<td headers="sd" class="gt_row gt_right">0.15   </td>
<td headers="mad" class="gt_row gt_right">0.15   </td>
<td headers="q5" class="gt_row gt_right">−2.8    </td>
<td headers="q95" class="gt_row gt_right">−2.3   </td>
<td headers="rhat" class="gt_row gt_right">0.999</td>
<td headers="ess_bulk" class="gt_row gt_right">421</td>
<td headers="ess_tail" class="gt_row gt_right">373</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(sample_type):dummy_rectal1.1</td>
<td headers="mean" class="gt_row gt_right">−0.00010   </td>
<td headers="median" class="gt_row gt_right">−0.00020  </td>
<td headers="sd" class="gt_row gt_right">0.0031 </td>
<td headers="mad" class="gt_row gt_right">0.0030 </td>
<td headers="q5" class="gt_row gt_right">−0.0054 </td>
<td headers="q95" class="gt_row gt_right"> 0.0050</td>
<td headers="rhat" class="gt_row gt_right">0.996</td>
<td headers="ess_bulk" class="gt_row gt_right">378</td>
<td headers="ess_tail" class="gt_row gt_right">411</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(sample_type):dummy_rectal1.2</td>
<td headers="mean" class="gt_row gt_right">−0.000077  </td>
<td headers="median" class="gt_row gt_right">−0.00013  </td>
<td headers="sd" class="gt_row gt_right">0.0029 </td>
<td headers="mad" class="gt_row gt_right">0.0029 </td>
<td headers="q5" class="gt_row gt_right">−0.0051 </td>
<td headers="q95" class="gt_row gt_right"> 0.0047</td>
<td headers="rhat" class="gt_row gt_right">1.02 </td>
<td headers="ess_bulk" class="gt_row gt_right">411</td>
<td headers="ess_tail" class="gt_row gt_right">372</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day):sample_typeFecal.1</td>
<td headers="mean" class="gt_row gt_right"> 0.30      </td>
<td headers="median" class="gt_row gt_right"> 0.33     </td>
<td headers="sd" class="gt_row gt_right">0.51   </td>
<td headers="mad" class="gt_row gt_right">0.47   </td>
<td headers="q5" class="gt_row gt_right">−0.52   </td>
<td headers="q95" class="gt_row gt_right"> 1.1   </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">403</td>
<td headers="ess_tail" class="gt_row gt_right">373</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day):sample_typeFecal.2</td>
<td headers="mean" class="gt_row gt_right"> 0.18      </td>
<td headers="median" class="gt_row gt_right"> 0.13     </td>
<td headers="sd" class="gt_row gt_right">1.3    </td>
<td headers="mad" class="gt_row gt_right">1.4    </td>
<td headers="q5" class="gt_row gt_right">−2.1    </td>
<td headers="q95" class="gt_row gt_right"> 2.3   </td>
<td headers="rhat" class="gt_row gt_right">0.999</td>
<td headers="ess_bulk" class="gt_row gt_right">406</td>
<td headers="ess_tail" class="gt_row gt_right">415</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day):sample_typeFecal.3</td>
<td headers="mean" class="gt_row gt_right"> 1.5       </td>
<td headers="median" class="gt_row gt_right"> 1.4      </td>
<td headers="sd" class="gt_row gt_right">1.8    </td>
<td headers="mad" class="gt_row gt_right">1.8    </td>
<td headers="q5" class="gt_row gt_right">−1.7    </td>
<td headers="q95" class="gt_row gt_right"> 4.3   </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">406</td>
<td headers="ess_tail" class="gt_row gt_right">375</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day):sample_typeFecal.4</td>
<td headers="mean" class="gt_row gt_right">−0.47      </td>
<td headers="median" class="gt_row gt_right">−0.51     </td>
<td headers="sd" class="gt_row gt_right">0.56   </td>
<td headers="mad" class="gt_row gt_right">0.49   </td>
<td headers="q5" class="gt_row gt_right">−1.4    </td>
<td headers="q95" class="gt_row gt_right"> 0.52  </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">420</td>
<td headers="ess_tail" class="gt_row gt_right">373</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day):sample_typeRectal.1</td>
<td headers="mean" class="gt_row gt_right">−2.3       </td>
<td headers="median" class="gt_row gt_right">−2.3      </td>
<td headers="sd" class="gt_row gt_right">1.5    </td>
<td headers="mad" class="gt_row gt_right">1.4    </td>
<td headers="q5" class="gt_row gt_right">−4.6    </td>
<td headers="q95" class="gt_row gt_right"> 0.32  </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">440</td>
<td headers="ess_tail" class="gt_row gt_right">417</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day):sample_typeRectal.2</td>
<td headers="mean" class="gt_row gt_right">−2.0       </td>
<td headers="median" class="gt_row gt_right">−2.1      </td>
<td headers="sd" class="gt_row gt_right">3.3    </td>
<td headers="mad" class="gt_row gt_right">3.4    </td>
<td headers="q5" class="gt_row gt_right">−7.0    </td>
<td headers="q95" class="gt_row gt_right"> 3.7   </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">381</td>
<td headers="ess_tail" class="gt_row gt_right">374</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day):sample_typeRectal.3</td>
<td headers="mean" class="gt_row gt_right">−0.76      </td>
<td headers="median" class="gt_row gt_right">−1.1      </td>
<td headers="sd" class="gt_row gt_right">4.9    </td>
<td headers="mad" class="gt_row gt_right">5.1    </td>
<td headers="q5" class="gt_row gt_right">−8.7    </td>
<td headers="q95" class="gt_row gt_right"> 7.4   </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">418</td>
<td headers="ess_tail" class="gt_row gt_right">416</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day):sample_typeRectal.4</td>
<td headers="mean" class="gt_row gt_right"> 2.3       </td>
<td headers="median" class="gt_row gt_right"> 2.3      </td>
<td headers="sd" class="gt_row gt_right">1.9    </td>
<td headers="mad" class="gt_row gt_right">2.0    </td>
<td headers="q5" class="gt_row gt_right">−1.1    </td>
<td headers="q95" class="gt_row gt_right"> 5.4   </td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">491</td>
<td headers="ess_tail" class="gt_row gt_right">455</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.1</td>
<td headers="mean" class="gt_row gt_right"> 0.000035  </td>
<td headers="median" class="gt_row gt_right">−0.000011 </td>
<td headers="sd" class="gt_row gt_right">0.00095</td>
<td headers="mad" class="gt_row gt_right">0.00095</td>
<td headers="q5" class="gt_row gt_right">−0.0015 </td>
<td headers="q95" class="gt_row gt_right"> 0.0015</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">476</td>
<td headers="ess_tail" class="gt_row gt_right">415</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.2</td>
<td headers="mean" class="gt_row gt_right">−0.000033  </td>
<td headers="median" class="gt_row gt_right">−0.000045 </td>
<td headers="sd" class="gt_row gt_right">0.00093</td>
<td headers="mad" class="gt_row gt_right">0.00093</td>
<td headers="q5" class="gt_row gt_right">−0.0016 </td>
<td headers="q95" class="gt_row gt_right"> 0.0014</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">358</td>
<td headers="ess_tail" class="gt_row gt_right">368</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.3</td>
<td headers="mean" class="gt_row gt_right">−0.0000073 </td>
<td headers="median" class="gt_row gt_right">−0.0000050</td>
<td headers="sd" class="gt_row gt_right">0.00090</td>
<td headers="mad" class="gt_row gt_right">0.00096</td>
<td headers="q5" class="gt_row gt_right">−0.0015 </td>
<td headers="q95" class="gt_row gt_right"> 0.0014</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">330</td>
<td headers="ess_tail" class="gt_row gt_right">311</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.4</td>
<td headers="mean" class="gt_row gt_right">−0.000056  </td>
<td headers="median" class="gt_row gt_right">−0.000060 </td>
<td headers="sd" class="gt_row gt_right">0.00095</td>
<td headers="mad" class="gt_row gt_right">0.00092</td>
<td headers="q5" class="gt_row gt_right">−0.0016 </td>
<td headers="q95" class="gt_row gt_right"> 0.0015</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">344</td>
<td headers="ess_tail" class="gt_row gt_right">408</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.5</td>
<td headers="mean" class="gt_row gt_right">−0.00011   </td>
<td headers="median" class="gt_row gt_right">−0.000091 </td>
<td headers="sd" class="gt_row gt_right">0.0013 </td>
<td headers="mad" class="gt_row gt_right">0.0013 </td>
<td headers="q5" class="gt_row gt_right">−0.0021 </td>
<td headers="q95" class="gt_row gt_right"> 0.0021</td>
<td headers="rhat" class="gt_row gt_right">0.998</td>
<td headers="ess_bulk" class="gt_row gt_right">322</td>
<td headers="ess_tail" class="gt_row gt_right">399</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.6</td>
<td headers="mean" class="gt_row gt_right">−0.000075  </td>
<td headers="median" class="gt_row gt_right">−0.00011  </td>
<td headers="sd" class="gt_row gt_right">0.00095</td>
<td headers="mad" class="gt_row gt_right">0.00090</td>
<td headers="q5" class="gt_row gt_right">−0.0017 </td>
<td headers="q95" class="gt_row gt_right"> 0.0015</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">411</td>
<td headers="ess_tail" class="gt_row gt_right">291</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.7</td>
<td headers="mean" class="gt_row gt_right"> 0.000066  </td>
<td headers="median" class="gt_row gt_right"> 0.000019 </td>
<td headers="sd" class="gt_row gt_right">0.00095</td>
<td headers="mad" class="gt_row gt_right">0.00092</td>
<td headers="q5" class="gt_row gt_right">−0.0014 </td>
<td headers="q95" class="gt_row gt_right"> 0.0018</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">357</td>
<td headers="ess_tail" class="gt_row gt_right">375</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.8</td>
<td headers="mean" class="gt_row gt_right">−0.0000043 </td>
<td headers="median" class="gt_row gt_right"> 0.000042 </td>
<td headers="sd" class="gt_row gt_right">0.00095</td>
<td headers="mad" class="gt_row gt_right">0.0010 </td>
<td headers="q5" class="gt_row gt_right">−0.0015 </td>
<td headers="q95" class="gt_row gt_right"> 0.0014</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">408</td>
<td headers="ess_tail" class="gt_row gt_right">375</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.9</td>
<td headers="mean" class="gt_row gt_right">−0.000038  </td>
<td headers="median" class="gt_row gt_right">−0.000037 </td>
<td headers="sd" class="gt_row gt_right">0.00095</td>
<td headers="mad" class="gt_row gt_right">0.00090</td>
<td headers="q5" class="gt_row gt_right">−0.0015 </td>
<td headers="q95" class="gt_row gt_right"> 0.0016</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">362</td>
<td headers="ess_tail" class="gt_row gt_right">411</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.10</td>
<td headers="mean" class="gt_row gt_right">−0.0000085 </td>
<td headers="median" class="gt_row gt_right">−0.000011 </td>
<td headers="sd" class="gt_row gt_right">0.0014 </td>
<td headers="mad" class="gt_row gt_right">0.0014 </td>
<td headers="q5" class="gt_row gt_right">−0.0022 </td>
<td headers="q95" class="gt_row gt_right"> 0.0022</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">358</td>
<td headers="ess_tail" class="gt_row gt_right">340</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.11</td>
<td headers="mean" class="gt_row gt_right">−0.000029  </td>
<td headers="median" class="gt_row gt_right">−0.000046 </td>
<td headers="sd" class="gt_row gt_right">0.0010 </td>
<td headers="mad" class="gt_row gt_right">0.00096</td>
<td headers="q5" class="gt_row gt_right">−0.0018 </td>
<td headers="q95" class="gt_row gt_right"> 0.0017</td>
<td headers="rhat" class="gt_row gt_right">0.997</td>
<td headers="ess_bulk" class="gt_row gt_right">406</td>
<td headers="ess_tail" class="gt_row gt_right">412</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.12</td>
<td headers="mean" class="gt_row gt_right"> 0.0000013 </td>
<td headers="median" class="gt_row gt_right"> 0.000077 </td>
<td headers="sd" class="gt_row gt_right">0.00099</td>
<td headers="mad" class="gt_row gt_right">0.00095</td>
<td headers="q5" class="gt_row gt_right">−0.0018 </td>
<td headers="q95" class="gt_row gt_right"> 0.0016</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">400</td>
<td headers="ess_tail" class="gt_row gt_right">415</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.13</td>
<td headers="mean" class="gt_row gt_right">−0.000050  </td>
<td headers="median" class="gt_row gt_right">−0.000087 </td>
<td headers="sd" class="gt_row gt_right">0.00095</td>
<td headers="mad" class="gt_row gt_right">0.0010 </td>
<td headers="q5" class="gt_row gt_right">−0.0015 </td>
<td headers="q95" class="gt_row gt_right"> 0.0016</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">358</td>
<td headers="ess_tail" class="gt_row gt_right">338</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.14</td>
<td headers="mean" class="gt_row gt_right">−0.000015  </td>
<td headers="median" class="gt_row gt_right">−0.000067 </td>
<td headers="sd" class="gt_row gt_right">0.00094</td>
<td headers="mad" class="gt_row gt_right">0.0010 </td>
<td headers="q5" class="gt_row gt_right">−0.0017 </td>
<td headers="q95" class="gt_row gt_right"> 0.0014</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">384</td>
<td headers="ess_tail" class="gt_row gt_right">406</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.15</td>
<td headers="mean" class="gt_row gt_right">−0.000011  </td>
<td headers="median" class="gt_row gt_right"> 0.0000040</td>
<td headers="sd" class="gt_row gt_right">0.0014 </td>
<td headers="mad" class="gt_row gt_right">0.0014 </td>
<td headers="q5" class="gt_row gt_right">−0.0024 </td>
<td headers="q95" class="gt_row gt_right"> 0.0023</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">370</td>
<td headers="ess_tail" class="gt_row gt_right">376</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.16</td>
<td headers="mean" class="gt_row gt_right"> 0.000011  </td>
<td headers="median" class="gt_row gt_right"> 0.000015 </td>
<td headers="sd" class="gt_row gt_right">0.00097</td>
<td headers="mad" class="gt_row gt_right">0.00094</td>
<td headers="q5" class="gt_row gt_right">−0.0016 </td>
<td headers="q95" class="gt_row gt_right"> 0.0016</td>
<td headers="rhat" class="gt_row gt_right">0.998</td>
<td headers="ess_bulk" class="gt_row gt_right">393</td>
<td headers="ess_tail" class="gt_row gt_right">412</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.17</td>
<td headers="mean" class="gt_row gt_right"> 0.000011  </td>
<td headers="median" class="gt_row gt_right"> 0.000098 </td>
<td headers="sd" class="gt_row gt_right">0.00092</td>
<td headers="mad" class="gt_row gt_right">0.00099</td>
<td headers="q5" class="gt_row gt_right">−0.0015 </td>
<td headers="q95" class="gt_row gt_right"> 0.0014</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">350</td>
<td headers="ess_tail" class="gt_row gt_right">399</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.18</td>
<td headers="mean" class="gt_row gt_right">−0.000028  </td>
<td headers="median" class="gt_row gt_right">−0.000044 </td>
<td headers="sd" class="gt_row gt_right">0.00093</td>
<td headers="mad" class="gt_row gt_right">0.00085</td>
<td headers="q5" class="gt_row gt_right">−0.0015 </td>
<td headers="q95" class="gt_row gt_right"> 0.0016</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">448</td>
<td headers="ess_tail" class="gt_row gt_right">410</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.19</td>
<td headers="mean" class="gt_row gt_right"> 0.000023  </td>
<td headers="median" class="gt_row gt_right"> 0.000070 </td>
<td headers="sd" class="gt_row gt_right">0.00094</td>
<td headers="mad" class="gt_row gt_right">0.00099</td>
<td headers="q5" class="gt_row gt_right">−0.0016 </td>
<td headers="q95" class="gt_row gt_right"> 0.0016</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">384</td>
<td headers="ess_tail" class="gt_row gt_right">375</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.20</td>
<td headers="mean" class="gt_row gt_right"> 0.000054  </td>
<td headers="median" class="gt_row gt_right"> 0.000045 </td>
<td headers="sd" class="gt_row gt_right">0.0013 </td>
<td headers="mad" class="gt_row gt_right">0.0014 </td>
<td headers="q5" class="gt_row gt_right">−0.0021 </td>
<td headers="q95" class="gt_row gt_right"> 0.0022</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">322</td>
<td headers="ess_tail" class="gt_row gt_right">326</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.21</td>
<td headers="mean" class="gt_row gt_right"> 0.000016  </td>
<td headers="median" class="gt_row gt_right"> 0.000025 </td>
<td headers="sd" class="gt_row gt_right">0.00099</td>
<td headers="mad" class="gt_row gt_right">0.00099</td>
<td headers="q5" class="gt_row gt_right">−0.0015 </td>
<td headers="q95" class="gt_row gt_right"> 0.0016</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">454</td>
<td headers="ess_tail" class="gt_row gt_right">338</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.22</td>
<td headers="mean" class="gt_row gt_right">−0.000019  </td>
<td headers="median" class="gt_row gt_right">−0.000035 </td>
<td headers="sd" class="gt_row gt_right">0.00092</td>
<td headers="mad" class="gt_row gt_right">0.0010 </td>
<td headers="q5" class="gt_row gt_right">−0.0015 </td>
<td headers="q95" class="gt_row gt_right"> 0.0014</td>
<td headers="rhat" class="gt_row gt_right">0.997</td>
<td headers="ess_bulk" class="gt_row gt_right">407</td>
<td headers="ess_tail" class="gt_row gt_right">414</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.23</td>
<td headers="mean" class="gt_row gt_right">−0.00010   </td>
<td headers="median" class="gt_row gt_right">−0.000047 </td>
<td headers="sd" class="gt_row gt_right">0.00099</td>
<td headers="mad" class="gt_row gt_right">0.00091</td>
<td headers="q5" class="gt_row gt_right">−0.0018 </td>
<td headers="q95" class="gt_row gt_right"> 0.0015</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">390</td>
<td headers="ess_tail" class="gt_row gt_right">375</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.24</td>
<td headers="mean" class="gt_row gt_right"> 0.000016  </td>
<td headers="median" class="gt_row gt_right"> 0.000034 </td>
<td headers="sd" class="gt_row gt_right">0.00091</td>
<td headers="mad" class="gt_row gt_right">0.00088</td>
<td headers="q5" class="gt_row gt_right">−0.0016 </td>
<td headers="q95" class="gt_row gt_right"> 0.0014</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">404</td>
<td headers="ess_tail" class="gt_row gt_right">330</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day,gender_age):dummy_rectal1.25</td>
<td headers="mean" class="gt_row gt_right">−0.000022  </td>
<td headers="median" class="gt_row gt_right">−0.000014 </td>
<td headers="sd" class="gt_row gt_right">0.0014 </td>
<td headers="mad" class="gt_row gt_right">0.0016 </td>
<td headers="q5" class="gt_row gt_right">−0.0023 </td>
<td headers="q95" class="gt_row gt_right"> 0.0022</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">455</td>
<td headers="ess_tail" class="gt_row gt_right">296</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year):sample_typeFecal.1</td>
<td headers="mean" class="gt_row gt_right"> 1.6       </td>
<td headers="median" class="gt_row gt_right"> 1.6      </td>
<td headers="sd" class="gt_row gt_right">0.31   </td>
<td headers="mad" class="gt_row gt_right">0.30   </td>
<td headers="q5" class="gt_row gt_right"> 1.1    </td>
<td headers="q95" class="gt_row gt_right"> 2.1   </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">369</td>
<td headers="ess_tail" class="gt_row gt_right">341</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year):sample_typeFecal.2</td>
<td headers="mean" class="gt_row gt_right"> 0.55      </td>
<td headers="median" class="gt_row gt_right"> 0.56     </td>
<td headers="sd" class="gt_row gt_right">0.38   </td>
<td headers="mad" class="gt_row gt_right">0.37   </td>
<td headers="q5" class="gt_row gt_right">−0.097  </td>
<td headers="q95" class="gt_row gt_right"> 1.2   </td>
<td headers="rhat" class="gt_row gt_right">0.998</td>
<td headers="ess_bulk" class="gt_row gt_right">382</td>
<td headers="ess_tail" class="gt_row gt_right">374</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year):sample_typeFecal.3</td>
<td headers="mean" class="gt_row gt_right">−1.4       </td>
<td headers="median" class="gt_row gt_right">−1.4      </td>
<td headers="sd" class="gt_row gt_right">0.39   </td>
<td headers="mad" class="gt_row gt_right">0.39   </td>
<td headers="q5" class="gt_row gt_right">−2.0    </td>
<td headers="q95" class="gt_row gt_right">−0.71  </td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">355</td>
<td headers="ess_tail" class="gt_row gt_right">417</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year):sample_typeRectal.1</td>
<td headers="mean" class="gt_row gt_right"> 0.93      </td>
<td headers="median" class="gt_row gt_right"> 0.90     </td>
<td headers="sd" class="gt_row gt_right">0.40   </td>
<td headers="mad" class="gt_row gt_right">0.38   </td>
<td headers="q5" class="gt_row gt_right"> 0.31   </td>
<td headers="q95" class="gt_row gt_right"> 1.6   </td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">399</td>
<td headers="ess_tail" class="gt_row gt_right">191</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year):sample_typeRectal.2</td>
<td headers="mean" class="gt_row gt_right">−0.018     </td>
<td headers="median" class="gt_row gt_right">−0.052    </td>
<td headers="sd" class="gt_row gt_right">0.49   </td>
<td headers="mad" class="gt_row gt_right">0.49   </td>
<td headers="q5" class="gt_row gt_right">−0.85   </td>
<td headers="q95" class="gt_row gt_right"> 0.83  </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">370</td>
<td headers="ess_tail" class="gt_row gt_right">375</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year):sample_typeRectal.3</td>
<td headers="mean" class="gt_row gt_right">−0.47      </td>
<td headers="median" class="gt_row gt_right">−0.43     </td>
<td headers="sd" class="gt_row gt_right">0.44   </td>
<td headers="mad" class="gt_row gt_right">0.45   </td>
<td headers="q5" class="gt_row gt_right">−1.2    </td>
<td headers="q95" class="gt_row gt_right"> 0.21  </td>
<td headers="rhat" class="gt_row gt_right">0.999</td>
<td headers="ess_bulk" class="gt_row gt_right">501</td>
<td headers="ess_tail" class="gt_row gt_right">463</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.1</td>
<td headers="mean" class="gt_row gt_right"> 0.00016   </td>
<td headers="median" class="gt_row gt_right"> 0.00013  </td>
<td headers="sd" class="gt_row gt_right">0.0016 </td>
<td headers="mad" class="gt_row gt_right">0.0017 </td>
<td headers="q5" class="gt_row gt_right">−0.0022 </td>
<td headers="q95" class="gt_row gt_right"> 0.0028</td>
<td headers="rhat" class="gt_row gt_right">0.998</td>
<td headers="ess_bulk" class="gt_row gt_right">348</td>
<td headers="ess_tail" class="gt_row gt_right">397</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.2</td>
<td headers="mean" class="gt_row gt_right"> 0.00012   </td>
<td headers="median" class="gt_row gt_right"> 0.00013  </td>
<td headers="sd" class="gt_row gt_right">0.0017 </td>
<td headers="mad" class="gt_row gt_right">0.0016 </td>
<td headers="q5" class="gt_row gt_right">−0.0027 </td>
<td headers="q95" class="gt_row gt_right"> 0.0027</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">449</td>
<td headers="ess_tail" class="gt_row gt_right">279</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.3</td>
<td headers="mean" class="gt_row gt_right"> 0.000015  </td>
<td headers="median" class="gt_row gt_right">−0.000020 </td>
<td headers="sd" class="gt_row gt_right">0.0018 </td>
<td headers="mad" class="gt_row gt_right">0.0020 </td>
<td headers="q5" class="gt_row gt_right">−0.0029 </td>
<td headers="q95" class="gt_row gt_right"> 0.0028</td>
<td headers="rhat" class="gt_row gt_right">0.999</td>
<td headers="ess_bulk" class="gt_row gt_right">383</td>
<td headers="ess_tail" class="gt_row gt_right">313</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.4</td>
<td headers="mean" class="gt_row gt_right"> 0.000066  </td>
<td headers="median" class="gt_row gt_right"> 0.00016  </td>
<td headers="sd" class="gt_row gt_right">0.0022 </td>
<td headers="mad" class="gt_row gt_right">0.0023 </td>
<td headers="q5" class="gt_row gt_right">−0.0036 </td>
<td headers="q95" class="gt_row gt_right"> 0.0034</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">438</td>
<td headers="ess_tail" class="gt_row gt_right">397</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.5</td>
<td headers="mean" class="gt_row gt_right">−0.00000023</td>
<td headers="median" class="gt_row gt_right">−0.0000099</td>
<td headers="sd" class="gt_row gt_right">0.0017 </td>
<td headers="mad" class="gt_row gt_right">0.0017 </td>
<td headers="q5" class="gt_row gt_right">−0.0027 </td>
<td headers="q95" class="gt_row gt_right"> 0.0027</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">361</td>
<td headers="ess_tail" class="gt_row gt_right">411</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.6</td>
<td headers="mean" class="gt_row gt_right"> 0.000083  </td>
<td headers="median" class="gt_row gt_right"> 0.00017  </td>
<td headers="sd" class="gt_row gt_right">0.0016 </td>
<td headers="mad" class="gt_row gt_right">0.0015 </td>
<td headers="q5" class="gt_row gt_right">−0.0026 </td>
<td headers="q95" class="gt_row gt_right"> 0.0026</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">320</td>
<td headers="ess_tail" class="gt_row gt_right">355</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.7</td>
<td headers="mean" class="gt_row gt_right"> 0.000063  </td>
<td headers="median" class="gt_row gt_right"> 0.000091 </td>
<td headers="sd" class="gt_row gt_right">0.0016 </td>
<td headers="mad" class="gt_row gt_right">0.0015 </td>
<td headers="q5" class="gt_row gt_right">−0.0026 </td>
<td headers="q95" class="gt_row gt_right"> 0.0025</td>
<td headers="rhat" class="gt_row gt_right">0.997</td>
<td headers="ess_bulk" class="gt_row gt_right">425</td>
<td headers="ess_tail" class="gt_row gt_right">443</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.8</td>
<td headers="mean" class="gt_row gt_right"> 0.00013   </td>
<td headers="median" class="gt_row gt_right"> 0.00013  </td>
<td headers="sd" class="gt_row gt_right">0.0022 </td>
<td headers="mad" class="gt_row gt_right">0.0021 </td>
<td headers="q5" class="gt_row gt_right">−0.0034 </td>
<td headers="q95" class="gt_row gt_right"> 0.0037</td>
<td headers="rhat" class="gt_row gt_right">0.999</td>
<td headers="ess_bulk" class="gt_row gt_right">393</td>
<td headers="ess_tail" class="gt_row gt_right">413</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.9</td>
<td headers="mean" class="gt_row gt_right">−0.000059  </td>
<td headers="median" class="gt_row gt_right">−0.00016  </td>
<td headers="sd" class="gt_row gt_right">0.0016 </td>
<td headers="mad" class="gt_row gt_right">0.0016 </td>
<td headers="q5" class="gt_row gt_right">−0.0027 </td>
<td headers="q95" class="gt_row gt_right"> 0.0028</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">406</td>
<td headers="ess_tail" class="gt_row gt_right">371</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.10</td>
<td headers="mean" class="gt_row gt_right">−0.000076  </td>
<td headers="median" class="gt_row gt_right">−0.00018  </td>
<td headers="sd" class="gt_row gt_right">0.0017 </td>
<td headers="mad" class="gt_row gt_right">0.0018 </td>
<td headers="q5" class="gt_row gt_right">−0.0028 </td>
<td headers="q95" class="gt_row gt_right"> 0.0029</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">368</td>
<td headers="ess_tail" class="gt_row gt_right">373</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.11</td>
<td headers="mean" class="gt_row gt_right">−0.000060  </td>
<td headers="median" class="gt_row gt_right">−0.00016  </td>
<td headers="sd" class="gt_row gt_right">0.0016 </td>
<td headers="mad" class="gt_row gt_right">0.0016 </td>
<td headers="q5" class="gt_row gt_right">−0.0029 </td>
<td headers="q95" class="gt_row gt_right"> 0.0025</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">387</td>
<td headers="ess_tail" class="gt_row gt_right">271</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.12</td>
<td headers="mean" class="gt_row gt_right">−0.00010   </td>
<td headers="median" class="gt_row gt_right">−0.000064 </td>
<td headers="sd" class="gt_row gt_right">0.0021 </td>
<td headers="mad" class="gt_row gt_right">0.0023 </td>
<td headers="q5" class="gt_row gt_right">−0.0036 </td>
<td headers="q95" class="gt_row gt_right"> 0.0032</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">452</td>
<td headers="ess_tail" class="gt_row gt_right">375</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.13</td>
<td headers="mean" class="gt_row gt_right"> 0.000048  </td>
<td headers="median" class="gt_row gt_right">−0.000033 </td>
<td headers="sd" class="gt_row gt_right">0.0017 </td>
<td headers="mad" class="gt_row gt_right">0.0017 </td>
<td headers="q5" class="gt_row gt_right">−0.0029 </td>
<td headers="q95" class="gt_row gt_right"> 0.0028</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">367</td>
<td headers="ess_tail" class="gt_row gt_right">326</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.14</td>
<td headers="mean" class="gt_row gt_right"> 0.00016   </td>
<td headers="median" class="gt_row gt_right"> 0.00016  </td>
<td headers="sd" class="gt_row gt_right">0.0017 </td>
<td headers="mad" class="gt_row gt_right">0.0017 </td>
<td headers="q5" class="gt_row gt_right">−0.0027 </td>
<td headers="q95" class="gt_row gt_right"> 0.0031</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">370</td>
<td headers="ess_tail" class="gt_row gt_right">404</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.15</td>
<td headers="mean" class="gt_row gt_right"> 0.000036  </td>
<td headers="median" class="gt_row gt_right">−0.000053 </td>
<td headers="sd" class="gt_row gt_right">0.0016 </td>
<td headers="mad" class="gt_row gt_right">0.0017 </td>
<td headers="q5" class="gt_row gt_right">−0.0025 </td>
<td headers="q95" class="gt_row gt_right"> 0.0028</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">447</td>
<td headers="ess_tail" class="gt_row gt_right">361</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.16</td>
<td headers="mean" class="gt_row gt_right"> 0.000064  </td>
<td headers="median" class="gt_row gt_right"> 0.00014  </td>
<td headers="sd" class="gt_row gt_right">0.0022 </td>
<td headers="mad" class="gt_row gt_right">0.0021 </td>
<td headers="q5" class="gt_row gt_right">−0.0036 </td>
<td headers="q95" class="gt_row gt_right"> 0.0033</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">387</td>
<td headers="ess_tail" class="gt_row gt_right">375</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.17</td>
<td headers="mean" class="gt_row gt_right">−0.000075  </td>
<td headers="median" class="gt_row gt_right">−0.00014  </td>
<td headers="sd" class="gt_row gt_right">0.0016 </td>
<td headers="mad" class="gt_row gt_right">0.0015 </td>
<td headers="q5" class="gt_row gt_right">−0.0026 </td>
<td headers="q95" class="gt_row gt_right"> 0.0026</td>
<td headers="rhat" class="gt_row gt_right">0.994</td>
<td headers="ess_bulk" class="gt_row gt_right">411</td>
<td headers="ess_tail" class="gt_row gt_right">367</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.18</td>
<td headers="mean" class="gt_row gt_right">−0.000070  </td>
<td headers="median" class="gt_row gt_right">−0.00013  </td>
<td headers="sd" class="gt_row gt_right">0.0016 </td>
<td headers="mad" class="gt_row gt_right">0.0015 </td>
<td headers="q5" class="gt_row gt_right">−0.0025 </td>
<td headers="q95" class="gt_row gt_right"> 0.0028</td>
<td headers="rhat" class="gt_row gt_right">0.996</td>
<td headers="ess_bulk" class="gt_row gt_right">373</td>
<td headers="ess_tail" class="gt_row gt_right">341</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.19</td>
<td headers="mean" class="gt_row gt_right"> 0.000042  </td>
<td headers="median" class="gt_row gt_right"> 0.000034 </td>
<td headers="sd" class="gt_row gt_right">0.0017 </td>
<td headers="mad" class="gt_row gt_right">0.0018 </td>
<td headers="q5" class="gt_row gt_right">−0.0027 </td>
<td headers="q95" class="gt_row gt_right"> 0.0027</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">309</td>
<td headers="ess_tail" class="gt_row gt_right">402</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(day_of_year,gender_age):dummy_rectal1.20</td>
<td headers="mean" class="gt_row gt_right"> 0.000050  </td>
<td headers="median" class="gt_row gt_right"> 0.00015  </td>
<td headers="sd" class="gt_row gt_right">0.0021 </td>
<td headers="mad" class="gt_row gt_right">0.0021 </td>
<td headers="q5" class="gt_row gt_right">−0.0032 </td>
<td headers="q95" class="gt_row gt_right"> 0.0037</td>
<td headers="rhat" class="gt_row gt_right">0.997</td>
<td headers="ess_bulk" class="gt_row gt_right">371</td>
<td headers="ess_tail" class="gt_row gt_right">347</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(fmi_normalized):dummy_rectal1.1</td>
<td headers="mean" class="gt_row gt_right"> 0.000014  </td>
<td headers="median" class="gt_row gt_right"> 0.000080 </td>
<td headers="sd" class="gt_row gt_right">0.0012 </td>
<td headers="mad" class="gt_row gt_right">0.0012 </td>
<td headers="q5" class="gt_row gt_right">−0.0020 </td>
<td headers="q95" class="gt_row gt_right"> 0.0019</td>
<td headers="rhat" class="gt_row gt_right">0.996</td>
<td headers="ess_bulk" class="gt_row gt_right">440</td>
<td headers="ess_tail" class="gt_row gt_right">412</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(fmi_normalized):dummy_rectal1.2</td>
<td headers="mean" class="gt_row gt_right"> 0.000027  </td>
<td headers="median" class="gt_row gt_right"> 0.000016 </td>
<td headers="sd" class="gt_row gt_right">0.00065</td>
<td headers="mad" class="gt_row gt_right">0.00062</td>
<td headers="q5" class="gt_row gt_right">−0.00099</td>
<td headers="q95" class="gt_row gt_right"> 0.0011</td>
<td headers="rhat" class="gt_row gt_right">0.997</td>
<td headers="ess_bulk" class="gt_row gt_right">410</td>
<td headers="ess_tail" class="gt_row gt_right">469</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(fmi_normalized):dummy_rectal1.3</td>
<td headers="mean" class="gt_row gt_right"> 0.00018   </td>
<td headers="median" class="gt_row gt_right"> 0.00015  </td>
<td headers="sd" class="gt_row gt_right">0.0077 </td>
<td headers="mad" class="gt_row gt_right">0.0067 </td>
<td headers="q5" class="gt_row gt_right">−0.012  </td>
<td headers="q95" class="gt_row gt_right"> 0.013 </td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">476</td>
<td headers="ess_tail" class="gt_row gt_right">341</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(fmi_normalized):dummy_rectal1.4</td>
<td headers="mean" class="gt_row gt_right">−0.14      </td>
<td headers="median" class="gt_row gt_right">−0.14     </td>
<td headers="sd" class="gt_row gt_right">0.087  </td>
<td headers="mad" class="gt_row gt_right">0.089  </td>
<td headers="q5" class="gt_row gt_right">−0.28   </td>
<td headers="q95" class="gt_row gt_right"> 0.0071</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">287</td>
<td headers="ess_tail" class="gt_row gt_right">338</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(reproductive_condition):dummy_repro1.1</td>
<td headers="mean" class="gt_row gt_right">−0.092     </td>
<td headers="median" class="gt_row gt_right">−0.039    </td>
<td headers="sd" class="gt_row gt_right">1.3    </td>
<td headers="mad" class="gt_row gt_right">1.3    </td>
<td headers="q5" class="gt_row gt_right">−2.2    </td>
<td headers="q95" class="gt_row gt_right"> 2.1   </td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">366</td>
<td headers="ess_tail" class="gt_row gt_right">393</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(reproductive_condition):dummy_repro1.2</td>
<td headers="mean" class="gt_row gt_right">−1.1       </td>
<td headers="median" class="gt_row gt_right">−1.2      </td>
<td headers="sd" class="gt_row gt_right">0.91   </td>
<td headers="mad" class="gt_row gt_right">0.94   </td>
<td headers="q5" class="gt_row gt_right">−2.6    </td>
<td headers="q95" class="gt_row gt_right"> 0.35  </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">422</td>
<td headers="ess_tail" class="gt_row gt_right">409</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(reproductive_condition):dummy_repro1.3</td>
<td headers="mean" class="gt_row gt_right">−0.64      </td>
<td headers="median" class="gt_row gt_right">−0.61     </td>
<td headers="sd" class="gt_row gt_right">0.73   </td>
<td headers="mad" class="gt_row gt_right">0.70   </td>
<td headers="q5" class="gt_row gt_right">−1.9    </td>
<td headers="q95" class="gt_row gt_right"> 0.52  </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">382</td>
<td headers="ess_tail" class="gt_row gt_right">396</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.1(reproductive_condition):dummy_repro1.4</td>
<td headers="mean" class="gt_row gt_right">−1.6       </td>
<td headers="median" class="gt_row gt_right">−1.5      </td>
<td headers="sd" class="gt_row gt_right">0.67   </td>
<td headers="mad" class="gt_row gt_right">0.69   </td>
<td headers="q5" class="gt_row gt_right">−2.9    </td>
<td headers="q95" class="gt_row gt_right">−0.63  </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">388</td>
<td headers="ess_tail" class="gt_row gt_right">413</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">(Intercept).2</td>
<td headers="mean" class="gt_row gt_right">−6.6       </td>
<td headers="median" class="gt_row gt_right">−6.6      </td>
<td headers="sd" class="gt_row gt_right">0.71   </td>
<td headers="mad" class="gt_row gt_right">0.65   </td>
<td headers="q5" class="gt_row gt_right">−7.8    </td>
<td headers="q95" class="gt_row gt_right">−5.4   </td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">364</td>
<td headers="ess_tail" class="gt_row gt_right">218</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.2(sample_type):dummy_rectal1.1</td>
<td headers="mean" class="gt_row gt_right">−0.000084  </td>
<td headers="median" class="gt_row gt_right"> 0.000026 </td>
<td headers="sd" class="gt_row gt_right">0.0050 </td>
<td headers="mad" class="gt_row gt_right">0.0050 </td>
<td headers="q5" class="gt_row gt_right">−0.0076 </td>
<td headers="q95" class="gt_row gt_right"> 0.0086</td>
<td headers="rhat" class="gt_row gt_right">0.997</td>
<td headers="ess_bulk" class="gt_row gt_right">428</td>
<td headers="ess_tail" class="gt_row gt_right">313</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.2(sample_type):dummy_rectal1.2</td>
<td headers="mean" class="gt_row gt_right">−0.000033  </td>
<td headers="median" class="gt_row gt_right">−0.00033  </td>
<td headers="sd" class="gt_row gt_right">0.0055 </td>
<td headers="mad" class="gt_row gt_right">0.0055 </td>
<td headers="q5" class="gt_row gt_right">−0.0089 </td>
<td headers="q95" class="gt_row gt_right"> 0.0092</td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">384</td>
<td headers="ess_tail" class="gt_row gt_right">342</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.2(day):sample_typeFecal.1</td>
<td headers="mean" class="gt_row gt_right"> 0.000061  </td>
<td headers="median" class="gt_row gt_right"> 0.000032 </td>
<td headers="sd" class="gt_row gt_right">0.0032 </td>
<td headers="mad" class="gt_row gt_right">0.0033 </td>
<td headers="q5" class="gt_row gt_right">−0.0054 </td>
<td headers="q95" class="gt_row gt_right"> 0.0056</td>
<td headers="rhat" class="gt_row gt_right">1.02 </td>
<td headers="ess_bulk" class="gt_row gt_right">296</td>
<td headers="ess_tail" class="gt_row gt_right">332</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.2(day):sample_typeFecal.2</td>
<td headers="mean" class="gt_row gt_right">−0.00030   </td>
<td headers="median" class="gt_row gt_right">−0.00024  </td>
<td headers="sd" class="gt_row gt_right">0.0056 </td>
<td headers="mad" class="gt_row gt_right">0.0057 </td>
<td headers="q5" class="gt_row gt_right">−0.010  </td>
<td headers="q95" class="gt_row gt_right"> 0.0092</td>
<td headers="rhat" class="gt_row gt_right">0.994</td>
<td headers="ess_bulk" class="gt_row gt_right">410</td>
<td headers="ess_tail" class="gt_row gt_right">417</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.2(day):sample_typeFecal.3</td>
<td headers="mean" class="gt_row gt_right">−0.00019   </td>
<td headers="median" class="gt_row gt_right">−0.00057  </td>
<td headers="sd" class="gt_row gt_right">0.0098 </td>
<td headers="mad" class="gt_row gt_right">0.0095 </td>
<td headers="q5" class="gt_row gt_right">−0.015  </td>
<td headers="q95" class="gt_row gt_right"> 0.018 </td>
<td headers="rhat" class="gt_row gt_right">0.998</td>
<td headers="ess_bulk" class="gt_row gt_right">406</td>
<td headers="ess_tail" class="gt_row gt_right">376</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.2(day):sample_typeFecal.4</td>
<td headers="mean" class="gt_row gt_right"> 0.020     </td>
<td headers="median" class="gt_row gt_right"> 0.047    </td>
<td headers="sd" class="gt_row gt_right">0.36   </td>
<td headers="mad" class="gt_row gt_right">0.32   </td>
<td headers="q5" class="gt_row gt_right">−0.60   </td>
<td headers="q95" class="gt_row gt_right"> 0.55  </td>
<td headers="rhat" class="gt_row gt_right">0.997</td>
<td headers="ess_bulk" class="gt_row gt_right">458</td>
<td headers="ess_tail" class="gt_row gt_right">449</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.2(day):sample_typeRectal.1</td>
<td headers="mean" class="gt_row gt_right">−0.00019   </td>
<td headers="median" class="gt_row gt_right"> 0.00017  </td>
<td headers="sd" class="gt_row gt_right">0.0064 </td>
<td headers="mad" class="gt_row gt_right">0.0067 </td>
<td headers="q5" class="gt_row gt_right">−0.012  </td>
<td headers="q95" class="gt_row gt_right"> 0.0097</td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">353</td>
<td headers="ess_tail" class="gt_row gt_right">416</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.2(day):sample_typeRectal.2</td>
<td headers="mean" class="gt_row gt_right"> 0.00083   </td>
<td headers="median" class="gt_row gt_right"> 0.00095  </td>
<td headers="sd" class="gt_row gt_right">0.010  </td>
<td headers="mad" class="gt_row gt_right">0.010  </td>
<td headers="q5" class="gt_row gt_right">−0.016  </td>
<td headers="q95" class="gt_row gt_right"> 0.018 </td>
<td headers="rhat" class="gt_row gt_right">0.995</td>
<td headers="ess_bulk" class="gt_row gt_right">394</td>
<td headers="ess_tail" class="gt_row gt_right">375</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.2(day):sample_typeRectal.3</td>
<td headers="mean" class="gt_row gt_right"> 0.0016    </td>
<td headers="median" class="gt_row gt_right"> 0.0023   </td>
<td headers="sd" class="gt_row gt_right">0.020  </td>
<td headers="mad" class="gt_row gt_right">0.019  </td>
<td headers="q5" class="gt_row gt_right">−0.033  </td>
<td headers="q95" class="gt_row gt_right"> 0.034 </td>
<td headers="rhat" class="gt_row gt_right">1.00 </td>
<td headers="ess_bulk" class="gt_row gt_right">394</td>
<td headers="ess_tail" class="gt_row gt_right">417</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.2(day):sample_typeRectal.4</td>
<td headers="mean" class="gt_row gt_right">−0.82      </td>
<td headers="median" class="gt_row gt_right">−0.84     </td>
<td headers="sd" class="gt_row gt_right">1.2    </td>
<td headers="mad" class="gt_row gt_right">1.1    </td>
<td headers="q5" class="gt_row gt_right">−2.5    </td>
<td headers="q95" class="gt_row gt_right"> 0.96  </td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">314</td>
<td headers="ess_tail" class="gt_row gt_right">196</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.2(day_of_year):sample_typeFecal.1</td>
<td headers="mean" class="gt_row gt_right">−1.4       </td>
<td headers="median" class="gt_row gt_right">−1.4      </td>
<td headers="sd" class="gt_row gt_right">1.4    </td>
<td headers="mad" class="gt_row gt_right">1.4    </td>
<td headers="q5" class="gt_row gt_right">−3.4    </td>
<td headers="q95" class="gt_row gt_right"> 0.95  </td>
<td headers="rhat" class="gt_row gt_right">0.998</td>
<td headers="ess_bulk" class="gt_row gt_right">377</td>
<td headers="ess_tail" class="gt_row gt_right">370</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.2(day_of_year):sample_typeFecal.2</td>
<td headers="mean" class="gt_row gt_right"> 3.2       </td>
<td headers="median" class="gt_row gt_right"> 3.2      </td>
<td headers="sd" class="gt_row gt_right">0.86   </td>
<td headers="mad" class="gt_row gt_right">0.80   </td>
<td headers="q5" class="gt_row gt_right"> 1.8    </td>
<td headers="q95" class="gt_row gt_right"> 4.7   </td>
<td headers="rhat" class="gt_row gt_right">1.01 </td>
<td headers="ess_bulk" class="gt_row gt_right">394</td>
<td headers="ess_tail" class="gt_row gt_right">398</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.2(day_of_year):sample_typeFecal.3</td>
<td headers="mean" class="gt_row gt_right"> 3.7       </td>
<td headers="median" class="gt_row gt_right"> 3.7      </td>
<td headers="sd" class="gt_row gt_right">0.96   </td>
<td headers="mad" class="gt_row gt_right">0.98   </td>
<td headers="q5" class="gt_row gt_right"> 2.2    </td>
<td headers="q95" class="gt_row gt_right"> 5.3   </td>
<td headers="rhat" class="gt_row gt_right">0.995</td>
<td headers="ess_bulk" class="gt_row gt_right">391</td>
<td headers="ess_tail" class="gt_row gt_right">266</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.2(day_of_year):sample_typeRectal.1</td>
<td headers="mean" class="gt_row gt_right">−0.33      </td>
<td headers="median" class="gt_row gt_right">−0.32     </td>
<td headers="sd" class="gt_row gt_right">1.0    </td>
<td headers="mad" class="gt_row gt_right">1.0    </td>
<td headers="q5" class="gt_row gt_right">−2.0    </td>
<td headers="q95" class="gt_row gt_right"> 1.2   </td>
<td headers="rhat" class="gt_row gt_right">0.994</td>
<td headers="ess_bulk" class="gt_row gt_right">385</td>
<td headers="ess_tail" class="gt_row gt_right">305</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.2(day_of_year):sample_typeRectal.2</td>
<td headers="mean" class="gt_row gt_right"> 2.0       </td>
<td headers="median" class="gt_row gt_right"> 1.9      </td>
<td headers="sd" class="gt_row gt_right">1.0    </td>
<td headers="mad" class="gt_row gt_right">1.0    </td>
<td headers="q5" class="gt_row gt_right"> 0.27   </td>
<td headers="q95" class="gt_row gt_right"> 3.7   </td>
<td headers="rhat" class="gt_row gt_right">0.999</td>
<td headers="ess_bulk" class="gt_row gt_right">420</td>
<td headers="ess_tail" class="gt_row gt_right">397</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">s.2(day_of_year):sample_typeRectal.3</td>
<td headers="mean" class="gt_row gt_right"> 1.7       </td>
<td headers="median" class="gt_row gt_right"> 1.7      </td>
<td headers="sd" class="gt_row gt_right">1.0    </td>
<td headers="mad" class="gt_row gt_right">1.0    </td>
<td headers="q5" class="gt_row gt_right">−0.043  </td>
<td headers="q95" class="gt_row gt_right"> 3.4   </td>
<td headers="rhat" class="gt_row gt_right">0.994</td>
<td headers="ess_bulk" class="gt_row gt_right">409</td>
<td headers="ess_tail" class="gt_row gt_right">447</td></tr>
  </tbody>
  
  
</table>
</div>
```

