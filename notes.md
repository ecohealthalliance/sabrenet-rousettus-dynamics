Notes:

- Should we model the viruses as exclusive outcomes?
- What about the one beta-clade 4?
- We estimate fraction juvenile from the ones caught in these data?

Key Tests:
(For "positivity", we ask this for each virus and for all viruses)
-  Do fecal samples have different peak periods than rectal?  For each virus or all?
-  Do fecal samples have different average or peak positivity than rectal? For each virus or all?
-  Does time or season have an effect on posititivty
-  Does FMI have an effect on positivity?
-  Does % juveniles have an effect on positivity?


What groups are worth looking at? Here's the breakdown of detections by group:
```
> dat_cleaned |> filter(sample_type == "Rectal swab") |> group_by(age, gender, reproductive_condition, reproductive_status_based_on_condition) |> summarize(cov_detections = sum(cov_detected), n = n(), frac = scales::percent(cov_detections/n))
`summarise()` has grouped output by 'age', 'gender', 'reproductive_condition'. You can override using the `.groups` argument.
# A tibble: 13 × 7
# Groups:   age, gender, reproductive_condition [11]
   age   gender reproductive_condition reproductive_status_based_on_condition cov_detections     n frac 
   <chr> <chr>  <chr>                  <chr>                                           <dbl> <int> <chr>
 1 A     F      Lactating              Active                                              2    18 11%  
 2 A     F      Not pregnant           Active                                              2    30 7%   
 3 A     F      Not pregnant           Not-active                                          7    38 18%  
 4 A     F      Pregnant               Active                                              3    48 6%   
 5 A     M      Not scrotal            Not-active                                          1    37 3%   
 6 A     M      Scrotal                Active                                              2    85 2%   
 7 SA    F      Lactating              Active                                             10    36 28%  
 8 SA    F      Not pregnant           Active                                              1     5 20%  
 9 SA    F      Not pregnant           Not-active                                         27   186 15%  
10 SA    F      Pregnant               Active                                              0     3 0%   
11 SA    M      Not recorded           Not recorded                                        1     5 20%  
12 SA    M      Not scrotal            Not-active                                         37   216 17%  
13 SA    M      Scrotal                Active                                              1     3 33%  
```
