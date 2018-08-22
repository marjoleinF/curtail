curtail: an R package for test curtailment
==========================================

A curtailed test is a variable-length test, which allows for early stopping of item administration when further items are unlikely or unable to change the final (classification) decision. Package curtail allows for creating and assessing deterministically and stochastically (based on empirical proportions) curtailed tests.

Example
-------

To get a first impression of how curtail works, we take a dataset of item scores on a 20-item test:

``` r
library(curtail)
head(itemscores)
```

First, we will apply deterministic curtailment on a subset of 500 observations, using a cut-off value of 17.

``` r
tmp1 <- Curtail(itemscores[501:1000, ], Xstar = 17)
#>              full length
#> curtailed     at risk not at risk
#>   at risk         125           0
#>   not at risk       0         375
#> 
#> accuracy =  1 
#> sensitivity =  1 
#> specificity =  1
```

![](inst/README-figures/README-unnamed-chunk-3-1.png)

The results show the number of observations flagged as 'at risk' or 'not at risk', according to the curtailed and full-length test administration. Also, the number of items administrered is depicted in a histogram. No classification errors with respect to the full-length test decesion are made, as is always the case with detereministic curtailment.

We can also obtain some desciptive statistics about the number of items administered and proportion of observations for which the testing was ceased before the full-length test was administered:

``` r
tmp1$curtailed.test.length.distribution
#> $mean
#> [1] 18.196
#> 
#> $standard.deviation
#> [1] 1.618277
#> 
#> $median
#> [1] 18
#> 
#> $proportion.curtailed
#> [1] 0.76
```

We managed to obtain a substantial reduction in test length. But perhaps we can further reduce test length through stochastic curtailment. We use the first 500 observations for training and the next 500 observations for testing:

``` r
tmp2 <- stochCurtail(itemscores[1:500,], dataset.test = itemscores[501:1000,], 
                     Xstar = 17)
#>              full length
#> curtailed     at risk not at risk
#>   at risk         121           0
#>   not at risk       4         375
#> 
#> accuracy =  0.992 
#> sensitivity =  0.968 
#> specificity =  1
```

![](inst/README-figures/README-unnamed-chunk-5-1.png)

``` r
tmp2$curtailed.test.length.distribution
#> $mean
#> [1] 17.37
#> 
#> $standard.deviation
#> [1] 2.435768
#> 
#> $median
#> [1] 18
#> 
#> $proportion.curtailed
#> [1] 0.77
```

We were able to reduce the number of items administered somewhat, at the cost of four incorrect decisions (out of 500).

We can further reduce test length by lowering the gamma values, that is, the thresholds for the probability that the classification decision of the stochastically curtailed version will match that of the full-length test. Note that lower gamma values will thus yield higher misclassification rates:

``` r
tmp3 <- stochCurtail(itemscores[1:500, ], dataset.test = itemscores[501:1000,], 
                     Xstar = 17, gamma0 = .6, gamma1 = .6, plot = FALSE)
#>              full length
#> curtailed     at risk not at risk
#>   at risk         100           4
#>   not at risk      25         371
#> 
#> accuracy =  0.942 
#> sensitivity =  0.8 
#> specificity =  0.9893333
```

``` r
tmp3$curtailed.test.length.distribution
#> $mean
#> [1] 13.496
#> 
#> $standard.deviation
#> [1] 3.503503
#> 
#> $median
#> [1] 14
#> 
#> $proportion.curtailed
#> [1] 0.932
```

We see that sensitivity is more strongly affected by lowering the gamma values. This is partly due to the value for Xstar we specified, which yielded the following base rate:

``` r
prop.table(table(rowSums(itemscores) > 17))
#> 
#> FALSE  TRUE 
#> 0.761 0.239
```

If the 'at-risk' class would have been more prevalent, then lowering both gamma values would have more strongly affected specificity. The value of gamma1 specifies the threshold for incorrect 'at-risk' decisions, whereas the value of gamma0 controls the threshold for incorrect 'not-at-risk' decisions. Thus, if we want to improve sensitivity, we should increase gamma0:

``` r
tmp4 <- stochCurtail(itemscores[1:500, ], dataset.test = itemscores[501:1000,], 
                     Xstar = 17, gamma0 = .9, gamma1 = .6, plot = FALSE)
#>              full length
#> curtailed     at risk not at risk
#>   at risk         119           4
#>   not at risk       6         371
#> 
#> accuracy =  0.98 
#> sensitivity =  0.952 
#> specificity =  0.9893333
```

Alternatively, if we want to improve specificity, we should increase the value of gamma1:

``` r
tmp5 <- stochCurtail(itemscores[1:500, ], dataset.test = itemscores[501:1000,], 
                     Xstar = 17, gamma0 = .6, gamma1 = .9, plot = FALSE)
#>              full length
#> curtailed     at risk not at risk
#>   at risk         100           1
#>   not at risk      25         374
#> 
#> accuracy =  0.948 
#> sensitivity =  0.8 
#> specificity =  0.9973333
```

Perhaps we want to inspect test length distributions for at-risk observations separately:

``` r
hist(tmp3$test.results$current.item[tmp2$test.results$curtailed.decision == "at risk"], 
     xlab = "Number of items administered", 
     main = "Test lengths for at-risk observations")
```

![](inst/README-figures/README-unnamed-chunk-12-1.png)

If we want to obtain tables with item-specific cutoff values, we can use the `Table` function (for deterministic curtailment):

``` r
Table(itemscores, Xstar = 17)
#>         item1 item2 item3 item4 item5 item6 item7 item8 item9 item10
#> no.risk    NA    NA    NA    NA    NA    NA    NA    NA    NA     NA
#> risk       NA    NA    NA    NA    NA    17    17    17    17     17
#>         item11 item12 item13 item14 item15 item16 item17 item18 item19
#> no.risk     NA     NA     NA     NA      1      4      7     10     13
#> risk        17     17     17     17     17     17     17     17     17
#>         item20
#> no.risk     16
#> risk        17
```

Values NA indicate that curtailment is not yet possible for that item and decision. For stochastic curtailment, we can employ the `stochTable` function:

``` r
stochTable(itemscores, Xstar = 17)
#>         item1 item2 item3 item4 item5 item6 item7 item8 item9 item10
#> no.risk    NA    NA    NA    NA    NA    NA    NA    NA    NA      0
#> risk       NA    NA    NA    NA    14    14    14    15    15     15
#>         item11 item12 item13 item14 item15 item16 item17 item18 item19
#> no.risk      1      2      3      5      6      8      9     10     13
#> risk        16     16     16     17     17     17     17     17     17
#>         item20
#> no.risk     16
#> risk        17
```

Here we see that stochastic curtailment allows for earlier stopping of item administration than deterministic curtailment.

Perhaps we want to assess the accuracy of decisions based on stochastic curtailment using leave-one-out cross validation. This can be done using the `stochCurtailXval` function. As this is computationally intensive, in this example we only apply the function to the first 100 observations, but normally we should apply this function to the whole dataset:

``` r
stochCurtailXval(itemscores[1:100,], Xstar = 17)
```

![](inst/README-figures/README-unnamed-chunk-15-1.png)

    #>              full length
    #> curtailed     at risk not at risk
    #>   at risk          11           0
    #>   not at risk       2          87
    #> 
    #> accuracy =  0.98 
    #> sensitivity =  0.8461538 
    #> specificity =  1

References
----------

Finkelman, M.D., Smits, N., Kim, W. & Riley, B. (2012). Curtailment and stochastic curtailment to shorten the CES-D. Applied Psychological Measurement, 36(8), 632-658.

Fokkema, M., Smits, N., Finkelman, M. D., Kelderman, H., & Cuijpers, P. (2014). Curtailment: A method to reduce the length of self-report questionnaires while maintaining diagnostic accuracy. Psychiatry Research 215, 477-482.

Fokkema, M., Smits, N., Kelderman, H., Carlier, I.V. & Van Hemert, A.M. (2014). Combining decision trees and stochastic curtailment for assessment length reduction of test batteries used for classification. Applied Psychological Measurement, 38(1), 3-17.
