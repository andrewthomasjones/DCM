
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DCM

<!-- badges: start -->
<!-- badges: end -->

The goal of DCM is to …

## Installation

You can install the development version of DCM from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("andrewthomasjones/DCM")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(DCM)

processedBW<-setUp(BWpriorities)

m1BW<-model_generator(processedBW, "fixed")
m2BW<-model_generator(processedBW, "random")
m3BW<-model_generator(processedBW, "one-factor")


r1BW<-runModel(m1BW)
#> [1] "You have the correct number of initial values."
#> Warning in sqrt(diag(solve(loglik1$hessian))): NaNs produced
r2BW<-runModel(m2BW)
#> [1] "You have the correct number of initial values."
#> Warning in sqrt(diag(solve(loglik1$hessian))): NaNs produced
r3BW<-runModel(m3BW)
#> [1] "You have the correct number of initial values."
#> Warning in sqrt(diag(solve(loglik1$hessian))): NaNs produced
```

``` r
summariseModelList(list(r1BW,r2BW,r3BW))
#> # A tibble: 3 × 6
#>   `Model Specifications` Parameters `Latent Variables` Log-Likelih…¹   AIC   BIC
#>   <chr>                       <dbl> <lgl>                      <dbl> <dbl> <dbl>
#> 1 "M1: "                         10 NA                        12086.  1.20  68.5
#> 2 "M2: "                         20 NA                        11227. 21.3  156. 
#> 3 "M3: "                         20 NA                        11227. 21.3  156. 
#> # … with abbreviated variable name ¹​`Log-Likelihood`
```

``` r
parPrintOld(r1BW)
#>  [1] "Printout of results for  2023-03-26 17:58\\\\"  
#>  [2] "log likelihood12085.8865\\\\"                   
#>  [3] "Number of parameters10\\\\"                     
#>  [4] "$\\\\"                                          
#>  [5] "\\begin{array}{lrr}"                            
#>  [6] "Model Parameter & Estimate & Standard Error\\\\"
#>  [7] "\\epsilon_{\\mu,1}&1.0592&NaN\\\\"              
#>  [8] "\\epsilon_{\\mu,2}&1.1867&NaN\\\\"              
#>  [9] "\\epsilon_{\\mu,3}&-0.6469&NaN\\\\"             
#> [10] "\\epsilon_{\\mu,4}&-0.113&NaN\\\\"              
#> [11] "\\epsilon_{\\mu,5}&0.4849&NaN\\\\"              
#> [12] "\\epsilon_{\\mu,6}&0.2573&NaN\\\\"              
#> [13] "\\epsilon_{\\mu,7}&-0.8583&NaN\\\\"             
#> [14] "\\epsilon_{\\mu,8}&-1.191&NaN\\\\"              
#> [15] "\\epsilon_{\\mu,9}&0.9775&NaN\\\\"              
#> [16] "\\epsilon_{\\mu,10}&-0.1564&NaN\\\\"            
#> [17] "\\end{array}"                                   
#> [18] "$\\\\"
```
