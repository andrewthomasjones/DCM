
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
#> 
#> 
#> ###################################################################################
#> #KKKKKKKKK    KKKKKKK     OOOOOOOOO     BBBBBBBBBBBBBBBBB   EEEEEEEEEEEEEEEEEEEEEE#
#> #K:::::::K    K:::::K   OO:::::::::OO   B::::::::::::::::B  E::::::::::::::::::::E#
#> #K:::::::K    K:::::K OO:::::::::::::OO B::::::BBBBBB:::::B E::::::::::::::::::::E#
#> #K:::::::K   K::::::KO:::::::OOO:::::::OBB:::::B     B:::::BEE::::::EEEEEEEEE::::E#
#> #KK::::::K  K:::::KKKO::::::O   O::::::O  B::::B     B:::::B  E:::::E       EEEEEE#
#> #K:::::K K:::::K   O:::::O     O:::::O  B::::B     B:::::B  E:::::E               #
#> #K::::::K:::::K    O:::::O     O:::::O  B::::BBBBBB:::::B   E::::::EEEEEEEEEE     #
#> #K:::::::::::K     O:::::O     O:::::O  B:::::::::::::BB    E:::::::::::::::E     #
#> #K:::::::::::K     O:::::O     O:::::O  B::::BBBBBB:::::B   E:::::::::::::::E     #
#> #K::::::K:::::K    O:::::O     O:::::O  B::::B     B:::::B  E::::::EEEEEEEEEE     #
#> #K:::::K K:::::K   O:::::O     O:::::O  B::::B     B:::::B  E:::::E               #
#> #KK::::::K  K:::::KKKO::::::O   O::::::O  B::::B     B:::::B  E:::::E       EEEEEE#
#> #K:::::::K   K::::::KO:::::::OOO:::::::OBB:::::BBBBBB::::::BEE::::::EEEEEEEE:::::E#
#> #K:::::::K    K:::::K OO:::::::::::::OO B:::::::::::::::::B E::::::::::::::::::::E#
#> #K:::::::K    K:::::K   OO:::::::::OO   B::::::::::::::::B  E::::::::::::::::::::E#
#> #KKKKKKKKK    KKKKKKK     OOOOOOOOO     BBBBBBBBBBBBBBBBB   EEEEEEEEEEEEEEEEEEEEEE#
#> ###################################################################################
#> 
#> version 0.1.2
#> 
#> Type 'citation("DCM")' for citing this R package in publications.

processedBW<-setUp(BWpriorities)

m1BW<-model_generator(processedBW, "fixed")
m2BW<-model_generator(processedBW, "random")
m3BW<-model_generator(processedBW, "one-factor")


r1BW<-runModel(m1BW)
#> Warning in sqrt(diag(solve(loglik1$hessian))): NaNs produced
r2BW<-runModel(m2BW)
r3BW<-runModel(m3BW)
#> Warning in sqrt(diag(solve(loglik1$hessian))): NaNs produced
```

``` r
summariseModelList(list(r1BW,r2BW,r3BW))
#> # A tibble: 3 × 6
#>   `Model Specifications`    Parameters `Latent Variables` `Log-Likelihood`   AIC
#>   <chr>                          <dbl> <lgl>                         <dbl> <dbl>
#> 1 M1: Generated model of t…         10 NA                           12086.  1.20
#> 2 M2: Generated model of t…         20 NA                           10158. 21.5 
#> 3 M3: Generated model of t…         20 NA                           11349. 21.3 
#> # ℹ 1 more variable: BIC <dbl>
```

``` r
parPrint(r1BW)
#>  [1] "Printout of results for Generated model of type fixed\\\\"
#>  [2] "Log-likelihood: 12085.8865\\\\"                           
#>  [3] "Number of parameters: 10\\\\"                             
#>  [4] "\\begin{center}"                                          
#>  [5] "\\begin{tabular}{lrr}"                                    
#>  [6] ""                                                         
#>  [7] "Model Parameter & Estimate & Standard Error\\\\"          
#>  [8] "\\hline \\\\"                                             
#>  [9] "$\\epsilon_ {\\mu, 1}$&$1.0592$&$NaN$\\\\"                
#> [10] "$\\epsilon_ {\\mu, 2}$&$1.1867$&$NaN$\\\\"                
#> [11] "$\\epsilon_ {\\mu, 3}$&$-0.6469$&$NaN$\\\\"               
#> [12] "$\\epsilon_ {\\mu, 4}$&$-0.113$&$NaN$\\\\"                
#> [13] "$\\epsilon_ {\\mu, 5}$&$0.4849$&$NaN$\\\\"                
#> [14] "$\\epsilon_ {\\mu, 6}$&$0.2573$&$NaN$\\\\"                
#> [15] "$\\epsilon_ {\\mu, 7}$&$-0.8583$&$NaN$\\\\"               
#> [16] "$\\epsilon_ {\\mu, 8}$&$-1.191$&$NaN$\\\\"                
#> [17] "$\\epsilon_ {\\mu, 9}$&$0.9775$&$NaN$\\\\"                
#> [18] "$\\epsilon_ {\\mu, 10}$&$-0.1564$&$NaN$\\\\"              
#> [19] "\\end{tabular}"                                           
#> [20] "\\end{center}"
```
