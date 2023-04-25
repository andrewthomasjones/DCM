
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
#> Warning in sqrt(diag(solve(loglik1$hessian))): NaNs produced
r2BW<-runModel(m2BW)
#> Warning in sqrt(diag(solve(loglik1$hessian))): NaNs produced
r3BW<-runModel(m3BW)
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
cat(parPrint(r1BW))
#> Printout of results for \\ Log-likelihood: 12085.8865\\ Number of parameters: 10\\ \begin{center} \begin{tabular}{lrr}  Model Parameter & Estimate & Standard Error\\ \hline \\ $\epsilon_ {\mu, 1}$&$1.0592$&$NaN$\\ $\epsilon_ {\mu, 2}$&$1.1867$&$NaN$\\ $\epsilon_ {\mu, 3}$&$-0.6469$&$NaN$\\ $\epsilon_ {\mu, 4}$&$-0.113$&$NaN$\\ $\epsilon_ {\mu, 5}$&$0.4849$&$NaN$\\ $\epsilon_ {\mu, 6}$&$0.2573$&$NaN$\\ $\epsilon_ {\mu, 7}$&$-0.8583$&$NaN$\\ $\epsilon_ {\mu, 8}$&$-1.191$&$NaN$\\ $\epsilon_ {\mu, 9}$&$0.9775$&$NaN$\\ $\epsilon_ {\mu, 10}$&$-0.1564$&$NaN$\\ \end{tabular} \end{center}
```
