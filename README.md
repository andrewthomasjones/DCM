
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

## Examples

### Basic Models

Load example data

``` r
library(DCM)
#> Loading required package: foreach
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
#> version 0.2.0
#> 
#> Type 'citation("DCM")' for citing this R package in publications.
```

``` r
#load test data
processedBW <- setUp(BWpriorities)
```

Set up models of different kinds

``` r
# set up models for best-worst data
m1BW <- modelGenerator(processedBW, "fixed")
m2BW <- modelGenerator(processedBW, "random")
m3BW <- modelGenerator(processedBW, "one-factor")
```

Get estimates

``` r
#run models for best-worst data
r1BW <- runModel(m1BW)
r2BW <- runModel(m2BW)
r3BW <- runModel(m3BW)
#> Warning in sqrt(diag(object$cov.fixed)): NaNs produced
```

Print results for a model

``` r
parPrint(r1BW)
#>  [1] "Printout of results for fixed\\\\"                
#>  [2] "Log-likelihood: 12085.8865\\\\"                   
#>  [3] "Number of parameters: 11\\\\"                     
#>  [4] "\\begin{center}"                                  
#>  [5] "\\begin{tabular}{lrr}"                            
#>  [6] ""                                                 
#>  [7] "Model Parameter & Estimate & Standard Error\\\\"  
#>  [8] "\\hline \\\\"                                     
#>  [9] "$\\epsilon_ {\\mu, 1}$&$1.9592$&$70393.3518$\\\\" 
#> [10] "$\\epsilon_ {\\mu, 2}$&$0.7436$&$70393.3509$\\\\" 
#> [11] "$\\epsilon_ {\\mu, 3}$&$2.0867$&$70393.3521$\\\\" 
#> [12] "$\\epsilon_ {\\mu, 4}$&$0.2531$&$70393.3517$\\\\" 
#> [13] "$\\epsilon_ {\\mu, 5}$&$0.787$&$70393.3509$\\\\"  
#> [14] "$\\epsilon_ {\\mu, 6}$&$1.3849$&$70393.3511$\\\\" 
#> [15] "$\\epsilon_ {\\mu, 7}$&$1.1573$&$70393.3508$\\\\" 
#> [16] "$\\epsilon_ {\\mu, 8}$&$0.0417$&$70393.3518$\\\\" 
#> [17] "$\\epsilon_ {\\mu, 9}$&$-0.291$&$70393.3523$\\\\" 
#> [18] "$\\epsilon_ {\\mu, 10}$&$1.8775$&$70393.3519$\\\\"
#> [19] "$\\delta_ {\\sigma, 1}$&$1.9592$&$70393.3518$\\\\"
#> [20] "\\end{tabular}"                                   
#> [21] "\\end{center}"
```

### MTMM Models

Join two datasets:

``` r
processedDCE <- setUp(DCEpriorities)

processedBW_rem <- removeVariables(processedBW, "Accessibility_BW")

combined_data <- joinChoiceDatasets(processedBW_rem, processedDCE)
```

Run MTMM model

``` r
mtmm_model <- modelGenerator(combined_data, "mtmm")

mtmm_results <- runModel(mtmm_model)
#> Warning in sqrt(diag(solve(loglik1$hessian))): NaNs produced
```

Print results:

``` r
parPrint(mtmm_results)
#>  [1] "Printout of results for \\\\"                   
#>  [2] "Log-likelihood: 12634.0156\\\\"                 
#>  [3] "Number of parameters: 63\\\\"                   
#>  [4] "\\begin{center}"                                
#>  [5] "\\begin{tabular}{lrr}"                          
#>  [6] ""                                               
#>  [7] "Model Parameter & Estimate & Standard Error\\\\"
#>  [8] "\\hline \\\\"                                   
#>  [9] "$\\epsilon_ {\\mu, 1}$&$2.0923$&$0.099$\\\\"    
#> [10] "$\\epsilon_ {\\mu, 2}$&$2.5157$&$0.1017$\\\\"   
#> [11] "$\\epsilon_ {\\mu, 3}$&$-1.1263$&$0.1104$\\\\"  
#> [12] "$\\epsilon_ {\\mu, 4}$&$-0.0166$&$0.0771$\\\\"  
#> [13] "$\\epsilon_ {\\mu, 5}$&$1.1329$&$0.0934$\\\\"   
#> [14] "$\\epsilon_ {\\mu, 6}$&$0.7545$&$0.082$\\\\"    
#> [15] "$\\epsilon_ {\\mu, 7}$&$-1.2615$&$0.0877$\\\\"  
#> [16] "$\\epsilon_ {\\mu, 8}$&$-1.9138$&$0.1027$\\\\"  
#> [17] "$\\epsilon_ {\\mu, 9}$&$2.2839$&$0.1472$\\\\"   
#> [18] "$\\epsilon_ {\\mu, 10}$&$1.9492$&$0.0651$\\\\"  
#> [19] "$\\epsilon_ {\\mu, 11}$&$0.9747$&$0.0553$\\\\"  
#> [20] "$\\epsilon_ {\\mu, 12}$&$0.3261$&$0.049$\\\\"   
#> [21] "$\\epsilon_ {\\mu, 13}$&$0.095$&$0.0462$\\\\"   
#> [22] "$\\epsilon_ {\\mu, 14}$&$0.4094$&$0.0481$\\\\"  
#> [23] "$\\epsilon_ {\\mu, 15}$&$0.256$&$0.0504$\\\\"   
#> [24] "$\\epsilon_ {\\mu, 16}$&$0.0286$&$0.0492$\\\\"  
#> [25] "$\\epsilon_ {\\mu, 17}$&$-0.0082$&$0.0485$\\\\" 
#> [26] "$\\epsilon_ {\\mu, 18}$&$-0.8557$&$0.0575$\\\\" 
#> [27] "$\\gamma_ {1, 1}$&$2.0923$&$0.099$\\\\"         
#> [28] "$\\gamma_ {1, 3}$&$2.0923$&$0.099$\\\\"         
#> [29] "$\\gamma_ {2, 1}$&$2.5157$&$0.1017$\\\\"        
#> [30] "$\\gamma_ {2, 4}$&$2.5157$&$0.1017$\\\\"        
#> [31] "$\\gamma_ {3, 1}$&$-1.1263$&$0.1104$\\\\"       
#> [32] "$\\gamma_ {3, 5}$&$-1.1263$&$0.1104$\\\\"       
#> [33] "$\\gamma_ {4, 1}$&$-0.0166$&$0.0771$\\\\"       
#> [34] "$\\gamma_ {4, 6}$&$-0.0166$&$0.0771$\\\\"       
#> [35] "$\\gamma_ {5, 1}$&$1.1329$&$0.0934$\\\\"        
#> [36] "$\\gamma_ {5, 7}$&$1.1329$&$0.0934$\\\\"        
#> [37] "$\\gamma_ {6, 1}$&$0.7545$&$0.082$\\\\"         
#> [38] "$\\gamma_ {6, 8}$&$0.7545$&$0.082$\\\\"         
#> [39] "$\\gamma_ {7, 1}$&$-1.2615$&$0.0877$\\\\"       
#> [40] "$\\gamma_ {7, 9}$&$-1.2615$&$0.0877$\\\\"       
#> [41] "$\\gamma_ {8, 1}$&$-1.9138$&$0.1027$\\\\"       
#> [42] "$\\gamma_ {8, 10}$&$-1.9138$&$0.1027$\\\\"      
#> [43] "$\\gamma_ {9, 1}$&$2.2839$&$0.1472$\\\\"        
#> [44] "$\\gamma_ {9, 11}$&$2.2839$&$0.1472$\\\\"       
#> [45] "$\\gamma_ {10, 2}$&$1.9492$&$0.0651$\\\\"       
#> [46] "$\\gamma_ {10, 3}$&$1.9492$&$0.0651$\\\\"       
#> [47] "$\\gamma_ {11, 2}$&$0.9747$&$0.0553$\\\\"       
#> [48] "$\\gamma_ {11, 4}$&$0.9747$&$0.0553$\\\\"       
#> [49] "$\\gamma_ {12, 2}$&$0.3261$&$0.049$\\\\"        
#> [50] "$\\gamma_ {12, 5}$&$0.3261$&$0.049$\\\\"        
#> [51] "$\\gamma_ {13, 2}$&$0.095$&$0.0462$\\\\"        
#> [52] "$\\gamma_ {13, 6}$&$0.095$&$0.0462$\\\\"        
#> [53] "$\\gamma_ {14, 2}$&$0.4094$&$0.0481$\\\\"       
#> [54] "$\\gamma_ {14, 7}$&$0.4094$&$0.0481$\\\\"       
#> [55] "$\\gamma_ {15, 2}$&$0.256$&$0.0504$\\\\"        
#> [56] "$\\gamma_ {15, 8}$&$0.256$&$0.0504$\\\\"        
#> [57] "$\\gamma_ {16, 2}$&$0.0286$&$0.0492$\\\\"       
#> [58] "$\\gamma_ {16, 9}$&$0.0286$&$0.0492$\\\\"       
#> [59] "$\\gamma_ {17, 2}$&$-0.0082$&$0.0485$\\\\"      
#> [60] "$\\gamma_ {17, 10}$&$-0.0082$&$0.0485$\\\\"     
#> [61] "$\\gamma_ {18, 2}$&$-0.8557$&$0.0575$\\\\"      
#> [62] "$\\gamma_ {18, 11}$&$-0.8557$&$0.0575$\\\\"     
#> [63] "$\\beta_ {3, 12}$&$-1.1263$&$0.1104$\\\\"       
#> [64] "$\\beta_ {4, 12}$&$-0.0166$&$0.0771$\\\\"       
#> [65] "$\\beta_ {5, 12}$&$1.1329$&$0.0934$\\\\"        
#> [66] "$\\beta_ {6, 12}$&$0.7545$&$0.082$\\\\"         
#> [67] "$\\beta_ {7, 12}$&$-1.2615$&$0.0877$\\\\"       
#> [68] "$\\beta_ {8, 12}$&$-1.9138$&$0.1027$\\\\"       
#> [69] "$\\beta_ {9, 12}$&$2.2839$&$0.1472$\\\\"        
#> [70] "$\\beta_ {10, 12}$&$1.9492$&$0.0651$\\\\"       
#> [71] "$\\beta_ {11, 12}$&$0.9747$&$0.0553$\\\\"       
#> [72] "\\end{tabular}"                                 
#> [73] "\\end{center}"
```

### Graph Output

#### Graph the basic models:

``` r
library(DiagrammeR)
render_graph(modelGraph(m2BW))
```

<div class="grViz html-widget html-fill-item" id="htmlwidget-22faee35d55568e46279" style="width:100%;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-22faee35d55568e46279">{"x":{"diagram":"digraph {\n\ngraph [layout = \"dot\",\n       outputorder = \"edgesfirst\",\n       bgcolor = \"white\",\n       concentrate = \"true\",\n       ranksep = \"2\"]\n\nnode [fontname = \"Helvetica\",\n      fontsize = \"10\",\n      shape = \"circle\",\n      fixedsize = \"true\",\n      width = \"0.5\",\n      style = \"filled\",\n      fillcolor = \"aliceblue\",\n      color = \"gray70\",\n      fontcolor = \"gray50\"]\n\nedge [fontname = \"Helvetica\",\n     fontsize = \"8\",\n     len = \"1.5\",\n     color = \"gray80\",\n     arrowsize = \"0.5\"]\n\n  \"1\" [label = <HoP_1: ζ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"2\" [label = <HoP_2: ζ<FONT POINT-SIZE=\"8\"><SUB>2<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"3\" [label = <HoP_3: ζ<FONT POINT-SIZE=\"8\"><SUB>3<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"4\" [label = <HoP_4: ζ<FONT POINT-SIZE=\"8\"><SUB>4<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"5\" [label = <HoP_5: ζ<FONT POINT-SIZE=\"8\"><SUB>5<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"6\" [label = <HoP_6: ζ<FONT POINT-SIZE=\"8\"><SUB>6<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"7\" [label = <HoP_7: ζ<FONT POINT-SIZE=\"8\"><SUB>7<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"8\" [label = <HoP_8: ζ<FONT POINT-SIZE=\"8\"><SUB>8<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"9\" [label = <HoP_9: ζ<FONT POINT-SIZE=\"8\"><SUB>9<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"10\" [label = <HoP_10: ζ<FONT POINT-SIZE=\"8\"><SUB>10<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"11\" [label = <Safety_BW: μ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"12\" [label = <Reliability_BW: μ<FONT POINT-SIZE=\"8\"><SUB>2<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"13\" [label = <Comfort_BW: μ<FONT POINT-SIZE=\"8\"><SUB>3<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"14\" [label = <Ease_of_Use_BW: μ<FONT POINT-SIZE=\"8\"><SUB>4<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"15\" [label = <Convenience_BW: μ<FONT POINT-SIZE=\"8\"><SUB>5<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"16\" [label = <Efficiency_BW: μ<FONT POINT-SIZE=\"8\"><SUB>6<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"17\" [label = <Information_BW: μ<FONT POINT-SIZE=\"8\"><SUB>7<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"18\" [label = <Staff_helpfulness_BW: μ<FONT POINT-SIZE=\"8\"><SUB>8<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"19\" [label = <Affordability_BW: μ<FONT POINT-SIZE=\"8\"><SUB>9<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"20\" [label = <Accessibility_BW: μ<FONT POINT-SIZE=\"8\"><SUB>10<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n\"1\"->\"11\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT>>] \n\"2\"->\"12\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>2<\/SUB><\/FONT>>] \n\"3\"->\"13\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>3<\/SUB><\/FONT>>] \n\"4\"->\"14\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>4<\/SUB><\/FONT>>] \n\"5\"->\"15\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>5<\/SUB><\/FONT>>] \n\"6\"->\"16\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>6<\/SUB><\/FONT>>] \n\"7\"->\"17\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>7<\/SUB><\/FONT>>] \n\"8\"->\"18\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>8<\/SUB><\/FONT>>] \n\"9\"->\"19\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>9<\/SUB><\/FONT>>] \n\"10\"->\"20\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>10<\/SUB><\/FONT>>] \n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

``` r
render_graph(modelGraph(m2BW))
```

<div class="grViz html-widget html-fill-item" id="htmlwidget-add2b30ed3dac9352aec" style="width:100%;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-add2b30ed3dac9352aec">{"x":{"diagram":"digraph {\n\ngraph [layout = \"dot\",\n       outputorder = \"edgesfirst\",\n       bgcolor = \"white\",\n       concentrate = \"true\",\n       ranksep = \"2\"]\n\nnode [fontname = \"Helvetica\",\n      fontsize = \"10\",\n      shape = \"circle\",\n      fixedsize = \"true\",\n      width = \"0.5\",\n      style = \"filled\",\n      fillcolor = \"aliceblue\",\n      color = \"gray70\",\n      fontcolor = \"gray50\"]\n\nedge [fontname = \"Helvetica\",\n     fontsize = \"8\",\n     len = \"1.5\",\n     color = \"gray80\",\n     arrowsize = \"0.5\"]\n\n  \"1\" [label = <HoP_1: ζ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"2\" [label = <HoP_2: ζ<FONT POINT-SIZE=\"8\"><SUB>2<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"3\" [label = <HoP_3: ζ<FONT POINT-SIZE=\"8\"><SUB>3<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"4\" [label = <HoP_4: ζ<FONT POINT-SIZE=\"8\"><SUB>4<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"5\" [label = <HoP_5: ζ<FONT POINT-SIZE=\"8\"><SUB>5<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"6\" [label = <HoP_6: ζ<FONT POINT-SIZE=\"8\"><SUB>6<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"7\" [label = <HoP_7: ζ<FONT POINT-SIZE=\"8\"><SUB>7<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"8\" [label = <HoP_8: ζ<FONT POINT-SIZE=\"8\"><SUB>8<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"9\" [label = <HoP_9: ζ<FONT POINT-SIZE=\"8\"><SUB>9<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"10\" [label = <HoP_10: ζ<FONT POINT-SIZE=\"8\"><SUB>10<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"11\" [label = <Safety_BW: μ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"12\" [label = <Reliability_BW: μ<FONT POINT-SIZE=\"8\"><SUB>2<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"13\" [label = <Comfort_BW: μ<FONT POINT-SIZE=\"8\"><SUB>3<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"14\" [label = <Ease_of_Use_BW: μ<FONT POINT-SIZE=\"8\"><SUB>4<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"15\" [label = <Convenience_BW: μ<FONT POINT-SIZE=\"8\"><SUB>5<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"16\" [label = <Efficiency_BW: μ<FONT POINT-SIZE=\"8\"><SUB>6<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"17\" [label = <Information_BW: μ<FONT POINT-SIZE=\"8\"><SUB>7<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"18\" [label = <Staff_helpfulness_BW: μ<FONT POINT-SIZE=\"8\"><SUB>8<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"19\" [label = <Affordability_BW: μ<FONT POINT-SIZE=\"8\"><SUB>9<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"20\" [label = <Accessibility_BW: μ<FONT POINT-SIZE=\"8\"><SUB>10<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n\"1\"->\"11\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT>>] \n\"2\"->\"12\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>2<\/SUB><\/FONT>>] \n\"3\"->\"13\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>3<\/SUB><\/FONT>>] \n\"4\"->\"14\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>4<\/SUB><\/FONT>>] \n\"5\"->\"15\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>5<\/SUB><\/FONT>>] \n\"6\"->\"16\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>6<\/SUB><\/FONT>>] \n\"7\"->\"17\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>7<\/SUB><\/FONT>>] \n\"8\"->\"18\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>8<\/SUB><\/FONT>>] \n\"9\"->\"19\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>9<\/SUB><\/FONT>>] \n\"10\"->\"20\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>10<\/SUB><\/FONT>>] \n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

``` r
render_graph(modelGraph(m3BW))
```

<div class="grViz html-widget html-fill-item" id="htmlwidget-9453652d42f647e26a47" style="width:100%;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-9453652d42f647e26a47">{"x":{"diagram":"digraph {\n\ngraph [layout = \"dot\",\n       outputorder = \"edgesfirst\",\n       bgcolor = \"white\",\n       concentrate = \"true\",\n       ranksep = \"2\"]\n\nnode [fontname = \"Helvetica\",\n      fontsize = \"10\",\n      shape = \"circle\",\n      fixedsize = \"true\",\n      width = \"0.5\",\n      style = \"filled\",\n      fillcolor = \"aliceblue\",\n      color = \"gray70\",\n      fontcolor = \"gray50\"]\n\nedge [fontname = \"Helvetica\",\n     fontsize = \"8\",\n     len = \"1.5\",\n     color = \"gray80\",\n     arrowsize = \"0.5\"]\n\n  \"1\" [label = <HoP_1: ζ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"2\" [label = <Safety_BW: μ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"3\" [label = <Reliability_BW: μ<FONT POINT-SIZE=\"8\"><SUB>2<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"4\" [label = <Comfort_BW: μ<FONT POINT-SIZE=\"8\"><SUB>3<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"5\" [label = <Ease_of_Use_BW: μ<FONT POINT-SIZE=\"8\"><SUB>4<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"6\" [label = <Convenience_BW: μ<FONT POINT-SIZE=\"8\"><SUB>5<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"7\" [label = <Efficiency_BW: μ<FONT POINT-SIZE=\"8\"><SUB>6<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"8\" [label = <Information_BW: μ<FONT POINT-SIZE=\"8\"><SUB>7<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"9\" [label = <Staff_helpfulness_BW: μ<FONT POINT-SIZE=\"8\"><SUB>8<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"10\" [label = <Affordability_BW: μ<FONT POINT-SIZE=\"8\"><SUB>9<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"11\" [label = <Accessibility_BW: μ<FONT POINT-SIZE=\"8\"><SUB>10<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n\"1\"->\"2\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>1, 1<\/SUB><\/FONT>>] \n\"1\"->\"3\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>2, 1<\/SUB><\/FONT>>] \n\"1\"->\"4\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>3, 1<\/SUB><\/FONT>>] \n\"1\"->\"5\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>4, 1<\/SUB><\/FONT>>] \n\"1\"->\"6\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>5, 1<\/SUB><\/FONT>>] \n\"1\"->\"7\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>6, 1<\/SUB><\/FONT>>] \n\"1\"->\"8\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>7, 1<\/SUB><\/FONT>>] \n\"1\"->\"9\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>8, 1<\/SUB><\/FONT>>] \n\"1\"->\"10\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>9, 1<\/SUB><\/FONT>>] \n\"1\"->\"11\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>10, 1<\/SUB><\/FONT>>] \n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

#### Graph model estimates:

``` r
render_graph(modelGraph(r1BW))
```

<div class="grViz html-widget html-fill-item" id="htmlwidget-77857e5507de38e1e0cd" style="width:100%;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-77857e5507de38e1e0cd">{"x":{"diagram":"digraph {\n\ngraph [layout = \"dot\",\n       outputorder = \"edgesfirst\",\n       bgcolor = \"white\",\n       concentrate = \"true\",\n       ranksep = \"2\"]\n\nnode [fontname = \"Helvetica\",\n      fontsize = \"10\",\n      shape = \"circle\",\n      fixedsize = \"true\",\n      width = \"0.5\",\n      style = \"filled\",\n      fillcolor = \"aliceblue\",\n      color = \"gray70\",\n      fontcolor = \"gray50\"]\n\nedge [fontname = \"Helvetica\",\n     fontsize = \"8\",\n     len = \"1.5\",\n     color = \"gray80\",\n     arrowsize = \"0.5\"]\n\n  \"2\" [label = <Safety_BW: μ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT> = 1.959>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"3\" [label = <Reliability_BW: μ<FONT POINT-SIZE=\"8\"><SUB>2<\/SUB><\/FONT> = 0.744>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"4\" [label = <Comfort_BW: μ<FONT POINT-SIZE=\"8\"><SUB>3<\/SUB><\/FONT> = 2.087>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"5\" [label = <Ease_of_Use_BW: μ<FONT POINT-SIZE=\"8\"><SUB>4<\/SUB><\/FONT> = 0.253>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"6\" [label = <Convenience_BW: μ<FONT POINT-SIZE=\"8\"><SUB>5<\/SUB><\/FONT> = 0.787>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"7\" [label = <Efficiency_BW: μ<FONT POINT-SIZE=\"8\"><SUB>6<\/SUB><\/FONT> = 1.385>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"8\" [label = <Information_BW: μ<FONT POINT-SIZE=\"8\"><SUB>7<\/SUB><\/FONT> = 1.157>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"9\" [label = <Staff_helpfulness_BW: μ<FONT POINT-SIZE=\"8\"><SUB>8<\/SUB><\/FONT> = 0.042>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"10\" [label = <Affordability_BW: μ<FONT POINT-SIZE=\"8\"><SUB>9<\/SUB><\/FONT> = -0.291>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"11\" [label = <Accessibility_BW: μ<FONT POINT-SIZE=\"8\"><SUB>10<\/SUB><\/FONT> = 1.878>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

``` r
render_graph(modelGraph(r2BW))
```

<div class="grViz html-widget html-fill-item" id="htmlwidget-0a68f0be577be34a14c1" style="width:100%;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-0a68f0be577be34a14c1">{"x":{"diagram":"digraph {\n\ngraph [layout = \"dot\",\n       outputorder = \"edgesfirst\",\n       bgcolor = \"white\",\n       concentrate = \"true\",\n       ranksep = \"2\"]\n\nnode [fontname = \"Helvetica\",\n      fontsize = \"10\",\n      shape = \"circle\",\n      fixedsize = \"true\",\n      width = \"0.5\",\n      style = \"filled\",\n      fillcolor = \"aliceblue\",\n      color = \"gray70\",\n      fontcolor = \"gray50\"]\n\nedge [fontname = \"Helvetica\",\n     fontsize = \"8\",\n     len = \"1.5\",\n     color = \"gray80\",\n     arrowsize = \"0.5\"]\n\n  \"1\" [label = <HoP_1: ζ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"2\" [label = <HoP_2: ζ<FONT POINT-SIZE=\"8\"><SUB>2<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"3\" [label = <HoP_3: ζ<FONT POINT-SIZE=\"8\"><SUB>3<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"4\" [label = <HoP_4: ζ<FONT POINT-SIZE=\"8\"><SUB>4<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"5\" [label = <HoP_5: ζ<FONT POINT-SIZE=\"8\"><SUB>5<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"6\" [label = <HoP_6: ζ<FONT POINT-SIZE=\"8\"><SUB>6<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"7\" [label = <HoP_7: ζ<FONT POINT-SIZE=\"8\"><SUB>7<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"8\" [label = <HoP_8: ζ<FONT POINT-SIZE=\"8\"><SUB>8<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"9\" [label = <HoP_9: ζ<FONT POINT-SIZE=\"8\"><SUB>9<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"10\" [label = <HoP_10: ζ<FONT POINT-SIZE=\"8\"><SUB>10<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"11\" [label = <Safety_BW: μ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT> = 3.253>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"12\" [label = <Reliability_BW: μ<FONT POINT-SIZE=\"8\"><SUB>2<\/SUB><\/FONT> = 0.379>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"13\" [label = <Comfort_BW: μ<FONT POINT-SIZE=\"8\"><SUB>3<\/SUB><\/FONT> = 3.274>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"14\" [label = <Ease_of_Use_BW: μ<FONT POINT-SIZE=\"8\"><SUB>4<\/SUB><\/FONT> = -0.77>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"15\" [label = <Convenience_BW: μ<FONT POINT-SIZE=\"8\"><SUB>5<\/SUB><\/FONT> = 0.46>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"16\" [label = <Efficiency_BW: μ<FONT POINT-SIZE=\"8\"><SUB>6<\/SUB><\/FONT> = 1.797>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"17\" [label = <Information_BW: μ<FONT POINT-SIZE=\"8\"><SUB>7<\/SUB><\/FONT> = 1.333>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"18\" [label = <Staff_helpfulness_BW: μ<FONT POINT-SIZE=\"8\"><SUB>8<\/SUB><\/FONT> = -1.103>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"19\" [label = <Affordability_BW: μ<FONT POINT-SIZE=\"8\"><SUB>9<\/SUB><\/FONT> = -2.138>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"20\" [label = <Accessibility_BW: μ<FONT POINT-SIZE=\"8\"><SUB>10<\/SUB><\/FONT> = 3.515>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n\"1\"->\"11\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT> = 3.032>] \n\"2\"->\"12\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>2<\/SUB><\/FONT> = 1.686>] \n\"3\"->\"13\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>3<\/SUB><\/FONT> = 2.173>] \n\"4\"->\"14\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>4<\/SUB><\/FONT> = 1.66>] \n\"5\"->\"15\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>5<\/SUB><\/FONT> = 1.593>] \n\"6\"->\"16\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>6<\/SUB><\/FONT> = 1.358>] \n\"7\"->\"17\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>7<\/SUB><\/FONT> = 1.6>] \n\"8\"->\"18\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>8<\/SUB><\/FONT> = 2.465>] \n\"9\"->\"19\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>9<\/SUB><\/FONT> = 3.463>] \n\"10\"->\"20\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>10<\/SUB><\/FONT> = 1.899>] \n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

``` r
render_graph(modelGraph(r3BW))
```

<div class="grViz html-widget html-fill-item" id="htmlwidget-f913bf0cbdde04273a60" style="width:100%;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-f913bf0cbdde04273a60">{"x":{"diagram":"digraph {\n\ngraph [layout = \"dot\",\n       outputorder = \"edgesfirst\",\n       bgcolor = \"white\",\n       concentrate = \"true\",\n       ranksep = \"2\"]\n\nnode [fontname = \"Helvetica\",\n      fontsize = \"10\",\n      shape = \"circle\",\n      fixedsize = \"true\",\n      width = \"0.5\",\n      style = \"filled\",\n      fillcolor = \"aliceblue\",\n      color = \"gray70\",\n      fontcolor = \"gray50\"]\n\nedge [fontname = \"Helvetica\",\n     fontsize = \"8\",\n     len = \"1.5\",\n     color = \"gray80\",\n     arrowsize = \"0.5\"]\n\n  \"1\" [label = <HoP_1: ζ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT>>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"2\" [label = <Safety_BW: μ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT> = 2.093>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"3\" [label = <Reliability_BW: μ<FONT POINT-SIZE=\"8\"><SUB>2<\/SUB><\/FONT> = 0.643>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"4\" [label = <Comfort_BW: μ<FONT POINT-SIZE=\"8\"><SUB>3<\/SUB><\/FONT> = 2.203>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"5\" [label = <Ease_of_Use_BW: μ<FONT POINT-SIZE=\"8\"><SUB>4<\/SUB><\/FONT> = 0.086>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"6\" [label = <Convenience_BW: μ<FONT POINT-SIZE=\"8\"><SUB>5<\/SUB><\/FONT> = 0.579>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"7\" [label = <Efficiency_BW: μ<FONT POINT-SIZE=\"8\"><SUB>6<\/SUB><\/FONT> = 1.511>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"8\" [label = <Information_BW: μ<FONT POINT-SIZE=\"8\"><SUB>7<\/SUB><\/FONT> = 1.201>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"9\" [label = <Staff_helpfulness_BW: μ<FONT POINT-SIZE=\"8\"><SUB>8<\/SUB><\/FONT> = -0.212>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"10\" [label = <Affordability_BW: μ<FONT POINT-SIZE=\"8\"><SUB>9<\/SUB><\/FONT> = -0.686>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"11\" [label = <Accessibility_BW: μ<FONT POINT-SIZE=\"8\"><SUB>10<\/SUB><\/FONT> = 2.582>, shape = \"circle\", fixedsize = \"FALSE\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n\"1\"->\"2\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT> = 0.556>] \n\"1\"->\"3\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT> = 0.556>] \n\"1\"->\"4\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT> = 0.556>] \n\"1\"->\"5\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT> = 0.556>] \n\"1\"->\"6\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT> = 0.556>] \n\"1\"->\"7\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT> = 0.556>] \n\"1\"->\"8\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT> = 0.556>] \n\"1\"->\"9\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT> = 0.556>] \n\"1\"->\"10\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT> = 0.556>] \n\"1\"->\"11\" [label = <σ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT> = 0.556>] \n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

#### Same with MTMM mode and estimate:

``` r
render_graph(modelGraph(mtmm_model))
```

<div class="grViz html-widget html-fill-item" id="htmlwidget-eff71dbf842ac6c086e9" style="width:100%;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-eff71dbf842ac6c086e9">{"x":{"diagram":"digraph {\n\ngraph [layout = \"neato\",\n       outputorder = \"edgesfirst\",\n       bgcolor = \"white\"]\n\nnode [fontname = \"Helvetica\",\n      fontsize = \"10\",\n      shape = \"circle\",\n      fixedsize = \"true\",\n      width = \"0.5\",\n      style = \"filled\",\n      fillcolor = \"aliceblue\",\n      color = \"gray70\",\n      fontcolor = \"gray50\"]\n\nedge [fontname = \"Helvetica\",\n     fontsize = \"8\",\n     len = \"1.5\",\n     color = \"gray80\",\n     arrowsize = \"0.5\"]\n\n  \"1\" [label = <Method_1: ζ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"19.5,0!\"] \n  \"2\" [label = <Method_2: ζ<FONT POINT-SIZE=\"8\"><SUB>2<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"37.5,0!\"] \n  \"3\" [label = <Trait_1: ζ<FONT POINT-SIZE=\"8\"><SUB>3<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"7.5,6!\"] \n  \"4\" [label = <Trait_2: ζ<FONT POINT-SIZE=\"8\"><SUB>4<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"10.5,6!\"] \n  \"5\" [label = <Trait_3: ζ<FONT POINT-SIZE=\"8\"><SUB>5<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"13.5,6!\"] \n  \"6\" [label = <Trait_4: ζ<FONT POINT-SIZE=\"8\"><SUB>6<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"28.5,8!\"] \n  \"7\" [label = <Trait_5: ζ<FONT POINT-SIZE=\"8\"><SUB>7<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"19.5,6!\"] \n  \"8\" [label = <Trait_6: ζ<FONT POINT-SIZE=\"8\"><SUB>8<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"22.5,6!\"] \n  \"9\" [label = <Trait_7: ζ<FONT POINT-SIZE=\"8\"><SUB>9<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"25.5,6!\"] \n  \"10\" [label = <Trait_8: ζ<FONT POINT-SIZE=\"8\"><SUB>10<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"28.5,6!\"] \n  \"11\" [label = <Trait_9: ζ<FONT POINT-SIZE=\"8\"><SUB>11<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"31.5,6!\"] \n  \"12\" [label = <HoP_1: ζ<FONT POINT-SIZE=\"8\"><SUB>12<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"34.5,6!\"] \n  \"13\" [label = <Safety_BW: μ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"3,3!\"] \n  \"14\" [label = <Reliability_BW: μ<FONT POINT-SIZE=\"8\"><SUB>2<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"6,3!\"] \n  \"15\" [label = <Comfort_BW: μ<FONT POINT-SIZE=\"8\"><SUB>3<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"9,3!\"] \n  \"16\" [label = <Ease_of_Use_BW: μ<FONT POINT-SIZE=\"8\"><SUB>4<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"12,3!\"] \n  \"17\" [label = <Convenience_BW: μ<FONT POINT-SIZE=\"8\"><SUB>5<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"15,3!\"] \n  \"18\" [label = <Efficiency_BW: μ<FONT POINT-SIZE=\"8\"><SUB>6<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"18,3!\"] \n  \"19\" [label = <Information_BW: μ<FONT POINT-SIZE=\"8\"><SUB>7<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"21,3!\"] \n  \"20\" [label = <Staff_helpfulness_BW: μ<FONT POINT-SIZE=\"8\"><SUB>8<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"24,3!\"] \n  \"21\" [label = <Affordability_BW: μ<FONT POINT-SIZE=\"8\"><SUB>9<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"27,3!\"] \n  \"22\" [label = <Safety_DCE: μ<FONT POINT-SIZE=\"8\"><SUB>10<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"30,3!\"] \n  \"23\" [label = <Reliability_DCE: μ<FONT POINT-SIZE=\"8\"><SUB>11<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"33,3!\"] \n  \"24\" [label = <Comfort_DCE: μ<FONT POINT-SIZE=\"8\"><SUB>12<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"36,3!\"] \n  \"25\" [label = <Ease_of_Use_DCE: μ<FONT POINT-SIZE=\"8\"><SUB>13<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"39,3!\"] \n  \"26\" [label = <Convenience_DCE: μ<FONT POINT-SIZE=\"8\"><SUB>14<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"42,3!\"] \n  \"27\" [label = <Efficiency_DCE: μ<FONT POINT-SIZE=\"8\"><SUB>15<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"45,3!\"] \n  \"28\" [label = <Information_DCE: μ<FONT POINT-SIZE=\"8\"><SUB>16<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"48,3!\"] \n  \"29\" [label = <Staff_helpfulness_DCE: μ<FONT POINT-SIZE=\"8\"><SUB>17<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"51,3!\"] \n  \"30\" [label = <Affordability_DCE: μ<FONT POINT-SIZE=\"8\"><SUB>18<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"54,3!\"] \n\"3\"->\"12\" [label = <β<FONT POINT-SIZE=\"8\"><SUB>3, 12<\/SUB><\/FONT>>] \n\"4\"->\"12\" [label = <β<FONT POINT-SIZE=\"8\"><SUB>4, 12<\/SUB><\/FONT>>] \n\"5\"->\"12\" [label = <β<FONT POINT-SIZE=\"8\"><SUB>5, 12<\/SUB><\/FONT>>] \n\"6\"->\"12\" [label = <β<FONT POINT-SIZE=\"8\"><SUB>6, 12<\/SUB><\/FONT>>] \n\"7\"->\"12\" [label = <β<FONT POINT-SIZE=\"8\"><SUB>7, 12<\/SUB><\/FONT>>] \n\"8\"->\"12\" [label = <β<FONT POINT-SIZE=\"8\"><SUB>8, 12<\/SUB><\/FONT>>] \n\"9\"->\"12\" [label = <β<FONT POINT-SIZE=\"8\"><SUB>9, 12<\/SUB><\/FONT>>] \n\"10\"->\"12\" [label = <β<FONT POINT-SIZE=\"8\"><SUB>10, 12<\/SUB><\/FONT>>] \n\"11\"->\"12\" [label = <β<FONT POINT-SIZE=\"8\"><SUB>11, 12<\/SUB><\/FONT>>] \n\"1\"->\"13\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>1, 1<\/SUB><\/FONT>>] \n\"1\"->\"14\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>2, 1<\/SUB><\/FONT>>] \n\"1\"->\"15\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>3, 1<\/SUB><\/FONT>>] \n\"1\"->\"16\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>4, 1<\/SUB><\/FONT>>] \n\"1\"->\"17\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>5, 1<\/SUB><\/FONT>>] \n\"1\"->\"18\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>6, 1<\/SUB><\/FONT>>] \n\"1\"->\"19\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>7, 1<\/SUB><\/FONT>>] \n\"1\"->\"20\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>8, 1<\/SUB><\/FONT>>] \n\"1\"->\"21\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>9, 1<\/SUB><\/FONT>>] \n\"2\"->\"22\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>10, 2<\/SUB><\/FONT>>] \n\"2\"->\"23\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>11, 2<\/SUB><\/FONT>>] \n\"2\"->\"24\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>12, 2<\/SUB><\/FONT>>] \n\"2\"->\"25\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>13, 2<\/SUB><\/FONT>>] \n\"2\"->\"26\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>14, 2<\/SUB><\/FONT>>] \n\"2\"->\"27\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>15, 2<\/SUB><\/FONT>>] \n\"2\"->\"28\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>16, 2<\/SUB><\/FONT>>] \n\"2\"->\"29\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>17, 2<\/SUB><\/FONT>>] \n\"2\"->\"30\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>18, 2<\/SUB><\/FONT>>] \n\"3\"->\"13\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>1, 3<\/SUB><\/FONT>>] \n\"3\"->\"22\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>10, 3<\/SUB><\/FONT>>] \n\"4\"->\"14\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>2, 4<\/SUB><\/FONT>>] \n\"4\"->\"23\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>11, 4<\/SUB><\/FONT>>] \n\"5\"->\"15\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>3, 5<\/SUB><\/FONT>>] \n\"5\"->\"24\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>12, 5<\/SUB><\/FONT>>] \n\"6\"->\"16\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>4, 6<\/SUB><\/FONT>>] \n\"6\"->\"25\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>13, 6<\/SUB><\/FONT>>] \n\"7\"->\"17\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>5, 7<\/SUB><\/FONT>>] \n\"7\"->\"26\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>14, 7<\/SUB><\/FONT>>] \n\"8\"->\"18\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>6, 8<\/SUB><\/FONT>>] \n\"8\"->\"27\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>15, 8<\/SUB><\/FONT>>] \n\"9\"->\"19\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>7, 9<\/SUB><\/FONT>>] \n\"9\"->\"28\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>16, 9<\/SUB><\/FONT>>] \n\"10\"->\"20\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>8, 10<\/SUB><\/FONT>>] \n\"10\"->\"29\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>17, 10<\/SUB><\/FONT>>] \n\"11\"->\"21\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>9, 11<\/SUB><\/FONT>>] \n\"11\"->\"30\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>18, 11<\/SUB><\/FONT>>] \n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

``` r
render_graph(modelGraph(mtmm_results))
```

<div class="grViz html-widget html-fill-item" id="htmlwidget-7f69c734ef90611d154d" style="width:100%;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-7f69c734ef90611d154d">{"x":{"diagram":"digraph {\n\ngraph [layout = \"neato\",\n       outputorder = \"edgesfirst\",\n       bgcolor = \"white\"]\n\nnode [fontname = \"Helvetica\",\n      fontsize = \"10\",\n      shape = \"circle\",\n      fixedsize = \"true\",\n      width = \"0.5\",\n      style = \"filled\",\n      fillcolor = \"aliceblue\",\n      color = \"gray70\",\n      fontcolor = \"gray50\"]\n\nedge [fontname = \"Helvetica\",\n     fontsize = \"8\",\n     len = \"1.5\",\n     color = \"gray80\",\n     arrowsize = \"0.5\"]\n\n  \"1\" [label = <Method_1: ζ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"19.5,0!\"] \n  \"2\" [label = <Method_2: ζ<FONT POINT-SIZE=\"8\"><SUB>2<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"37.5,0!\"] \n  \"3\" [label = <Trait_1: ζ<FONT POINT-SIZE=\"8\"><SUB>3<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"7.5,6!\"] \n  \"4\" [label = <Trait_2: ζ<FONT POINT-SIZE=\"8\"><SUB>4<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"10.5,6!\"] \n  \"5\" [label = <Trait_3: ζ<FONT POINT-SIZE=\"8\"><SUB>5<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"13.5,6!\"] \n  \"6\" [label = <Trait_4: ζ<FONT POINT-SIZE=\"8\"><SUB>6<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"28.5,8!\"] \n  \"7\" [label = <Trait_5: ζ<FONT POINT-SIZE=\"8\"><SUB>7<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"19.5,6!\"] \n  \"8\" [label = <Trait_6: ζ<FONT POINT-SIZE=\"8\"><SUB>8<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"22.5,6!\"] \n  \"9\" [label = <Trait_7: ζ<FONT POINT-SIZE=\"8\"><SUB>9<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"25.5,6!\"] \n  \"10\" [label = <Trait_8: ζ<FONT POINT-SIZE=\"8\"><SUB>10<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"28.5,6!\"] \n  \"11\" [label = <Trait_9: ζ<FONT POINT-SIZE=\"8\"><SUB>11<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"31.5,6!\"] \n  \"12\" [label = <HoP_1: ζ<FONT POINT-SIZE=\"8\"><SUB>12<\/SUB><\/FONT>>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"34.5,6!\"] \n  \"13\" [label = <Safety_BW: μ<FONT POINT-SIZE=\"8\"><SUB>1<\/SUB><\/FONT> = 2.092>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"3,3!\"] \n  \"14\" [label = <Reliability_BW: μ<FONT POINT-SIZE=\"8\"><SUB>2<\/SUB><\/FONT> = 2.516>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"6,3!\"] \n  \"15\" [label = <Comfort_BW: μ<FONT POINT-SIZE=\"8\"><SUB>3<\/SUB><\/FONT> = -1.126>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"9,3!\"] \n  \"16\" [label = <Ease_of_Use_BW: μ<FONT POINT-SIZE=\"8\"><SUB>4<\/SUB><\/FONT> = -0.017>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"12,3!\"] \n  \"17\" [label = <Convenience_BW: μ<FONT POINT-SIZE=\"8\"><SUB>5<\/SUB><\/FONT> = 1.133>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"15,3!\"] \n  \"18\" [label = <Efficiency_BW: μ<FONT POINT-SIZE=\"8\"><SUB>6<\/SUB><\/FONT> = 0.755>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"18,3!\"] \n  \"19\" [label = <Information_BW: μ<FONT POINT-SIZE=\"8\"><SUB>7<\/SUB><\/FONT> = -1.261>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"21,3!\"] \n  \"20\" [label = <Staff_helpfulness_BW: μ<FONT POINT-SIZE=\"8\"><SUB>8<\/SUB><\/FONT> = -1.914>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"24,3!\"] \n  \"21\" [label = <Affordability_BW: μ<FONT POINT-SIZE=\"8\"><SUB>9<\/SUB><\/FONT> = 2.284>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"27,3!\"] \n  \"22\" [label = <Safety_DCE: μ<FONT POINT-SIZE=\"8\"><SUB>10<\/SUB><\/FONT> = 1.949>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"30,3!\"] \n  \"23\" [label = <Reliability_DCE: μ<FONT POINT-SIZE=\"8\"><SUB>11<\/SUB><\/FONT> = 0.975>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"33,3!\"] \n  \"24\" [label = <Comfort_DCE: μ<FONT POINT-SIZE=\"8\"><SUB>12<\/SUB><\/FONT> = 0.326>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"36,3!\"] \n  \"25\" [label = <Ease_of_Use_DCE: μ<FONT POINT-SIZE=\"8\"><SUB>13<\/SUB><\/FONT> = 0.095>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"39,3!\"] \n  \"26\" [label = <Convenience_DCE: μ<FONT POINT-SIZE=\"8\"><SUB>14<\/SUB><\/FONT> = 0.409>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"42,3!\"] \n  \"27\" [label = <Efficiency_DCE: μ<FONT POINT-SIZE=\"8\"><SUB>15<\/SUB><\/FONT> = 0.256>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"45,3!\"] \n  \"28\" [label = <Information_DCE: μ<FONT POINT-SIZE=\"8\"><SUB>16<\/SUB><\/FONT> = 0.029>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"48,3!\"] \n  \"29\" [label = <Staff_helpfulness_DCE: μ<FONT POINT-SIZE=\"8\"><SUB>17<\/SUB><\/FONT> = -0.008>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"51,3!\"] \n  \"30\" [label = <Affordability_DCE: μ<FONT POINT-SIZE=\"8\"><SUB>18<\/SUB><\/FONT> = -0.856>, shape = \"circle\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\", pos = \"54,3!\"] \n\"3\"->\"12\" [label = <β<FONT POINT-SIZE=\"8\"><SUB>3, 12<\/SUB><\/FONT> = 0.146>] \n\"4\"->\"12\" [label = <β<FONT POINT-SIZE=\"8\"><SUB>4, 12<\/SUB><\/FONT> = 1.19>] \n\"5\"->\"12\" [label = <β<FONT POINT-SIZE=\"8\"><SUB>5, 12<\/SUB><\/FONT> = 0.57>] \n\"6\"->\"12\" [label = <β<FONT POINT-SIZE=\"8\"><SUB>6, 12<\/SUB><\/FONT> = 89.635>] \n\"7\"->\"12\" [label = <β<FONT POINT-SIZE=\"8\"><SUB>7, 12<\/SUB><\/FONT> = 2.14>] \n\"8\"->\"12\" [label = <β<FONT POINT-SIZE=\"8\"><SUB>8, 12<\/SUB><\/FONT> = 1.109>] \n\"9\"->\"12\" [label = <β<FONT POINT-SIZE=\"8\"><SUB>9, 12<\/SUB><\/FONT> = 1.048>] \n\"10\"->\"12\" [label = <β<FONT POINT-SIZE=\"8\"><SUB>10, 12<\/SUB><\/FONT> = 0.283>] \n\"11\"->\"12\" [label = <β<FONT POINT-SIZE=\"8\"><SUB>11, 12<\/SUB><\/FONT> = 0.272>] \n\"1\"->\"13\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>1, 1<\/SUB><\/FONT> = 1.889>] \n\"1\"->\"14\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>2, 1<\/SUB><\/FONT> = 1.257>] \n\"1\"->\"15\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>3, 1<\/SUB><\/FONT> = 1.78>] \n\"1\"->\"16\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>4, 1<\/SUB><\/FONT> = -0.223>] \n\"1\"->\"17\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>5, 1<\/SUB><\/FONT> = 0.408>] \n\"1\"->\"18\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>6, 1<\/SUB><\/FONT> = 1.071>] \n\"1\"->\"19\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>7, 1<\/SUB><\/FONT> = 0.882>] \n\"1\"->\"20\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>8, 1<\/SUB><\/FONT> = 1.116>] \n\"1\"->\"21\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>9, 1<\/SUB><\/FONT> = 1.377>] \n\"2\"->\"22\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>10, 2<\/SUB><\/FONT> = 0.151>] \n\"2\"->\"23\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>11, 2<\/SUB><\/FONT> = -0.293>] \n\"2\"->\"24\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>12, 2<\/SUB><\/FONT> = 0.066>] \n\"2\"->\"25\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>13, 2<\/SUB><\/FONT> = 0.016>] \n\"2\"->\"26\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>14, 2<\/SUB><\/FONT> = 0.038>] \n\"2\"->\"27\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>15, 2<\/SUB><\/FONT> = 0.271>] \n\"2\"->\"28\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>16, 2<\/SUB><\/FONT> = 0.011>] \n\"2\"->\"29\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>17, 2<\/SUB><\/FONT> = 0.022>] \n\"2\"->\"30\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>18, 2<\/SUB><\/FONT> = -0.814>] \n\"3\"->\"13\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>1, 3<\/SUB><\/FONT> = 2.29>] \n\"3\"->\"22\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>10, 3<\/SUB><\/FONT> = 1.032>] \n\"4\"->\"14\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>2, 4<\/SUB><\/FONT> = 1.226>] \n\"4\"->\"23\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>11, 4<\/SUB><\/FONT> = 0.226>] \n\"5\"->\"15\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>3, 5<\/SUB><\/FONT> = 1.673>] \n\"5\"->\"24\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>12, 5<\/SUB><\/FONT> = 0.095>] \n\"6\"->\"16\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>4, 6<\/SUB><\/FONT> = 0.014>] \n\"6\"->\"25\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>13, 6<\/SUB><\/FONT> = -0.001>] \n\"7\"->\"17\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>5, 7<\/SUB><\/FONT> = 0.899>] \n\"7\"->\"26\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>14, 7<\/SUB><\/FONT> = 0.03>] \n\"8\"->\"18\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>6, 8<\/SUB><\/FONT> = 1.213>] \n\"8\"->\"27\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>15, 8<\/SUB><\/FONT> = 0.009>] \n\"9\"->\"19\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>7, 9<\/SUB><\/FONT> = 1.205>] \n\"9\"->\"28\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>16, 9<\/SUB><\/FONT> = 0.014>] \n\"10\"->\"20\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>8, 10<\/SUB><\/FONT> = 1.875>] \n\"10\"->\"29\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>17, 10<\/SUB><\/FONT> = 0.043>] \n\"11\"->\"21\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>9, 11<\/SUB><\/FONT> = 3.018>] \n\"11\"->\"30\" [label = <γ<FONT POINT-SIZE=\"8\"><SUB>18, 11<\/SUB><\/FONT> = -0.638>] \n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>
