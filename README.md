Learn Statistical
================
GongCC
2022-10-14

## Part One: Perceptron

===============Example One===============

``` r
library(learnML)
X <- iris[1:100,1:2]
y <- iris$Species[1:100]
m <- Perceptron$new(max_iteration = 10000)
m$fit(X, y)
```

    ## Initial weights is:  -0.5604756 -0.2301775 
    ##  Initial intercept is:  1.558708 
    ## epoch:  1541  
    ##  w:  7.869524 -10.04018  b:  -12.34129

``` r
m$plot()
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

===============Example Two ===============

``` r
library(learnML)
X <- matrix(c(3, 3, 4, 3, 1, 1), ncol = 2, byrow = TRUE)
y <- c(1, 1, -1)
fit <- Perceptron$new()
fit$fit(X, y, w = c(0,0), b = 0)
```

    ## Initial weights is:  0 0 
    ##  Initial intercept is:  0 
    ## epoch:  7  
    ##  w:  0.1 0.1     b:  -0.3

``` r
fit$plot()
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
