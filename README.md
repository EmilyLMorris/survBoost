# survBoost
survBoost

### Installation

In order to install the package, several other R packages must be installed. 
The code relies on Rcpp, RcppArmadillo, and RcppParallel in order to improve computational speed. 
Additionally the survival package is used for simulation and post selection inference and will be required for installation. 
The following line of R code installs the package.

```
# install.packages("devtools")
install_github("EmilyLMorris/survBoost")
```
[![Travis build status](https://travis-ci.org/EmilyLMorris/survBoost.svg?branch=master)](https://travis-ci.org/EmilyLMorris/survBoost)