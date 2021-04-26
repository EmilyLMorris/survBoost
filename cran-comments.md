## Resubmission  
In this version I have:

* Removed dependency on RcppParallel. 
* Removed LazyData from DESCRIPTION file. 

## Test environments
* local OS X install, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.5.2
* win-builder (release)

## R CMD check results
There were no errors or warnings on Windows, Ubuntu Linux, but Fedora/Debian have a preperror. We believe this is due to R and not our package. 

## Downstream dependencies
There are currently no downstream dependencies for this package. 