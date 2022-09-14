setwd('/home/pvermees/Documents/Programming/gamma')

library(testthat)

install.packages('.',type='source',repo=NULL,dependencies=TRUE)

library('gamma')

test_check("gamma")
