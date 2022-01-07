# if you are working local, setwd in simulation5 first!
# setwd("~/Dropbox/Emuladores/Emuladores_code/simulation5")

source("functions/data_generator.R")

## DEP describes the spatial dependence 
DEP <- c("Exponential","Matern")
res <- list(c(25,10),c(40,20),c(55,25))

# function.to.gen.data <- function(nDEP,nsigma2,nres,i)

nsimulations <- 10
# Data Scenario 1
lapply(1:nsimulations,function(i)
  function.to.gen.data(1,1,i))
# Data Scenario 2
lapply(1:nsimulations,function(i)
  function.to.gen.data(1,2,i))
# Data Scenario 3
lapply(1:nsimulations,function(i)
  function.to.gen.data(1,3,i))
# Data Scenario 4
lapply(1:nsimulations,function(i)
  function.to.gen.data(2,1,i))
# Data Scenario 5
lapply(1:nsimulations,function(i)
  function.to.gen.data(2,2,i))
# Data Scenario 6
lapply(1:nsimulations,function(i)
  function.to.gen.data(2,3,i))
