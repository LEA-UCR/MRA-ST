# if you are working local, setwd in simulation5 first!
# setwd("~/Dropbox/Emuladores/Emuladores_code/simulation5")

source("functions/data_generator.R")

## DEP describes the spatial dependence 
DEP <- c("Matern")
Sigma <- c(0.003,0.0003,0.00003)
res <- list(c(20,10,12),c(40,10,12),
            c(60,10,12))
rho <- 0.8
# function.to.gen.data <- function(nDEP,nsigma2,nres,i)


nsimulations <- 10
# Data Scenario 11
lapply(1:nsimulations,function(i)
  function.to.gen.data(1,1,i))
# Data Scenario 21
lapply(1:nsimulations,function(i)
  function.to.gen.data(1,2,i))

# Data Scenario 21
lapply(1:nsimulations,function(i)
  function.to.gen.data(2,1,i))
# Data Scenario 22
lapply(1:nsimulations,function(i)
  function.to.gen.data(2,2,i))


# Data Scenario 13
lapply(1:nsimulations,function(i)
  function.to.gen.data(1,3,i))

# Data Scenario 23
lapply(1:nsimulations,function(i)
  function.to.gen.data(2,3,i))

# Data Scenario 13
lapply(6:nsimulations,function(i)
  function.to.gen.data(3,1,i))

# Data Scenario 23
lapply(6:nsimulations,function(i)
  function.to.gen.data(3,2,i))

# Data Scenario 23
lapply(6:nsimulations,function(i)
  function.to.gen.data(3,3,i))
