
library(ezknitr)

#resumen de cada simulacion
c(3,5,7,8,9)
for(i in c(4:4)){
  model <- "INLA" # "MRA2"
  type <- "Matern" # "Matern"
  nres <- 3
  Mesh <- "g"   # g or r
  #i <- 1 # simulation ID # from 1 to 10
 # burn<- 1  #1200 for nres=2 and 500 for nres=1
  #taper <- 0.05    #0.3
output_suffix<-paste0("_",model,type,nres,"sim",i,Mesh,"_prediction.Rdata")

ezknitr::ezknit(file = "2.Output_INLA_prediction.Rmd", out_dir = "output_prediction",
       fig_dir = "myfigs",out_suffix=output_suffix,
       params=list(model=model,type=type,nres=nres,i=i),
       keep_md = FALSE,
       keep_html = TRUE)
}

