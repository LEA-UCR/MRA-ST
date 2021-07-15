
library(ezknitr)

#resumen de cada simulacion

for(i in 6:9){
  model <- "MRA1" # "MRA2"
  type <- "Exponential" # "Matern"
  nres <- 2
  #i <- 1 # simulation ID # from 1 to 10
  burn<- 500  #1100 for nres=2 and 500 for nres=1
output_suffix<-paste0("_",model,type,nres,"sim",i,".Rdata")

ezknitr::ezknit(file = "2.Output.Rmd", out_dir = "output",
       fig_dir = "myfigs",out_suffix=output_suffix,
       params=list(model=model,type=type,nres=nres,i=i,
                   burn=burn),
       keep_md = FALSE,
       keep_html = TRUE)
}

#resumen de cada escenario

model <- "MRA2" # "MRA2"
type <- "Exponential"
nres <- 2
sigma2<- 2 # 2 or 0.5
burn <- 1100    #1100 for nres=2 and 500 for nres=1

rmarkdown::render('2.Reporte2.Rmd',
                  output_file = paste('report.',"_",model,type,nres,
                                      '.html', sep=''),
                  params=list(model=model,type=type,nres=nres,sigma2=sigma2,
                              burn=burn))

