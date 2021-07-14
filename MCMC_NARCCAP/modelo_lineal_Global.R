library(tidyverse)
library(sf)
library(sp)

variable_narccap <- 'Temp' # Temp/Prec

if(variable_narccap=='Temp'){
  load('./data_narccap/TStotal.RData')
  base_cruda <- TS_tot
  rm(TS_tot)
}else{
  load('./data_narccap/PRtotal.RData')
  base_cruda <- pr_tot
  rm(pr_tot)
}

base_recortada <- base_cruda %>% 
  filter(lat >= 20, lat <= 40, lon >= 240, lon <= 265)
rm(base_cruda)

base_recortada <- base_recortada %>%
  mutate(across(c(TREFHT,OMEGA,PSL,U,V),function(x) as.vector(scale(x))))
         
         
cor(base_recortada[,7:11],use = 'na.')
modelo1 <- lm(TREFHT~OMEGA+PSL+U+V,data = base_recortada)
drop1(modelo1,test = 'Chisq')

modelo2 <- lm(TREFHT~PSL+U+V,data = base_recortada)
drop1(modelo2,test = 'Chisq')
summary(modelo2)

