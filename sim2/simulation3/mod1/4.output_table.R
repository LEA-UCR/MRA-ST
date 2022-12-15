library(tidyverse)
library(kableExtra)
library(data.table)


table11<- read.csv("output/metrica_AR11.csv")
table12<- read.csv("output/metrica_AR12.csv")
table13<- read.csv("output/metrica_AR13.csv")
table21<- read.csv("output/metrica_AR21.csv")
table22<- read.csv("output/metrica_AR22.csv")
table23<- read.csv("output/metrica_AR23.csv")
table31<- read.csv("output/metrica_AR31.csv")
table32<- read.csv("output/metrica_AR32.csv")
table33<- read.csv("output/metrica_AR33.csv")

pred11<-read.csv("output/metrica_pred_AR11.csv")
pred12<-read.csv("output/metrica_pred_AR12.csv")
pred13<-read.csv("output/metrica_pred_AR13.csv")
pred21<-read.csv("output/metrica_pred_AR21.csv")
pred22<-read.csv("output/metrica_pred_AR22.csv")
pred23<-read.csv("output/metrica_pred_AR23.csv")
pred31<-read.csv("output/metrica_pred_AR31.csv")
pred32<-read.csv("output/metrica_pred_AR32.csv")
pred33<-read.csv("output/metrica_pred_AR33.csv")

pred.table<-rbind(pred11,pred12,pred13,
                  pred21,pred22,pred23,
                  pred31,pred32,pred33)

label<-rep(c("pred11","pred12","pred13",
             "pred21","pred22","pred23",
             "pred31","pred32","pred33"),each=2)


pred.table<-cbind(label,pred.table)

pred.table %>% kbl(digits=4,row.names=FALSE)

pred.table %>% kbl(digits=4,format="latex",row.names=FALSE)%>%
  kable_minimal() %>%
  kable_paper(full_width = F)


time11<-read.csv("output/time_exec_AR11.csv")
time12<-read.csv("output/time_exec_AR12.csv")
time13<-read.csv("output/time_exec_AR13.csv")
time21<-read.csv("output/time_exec_AR21.csv")
time22<-read.csv("output/time_exec_AR22.csv")
time23<-read.csv("output/time_exec_AR23.csv")
time31<-read.csv("output/time_exec_AR31.csv")
time32<-read.csv("output/time_exec_AR32.csv")
time33<-read.csv("output/time_exec_AR33.csv")


time.table<-cbind(time11,time12,time13,
                  time21,time22,time23,
                  time31,time32,time33)
time.table.summary<-colMeans(time.table)

time.table.summary<-matrix(time.table.summary,nrow = 3,byrow = TRUE)

rownames(time.table.summary)<-c("var1","var2","var3")

colnames(time.table.summary)<-c("res1","res2","res3")

time.table.summary%>% kbl(digits=4)%>%
  kable_minimal() %>%
  kable_paper(full_width = F)

time.table.summary%>% kbl(digits=4,format="latex",row.names=FALSE)%>%
  kable_minimal() %>%
  kable_paper(full_width = F)
