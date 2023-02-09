library(tidyverse)
library(kableExtra)
library(data.table)


table11<- read.csv("output/metrica_IID11.csv")
table12<- read.csv("output/metrica_IID12.csv")
table13<- read.csv("output/metrica_IID13.csv")
table21<- read.csv("output/metrica_IID21.csv")
table22<- read.csv("output/metrica_IID22.csv")
table23<- read.csv("output/metrica_IID23.csv")
table31<- read.csv("output/metrica_IID31.csv")
table32<- read.csv("output/metrica_IID32.csv")
table33<- read.csv("output/metrica_IID33.csv")

pred11<-read.csv("output/metrica_pred_IID11.csv")
pred12<-read.csv("output/metrica_pred_IID12.csv")
pred13<-read.csv("output/metrica_pred_IID13.csv")
pred21<-read.csv("output/metrica_pred_IID21.csv")
pred22<-read.csv("output/metrica_pred_IID22.csv")
pred23<-read.csv("output/metrica_pred_IID23.csv")
pred31<-read.csv("output/metrica_pred_IID31.csv")
pred32<-read.csv("output/metrica_pred_IID32.csv")
pred33<-read.csv("output/metrica_pred_IID33.csv")

pred.table1 <- cbind(pred11[,-3],pred12[,-c(1,3)],pred13[,-c(1,3)])
pred.table2 <- cbind(pred21[,-3],pred22[,-c(1,3)],pred23[,-c(1,3)])
pred.table3 <- cbind(pred31[,-3],pred32[,-c(1,3)],pred33[,-c(1,3)])

pred.table <- rbind(pred.table1,pred.table2,pred.table3)
label<-rep(c("1","2", "3"),each=2)
pred.table<-cbind(label,pred.table)

pred.table %>%  kbl(digits=4,row.names=FALSE) %>%
  add_header_above( c("Scenario" = 2, "res1" = 2, "res2" = 2, "res3" = 2))


pred.table %>% kbl(digits=4,format="latex",row.names=FALSE)%>%
  add_header_above( c("Scenario" = 2, "res1" = 2, "res2" = 2, "res3" = 2)) %>%
  kable_minimal() %>%
  kable_paper(full_width = F)


time11<-read.csv("output/time_exec_IID11.csv")
time12<-read.csv("output/time_exec_IID12.csv")
time13<-read.csv("output/time_exec_IID13.csv")
time21<-read.csv("output/time_exec_IID21.csv")
time22<-read.csv("output/time_exec_IID22.csv")
time23<-read.csv("output/time_exec_IID23.csv")
time31<-read.csv("output/time_exec_IID31.csv")
time32<-read.csv("output/time_exec_IID32.csv")
time33<-read.csv("output/time_exec_IID33.csv")



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
