setwd("C:/Users/10929/Desktop/毕业数据")
library(riskRegression)
library(prodlim)
rm(list = ls())
aa <-read.csv('daw.csv')
#批量数值转因子
for (i in names(aa)[c(5:15)]){aa[,i] <- as.factor(aa[,i])}
m1 <-  FGR(Hist(msi,status)~sex+age+race+type+stage+surg+radi,cause=1,data=aa)
summary(m1)
a <- summary(m1)
res <- cbind(a$coef[,c(1,3,5)],a$conf.int[,c(1,3,4)])
round(res,4)#结果
