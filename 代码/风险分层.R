setwd("C:/Users/10929/Desktop/毕业数据")
library(QHScrnomo)
library(rms)
library(riskRegression)
library(prodlim)
library(writexl)
rm(list = ls())
aa <-read.csv('daw.csv')
#跟Cox回归类似，不过结局要指定为factor()形式。
#批量数值转因子
for (i in names(aa)[c(5:15)]){aa[,i] <- as.factor(aa[,i])}
#0. 创建环境
dd <- datadist(aa)
options(datadist = "dd")
#1. 先拟合生存
m1 <-  FGR(Hist(msi,status)~sex+age+race+type,cause=1,data=aa)
summary(m1)
set.seed(123)
riskscore = predict(m1, type = "risk", newdata = aa)
aa$riskscore <- riskscore
write_xlsx(aa, path = "dawfx.xlsx")


