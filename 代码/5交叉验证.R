setwd("C:/Users/10929/Desktop/毕业数据")
library(ggprism)
library(survival)#cox回归用
library(caret)#K折交叉验证用
library(riskRegression)#cox回归时间AUC用
library(pec)#cox回归时间c指数用
library(writexl)

rm(list = ls()) 
aa<- read.csv('daw.csv')
str(aa)#查看变量性质
for (i in names(aa)[c(5:15)]){aa[,i] <- as.factor(aa[,i])}#批量数值转因子
dd <- datadist(aa)
options(datadist = "dd")
str(aa)#再次查看变量性质
#多次K折交叉验证
res <-as.numeric()#建个空表res备用，盛放结果
set.seed(1)#设置随机种子，使数据分割可重复

folds <-createMultiFolds(y=aa$status,k=5,times=200)#设定K和N,预测3年AUC的5折200次验证

auc_5<-as.numeric()
for(i in 1:10){#1000次循环（1000组新数据）
  train<- aa[ folds[[i]],] 
  test <- aa[-folds[[i]],] 
  m3<-FGR(Hist(msi,status)~age+sex+race+type+stage+surg+radi,data = aa,cause = 1)
  mod<-  Score(list(model1=m3),formula=Surv(msi,status)~1,
               data=test,
               metrics="auc",
               times=5)
  auc_5 <-append(auc_5,as.numeric(mod$AUC$score$AUC))#求1000个AUC
}
res <-data.frame(auc_5)#1-建立空表，放每次的AUC

auc_10<-as.numeric()
for(i in 1:10){#1000次循环（1000组新数据）
  train<- aa[ folds[[i]],] 
  test <- aa[-folds[[i]],] 
  m3<-FGR(Hist(msi,status)~age+sex+race+type+stage+surg+radi,data = aa,cause = 1)
  mod<-  Score(list(model1=m3),formula=Surv(msi,status)~1,
               data=test,
               metrics="auc",
               times=10)
  auc_10 <-append(auc_10,as.numeric(mod$AUC$score$AUC))#求1000个AUC
}
res$auc_10 <- with(res,auc_10)#1-建立空表，放每次的AUC

auc_15<-as.numeric()
for(i in 1:10){#1000次循环（1000组新数据）
  train<- aa[ folds[[i]],] 
  test <- aa[-folds[[i]],] 
  m3<-FGR(Hist(msi,status)~age+sex+race+type+stage+surg+radi,data = aa,cause = 1)
  mod<-  Score(list(model1=m3),formula=Surv(msi,status)~1,
               data=test,
               metrics="auc",
               times=15)
  auc_15 <-append(auc_15,as.numeric(mod$AUC$score$AUC))
}
res$auc_15 <- with(res,auc_15)

auc_20<-as.numeric()
for(i in 1:10){#1000次循环（1000组新数据）
  train<- aa[ folds[[i]],] 
  test <- aa[-folds[[i]],] 
  m3<-FGR(Hist(msi,status)~age+sex+race+type+stage+surg+radi,data = aa,cause = 1)
  mod<-  Score(list(model1=m3),formula=Surv(msi,status)~1,
               data=test,
               metrics="auc",
               times=20)
  auc_20 <-append(auc_20,as.numeric(mod$AUC$score$AUC))
}
res$auc_20 <- with(res,auc_20)
write_xlsx(res, path = "data.xlsx")


library(reshape2)
res <- data.frame(res)
res1<- melt(res,
            measure.vars = c("auc_5","auc_10","auc_15"),
            variable.name = "Sample",
            value.name = "value")
#需要转换的列名：measure.vars()  
#新数据标签列名：variable.name()
#新数数据数值名：value.name()    
library("ggplot2")
#install.packages('ggsci')
library('ggsci')
ggplot(res1, aes(Sample,value)) +
  ylim(0.0,1)+
  geom_dotplot(aes(fill = Sample), binaxis = "y", 
               stackdir = "center",binwidth =0.01) +
  scale_color_manual(limits=c("auc_5","auc_10","auc_15"), 
                     values=c("#c1121f","#0466c8","#2e7542"))+ 
  theme_prism(border =T)+
  theme(legend.position = "none")   

scale_fill_jco(palette = c("default"), alpha = 1)+
  
  
  ggplot(data=res1)+ 
  ylim(0.0,1.0)+
  geom_boxplot(mapping=aes(x=Sample,y=value,colour =Sample),
               alpha = 0.5,size=1.5,width = 0.6)+ 
  geom_jitter(mapping=aes(x=Sample,y=value,colour =Sample), 
              alpha = 0.3,size=1.5)+
  scale_color_manual(limits=c("auc_5","auc_10","auc_15"), 
                     values=c("#c1121f","#0466c8","#2e7542"))+ 
  theme_prism(base_size =12,border =T)+
  theme(legend.position = "none")

#----c指数

c_value5<-as.numeric()
for(i in 1:1000){
  train<- aa[ folds[[i]],] 
  test <- aa[-folds[[i]],] 
  m3<-FGR(Hist(msi,status)~age+sex+race+type+stage+surg+radi,data = aa,cause = 1)
  mod<-cindex(list(model1=m3),formula=Surv(msi,status)~1,
              data=test,eval.times=5)
  c_value5<- append(c_value5,as.numeric(mod$AppCindex))
}
re <-data.frame(c_value5)

c_value10<-as.numeric()
for(i in 1:1000){
  train<- aa[ folds[[i]],] 
  test <- aa[-folds[[i]],] 
  m3<-FGR(Hist(msi,status)~age+sex+race+type+stage+surg+radi,data = aa,cause = 1)
  mod<-cindex(list(model1=m3),formula=Surv(msi,status)~1,
              data=test,eval.times=10)
  c_value10<- append(c_value10,as.numeric(mod$AppCindex))
}

c_value15<-as.numeric()
for(i in 1:1000){
  train<- aa[ folds[[i]],] 
  test <- aa[-folds[[i]],] 
  m3<-FGR(Hist(msi,status)~age+sex+race+type+stage+surg+radi,data = aa,cause = 1)
  mod<-cindex(list(model1=m3),formula=Surv(msi,status)~1,
              data=test,eval.times=15)
  c_value15<- append(c_value15,as.numeric(mod$AppCindex))
}

c_value20<-as.numeric()
for(i in 1:1000){
  train<- aa[ folds[[i]],] 
  test <- aa[-folds[[i]],] 
  m3<-FGR(Hist(msi,status)~age+sex+race+type+stage+surg+radi,data = aa,cause = 1)
  mod<-cindex(list(model1=m3),formula=Surv(msi,status)~1,
              data=test,eval.times=20)
  c_value20<- append(c_value20,as.numeric(mod$AppCindex))
}

re$c_value10 <- with(re,c_value10)  
re$c_value15 <- with(re,c_value15) 
re$c_value20 <- with(re,c_value20) 

write_xlsx(re, path = "validation.xlsx")
