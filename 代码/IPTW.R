library(survival)
library(survminer)
library(RISCA)
library(ggprism)
#加载数据
rm(list = ls())
aa <- read.csv('sIPTW.csv')
#status==0为复发；rt为治疗变量，1=接受放疗（RT）
#将数据中的分类变量转为factor，1和4-10列。
for (i in names(aa)[c(1,4:10)]){aa[,i] <- as.factor(aa[,i])}

km <- survfit(Surv(time,status==0) ~rt,data =aa) 
#算P值
survdiff(Surv(time,status==0)~rt,rho=0,data=aa)
#KM曲线
km1 <- ggsurvplot(km,
                  data = aa, 
                  conf.int = T,
                  size =2, 
                  legend.labs=c("No", "Yes"), 
                  palette="lancet",
                  legend.title="Radiotherapy,p-value=0.55", 
                  legend=c(0.2,0.25),
                  censor.size=4.5,# 删失形状的大小
                  risk.table = TRUE, 
                  risk.table.pos ="in",
                  risk.table.col = "strata", 
                  break.x.by = 12,
                  break.y.by = 0.2);km1 