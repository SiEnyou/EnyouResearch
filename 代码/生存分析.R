library(survival)
library(autoReg)
library(dplyr)
rm(list = ls())
aa <- read.csv('daw.csv')

#attach(aa) # 绑定数据集
fit <- survfit(Surv(su,status) ~ site, data = aa) 
fit

library("survminer")
ggsurvplot(fit, # 创建拟合对象
           data = aa, # 指定数据集
           conf.int = TRUE, # 显示置信区间
           pval = TRUE, # 添加P值
           surv.median.line = "hv", # 添加中位生存时间线
           risk.table = TRUE, # 添加风险表
           xlab = "Follow up time(d)", # 指定x轴标签
           legend = c(0.8,0.75), # 指定图例位置
           legend.title = "", # 设置图例标题
           #legend.labs = c("Male", "Female"), # 指定图例分组标签
           break.x.by = 5, # 设置x轴刻度间距
           palette = "lancet") # 自定义调色板


#参考：https://mp.weixin.qq.com/s/bV1fNA_1ZiyK7lE-yfY1Lg
survdiff(Surv(su,status) ~ sex,aa)
pairwise_survdiff(Surv(su,status) ~ sex,aa,p.adjust.method = "BH")

fit <- coxph(Surv(time,status)~age+sex+ph.ecog+ph.karno+pat.karno+meal.cal+wt.loss, 
             data = mydata)
summary(fit)


##cox回归结果快速整理
#install.packages("autoReg")
library(autoReg)
autoReg(fit,uni=TRUE,threshold=0.05)
autoReg(fit)
coxresult<-autoReg(fit, uni=TRUE) %>% myft()
library(rrtable)
table2pptx(coxresult) #Exported table as Report.pptx
table2docx(coxresult) #Exported table as Report.docx
