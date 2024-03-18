setwd("C:/Users/10929/Desktop/毕业数据")
library(QHScrnomo)
library(rms)
rm(list = ls())
aa <-read.csv('daw.csv')

for (i in names(aa)[c(5:15)]){aa[,i] <- as.factor(aa[,i])}
dd <- datadist(aa)
oldoption <-options(datadist = "dd")

#先拟合生存
mul <- cph(Surv(msi,status==1)~ sex+age+race+type+stage+surg+radi,data = aa,
          x=T, y=T, surv=T,time.inc=20)
#竞争风险回归
m3 <- crr.fit(mul,failcode= 1,cencode = 0)
m3
HR <- round(exp(m3$coef),3);HR

nomo <-Newlabels(fit = m3,
                 labels =c(age="年龄",
                          sex="性别",
                          race="人种",
                          stage="分期",
                          surg="手术",
                          radi="放疗",
                          type="类型"))
#亚变量名设置
nomo <-Newlevels(fit=nomo, 
                 list(sex=c('女性','男性'),
                      age=c('<55岁','55-75岁','>75岁'),
                      race=c('黑种人','黄种人','白种人'),
                      stage=c('局部进展','区域进展','远处进展'),
                      surg=c('无或未知','手术'),
                      radi=c('无或未知','放疗'),
                      chem=c('无或未知','化疗'),
                      type=c('胸腺癌','胸腺瘤')))
# 绘制列线图     
nom<-nomogram.crr(fit =nomo ,
             lp = F,           
             xfrac = 0.3,#控制左侧文字与轴的距离
             fun.at =seq(from=0, to=1, by= 0.1) , 
             failtime =c(5,10,15,20),
             funlabel = c("5年的胸腺外癌症累积发病率",
                          "10年的胸腺外癌症累积发病率",
                          "15年的胸腺外癌症累积发病率",
                          "20年的胸腺外癌症累积发病率"))

#---------------------------------
library(nomogramFormula)
set.seed(123)

options(oldoption)

results <- formula_lp(nom)
results$formula
##                        b0      x^1
## linear predictor 131.9182 821.7001
points <- points_cal(formula = results$formula, lp = f$linear.predictors)
head(points)
##         1         2         3         4         5         6 
## 104.02111 146.65643 177.84712  94.76958  70.88473  68.48810