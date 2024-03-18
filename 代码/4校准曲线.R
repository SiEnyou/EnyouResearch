setwd("C:/Users/10929/Desktop/毕业数据")
library(QHScrnomo)
library(rms)
rm(list = ls())
aa <-read.csv('daw.csv')
#跟Cox回归类似，不过结局要指定为factor()形式。
#批量数值转因子
for (i in names(aa)[c(5:15)]){aa[,i] <- as.factor(aa[,i])}
#0. 创建环境
dd <- datadist(aa)
options(datadist = "dd")
#1. 先拟合生存
mul <-cph(Surv(msi,status==1)~age+sex+race+type+stage+surg+radi,data = aa,
          x=T, y=T, surv=T,time.inc=20)
#2. 竞争风险回归
m3 <- crr.fit(mul,failcode=1,cencode = 0)
m3
HR <- round(exp(m3$coef),3);HR
#HR与上期没有区别，模型可靠↓

#数学表达式
sas.cmprsk(m3,time = 20)
#c-index
#1. 预测
set.seed(123)          
aa$pro1 <- tenf.crr(m3,time = 5)
aa$pro2 <- tenf.crr(m3,time = 10)
aa$pro3 <- tenf.crr(m3,time = 15)
aa$pro4 <- tenf.crr(m3,time = 20)
#2. c指数
cindex(aa$pro1,          
       fstatus = aa$status,          
       ftime = aa$msi,          
       type = "crr",          
       failcode = 1,
       cencode = 0,
       tol = 1e-20)
cindex(aa$pro2,          
       fstatus = aa$status,          
       ftime = aa$msi,          
       type = "crr",          
       failcode = 1,
       cencode = 0,
       tol = 1e-20)
cindex(aa$pro3,          
       fstatus = aa$status,          
       ftime = aa$msi,          
       type = "crr",          
       failcode = 1,
       cencode = 0,
       tol = 1e-20)
cindex(aa$pro4,          
       fstatus = aa$status,          
       ftime = aa$msi,          
       type = "crr",          
       failcode = 1,
       cencode = 0,
       tol = 1e-20)

groupci(x=aa$pro1,        
        ftime = aa$msi,  
        fstatus = aa$status, 
        failcode = 1, 
        cencode = 0,
        ci = TRUE,
        g = 5, #组数    
        m = 1000,#每组中的最小观察数。
        u = 5, #预测时间,这个要跟之前的对上，很重要        
        xlab = "Predicted ",         
        ylab = "Actual ",
        lty=1,
        lwd=2,
        #type = 'b', 
        col="#2e7542",
        errbar.col = c("#2e7542"),
        xlim=c(0,0.5),
        ylim=c(0,0.5),
        #conf.int=F,
        add =F)
groupci(x=aa$pro2,        
        ftime = aa$msi,  
        fstatus = aa$status, 
        failcode = 1, 
        cencode = 0,
        ci = TRUE,
        g = 5, #组数    
        m = 1000,#每组中的最小观察数。
        u = 10, #预测时间,这个要跟之前的对上，很重要        
        xlab = "Predicted ",         
        ylab = "Actual ",
        lty=1,
        lwd=2,
        #type = 'b', 
        col="purple",
        errbar.col = c("purple"),
        xlim=c(0,0.5),
        ylim=c(0,0.5),
        #conf.int=F,
        add =T)
groupci(x=aa$pro3,        
        ftime = aa$msi,  
        fstatus = aa$status, 
        failcode = 1, 
        cencode = 0,
        ci = TRUE,
        g = 5, #组数    
        m = 1000,#每组中的最小观察数。
        u = 15, #预测时间,这个要跟之前的对上，很重要        
        xlab = "Predicted ",         
        ylab = "Actual ",
        lty=1,
        lwd=2,
        #type = 'b', 
        col="orange",
        errbar.col = c("orange"),
        xlim=c(0,0.5),
        ylim=c(0,0.5),
        #conf.int=F,
        add =T)
groupci(x=aa$pro4,        
        ftime = aa$msi,  
        fstatus = aa$status, 
        failcode = 1, 
        cencode = 0,
        ci = TRUE,
        g = 5, #组数    
        m = 1000,#每组中的最小观察数。
        u = 20, #预测时间,这个要跟之前的对上，很重要        
        xlab = "Predicted ",         
        ylab = "Actual ",
        lty=1,
        lwd=2,
        #type = 'b', 
        col="#c1121f",
        errbar.col = c("#c1121f"),
        xlim=c(0,0.5),
        ylim=c(0,0.5),
        #conf.int=F,
        add =T)
