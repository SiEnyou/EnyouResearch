setwd("C:/Users/10929/Desktop/毕业数据")
library(cmprsk)
library(survival)
library(riskRegression)
library(prodlim)

rm(list = ls())
aa <-read.csv('daw.csv')
str(aa)
dt <- na.omit(aa)
fgr1<-FGR(Hist(msi,status)~age+sex+race+type+stage+surg+radi,data = aa,cause = 1)
summary(fgr1)

fit1 <- Score(list('model1'=fgr1),
              formula=Hist(msi,status)~1,
              data = aa ,se.fit=1L,times=c(5,10,15,20),
              plots="ROC",
              metrics ="auc")
fit1
###提取AUC
d <- as.data.frame(fit1$AUC$score)
a <- d[d$times %in% c(5,10,15,20),]
a$AUC_com <- paste(round(a$AUC,3),'(',round(a$lower,3),',',round(a$upper,3),')',sep='')

col = c("#2e7542","purple","orange","#c1121f")  ##制作一个颜色变量
plotROC(fit1, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        col=col[1],
        cex=0.5,
        legend="",
        auc.in.legend = F, ##不显示默认图例，在后面编辑新图例
        times = 5)
plotROC(fit1,
        col=col[2],
        legend = '', 
        cex=0.5,
        times = 10,
        auc.in.legend = F,
        add=T)
plotROC(fit1,
        col=col[3], 
        times = 15,  
        add=T, 
        cex=0.5, 
        legend = '',
        auc.in.legend = F)
plotROC(fit1,
        col=col[4], 
        times = 20,  
        add=T, 
        cex=0.5, 
        legend = '',
        auc.in.legend = F)
###加上图例
leg <- paste(c("5年AUC:","10年AUC:","15年AUC:","20年AUC:"),a$AUC_com)
legend(0.3,0.4,legend=leg,cex = 0.5,bty='n',title='fit1',
       col=col,lwd=2)

#---------------------
setwd("C:/Users/10929/Desktop/毕业数据")
library(cmprsk)
library(survival)
library(riskRegression)
library(prodlim)

rm(list = ls())
aa <-read.csv('daw.csv')
str(aa)
fgr1<-FGR(Hist(msi,status)~age+sex+race+type+stage+surg+radi,data = aa,cause = 1)
summary(fgr1)

fit1 <- Score(list('model1'=fgr1),
              formula=Hist(msi,status)~1,
              data = aa ,se.fit=1L,times=c(5,10,15,20),
              plots="ROC",
              metrics ="auc")
d <- as.data.frame(fit1$AUC$score)
c <- as.data.frame(fit1$ROC$plotframe)

a <- d[d$times %in% c(5,10,15,20),]
a$AUC_com <- paste(round(a$AUC,3),'(',round(a$lower,3),',',round(a$upper,3),')',sep='')

fit1$AUC
df_plot <- data.frame(tpr = as.numeric(c$TP),
                      fpr = as.numeric(c$FP),
                      time = rep(c("5","10","15","20"),each = nrow(c$TP)))

library(ggplot2)
p <- ggplot(df_plot, aes(fpr,tpr, color = time)) +
  geom_smooth(se=FALSE, linewidth=1)+ # 这就是平滑曲线的关键
  xlim(0,1)+
  ylim(0,1)+
  geom_abline(slope = 1, intercept = 0, color = "black",linetype = 2) +
  scale_color_manual(values = c('#2e7542','#0466c8','orange',"#c1121f"),
                     name = NULL, 
                     labels = c(paste0("AUC at 5: ",round(a[["AUC"]][1],3)), 
                                paste0("AUC at 10: ",round(a[["AUC"]][2],3)), 
                                paste0("AUC at 15: ",round(a[["AUC"]][3],3)),
                                paste0("AUC at 20: ",round(a[["AUC"]][4],3)))) + 
  coord_fixed(ratio = 1) +
  labs(x = "1 - Specificity", y = "Sensitivity") +
  theme_minimal(base_size = 12, base_family = "sans") +
  theme(legend.position = c(0.5,0.15), 
        panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA),
        ##panel.grid=element_blank(),
        axis.text = element_text(color = "black"))

plot(p)

dev.off()