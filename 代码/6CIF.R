setwd("C:/Users/10929/Desktop/毕业数据")
aa<-read.csv('dawfx.csv')

{
  library(tidycmprsk)
  library(gtsummary)
  library(ggsurvfit)
  library(ggprism)
  {
    aa$status<-factor(aa$status, levels=c(0,1,2),labels=c("0","发生胸腺外原发恶性肿瘤","死于胸腺肿瘤或其他非恶性疾病"))
    aa$sex<-factor(aa$sex, levels=c(0,1),labels=c("女性","男性"))
    aa$age<-factor(aa$age, levels=c(0,1,2),labels=c("小于55岁","55-75岁","大于75岁"))
    aa$race<-factor(aa$race, levels=c(0,1,2),labels=c("黑色人种","黄色人种","白色人种"))
    aa$mari<-factor(aa$mari, levels=c(0,1),labels=c("其他婚姻状态","已婚"))
    aa$type<-factor(aa$type, levels=c(0,1),labels=c("胸腺癌","胸腺瘤"))
    aa$stage<-factor(aa$stage, levels=c(0,1,2),labels=c("局部进展","区域进展","远处进展"))
    aa$surg<-factor(aa$surg, levels=c(0,1),labels=c("无或未知","手术治疗"))
    aa$radi<-factor(aa$radi, levels=c(0,1),labels=c("无或未知","放射治疗"))
    aa$chem<-factor(aa$chem, levels=c(0,1),labels=c("无或未知","化学治疗"))
    aa$syst<-factor(aa$syst, levels=c(0,1),labels=c("无或未知","系统治疗"))
  }

#单因素分组------------------#全人群的CIF：改为~1
  cif1 <- tidycmprsk::cuminc(Surv(msi, status) ~ gr, data = aa)#单因素的CIF
  #cif1结果汇报了 Gray's Test的结果
  cif1 
  #是1-5年的发生率、事件数和95%CI等信息
  tidy(cif1,times = c(5,10,15,20))
  #直接输出了结果的表格
  tbl_cuminc(cif1,
            #statistic = "{estimate}% ({conf.low}%, {conf.high}%)",
            times = c(5,10,15,20),
            outcomes = c("发生胸腺外原发恶性肿瘤","死于胸腺肿瘤或其他非恶性疾病"),
            estimate_fun = NULL,
            label_header = "**{time}年累计发生率**") %>% 
    add_p() %>%
    add_n(location = "level")
#可视化
  ggcuminc(cif1,outcome = c("发生胸腺外原发恶性肿瘤","死于胸腺肿瘤或其他非恶性疾病"),size=1.5)+
    labs(x = "确诊后时长(年)")+
    add_confidence_interval() +
    add_risktable()+
    add_quantile(y_value = 0.20, size = 1) +#y=0.2的x值
    scale_x_continuous(breaks = seq(0,20,by=5),limits = c(0,20))+
    scale_y_continuous(label = scales::percent,
                     breaks = seq(0,0.6,by=0.2),limits = c(0,0.6))+
    theme_prism()+
    theme(legend.position=c(0.4,0.8),
        panel.grid=element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"))+
  #刻度位置向内
    theme(axis.ticks.length.x = unit(-0.2,"cm"),
        axis.ticks.x=element_line(color="black",size=1,lineend =1))+
    theme(axis.ticks.length.y= unit(-0.2,"cm"),
        axis.ticks.y=element_line(color="black",size=1,lineend = 1))


}

