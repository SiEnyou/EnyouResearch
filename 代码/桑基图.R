setwd("C:/Users/10929/Desktop/毕业数据")
rm(list = ls())
library(ggalluvial)
library(ggplot2)
library(dplyr)
library(readxl)
data <- read_excel("dawd.xlsx") 

##data$Risk_Group <- factor(data$Risk_Group,levels=c(0,1,2),labels=c("Low risk","Middle risk","High risk"))
##data$Progression_Type <- factor(data$Progression_Type,levels=c(0,1,2),labels=c("Primary progression","Distance metastasis","Multiple progression"))


df <- to_lodes_form(data[,1:ncol(data)],
                    axes = 1:ncol(data),
                    id = "value")
print(df)#预览数据

##绘制桑基图（Sankey diagram）
col<- rep(c('green4', 'cornflowerblue','brown2','lightgreen', 'aquamarine', 'antiquewhite', 'purple', 'lightblue',
            'chocolate1', 'yellow', 'green', 'cyan', 'green4'), 3)#自定义颜色
#新建一个PDF文件
pdf("test.pdf",width = 8, height = 10)
#绘图
ggplot(df, aes(x = x, fill=stratum, label=stratum,
               stratum = stratum, alluvium  = value))+#数据
  geom_flow(width = 0.5,#连线宽度
            curve_type = "cubic",#曲线形状，有linear、cubic、quintic、sine、arctangent、sigmoid几种类型可供调整
            alpha = 0.5,#透明度
            color = 'white',#间隔颜色
            linewidth = 0.6)+#间隔宽度
  geom_stratum(width = 0.6)+#图中方块的宽度
  geom_text(stat = 'stratum', size = 3, color = 'black')+
  scale_fill_manual(values = col)+#自定义颜色
  theme_void()+#主题（无轴及网格线）
  theme(legend.position = 'none')#去除图例
df
dev.off()#关闭PDF
