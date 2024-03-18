
trf<-read.csv("dawd.csv")
str(trf)
library(MatchIt)
trf$size2<-trf$size
trf$size2<- as.logical(trf$size2 == '1')
data_match<- matchit(size2~gender+age+number+invasion+VIa+metastasis,
                     data = trf, method="nearest", ratio=1, caliper = 0.02)
summary(data_match)
plot (data_match, type = "jitter")plot (data_match, type = "hist")


trf2<- match.data(data_match)
glm.log<glm(VIB~LN.prRLN~size+invasion+VIa+metastasis,family= binomial(link ="logit"),data = trf2)
summary(glm.log)
reportReg(glm.log)

