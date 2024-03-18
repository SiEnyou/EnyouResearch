
library(robust)
library(qcc)
rm(list = ls())
aa <-read.csv('daw.csv')
f1<-glm(Loss~sex+age+race+type.,data = aa,family = poisson(link = "log"))
summary(f1)

coef(f1)
exp(coef(f1))
deviance(f1)/df.residual(f1)
qcc.overdispersion.test(stack.dat$Loss,type = "poisson")
