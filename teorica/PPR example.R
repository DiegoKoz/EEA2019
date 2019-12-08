###################
# Example of Projection Pursuit Regression
###################
library(MASS)
library(rgl)
library(pls)

n<-10000
x1<-runif(n)*10-5
x2<-runif(n)*10-5
eps<-rnorm(n)/20
y<-1/(1+exp(-(x1+x2)))+eps
mat<-cbind(x1,x2,y)
# Plotting
plot3d(mat)
segments3d(c(-5,5),c(-5,5),c(0,0),col='green',lwd=3)
segments3d(c(-5,5),c(5,-5),c(0,0),col='red',lwd=3)

ajus.ppr <- ppr(y ~ x1 + x2, nterms = 2, max.terms = 2,trace=TRUE)
summary(ajus.ppr)
plot(ajus.ppr)

############ A Real exmaple
### Wind turbine data from Rawson

came<-read.table('came')
oks<-came[,135:177]
oks.sum<-apply(oks,1,sum)
cuales<-which(oks.sum==(43*600))
prod<-came[cuales,178]+came[cuales,179]
vtos<-came[cuales,6:48]
vtos.prom<-apply(vtos,1,mean)
plot(vtos.prom,prod)
cuales2<-which(vtos.prom<20)
vtos<-vtos[cuales2,]
prod<-prod[cuales2]
vtos.prom<-vtos.prom[cuales2]
plot(vtos.prom,prod,xlab='Viento Promedio',ylab='Produccion Electrica (KW)')
#######

ajus.ppr <- ppr(prod ~ .,data=vtos, nterms = 2, max.terms = 2,trace=TRUE)
plot(ajus.ppr)
summary(ajus.ppr)
c(ajus.ppr$yb,mean(prod,na.rm=T))
#

ajus.ridge <- lm.ridge(prod ~ .,data=vtos, lambda = 10^4)
barplot(coef((ajus.ridge))[-1])

# PCR

ajus.pcr <- pcr(prod ~ .,data=vtos,  scale = FALSE, validation = "CV")
ajus.pcr
summary(ajus.pcr)
validationplot(ajus.pcr)

plot(ajus.pcr)