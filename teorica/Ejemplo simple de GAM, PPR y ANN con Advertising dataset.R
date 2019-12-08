######################
# gam, ppr y nnet
#######################
library(gam)
library(ISLR) # contains the Wage dataset used here
library(HoRM)
library(NeuralNetTools)
library(nnet)
### Usamos los datos de publicidad
setwd('/home/andres/Dropbox/Tecnicas Avanzadas de Regresion/')
advdata<-read.csv("Advertising.csv",header=TRUE)
attach(advdata)
head(advdata)
pairs(advdata[,-1])
########### con GAM

ajus.gam <- gam(sales ~  s(TV) + s(radio) + s(newspaper), data = advdata)
summary(ajus.gam)
par(mfrow=c(2,2))
par(mar=c(rep(4,4)))
plot(ajus.gam,se=T)
par(mfrow=c(1,1))
coefficients(ajus.gam)
# para predecir 

pred<-predict(ajus.gam)
plot(sales,pred)
abline(0,1)

########### con PPr

ajus.ppr <- ppr(sales ~ TV+radio+newspaper, data=advdata, nterms = 3, max.terms = 3,trace=TRUE)
summary(ajus.ppr)
plot(ajus.ppr)

# to get the ridge functions
funciones<-ppr_funs(ajus.ppr)
plot(funciones$x[,1],funciones$y[,1])
lafun<-splinefun(funciones$x[,1],funciones$y[,1])
lafun(10)
lafun(100)
# to predict
pred <- predict(ajus.ppr, newdata=advdata)
plot(sales, pred, cex=0.5, pch=1)
abline(0,1,col='green')

##### con ANN

ajus.nnet <- nnet(sales ~ TV+radio+newspaper, data=advdata, size=5, decay=0.5, 
                  maxit=3000, linout=1, trace=TRUE)
plotnet(ajus.nnet)
summary(ajus.nnet)
# Prediction
pred <- predict(ajus.nnet, newdata=advdata)
plot(sales, pred, cex=0.5, pch=1)
abline(0,1,col='green')

