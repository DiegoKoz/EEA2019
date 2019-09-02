######################
# Overfitting and Neural Nets
#######################
library(rgl)
library(NeuralNetTools)
library(ggplot2)
library(readr)
library(dplyr)
library(caret)
library(nnet)
library(doMC)
# Benchmarking function=MAE
mae<-function(a,b){mean(abs(a-b),na.rm=TRUE)}
# Genrating the data
set.seed(1)
n<-200 # sample size
x1<-runif(n)
x2<-runif(n)
x3<-runif(n)
x4<-runif(n)
x5<-runif(n)
x6<-runif(n)
x7<-runif(n)
x8<-runif(n)
x9<-runif(n)
x10<-runif(n)
x11<-runif(n)
x12<-runif(n)
x13<-runif(n)
x14<-runif(n)
x15<-runif(n)
ruido<-rnorm(n)/4 # the added error
y<-4+3*x1+ruido # y explained just by x1
plot(x1,y)
plot3d(x1,x2,y)
datos<-data.frame(cbind(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15))
datos.train<-datos[1:(n/2),]
datos.test<-datos[((n/2)+1):n,]
plot(datos.train$x1,datos.train$y)
suave<-lowess(datos.train$x1,datos.train$y)
lines(suave,col='green',lwd=3)
# True MAE
mae(0,ruido)
# Trying a simple liear model
ajus.lin<-lm(y ~ ., data=datos.train)
summary(ajus.lin)
pred <- predict(ajus.lin, newdata=datos.test)
plot(datos.test$y, pred, xlab='Real', ylab='Predicho', cex=0.5, pch=1)
abline(0,1,col='green')
elmae<-mae(datos.test$y,pred)
title(paste('MAE =',round(elmae,2)))
#### Training the NNET model
set.seed(1000)
ajus.nnet <- nnet(y ~ ., data=datos.train, size=4, decay=0, 
                  maxit=3000, linout=1, trace=TRUE)
ajus.nnet
plotnet(ajus.nnet)
summary(ajus.nnet)
##### To look at the results of the model
# Using the training dataset
pred <- predict(ajus.nnet, newdata=datos.train)
plot(datos.train$y, pred, xlab='Real', ylab='Predicho', cex=0.5, pch=1)
abline(0,1,col='green')
elmae<-mae(datos.train$y,pred)
title(paste('MAE =',round(elmae,2)))
# Using the Testing dataset
pred <- predict(ajus.nnet, newdata=datos.test)
plot(datos.test$y, pred, xlab='Real', ylab='Predicho', cex=0.5, pch=1)
abline(0,1,col='green')
elmae<-mae(datos.test$y,pred)
title(paste('MAE =',round(elmae,2)))
############
# What is the fitted function ????
for (lavar in 1:(ncol(datos)-1))
{
  datos.marg<-datos.test
  datos.marg[,-(lavar+1)]<-0 # no change in these variables
  datos.marg[,(lavar+1)]<-seq(from=-0,to=1,length.out=n/2) # change in here
  pred <- predict(ajus.nnet, newdata=datos.marg)
  plot(datos.marg[,lavar+1],pred,ylim=range(y))
  title(paste('Var x',as.character(lavar),sep=''))
}
# Weird things happen with NN fit
undato<-datos.test[1,]
undato[,2:16]<-0;undato[,14]<-1 # bad
undato[,2:16]<-0;undato[,14]<-1;undato[,15]<-1 # bad
undato[,2:16]<-0;undato[,2]<-1 # good
undato[,2:16]<-runif(15) # let us see
pred.nn <- predict(ajus.nnet, newdata=undato)
pred.lin <- predict(ajus.lin, newdata=undato)
pred.nn
pred.lin
#### let us generate many samples
N<-1000
mat<-matrix(runif(15*N),N,15)
manydatos<-data.frame(mat);names(manydatos)<-colnames(datos)[2:16]
pred.nn <- predict(ajus.nnet, newdata=manydatos)
plot(pred.nn)
abline(4,0,col='green');abline(7,0,col='green')
###############################################
# Grid Search and Hiperparametrization Approach
###############################################
maeSummary <- function (data,
                        lev = NULL,
                        model = NULL) {
  out <- mae(data$obs, data$pred)  
  names(out) <- "MAE"
  out
}
# Training
fitControl <- trainControl(method = "repeatedcv",
                           number = 2,
                           repeats = 10,
                           summaryFunction = maeSummary)

escen<-expand.grid(.size=seq(from=1,to=21,by=3),.decay=c(0,0.1,0.2,0.3))
model <- train(y~., data=datos.train, method='nnet', trace = TRUE,tuneGrid=escen,trControl = fitControl,maxit=3000,linout=1, metric = "MAE",maximize=FALSE)
plot(model)
#  importance of variables
import<-varImp(model, scale = FALSE)
plot(import)
## extracting the best model
best.model<-model$finalModel
best.model
summary(best.model)
plotnet(best.model)
##### To look at the results of the model
pred <- predict(best.model, newdata=datos.test)
plot(datos.test$y, pred, xlab='Real', ylab='Predicho', cex=0.5, pch=1)
abline(0,1,col='green')
elmae<-mae(datos.test$y,pred)
title(paste('MAE =',round(elmae,2)))
#
# What is the fitted function ????
for (lavar in 1:(ncol(datos)-1))
{
  datos.marg<-datos.test
  datos.marg[,-(lavar+1)]<-0 # no change in these variables
  datos.marg[,(lavar+1)]<-seq(from=-0,to=1,length.out=n/2) # change in here
  pred <- predict(best.model, newdata=datos.marg)
  plot(datos.marg[,lavar+1],pred,ylim=range(y))
  title(paste('Var x',as.character(lavar),sep=''))
}
##### let us generate many samples
N<-1000
mat<-matrix(runif(15*N),N,15)
manydatos<-data.frame(mat);names(manydatos)<-colnames(datos)[2:16]
pred.nn <- predict(best.model, newdata=manydatos)
plot(pred.nn)
abline(4,0,col='green');abline(7,0,col='green')
