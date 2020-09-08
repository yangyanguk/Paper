
# Set window
window <- 400

library(tseries)
library(forecast)
library(lubridate)
library(glmnet)
library(caret)
library(plyr)

data <- read.csv("/Users/yangy/Desktop/Codes/current.csv")
dat <- data[-1,]
dat$sasdate <- as.Date(dat$sasdate,"%m/%d/%Y")
trans.code <- data[1,]

plot(dat$sasdate,dat$S.P.500,type="l",main="SP 500 Series",xlab="Time",ylab="SP 500")

#transformation according to trans code
for (i in 2:length(dat[1,])){
  if (trans.code[i]==1){
    dat[,i] <- dat[,i]
  }
  else if (trans.code[i]==2){
    dat[,i] <- c(NA,diff(dat[,i]))
  }
  else if (trans.code[i]==3){
    dat[,i] <- c(NA,NA,diff(dat[,i],differences=2))
  }
  else if (trans.code[i]==4){
    dat[,i] <- log(dat[,i])
  }
  else if (trans.code[i]==5){
    dat[,i] <- c(NA,diff(log(dat[,i])))
  }
  else if (trans.code[i]==6){
    dat[,i] <- c(NA,NA,diff(log(dat[,i]),differences=2))
  }
  else if (trans.code[i]==7){
    a <- dat[,i]
    dat[,i] <- c(NA,NA,diff(diff(a)/lag(a[-length(a)])))
  }
}


# drop NA
dat$ACOGNO <- NULL
dat$ANDENOx <- NULL
dat$TWEXAFEGSMTHx <- NULL
dat$UMCSENTx <- NULL
dat$S.P..indust <- NULL
dat <- na.omit(dat)

# get lag data
dat$S.P.500.1 <- c(NA,lag(dat$S.P.500[-length(dat$S.P.500)]))

plot(dat$sasdate,dat$S.P.500,type="l",main="SP 500 Series--Transformation",
     xlab="Time",ylab="SP 500")




#section{ARIMA}


# get number of forecasts
number.forecast <- length(dat$S.P.500)-window
# initialize the forecast dataset
prediction.arima <- rep(NA,length(dat$S.P.500))
arima.order <- NULL

for (i in 1:number.forecast){
  # get train data
  train <- dat$S.P.500[i:(i+window-1)]
  # train best ARIMA based on train data
  arima.res <- auto.arima(train,seasonal = F)
  # get estimation order
  arima.order <- rbind(arima.order,arimaorder(arima.res))
  # one step forecast
  prediction.arima[(i+window)] <- forecast(arima.res,h=1)$mean[1]
}

count(arima.order)

plot(dat$sasdate,dat$S.P.500,type="l",main="SP 500 vs Rolling Forecast With ARIMA",
     xlab="Time",ylab="SP 500",lwd=0.4)
lines(dat$sasdate,prediction.arima,col="red",lwd=0.5)
legend("bottomleft",inset=.02,legend=c("SP500","Forecast-ARIMA"),
       lty=c(1,1),col=c("black","red"),lwd=0.5)

RMSE.arima <- sqrt(mean((dat$S.P.500-prediction.arima)^2,na.rm=TRUE))
RMSE.arima
MAPE.arima <- mean(abs((prediction.arima-dat$S.P.500)/dat$S.P.500),na.rm=TRUE)
MAPE.arima



#section{Regression}



# get number of forecasts
number.forecast <- length(dat$S.P.500)-window
# initialize the forecast dataset
prediction.reg <- rep(NA,length(dat$S.P.500))

for (i in 1:number.forecast){
  # get train data
  train <- dat[i:(i+window-1),-1]
  # regression based on train data
  reg.res <- lm(S.P.500~.,data = train)
  # one step forecast
  prediction.reg[(i+window)] <- predict(reg.res,newdata = dat[(i+window),-1])
}

plot(dat$sasdate,dat$S.P.500,type="l",main="SP 500 vs Rolling Forecast With Regression",
     xlab="Time",ylab="SP 500",lwd=0.4)
lines(dat$sasdate,prediction.reg,col="red",lwd=0.5)
legend("bottomleft",inset=.02,legend=c("SP500","Forecast-Regression"),
       lty=c(1,1),col=c("black","red"),lwd=0.5)

RMSE.reg <- sqrt(mean((dat$S.P.500-prediction.reg)^2,na.rm=TRUE))
RMSE.reg
MAPE.reg <- mean(abs((prediction.reg-dat$S.P.500)/dat$S.P.500),na.rm=TRUE)
MAPE.reg





#section{PCA}


# PCA for all X
prin <- prcomp(subset(dat, select=c(-sasdate,-S.P.500,-S.P.500.1)),scale = TRUE)
# get the proportion of variance explained
VE <- prin$sdev[1:10]^2
PVE <- VE / sum(VE)

par(mfrow=c(1,2))
plot(seq(1,10),PVE,type="l",xlab="Principal Component",ylab="PVE",
     main="Proportion of Variance")
plot(seq(1,10),cumsum(PVE),type="l",xlab="Principal Component",ylab="Cum PVE",
     main="Cumulative Proportion")
par(mfrow=c(1,1))

dat.p <- cbind(dat$S.P.500,dat$S.P.500.1,as.data.frame(prin$x[,1:7]))
names(dat.p)[c(1,2)] <- c("S.P.500","S.P.500.1")

# get number of forecasts
number.forecast <- length(dat$S.P.500)-window
# initialize the forecast dataset
prediction.pca <- rep(NA,length(dat$S.P.500))

for (i in 1:number.forecast){
  # get train data
  train <- dat.p[i:(i+window-1),]
  # regression based on train data
  pca.res <- lm(S.P.500~S.P.500.1+PC1+PC2+PC3+PC4+PC5+PC6+PC7,data = train)
  # one step forecast
  prediction.pca[(i+window)] <- predict(pca.res,newdata = dat.p[(i+window),])
}

plot(dat$sasdate,dat$S.P.500,type="l",main="SP 500 vs Rolling Forecast With PCA Regression",
     xlab="Time",ylab="SP 500",lwd=0.4)
lines(dat$sasdate,prediction.pca,col="red",lwd=0.5)
legend("bottomleft",inset=.02,legend=c("SP500","Forecast-PCR"),
       lty=c(1,1),col=c("black","red"),lwd=0.5)

RMSE.pca <- sqrt(mean((dat$S.P.500-prediction.pca)^2,na.rm=TRUE))
RMSE.pca
MAPE.pca <- mean(abs((prediction.pca-dat$S.P.500)/dat$S.P.500),na.rm=TRUE)
MAPE.pca


#section{PCA Quadratic}


# get number of forecasts
number.forecast <- length(dat$S.P.500)-window
# initialize the forecast dataset
prediction.pca2 <- rep(NA,length(dat$S.P.500))

for (i in 1:number.forecast){
  # get train data
  train <- dat.p[i:(i+window-1),]
  # regression based on train data
  pca2.res <- lm(S.P.500~S.P.500.1+PC1+PC2+PC3+PC4+PC5+PC6+PC7
                 +I(PC1^2)+I(PC2^2)+I(PC3^2)+I(PC4^2)+I(PC5^2)+I(PC6^2)+I(PC7^2)
                 ,data = train)
  # one step forecast
  prediction.pca2[(i+window)] <- predict(pca2.res,newdata = dat.p[(i+window),])
}

plot(dat$sasdate,dat$S.P.500,type="l",main="SP 500 vs Rolling Forecast With SPCR",
     xlab="Time",ylab="SP 500",lwd=0.4)
lines(dat$sasdate,prediction.pca2,col="red",lwd=0.5)
legend("bottomleft",inset=.02,legend=c("SP500","Forecast-SPCR"),
       lty=c(1,1),col=c("black","red"),lwd=0.5)

RMSE.pca2 <- sqrt(mean((dat$S.P.500-prediction.pca2)^2,na.rm=TRUE))
RMSE.pca2
MAPE.pca2 <- mean(abs((prediction.pca2-dat$S.P.500)/dat$S.P.500),na.rm=TRUE)
MAPE.pca2




#section{LASSO}



# get number of forecasts
number.forecast <- length(dat$S.P.500)-window
# initialize the forecast dataset
prediction.lasso <- rep(NA,length(dat$S.P.500))
coefl <- NULL
for (i in 1:number.forecast){
  # 1. get train data
  train <- dat[i:(i+window-1),]
  train <- na.omit(train)
  # glmnet requires matrix input, transform to matrix input
  x_train <- as.matrix(subset(train, select=c(-sasdate,-S.P.500)))
  y_train <- as.matrix(train$S.P.500)
  # get one step forward data
  x_test <- as.matrix(subset(dat,select=c(-sasdate,-S.P.500))[(i+window),])
  # 2. Tuning lambda
  # first pick a sequence of lambdas
  lambdas <- 10^seq(2, -3, by = -0.1)
  # 10-folder cross validation on train data to find out lambda with lowest cross validation error
  # alpha=1 is for LASSO
  cv_output <- cv.glmnet(x_train, y_train,
                         alpha = 1, lambda = lambdas,
                         nfolds = 10)
  # get best lambda
  best_lam <- cv_output$lambda.min
  # 3. LASSO based on train data with best lambda
  lasso.res <- glmnet(x_train, y_train, alpha = 1, lambda = best_lam)
  # get the coefficients
  coefl <- rbind(coef(lasso.res)[,1],coefl)
  # one step forecast
  prediction.lasso[(i+window)] <- predict(lasso.res,newx = x_test)
}

aa <- apply(coefl!=0,2,mean)
aa[order(aa,decreasing=TRUE)]

plot(dat$sasdate,dat$S.P.500,type="l",main="SP 500 vs Rolling Forecast With LASSO",
     xlab="Time",ylab="SP 500",lwd=0.4)
lines(dat$sasdate,prediction.lasso,col="red",lwd=0.5)
legend("bottomleft",inset=.02,legend=c("SP500","Forecast-LASSO"),
       lty=c(1,1),col=c("black","red"),lwd=0.5)

RMSE.lasso <- sqrt(mean((dat$S.P.500-prediction.lasso)^2,na.rm=TRUE))
RMSE.lasso
MAPE.lasso <- mean(abs((prediction.lasso-dat$S.P.500)/dat$S.P.500),na.rm=TRUE)
MAPE.lasso




#section{Ridge}


# get number of forecasts
number.forecast <- length(dat$S.P.500)-window
# initialize the forecast dataset
prediction.ridge <- rep(NA,length(dat$S.P.500))

for (i in 1:number.forecast){
  # 1. get train data 
  train <- dat[i:(i+window-1),]
  train <- na.omit(train)
  # glmnet requires matrix input, transform to matrix input
  x_train <- as.matrix(subset(train, select=c(-sasdate,-S.P.500)))
  y_train <- as.matrix(train$S.P.500)
  # get one step forward data
  x_test <- as.matrix(subset(dat,select=c(-sasdate,-S.P.500))[(i+window),])
  
  # 2. Tuning lambda 
  # first pick a sequence of lambdas
  lambdas <- 10^seq(2, -3, by = -0.1)
  # 10-folder cross validation on train data to find out lambda with lowest cross validation error
  # alpha=1 is for Ridge
  cv_output <- cv.glmnet(x_train, y_train,
                         alpha = 0, lambda = lambdas, 
                         nfolds = 10)
  # get best lambda
  best_lam <- cv_output$lambda.min
  
  # 3. Ridge based on train data with best lambda
  ridge.res <- glmnet(x_train, y_train, alpha = 0, lambda = best_lam)
  # one step forecast
  prediction.ridge[(i+window)] <- predict(ridge.res,newx = x_test)
}

plot(dat$sasdate,dat$S.P.500,type="l",main="SP 500 vs Rolling Forecast With Ridge",
     xlab="Time",ylab="SP 500",lwd=0.4)
lines(dat$sasdate,prediction.ridge,col="red",lwd=0.5)
legend("bottomleft",inset=.02,legend=c("SP500","Forecast-ridge"),
       lty=c(1,1),col=c("black","red"),lwd=0.5)

RMSE.ridge <- sqrt(mean((dat$S.P.500-prediction.ridge)^2,na.rm=TRUE))
RMSE.ridge
MAPE.ridge <- mean(abs((prediction.ridge-dat$S.P.500)/dat$S.P.500),na.rm=TRUE)
MAPE.ridge



#section{ELNET}


# get number of forecasts
number.forecast <- length(dat$S.P.500)-window
# initialize the forecast dataset
prediction.elnet <- rep(NA,length(dat$S.P.500))

for (i in 1:number.forecast){
  # 1. get train data
  train <- dat[i:(i+window-1),]
  train <- na.omit(train)
  # get one step forward data
  x_test <- as.matrix(subset(dat,select=c(-sasdate,-S.P.500))[(i+window),])
  
  # 2. Tuning alpha and lambda 
  # Fit the elnet with best alpha and lambda through cross validation
  model <- train(
    S.P.500 ~., data = train[,-1], method = "glmnet",
    # 10-folder cross validation
    trControl = trainControl("cv", number = 10),
    # try 50 random alpha and lambda pairs
    tuneLength = 50
  )
  
  # 3. one step forecast
  prediction.elnet[(i+window)] <- model %>% predict(x_test)
}

plot(dat$sasdate,dat$S.P.500,type="l",main="SP 500 vs Rolling Forecast With ElNet",
     xlab="Time",ylab="SP 500",lwd=0.4)
lines(dat$sasdate,prediction.elnet,col="red",lwd=0.5)
legend("bottomleft",inset=.02,legend=c("SP500","Forecast-ElNet"),
       lty=c(1,1),col=c("black","red"),lwd=0.5)

RMSE.elnet <- sqrt(mean((dat$S.P.500-prediction.elnet)^2,na.rm=TRUE))
RMSE.elnet
MAPE.elnet <- mean(abs((prediction.elnet-dat$S.P.500)/dat$S.P.500),na.rm=TRUE)
MAPE.elnet


pred <- cbind(dat$sasdate,dat$S.P.500,prediction.arima,prediction.reg,
              prediction.pca,prediction.pca2,prediction.lasso,
              prediction.ridge,prediction.elnet)

write.csv(pred,"/Users/yangy/Desktop/Codes/pred.csv")


