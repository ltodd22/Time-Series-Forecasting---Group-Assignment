library(fBasics)
library(forecast)
library(fGarch)

# Set the folder (path) that contains this R file as the working directory
dir <- dirname("/Users/nachocriado/Desktop/Classes/Time Series Forecasting")
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(dir)
data<-read.csv("coca_cola_earnings.csv",header=TRUE,sep=";",dec=",")

# Reading the files indicating the path
# Once the time series are read into R, we give a name to each time series data 
# (represented by each column)
series1=data[,2]

y<-series1 #from now, "y" is the data we are going to work wit

par(mfrow=c(3,1))
ts.plot(y)        # Histogram will be plotted on line 36
acf(y)
pacf(y)
Box.test(y, lag = 20, type="Ljung")
shapiro.test(y)

s = 4
nsdiffs(y, m=4, test=c("ocsb")) # number of transformations
ndiffs(y, alpha=0.05, test=c("adf"))

y.log <- log(y)

nsdiffs(y.log, m=4, test=c("ocsb")) # number of transformations
ndiffs(y.log, alpha=0.05, test=c("adf"))

par(mfrow=c(3,1))
ts.plot(y.log)        # Histogram will be plotted on line 36
acf(y.log)
pacf(y.log)
Box.test(y.log, lag = 20, type="Ljung")

nlags <- 60

#Area to test different combinations:
fit <- arima(y.log, order=c(5,0,0), seasonal=list(order=c(1,0,2), period=s))

fit <- arima(y.log, order=c(5,0,0), seasonal=list(order=c(1,1,2), period=s))

fit
ts.plot(fit$residuals)

Box.test(fit$residuals, lag = 36, type="Ljung")
nsdiffs(fit$residuals, m=4, test=c("ocsb")) # number of transformations
ndiffs(fit$residuals, alpha=0.05, test=c("adf"))

par(mfrow=c(2,1))
acf(fit$residuals, nlags)
pacf(fit$residuals, nlags)


################## Model 1 (0,1,5) x (0,1,0) s=4 #################
fit1 <- arima(y.log, order=c(0,1,5), seasonal=list(order=c(0,1,0), period=s, lambda=0))
fit1
ts.plot(fit1$residuals)


Box.test(fit1$residuals, lag = 36, type="Ljung")
nsdiffs(fit1$residuals, m=4, test=c("ocsb")) # number of transformations
ndiffs(fit1$residuals, alpha=0.05, test=c("adf"))

#now I'll plot
par(mfrow=c(2,1))
acf(fit1$residuals, nlags)
pacf(fit1$residuals, nlags)


#z <- (10 ** y)
#z <- y

# point predictions and standard errors

y.log.pred1<-predict(fit1,n.ahead=24)
y.pred1<-predict(fit1,n.ahead=24)
y.pred1$pred <- exp(y.log.pred1$pred) # point predictions
y.pred1$se <- exp(y.log.pred1$se) # standard errors


# plotting real data with point predictions

new <- c(y,y.pred1$pred) # real data + predicted values

plot.ts(new,main="Predictions Model 1: (0,1,5) (0,1,0)",
        ylab="Dollars",col=3,lwd=2) # time series plot
lines(y,col=4,lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c(3,4),
       bty="n",lwd=2)

# ERRORS
n<-length(y)
n.estimation<-83 # 
n.forecasting<-n-n.estimation # 198 observations
horizontes<-6 # number of periods ahead

predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 
MSFE_1<-matrix(0,nrow=horizontes,ncol=1)
MAPE_1<-matrix(0,nrow=horizontes,ncol=1)

for (Periods_ahead in 1:horizontes) {
        for (i in 1:n.forecasting) {
                aux.y<-y[1:(n.estimation-Periods_ahead+i)];
                aux.y.log <- log(aux.y)
                fit<-arima(aux.y.log, order=c(0,1,5), 
                           seasonal=list(order=c(0,1,0), period=s));
                y.pred.log<-predict(fit,n.ahead=Periods_ahead);
                y.pred1$pred <- exp(y.pred.log$pred)
                y.pred1$se <- exp(y.pred.log$se)
                predicc[i,Periods_ahead]<- (y.pred1$pred[Periods_ahead]);
        }
        error<-real-predicc[,Periods_ahead];
        MSFE_1[Periods_ahead]<-mean(error^2);
        MAPE_1[Periods_ahead]<-mean(abs(error/real)) * 100;
}

MSFE_1

l################## Model 2 (0,1,5) x (2,0,0) s=4  #################
fit2 <- arima(y.log, order=c(0,1,5), seasonal=list(order=c(2,0,0), period=s))
fit2
ts.plot(fit2$residuals)

Box.test(fit2$residuals, lag = 36, type="Ljung")
nsdiffs(fit2$residuals, m=4, test=c("ocsb")) # number of transformations
ndiffs(fit2$residuals, alpha=0.05, test=c("adf"))
#now I'll plot
par(mfrow=c(2,1))
acf(fit2$residuals, nlags)
pacf(fit2$residuals, nlags)


#z <- (10 ** y)
#z <- y

y.log.pred1<-predict(fit2,n.ahead=24)
y.pred1$pred <- exp(y.log.pred1$pred)
y.pred1$se <- exp(y.log.pred1$se)# point predictions
# standard errors


# plotting real data with point predictions

new <- c(y,y.pred1$pred) # real data + predicted values

plot.ts(new,main="Predictions Model 2: (0,1,5) (0,1,0)",
        ylab="Dollars",col=3,lwd=2) # time series plot
lines(y,col=4,lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c(3,4),
       bty="n",lwd=2)

# ERRORS
predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 
MSFE_2<-matrix(0,nrow=horizontes,ncol=1)
MAPE_2<-matrix(0,nrow=horizontes,ncol=1)

for (Periods_ahead in 1:horizontes) {
        for (i in 1:n.forecasting) {
                aux.y<-y[1:(n.estimation-Periods_ahead+i)];
                aux.y.log <- log(aux.y)
                fit<-arima(aux.y.log, order=c(0,1,5), 
                           seasonal=list(order=c(2,0,0), period=s));
                y.pred.log<-predict(fit,n.ahead=Periods_ahead);
                y.pred1$pred <- exp(y.pred.log$pred)
                y.pred1$se <- exp(y.pred.log$se)
                predicc[i,Periods_ahead]<- (y.pred1$pred[Periods_ahead]);
        }
        error<-real-predicc[,Periods_ahead];
        MSFE_2[Periods_ahead]<-mean(error^2);
        MAPE_2[Periods_ahead]<-mean(abs(error/real)) *100;
}

################## Model 3 (0,1,1) x (0,1,1) s=4  #################
fit3 <- arima(y.log, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=s))
fit3
ts.plot(fit3$residuals)

Box.test(fit3$residuals, lag = 36, type="Ljung")
nsdiffs(fit3$residuals, m=4, test=c("ocsb")) # number of transformations
ndiffs(fit3$residuals, alpha=0.05, test=c("adf"))

#now I'll plot
par(mfrow=c(3,1))
acf(fit3$residuals, nlags)
pacf(fit3$residuals, nlags)


#z <- (10 ** y)
#z <- y
y.log.pred1<-predict(fit3,n.ahead=24)
y.pred1$pred <- exp(y.log.pred1$pred)
y.pred1$se <- exp(y.log.pred1$se)
# point predictions and standard errors

y.log.pred1<-predict(fit3,n.ahead=24)
y.pred1$pred <- exp(y.log.pred1$pred)
y.pred1$se <- exp(y.log.pred1$se)# point predictions
# standard errors


# plotting real data with point predictions

new <- c(y,y.pred1$pred) # real data + predicted values

plot.ts(new,main="Predictions Model 3: (0,1,5) (0,1,0)",
        ylab="Dollars",col=3,lwd=2) # time series plot
lines(y,col=4,lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c(3,4),
       bty="n",lwd=2)

# ERRORS
predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 
MSFE_3<-matrix(0,nrow=horizontes,ncol=1)
MAPE_3<-matrix(0,nrow=horizontes,ncol=1)

for (Periods_ahead in 1:horizontes) {
        for (i in 1:n.forecasting) {
                aux.y<-y[1:(n.estimation-Periods_ahead+i)];
                aux.y.log <- log(aux.y)
                fit<-arima(aux.y.log, order=c(0,1,1), 
                           seasonal=list(order=c(0,1,1), period=s));
                y.pred.log<-predict(fit,n.ahead=Periods_ahead);
                y.pred1$pred <- exp(y.pred.log$pred)
                y.pred1$se <- exp(y.pred.log$se)
                predicc[i,Periods_ahead]<- (y.pred1$pred[Periods_ahead]);
        }
        error<-real-predicc[,Periods_ahead];
        MSFE_3[Periods_ahead]<-mean(error^2);
        MAPE_3[Periods_ahead]<-mean(abs(error/real)) *100;
}



################## Model 4 (3,0,5) x (3,0,0) s=4  #################
fit4 <- arima(y.log, order=c(3,0,5), seasonal=list(order=c(3,0,0), period=s))
fit4
ts.plot(fit4$residuals)

Box.test(fit4$residuals, lag = 36, type="Ljung")
nsdiffs(fit4$residuals, m=4, test=c("ocsb")) # number of transformations
ndiffs(fit4$residuals, alpha=0.05, test=c("adf"))

#now I'll plot
par(mfrow=c(2,1))
acf(fit4$residuals, nlags)
pacf(fit4$residuals, nlags)

#z <- (10 ** y)
#z <- y

y.log.pred1<-predict(fit4,n.ahead=24)
y.pred1$pred <- exp(y.log.pred1$pred)
y.pred1$se <- exp(y.log.pred1$se)# point predictions
# standard errors

# plotting real data with point predictions

new <- c(y,y.pred1$pred) # real data + predicted values

plot.ts(new,main="Predictions Model 3: (0,1,5) (0,1,0)",
        ylab="Dollars",col=3,lwd=2) # time series plot
lines(y,col=4,lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c(3,4),
       bty="n",lwd=2)

# ERRORS
predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 
MSFE_4<-matrix(0,nrow=horizontes,ncol=1)
MAPE_4<-matrix(0,nrow=horizontes,ncol=1)

for (Periods_ahead in 1:horizontes) {
        for (i in 1:n.forecasting) {
                aux.y<-y[1:(n.estimation-Periods_ahead+i)];
                aux.y.log <- log(aux.y)
                fit<-arima(aux.y.log, order=c(1,0,0), 
                           seasonal=list(order=c(1,0,0), period=s));
                y.pred.log<-predict(fit,n.ahead=Periods_ahead);
                y.pred1$pred <- exp(y.pred.log$pred)
                y.pred1$se <- exp(y.pred.log$se)
                predicc[i,Periods_ahead]<- (y.pred1$pred[Periods_ahead]);
        }
        error<-real-predicc[,Periods_ahead];
        MSFE_4[Periods_ahead]<-mean(error^2);
        MAPE_4[Periods_ahead]<-mean(abs(error/real)) *100;
}



################# Model 5 (5,0,0) x (1,1,2) s=4 ###############
fit5 <- arima(y.log, order=c(5,0,0), seasonal=list(order=c(1,1,2), period=s))
fit5
ts.plot(fit5$residuals)

Box.test(fit5$residuals, lag = 36, type="Ljung")
nsdiffs(fit5$residuals, m=4, test=c("ocsb")) # number of transformations
ndiffs(fit5$residuals, alpha=0.05, test=c("adf"))

#now I'll plot
par(mfrow=c(2,1))
acf(fit5$residuals, nlags)
pacf(fit5$residuals, nlags)

#z <- (10 ** y)
#z <- y

y.log.pred1<-predict(fit5,n.ahead=24)
y.pred1$pred <- exp(y.log.pred1$pred)
y.pred1$se <- exp(y.log.pred1$se)# point predictions
# standard errors

# plotting real data with point predictions

new <- c(y,y.pred1$pred) # real data + predicted values

plot.ts(new,main="Predictions Model 3: (0,1,5) (0,1,0)",
        ylab="Dollars",col=3,lwd=2) # time series plot
lines(y,col=4,lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c(3,4),
       bty="n",lwd=2)

# ERRORS
predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 
MSFE_5<-matrix(0,nrow=horizontes,ncol=1)
MAPE_5<-matrix(0,nrow=horizontes,ncol=1)

for (Periods_ahead in 1:horizontes) {
        for (i in 1:n.forecasting) {
                aux.y<-y[1:(n.estimation-Periods_ahead+i)];
                aux.y.log <- log(aux.y)
                fit<-arima(aux.y.log, order=c(5,0,0), 
                           seasonal=list(order=c(1,1,2), period=s));
                y.pred.log<-predict(fit,n.ahead=Periods_ahead);
                y.pred1$pred <- exp(y.pred.log$pred)
                y.pred1$se <- exp(y.pred.log$se)
                predicc[i,Periods_ahead]<- (y.pred1$pred[Periods_ahead]);
        }
        error<-real-predicc[,Periods_ahead];
        MSFE_5[Periods_ahead]<-mean(error^2);
        MAPE_5[Periods_ahead]<-mean(abs(error/real)) *100;
}


################ COMPARING ERRORS ################

#mfse_model1 <- mean(error_model1^2)
#rmse_model1 <- sqrt(mean(error_model1^2))
#mae_model1 <- mean(abs(error_model1))
#mape_model1 <- mean(perc_err1)
#mfse_model2 <- mean(error_model2^2)
#rmse_model2 <- sqrt(mean(error_model2^2))
#mae_model2 <- mean(abs(error_model2))
#mape_model2 <- mean(perc_err2)
#mfse_model3 <- mean(error_model3^2)
#rmse_model3 <- sqrt(mean(error_model3^2))
#mae_model3 <- mean(abs(error_model3))
#mape_model3 <- mean(perc_err3)
#mfse_model4 <- mean(error_model4^2)
#rmse_model4 <- sqrt(mean(error_model4^2))
#mae_model4 <- mean(abs(error_model4))
#mape_model4 <- mean(perc_err4)
#mfse_model5 <- mean(error_model5^2)
#rmse_model5 <- sqrt(mean(error_model5^2))
#mae_model5 <- mean(abs(error_model5))
#mape_model5 <- mean(perc_err5)

df <- data.frame(mfse_lag1 = c(MSFE_1[1] ,MSFE_2[1], MSFE_3[1], MSFE_4[1], MSFE_5[1]),
                 mape_lag1 = c(MAPE_1[1] ,MAPE_2[1], MAPE_3[1], MAPE_4[1], MAPE_5[1]),
                 mfse_lag2 = c(MSFE_1[2] ,MSFE_2[2], MSFE_3[2], MSFE_4[2], MSFE_5[2]),
                 mape_lag2 = c(MAPE_1[2] ,MAPE_2[2], MAPE_3[2], MAPE_4[2], MAPE_5[2]),
                 mfse_lag3 = c(MSFE_1[3] ,MSFE_2[3], MSFE_3[3], MSFE_4[3], MSFE_5[3]),
                 mape_lag3 = c(MAPE_1[3] ,MAPE_2[3], MAPE_3[3], MAPE_4[3], MAPE_5[3]),
                 mfse_lag4 = c(MSFE_1[4] ,MSFE_2[4], MSFE_3[4], MSFE_4[4], MSFE_5[4]),
                 mape_lag4 = c(MAPE_1[4] ,MAPE_2[4], MAPE_3[4], MAPE_4[4], MAPE_5[4]));
rownames(df) <- c("Model_1", "Model_2", "Model_3", "Model_4", "Model_5")

df[order(df$mape_lag1),]

df

######################## Plotting Predictions (use the screenshot attached in the email) #################


new <- c(y,y.pred1$pred) # real data + predicted values

real_data = ts(data[84:107,2], start = 84)

plot.ts(real_data,main="Comparisson of the 3 best models against real data",
        ylab="Dollars",col=1,lwd=2) # time series plot
lines(y.pred1$pred,col=3,lwd=2)
lines(train3.pred,col=4,lwd=2) 
lines(train5.pred,col=7,lwd=2) 
legend("topleft",legend=c("Real Data","Model 1","Model 3","Model 5"),col=c(1,3,4,7),
       bty="n",lwd=2)

