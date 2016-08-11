#This script fits an ARMA/ARIMA/SARIMA model on the residuals for each month
library(ggplot2)
library(lubridate)

data=read.csv("/home/drishi/coursework/STAT 929/Time-Series-Project/rf150predictedTrain.csv")
test<-read.csv("/home/drishi/coursework/STAT 929/Time-Series-Project/test.csv")
Locstrain   <- year(ymd_hms(data$datetime))==2011 & month(ymd_hms(data$datetime))==1
submission <- data.frame(datetime=test$datetime, predictedResiduals=NA)

locsTest<-year(ymd_hms(test$datetime))==2011 & month(ymd_hms(test$datetime))==1
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
acf2(d2)
acf2(diff(d2))
sarima(d2,3,1,3)
(dtest<-test[locsTest,])

s<-sarima.for(d2,nrow(dtest),3,1,3)
s
submission[locsTest, "predictedResiduals"] <- s$pred+mean1

#MOnth 2 year 2011
Locstrain   <- year(ymd_hms(data$datetime))==2011 & month(ymd_hms(data$datetime))==2
locsTest<-year(ymd_hms(test$datetime))==2011 & month(ymd_hms(test$datetime))==2
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
acf2(d2)
acf2(diff(d2,1))
sarima(d2,2,0,2)

(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),2,0,2)
s

submission[locsTest, "predictedResiduals"] <- s$pred+mean1


#month 3 in 2011
print('MOnth 3')
Locstrain   <- year(ymd_hms(data$datetime))==2011 & month(ymd_hms(data$datetime))==3
locsTest<-year(ymd_hms(test$datetime))==2011 & month(ymd_hms(test$datetime))==3
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
acf2(d2)
acf2(diff(d2,1))
sarima(d2,2,1,2)

(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),2,1,2)
s

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('MOnth 3 done')


#month4 in 2011
print('Month 4')
Locstrain   <- year(ymd_hms(data$datetime))==2011 & month(ymd_hms(data$datetime))==4
locsTest<-year(ymd_hms(test$datetime))==2011 & month(ymd_hms(test$datetime))==4
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
acf2(d2)
acf2(diff(d2,1))
sarima(d2,2,1,2)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),2,1,2)
s$pred

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 4 done')

#Month 5 2011
print('Month 5')
Locstrain   <- year(ymd_hms(data$datetime))==2011 & month(ymd_hms(data$datetime))==5
locsTest<-year(ymd_hms(test$datetime))==2011 & month(ymd_hms(test$datetime))==5
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
acf2(d2)
acf2(diff(d2,1))
sarima(d2,2,0,1)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),2,0,1)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 5 done')

#Month 6 2011
print('Month 6')
Locstrain   <- year(ymd_hms(data$datetime))==2011 & month(ymd_hms(data$datetime))==6
locsTest<-year(ymd_hms(test$datetime))==2011 & month(ymd_hms(test$datetime))==6
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
acf2(d2)
acf2(diff(d2,1))
sarima(d2,2,1,2)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),2,1,2)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 6 done')


#Month 7 2011
print('Month 7')
Locstrain   <- year(ymd_hms(data$datetime))==2011 & month(ymd_hms(data$datetime))==7
locsTest<-year(ymd_hms(test$datetime))==2011 & month(ymd_hms(test$datetime))==7
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
acf2(d2)
acf2(diff(d2,1))
sarima(d2,1,0,2)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),1,0,2)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 7 done')

#Month 8 2011
print('Month 8')
Locstrain   <- year(ymd_hms(data$datetime))==2011 & month(ymd_hms(data$datetime))==8
locsTest<-year(ymd_hms(test$datetime))==2011 & month(ymd_hms(test$datetime))==8
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
acf2(d2)
acf2(diff(d2,1))
sarima(d2,2,0,2)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),2,0,2)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 8 done')


#Month 9 2011
print('Month 9')
Locstrain   <- year(ymd_hms(data$datetime))==2011 & month(ymd_hms(data$datetime))==9
locsTest<-year(ymd_hms(test$datetime))==2011 & month(ymd_hms(test$datetime))==9
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
acf2(d2)
acf2(diff(d2,1))
sarima(d2,2,1,9)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),2,1,9)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 9 done')

#Month 10 2011
print('Month 10')
Locstrain   <- year(ymd_hms(data$datetime))==2011 & month(ymd_hms(data$datetime))==10
locsTest<-year(ymd_hms(test$datetime))==2011 & month(ymd_hms(test$datetime))==10
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
acf2(d2)
acf2(diff(d2,1))
sarima(d2,1,1,4)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),1,1,4)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 10 done')


#Month 11 2011
print('Month 11')
Locstrain   <- year(ymd_hms(data$datetime))==2011 & month(ymd_hms(data$datetime))==11
locsTest<-year(ymd_hms(test$datetime))==2011 & month(ymd_hms(test$datetime))==11
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
acf2(d2)
acf2(diff(d2,24))
sarima(d2,1,0,2,0,0,1,24)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),1,0,2,0,0,1,24)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 11 done')

#Month 12 2011
print('Month 12')
Locstrain   <- year(ymd_hms(data$datetime))==2011 & month(ymd_hms(data$datetime))==12
locsTest<-year(ymd_hms(test$datetime))==2011 & month(ymd_hms(test$datetime))==12
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
acf2(d2)
acf2(diff(d2,1))
sarima(d2,0,1,2)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),0,1,2)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 12 done')

#Month 1 2012
print('Month 1 2012 ')
Locstrain   <- year(ymd_hms(data$datetime))==2012 & month(ymd_hms(data$datetime))==1
locsTest<-year(ymd_hms(test$datetime))==2012 & month(ymd_hms(test$datetime))==1
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
k=kernel("daniell",4)
spec.pgram(d2,k)
ts.plot(d2)
acf2(d2,50)
acf2(diff(d2,24),50)
d3=diff(d2,1)

acf2(diff(d3,24),50)
sarima(d2,1,0,1,0,1,2,24)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),1,0,1,0,1,2,24)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 1 2012 done')


#Month 2 2012
print('Month 2 2012 ')
Locstrain   <- year(ymd_hms(data$datetime))==2012 & month(ymd_hms(data$datetime))==2
locsTest<-year(ymd_hms(test$datetime))==2012 & month(ymd_hms(test$datetime))==2
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
#k=kernel("daniell",4)
#spec.pgram(d2,k)
ts.plot(d2)
acf2(d2,50)
acf2(diff(d2,1),50)
d3=diff(d2,1)

acf2(diff(d3,24),50)
sarima(d2,1,0,1,0,1,2,24)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),1,0,1,0,1,2,24)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 2 2012 done')

#Month 3 2012
print('Month 3 2012 ')
Locstrain   <- year(ymd_hms(data$datetime))==2012 & month(ymd_hms(data$datetime))==3
locsTest<-year(ymd_hms(test$datetime))==2012 & month(ymd_hms(test$datetime))==3
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
#k=kernel("daniell",4)
#spec.pgram(d2,k)
ts.plot(d2)
acf2(d2,50)
acf2(diff(d2,1),50)
d3=diff(d2,1)

acf2(diff(d3,24),50)
sarima(d2,7,0,1,0,1,2,24)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),7,0,1,0,1,2,24)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 3 2012 done')


#Month 4 2012
print('Month 4 2012 ')
Locstrain   <- year(ymd_hms(data$datetime))==2012 & month(ymd_hms(data$datetime))==4
locsTest<-year(ymd_hms(test$datetime))==2012 & month(ymd_hms(test$datetime))==4
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
#k=kernel("daniell",4)
#spec.pgram(d2,k)
ts.plot(d2)
acf2(d2,50)
acf2(diff(d2,1),50)
d3=diff(d2,1)

acf2(diff(d3,24),50)
sarima(d2,1,0,1,0,1,1,24)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),1,0,1,0,1,1,24)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 4 2012 done')

#Month 5 2012
year=2012
month=5
print('Month 5 2012 ')
Locstrain   <- year(ymd_hms(data$datetime))==2012 & month(ymd_hms(data$datetime))==5
locsTest<-year(ymd_hms(test$datetime))==2012 & month(ymd_hms(test$datetime))==5
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
#k=kernel("daniell",4)
#spec.pgram(d2,k)
ts.plot(d2,main="month 5")
acf2(d2,50)
acf2(diff(d2,1),50)
d3=diff(d2,1)

acf2(diff(d3,24),50)
sarima(d2,3,1,2,1,0,1,24)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),3,1,2,1,0,1,24)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 5 2012 done')

#Month 6 2012
year=2012
month=6
print('Month 6 2012 ')
Locstrain   <- year(ymd_hms(data$datetime))==2012 & month(ymd_hms(data$datetime))==6
locsTest<-year(ymd_hms(test$datetime))==2012 & month(ymd_hms(test$datetime))==6
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
#k=kernel("daniell",4)
#spec.pgram(d2,k)
ts.plot(d2,main="month 6")
acf2(d2,50)
acf2(diff(d2,1),50)
d3=diff(d2,1)

acf2(diff(d3,24),50)
sarima(d2,3,1,1,1,0,1,24)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),3,1,1,1,0,1,24)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 6 2012 done')



#Month 7 2012
year=2012
month=7
print('Month 7 2012 ')
Locstrain   <- year(ymd_hms(data$datetime))==2012 & month(ymd_hms(data$datetime))==7
locsTest<-year(ymd_hms(test$datetime))==2012 & month(ymd_hms(test$datetime))==7
locsTest
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
#k=kernel("daniell",4)
#spec.pgram(d2,k)
ts.plot(d2,main="month 7")
acf2(d2,50)
acf2(diff(d2,1),50)
d3=diff(d2,1)

acf2(diff(d3,24),50)
sarima(d2,1,0,0,1,0,1,24)
nrow(dtest)
(dtest<-test[locsTest,])
nrow(dtest)
s<-sarima.for(d2,nrow(dtest),1,0,0,1,0,1,24)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 7 2012 done')

#Month 8 2012
year=2012
month=8
print('Month 8 2012 ')
Locstrain   <- year(ymd_hms(data$datetime))==2012 & month(ymd_hms(data$datetime))==8
locsTest<-year(ymd_hms(test$datetime))==2012 & month(ymd_hms(test$datetime))==8
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
#k=kernel("daniell",4)
#spec.pgram(d2,k)
ts.plot(d2,main="month 8")
acf2(d2,50)
acf2(diff(d2,1),50)
d3=diff(d2,1)

acf2(diff(d3,24),50)
sarima(d2,1,0,0,1,0,0,24)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),1,0,0,1,0,0,24)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 8 2012 done')

#Month 9 2012 check again
year=2012 
month=9
print('Month 9 2012 ')
Locstrain   <- year(ymd_hms(data$datetime))==2012 & month(ymd_hms(data$datetime))==9
locsTest<-year(ymd_hms(test$datetime))==2012 & month(ymd_hms(test$datetime))==9
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
#k=kernel("daniell",4)
#spec.pgram(d2,k)
ts.plot(d2,main="month 9")
acf2(d2,50)
acf2(diff(d2,1),50)
d3=diff(d2,1)

acf2(diff(d3,24),50)
sarima(d2,3,0,2,1,0,0,24)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),3,0,2,1,0,0,24)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 9 2012 done')

#Month 10 2012 
year=2012 
month=10
print('Month 10 2012 ')
Locstrain   <- year(ymd_hms(data$datetime))==2012 & month(ymd_hms(data$datetime))==10
locsTest<-year(ymd_hms(test$datetime))==2012 & month(ymd_hms(test$datetime))==10
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
#k=kernel("daniell",4)
#spec.pgram(d2,k)
ts.plot(d2,main="month 10")
acf2(d2,50)
acf2(diff(d2,1),50)
d3=diff(d2,1)

acf2(diff(d3,24),50)
sarima(d2,2,1,6,1,0,0,24)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),2,1,6,1,0,0,24)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 10 2012 done')

#Month 11 2012 
year=2012 
month=11
print('Month 11 2012 ')
Locstrain   <- year(ymd_hms(data$datetime))==2012 & month(ymd_hms(data$datetime))==11
locsTest<-year(ymd_hms(test$datetime))==2012 & month(ymd_hms(test$datetime))==11
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
#k=kernel("daniell",4)
#spec.pgram(d2,k)
ts.plot(d2,main="month 11")
acf2(d2,50)
acf2(diff(d2,1),50)
d3=diff(d2,1)

acf2(diff(d3,24),50)
sarima(d2,1,0,0,1,0,0,24)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),1,0,0,1,0,0,24)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 11 2012 done')


#Month 12 2012 
year=2012 
month=12
print('Month 12 2012 ')
Locstrain   <- year(ymd_hms(data$datetime))==2012 & month(ymd_hms(data$datetime))==12
locsTest<-year(ymd_hms(test$datetime))==2012 & month(ymd_hms(test$datetime))==12
(d1 <- data[Locstrain,])
d2<-d1$residuals
(mean1<-mean(d2))
d2=d2-mean1
#k=kernel("daniell",4)
#spec.pgram(d2,k)
ts.plot(d2,main="month 11")
acf2(d2,50)
acf2(diff(d2,1),50)
d3=diff(d2,1)

acf2(diff(d3,24),50)
sarima(d2,1,0,1,1,0,1,24)
nrow(dtest)
(dtest<-test[locsTest,])
s<-sarima.for(d2,nrow(dtest),1,0,1,1,0,1,24)
s
s$pred+mean1

submission[locsTest, "predictedResiduals"] <- s$pred+mean1
print('Month 12 2012 done')

write.csv(submission, file = "/home/drishi/coursework/STAT 929/Time-Series-Project/predicted residuals.csv", row.names=FALSE)
