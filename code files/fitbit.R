library(lubridate)

data<-read.csv("/home/drishi/coursework/STAT 929/greg's data/fitbit/fit_bit_update.csv")
load("/home/drishi/coursework/STAT 929/tsa3.rda")

(calories=as.ts(data$Calories.Burned))

(steps=as.ts(data$Steps))

(distance=as.ts(data$Distance))

(floors=as.ts(data$Floors))

(MinutesSedentary=as.ts(data$Minutes.Sedentary))

(MinutesLightlyActive=as.ts(data$Minutes.Lightly.Active))

(MinutesFairlyActive=as.ts(data$Minutes.Lightly.Active))

(MinutesVeryActive=as.ts(data$Minutes.Very.Active))

(ActivityCalories=as.ts(data$Activity.Calories))

par(mfrow=c(3,3),mar=rep(2,4))
ts.plot(calories,main="Calories")
ts.plot(steps,main="Steps")
ts.plot(distance,main="Distance")
ts.plot(floors,main="Floors")
ts.plot(MinutesSedentary,main="Minutes Sedentary")
ts.plot(MinutesLightlyActive,main="Minutes Lightly Active")
ts.plot(MinutesFairlyActive,main="Minutes Fairly Active")
ts.plot(MinutesVeryActive,main="Minutes Very Active")
ts.plot(ActivityCalories,main="Activity Calories")

analyzeSteps<-function(){
  
  #par(mfrow=c(2,1))
  #par(op)
  par(oma=c(3,3,0,0),mar=c(3,3,2,2),mfrow=c(4,1))
  steps=steps-mean(steps)
  ts.plot(steps,main="Steps")
  steps.lm=lm(steps~time(steps),na.action=NULL)
  summary(steps.lm)
  plot(resid(steps.lm),main="dtrended residuals")
  #par(mfrow=c(3,1))
  #par(op)
  acf2(steps,30)
  #acf(residfit),main="ACF of Residuals from Regression")
  sarima(steps,0,0,1)
  par(mfrow=c(1,1))
  sarima(steps,1,0,7)
  sarima.for(steps,14,0,0,1)
}
analyzeSteps()

analyzeFloors<-function(){
  
  #par(mfrow=c(1,1))
  #par(op)
  par(oma=c(3,3,0,0),mar=c(3,3,2,2),mfrow=c(2,1))
  floors=floors-mean(floors)
  ts.plot(floors,main="Floors")
  fit=lm(steps~time(steps),na.action=NULL)
  summary(fit)
  plot(resid(fit),main="dtrended residuals")
  #par(mfrow=c(3,1))
  #par(op)
  acf2(floors)
  #acf(resid(fit),main="ACF of Residuals from Regression")
  #jpeg(file = "floors/floors fiited with ma 1.jpeg")
  sarima(floors,0,0,4)
  par(mfrow=c(1,1))
  sarima.for(floors,14,1,0,7)
 
}
analyzeFloors()

analyzeActivityCalories<-function(){
  
  #par(mfrow=c(1,1))
  #par(op)
  par(oma=c(3,3,0,0),mar=c(3,3,2,2),mfrow=c(2,1))
  ActivityCalories=ActivityCalories-mean(ActivityCalories)
  ts.plot(ActivityCalories,main="ActivityCalories")
  fit=lm(steps~time(steps),na.action=NULL)
  summary(fit)
  plot(resid(fit),main="dtrended residuals")
  #par(mfrow=c(3,1))
  #par(op)
  acf2(ActivityCalories,30)
  acf2(diff(ActivityCalories,1),30)
  #acf(resid(fit),main="ACF of Residuals from Regression")
  #jpeg(file = "ActivityCalories/ActivityCalories fiited with ma 1.jpeg")
  sarima(ActivityCalories,5,0,0)
  par(mfrow=c(1,1))
  sarima.for(ActivityCalories,14,5,0,0)
  
  
}
analyzeActivityCalories()


crosscorrelation<-function()
{
    (ccf1=ccf(steps,floors,30))
    par(oma=c(3,3,0,0),mar=c(3,3,2,2),mfrow=c(1,1))
    
    plot(ccf1,main="CCF between floors and steps")
    ccf2=ccf(floors,ActivityCalories,30)
    ccf(steps,ActivityCalories,30)
}
crosscorrelation()
