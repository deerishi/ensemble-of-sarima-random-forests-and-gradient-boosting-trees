dataSoi<-read.csv("/home/drishi/coursework/STAT 929/greg's data/lumpprod and soi/b.csv")
data=dataSoi$data
data=data-mean(data)
ts.plot(data,main="SOI")
acf2(data,48)
data2=diff(data,1)
acf2(data2,50)
data3=diff(data2,12)
acf2(data3,50)
sarima(data,1,1,1)
sarima.for(data,12,1,1,1)
ts.plot(data,main="SOI")

x=seq(1,length(data),by=1)
x
lin<-lm(data~x)
seg<-segmented(lin,seg.Z=~x,psi=list(x =c(10,50,100,200,350,500,750,900,1100,1300)))
ts.plot(data,main='SOI with Piecewise Linear curve fitted')
plot(seg,add=T)
resid=seg$residuals

ts.plot(resid,main='Residuals from segmented fit') 
k=kernel("daniell",4)
(spec.soi=spec.pgram(resid,k,taper=0 ,log="no"))

abline(v=0.0219907407,lty="dotted")
(nextn(length(data)))
(df=2*7*length(data)/nextn(length(data)))
(df = spec.soi$df)
U = qchisq(.025, df)
L = qchisq(.975, df)

(maxValue=spec.soi$spec[38])
(lowerInterval=df*maxValue/L)
(upperInterval=df*maxValue/U)
