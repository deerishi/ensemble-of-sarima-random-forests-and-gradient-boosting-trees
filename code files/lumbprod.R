datal<-read.csv("/home/drishi/coursework/STAT 929/greg's data/lumpprod and soi/lumbprod.csv")
#datal=as.ts(datal)
datal
Lumber.Production=datal$VALUE
Lumber.Production=Lumber.Production-mean(Lumber.Production)
ts.plot(Lumber.Production,main="Lumbprod")
acf2(Lumber.Production)
data=Lumber.Production
data1=diff(data,1)
acf2(data1,48)
seasonalityRemoved=diff(data1,12)
acf2((seasonalityRemoved),50)


data1=diff(seasonalityRemoved,1)
acf2(data1,48)
seasonalityRemoved=diff(data1,12)
acf2((seasonalityRemoved),50)

sarima(data,1,2,1,0,2,1,12) 

sarima.for(data,12,1,2,1,0,1,2,12)

k=kernel("daniell",3)
spec.pgram(data,k)
ts.plot(data,main='LumberProd')



datal<-read.csv("/home/drishi/coursework/STAT 929/greg's data/lumpprod and soi/lumbprod.csv")
#datal=as.ts(datal)
datal
Lumber.Production=datal$VALUE
Lumber.Production=Lumber.Production-mean(Lumber.Production)
ts.plot(Lumber.Production,main="Lumbprod")
data=Lumber.Production
x=seq(1,length(data),by=1)
x

lin<-lm(data~x)
seg<-segmented(lin,seg.Z=~x,psi=list(x =c(10,20,50,75,100,125,150,200)))
ts.plot(data,main='LumberProd with Piecewise Linear curve fitted')
plot(seg,add=T)
resid=seg$residuals

ts.plot(resid,main='Residuals from segmented fit') 
k=kernel("daniell",3)
(spec.lum=spec.pgram(resid,k,taper=0 ,log="no"))
abline(v=0.078703704,lty="dotted")
abline(v=0.0219907407,lty="dotted")
(nextn(length(data)))
(df=2*7*length(data)/nextn(length(data)))
(df = spec.lum$df)
U = qchisq(.025, df)
L = qchisq(.975, df)

(maxValue=spec.lum$spec[17])
(lowerInterval=df*maxValue/L)
(upperInterval=df*maxValue/U)


