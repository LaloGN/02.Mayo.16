# 02.Mayo.16
cremx<-read.csv("C:\\Users\\SALA-C16\\Downloads\\PIB_MEXICO.csv", header=T, dec=".")
cremxts<-ts(cremx[,2],start=1960, end=2014)
cremx1960<-window(cremxts, start=1960, end=2014)
plot(cremxts)

require (fpp)
plot(cremx1960, main="Tasa de crecimiento del pib en Mexico 1960 al 2014 ", 
     ylab="tasa de crecimiento", xlab="años")
res2 <- residuals(snaive(cremx1960))
res3 <- residuals(rwf(cremx1960, drift = T))
res4 <- residuals(meanf(cremx1960))
res5 <- residuals(naive(cremx1960))
res2 
res3
res4 
res5 

plot(res2, main="residuals de ingenuo estacional", 
     ylab="residuales", xlab="años")
Acf(res2, main="ACF de residuales")
hist(res2, main="histograma residuales")

plot(res3, main="residuals de deriva", 
     ylab="residuales", xlab="años")
Acf(res3, main="ACF de residuales")
hist(res3, main="histograma residuales")

plot(res4, main="residuals de la media", 
     ylab="residuales", xlab="años")
Acf(res4, main="ACF de residuales")
hist(res4, main="histograma residuales")

plot(res5, main="residuals del ingenuo", 
     ylab="residuales", xlab="años")
Acf(res5, main="ACF de residuales")
hist(res5, main="histograma residuales")

Box.test(res2, lag=10, fitdf=0)
Box.test(res3, lag=10, fitdf=0)
Box.test(res4, lag=10, fitdf=0)
Box.test(res5, lag=10, fitdf=0)
Box.test(res2, lag=10, fitdf=0, type="Lj")
Box.test(res3, lag=10, fitdf=0, type="Lj")
Box.test(res4, lag=10, fitdf=0, type="Lj")
Box.test(res5, lag=10, fitdf=0, type="Lj")

### Hipotesis nula: (aleatoriedad e independencia)
### P-value > 0.05 no se rechaza la hipotesis nula
