#1
x<-c(2,4,6,8,10,12)
y<-c(1,2,3,4,5,6)
spirmen = function(x,y) 
{x<-rank(x)
y<-rank(y)
s=0  
for (i in 1:length(x))
{s=s+(x[i]-y[i])^2

}
n<-length(x)
p=1-(6*s)/(n*(n-1)*(n+1))
return(p)
}
spirmen(x,y)
##########################################    №2
setwd ("D:/Group_126/Balieva/Julia")
dt=read.csv("eddypro.csv", header = TRUE, sep ="," ) 
dt[dt == -9999] = NA
dt<-dt[,c(-1,-2,-3,-30,-35,-70,-88:-99)]
dt<-dt[dt$DOY>74 & dt$DOY<166,c(1:ncol(dt))]
k<-c()
for (i in 1: ncol(dt))
{
  k<-c(k,spirmen(dt[,i],dt[,14]))  
}
k
n<-c(14)
for (i in 1:length(k))
{
  if ((k[i]>0.8 & k[i]<1) | (k[i]>(-1) & k[i]<(-0.8)))
  {
    n<-c(n,i)
  }
    
}
n
dt_1<-dt[c(n)]
l<-length(names(dt_1))
names(dt_1)
  model = lm(co2_flux~T. + un_H + un_co2_flux + w.ts_cov+ w.co2_cov, data =dt); 
  model
 plot(model)




