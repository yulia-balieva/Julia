#1
x<-c(1,7,5,2,9,2)
y<-c(1,3,5,7,9,2)
x1<-c(1,4,6)
y1<-c(11,8,9)
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
###################################################
#2