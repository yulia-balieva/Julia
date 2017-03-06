##1
x[1]=mean(iris[,c("Sepal.Length")])
x[2]=summary(iris[,c("Sepal.Width")])[4]
x[3]=summary(iris[,c("Petal.Length")])[4]
x[4]=summary(iris[,c("Petal.Width")])[4]
list<-list(x[1],x[2],x[3],x[4])
x[1]
##2
iris[,-5]
for (i in 1:150)
  { 
    y[i]<-summary(iris[,-5][i,])[4]
    
  }
y
##3
DNA<-sample(c("T","G","C","A"),1000,replace=TRUE)
n=0
m=0
for (j in 1:1000)
{  
  if (DNA[j]=="T" )
   { 
     n=n+1
  
  }
  if (DNA[j]=="A" )
  { 
    m=m+1
    
  }
  s<-n+m
}

length(DNA)
dna_at<-s/length(DNA)
##4
set<-sample(letters,1000,replace=TRUE)
n=0
for (i in 1:length(set))
{
  if (set[i]=="a" | set[i]=="e" | set[i]=="i" | set[i]=="o" | set[i]=="u" | set[i]=="y")
  {
    n=n+1
  }
}
n
##6
x<-sample(c(1,2,3,4,5,6,7,8,9),10,replace=TRUE)
x<-c(1,2,3,4,5,6,7,8,9,10)
mediana=function(k)
{
  k<-sort(k)
  if (length(k)/2-round(length(k)/2, 0)==0)
  {
    m<-(k[length(k)/2]+k[length(k)/2+1])/2
  }
  else
  {
    m<-k[round(length(k)/2, 0)+1]
  }
  return(m)
}
sort(x)
mediana(x)
##7
a<-iris[iris$Species=="setosa",c("Sepal.Length","Petal.Length","Species")]
b<-iris[iris$Species=="versicolor",c("Sepal.Length","Petal.Length","Species")]
c<-iris[iris$Species=="virginica",c("Sepal.Length","Petal.Length","Species")]
iris[,1]
plot(a[,2],a[,1],xlab="Petal.Length",ylab="Sepal.Length",type="p")
plot(b[,2],b[,1],xlab="Petal.Length",ylab="Sepal.Length",type="p")
plot(c[,2],c[,1],xlab="Petal.Length",ylab="Sepal.Length",type="p")
##8
library(ggplot2)
n<-c(0,0,0,0,0,0,0,0)
s<-c(0,0,0,0,0,0,0,0)
sred<-c(0,0,0,0,0,0,0,0)
for (j in 1:length(levels(diamonds$clarity)))
{ 
  for (i in 1:nrow(diamonds[diamonds$clarity==levels(diamonds$clarity)[j],c("carat","clarity","price")]))
  {
    if (diamonds[diamonds$clarity==levels(diamonds$clarity)[j],c("carat","clarity","price")][i,3]>1000)
    { n[j]=n[j]+1
    s[j]=s[j]+diamonds[diamonds$clarity==levels(diamonds$clarity)[j],c("carat","clarity","price")][i,3]/diamonds[diamonds$clarity==levels(diamonds$clarity)[j],c("carat","clarity","price")][i,1]
    }
  }
}
n
s
for (i in 1:length(levels(diamonds$clarity)))
{  
  sred[i]=s[[i]]/n[i]
}
sred
length(levels(diamonds$clarity))
##5
x<-c(0,0,0)
for (i in 1:length(levels(iris$Species)))
{
  x[i]=mean(iris[iris$Species==levels(iris$Species)[i],c("Petal.Length")])
}
x
x <- factor(x)
x
class(x)
levels(x) <- c("setosa", "versicolor","virginica")
x

