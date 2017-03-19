####################################          №1
for (i in 1:4)
{
 x<-c(mean(iris[,i]))
 list[i]<-list(x)
}
list
#####################################         №2
y<-c()
for (i in 1:nrow(iris))
  { x<-c()
    for (j in 1:4)
    {
      x<-c(x,iris[,-5][i,j])
    }
    y<-c(y,mean(x)) 
  }
y
#########################################     №3
DNA<-sample(c("T","G","C","A"),1000,replace=TRUE)
y<-factor(DNA)
n=0
n<-summary(y)[1] + summary(y)[4]
dna_at<-n/sum(summary(y))
n
dna_at


for (j in 1:1000)
{  
  if (DNA[j]=="T" | DNA[j]=="A"  )
   { 
     n=n+1
  
  }
}
dna_at<-n/length(DNA)
n
dna_at
########################################      №4
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
########################################      №6
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
########################################      №7
a<-iris[iris$Species=="setosa",c("Sepal.Length","Petal.Length","Species")]
b<-iris[iris$Species=="versicolor",c("Sepal.Length","Petal.Length","Species")]
c<-iris[iris$Species=="virginica",c("Sepal.Length","Petal.Length","Species")]
iris[,1]
plot(a[,2],a[,1],xlab="Petal.Length",ylab="Sepal.Length",type="p")
plot(b[,2],b[,1],xlab="Petal.Length",ylab="Sepal.Length",type="p")
plot(c[,2],c[,1],xlab="Petal.Length",ylab="Sepal.Length",type="p")
########################################      №8
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
for (i in 1:length(levels(diamonds$clarity)))
{  
  sred[i]=s[[i]]/n[i]
}
sred
########################################      №5
x<-c()
for (i in 1:length(levels(iris$Species)))
{
  x<-c(x,mean(iris[iris$Species==levels(iris$Species)[i],c("Petal.Length")]))
}
a = data.frame(setosa = x[1], versicolor= x[2], virginica = x[3])
a
sort(a)
names(sort(a))
a<-factor(names(sort(a)))
a


