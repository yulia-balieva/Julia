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
y[1]
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

