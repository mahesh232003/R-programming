#Binomial fit and goodness of fit
x<-c(0,1,2,3,4,5,6)
f<-c(7,64,140,210,132,75,12)
fx<-f*x
sumfx<-sum(f*x)
print(sumfx)
sumf<-sum(f)
print(sumf)
s<-length(x)-1
p=sumfx/sumf/s
print(p)
pr<-dbinom(0:6,size=s,prob=p)
pr<-round(pr,digits=5)
fee<-(pr*sumf)
fe<-round(fee,digits=0)
mydata<- data.frame(x,f,fx,pr,fee,fe)
print(mydata)
sums<-list(NA,sum(f),sum(f*x),NA,NA,sum(fe))
mydata<-rbind(mydata,sums)
print(mydata,row.names=FALSE)
result<-chisq.test(f,p=pr,rescale.p=TRUE)
print(result)
tablevalue<-qchisq(.95, df=s)
print(tablevalue)

#Poisson fit and goodness of fit
x<-c(0,1,2,3,4)
f<-c(211,90,19,5,0)
fx<-f*x
sumfx<-sum(f*x)
print(sumfx)
sumf<-sum(f)
print(sumf)
s<-length(x)-1
p=sumfx/sumf
print(p)
pr<-dpois(0:4,lambda=p)
pr<-round(pr,digits=5)
fee<-(pr*sumf)
fe<-round(fee,digits=0)
mydata<- data.frame(x,f,fx,pr,fee,fe)
print(mydata)
sums<-list(NA,sum(f),sum(f*x),NA,NA,sum(fe))
mydata<-rbind(mydata,sums)
print(mydata,row.names=FALSE)
result<-chisq.test(f,p=pr,rescale.p=TRUE)
print(result)
tablevalue<-qchisq(.95, df=s)
print(tablevalue)

# Goodness of fit for ND  
x<-c(1,3,5,7,9)
y<-c(3,5,7,9,11)
f<-c(1,4,6,4,1)
xi<-(x+y)/2
fx<-f*xi
fxx<-f*xi*xi
sumfx<-sum(f*xi)
print(sumfx)
sumfxx<-sum(f*xi*xi)
sumf<-sum(f)
print(sumf)
m<-sumfx/sumf
print(m)
sd<-(sumfxx/sumf)-(m*m)
sd<-sqrt(sd)
print(sd)
u<-pnorm(y,m,sd)
l<-pnorm(x,m,sd)
pr<-u-l
u<-round(u,digits=5)
l<-round(l,digits=5)
pr<-round(pr,digits=5)
fee<-(pr*sumf)
fe<-round(fee,digits=0)
mydata<- data.frame(x,y,xi,f,fx,fxx,u,l,pr,fee,fe)
print(mydata)
sums<-list(NA,NA,NA,sum(f),sum(f*xi),sum(f*xi*xi),NA,NA,NA,NA,sum(fe))
mydata<-rbind(mydata,sums)
print(mydata,row.names=FALSE)
result<-chisq.test(f,p=pr,rescale.p=TRUE)
print(result)



# Goodness of fit contingency table
m<-as.table(rbind(c(190,243,197),c(82,44,44),c(23,78,34),c(5,12,8)))
dimnames(m)=list(Empcategory=c("Labour","Clerks","Technicians","Executives"), BonusSchemes=c("Type1","Type2","Type3"))
print(m)
csum<-colSums (m)
rsum<-rowSums (m)
mytable<-(rbind(m,csum))
print(mytable)
mytable<-(cbind(m,rsum))
print(mytable)
test<-chisq.test(m)
print(test)
print(test$expected,3)


#Frequency polygon
v <- c(15,	35,	20,	10,	5	,15,	20,	15,	12,	13)
plot(v,type="o",xlab="Year",ylab="Profit",xlim=c(1,10),ylim=c(0,40), 
     col="green", main="RVRJC PHARMACEUTICAL FIRM")

#install.packages("tidyverse")
#library("tidyverse")
#v=c(12,45,3,23,12,56)
#qplot(x=v,geom="histogram")
#qplot(x=v,geom="freqpoly")
