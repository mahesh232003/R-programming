#t-test
data <- scan()
print(data)
print("Enter mean")
me <- scan()
print("Enter level of significance")
alpha <- scan()
t <- t.test(data,mu=me)
print(t)
n=length(data)
w <- qt(1-alpha/2,df=n-1)
print(w)

#t test calculation
data <- scan()
print(data)
n <- length(data)
print("Enter mean")
me <- scan()
n
print("Enter level of significance")
alpha <- scan()
m=mean(data)
m
std <- sd(data)
std
tcal <- (m-me)/(std/sqrt(n))
tcal
t <- t.test(data,mu=me)
print(t)
we <- qt(1-alpha/2,df=n-1)
we
we1 <- qt(1-aplha,df=n-1)
we1

if (tcal<we){
  print("We accept null hypothesis")
}else{
  print("We reject null hypothesis")
}

#t test calucation for one tailed

print("Enter n value")
n <- scan()
print("Enter mean")
me <- scan()
print("Enter sd")
sd <- scan()
print("Enter mu to check")
mu <- scan()
print("Enter level of sigificance")
alpha <- scan()
tcal <- (me-mu)/(sd/sqrt(n))
tcal
if (tcal<0){
  q <- qt(alpha,df=n-1)
}else{
  q <- qt(1-alpha,df=n-1)
}
q
if (tcal<0){
  if (q>tcal){
    print("Reject null hypothesis")
  }else{
    print("Accept null hypothesis")
  }
}else{
  if (q>tcal){
    print("Accept null hypothesis")
  }else{
    print("Reject null hypothesis")
  }
}
