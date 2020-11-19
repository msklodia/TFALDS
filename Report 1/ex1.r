set.seed(123456789)

mle = function(x) {
  n = length(x)
  return(-n/sum(log(x)) - 1)}

me = function(x) {
  mean = mean(x)
  return(-2-1/(mean -1))}
   
#e 
alpha = 5
beta = 1
n = 20
X = rbeta(n,alpha+1,beta)

mle(X)-alpha
m(X)-alpha
(mle(X)-alpha)^2 
(me(X)-alpha)^2 

n = 200
X = rbeta(n,alpha+1,beta)
   
mle(X)-alpha
m(X)-alpha
(mle(X)-alpha)^2 
(me(X)-alpha)^2 

#f
mle = function(times, n, alpha, beta) {
  a_mle = c()
  for (i in 1:times){
    X = rbeta(n,alpha+1,beta)
    a_mle[i]=-length(X)/sum(log(X))-1
  }
  return(a_mle)
}

me = function(times, n, alpha, beta) {
  a_me = c()
  for (i in 1:times){
    X = rbeta(n,alpha+1,beta)
    mean = mean(X)
    a_me[i]=-2-1/(mean -1)
  }
  return(a_me)
}

mle_for_20 = mle(1000, 20, 5, 1)
me_for_20 = me(1000, 20, 5, 1)

mle_for_200 = mle(1000, 200, 5, 1)
me_for_200 = me(1000, 200, 5, 1)

par(mfrow=c(2,2))
hist(mle_for_20, main="Histogram of maximum likelihood estimators for n=20",
     xlab="mle", col="cadetblue", breaks=10)
hist(me_for_20, main="Histogram of moment estimators for n=20",
     xlab="me", col="cadetblue", breaks=10)
hist(mle_for_200, main="Histogram of maximum likelihood estimators for n=200",
     xlab="mle", col="cadetblue", breaks=10)
hist(me_for_200, main="Histogram of moment estimators for n=200",
     xlab="me", col="cadetblue", breaks=14)

par(mfrow=c(2,2))
boxplot(mle_for_20,main = "Box-plot of maximum likelihood estimators for n=20",
        xlab="mle", col="cadetblue")
boxplot(me_for_20,main = "Box-plot of moment estimators for n=20",
        xlab="me", col="cadetblue")
boxplot(mle_for_200,main = "Box-plot of maximum likelihood estimators for n=200",
        xlab="mle", col="cadetblue")
boxplot(me_for_200,main = "Box-plot of moment estimators for n=200",
        xlab="me", col="cadetblue")


par(mfrow=c(2,2))
qqnorm(mle_for_20, main = "Q-Q plot of maximum likelihood estimators for n=20", col="cadetblue")
qqnorm(me_for_20, main = "Q-Q plot of moment estimators for n=20", col="cadetblue")
qqnorm(mle_for_200, main = "Q-Q plot of maximum likelihood estimators for n=200", col="cadetblue")
qqnorm(me_for_200, main = "Q-Q plot of moment estimators for n=200", col="cadetblue")

#bias 
mean(mle_for_20-alpha)
mean(mle_for_200-alpha)
mean(me_for_20-alpha)
mean(me_for_200-alpha)

qnorm(0.975)*sqrt(var(mle_for_20-alpha)/1000)
qnorm(0.975)*sqrt(var(mle_for_200-alpha)/1000)
qnorm(0.975)*sqrt(var(me_for_20-alpha)/1000)
qnorm(0.975)*sqrt(var(me_for_200-alpha)/1000)

#variance
mean((mle_for_20-mean(mle_for_20))^2)
mean((mle_for_200-mean(mle_for_200))^2)
mean((me_for_20-mean(me_for_20))^2)
mean((me_for_200-mean(me_for_200))^2)

qnorm(0.975)*sqrt(var((mle_for_20-mean(mle_for_20))^2)/1000)
qnorm(0.975)*sqrt(var((mle_for_200-mean(mle_for_200))^2)/1000)
qnorm(0.975)*sqrt(var((me_for_20-mean(me_for_20))^2)/1000)
qnorm(0.975)*sqrt(var((me_for_200-mean(me_for_200))^2)/1000)

#MSE
mean((mle_for_20-alpha)^2) 
mean((mle_for_200-alpha)^2) 
mean((me_for_20-alpha)^2) 
mean((me_for_200-alpha)^2) 

qnorm(0.975)*sqrt(var((mle_for_20-alpha)^2)/1000)
qnorm(0.975)*sqrt(var((mle_for_200-alpha)^2)/1000)
qnorm(0.975)*sqrt(var((me_for_20-alpha)^2)/1000)
qnorm(0.975)*sqrt(var((me_for_200-alpha)^2)/1000)