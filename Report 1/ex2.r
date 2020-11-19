set.seed(123456789)
#c
X = rexp(n=20,rate=5)
pvalue_for_20 = pgamma(sum(X), shape=20, rate=5, lower.tail=FALSE)

X = rexp(n=200,rate=5)
pvalue_for_200 = pgamma(sum(X), shape=200, rate=5, lower.tail=FALSE)

#ei
pv_generator = function(times, n, lambda1, lambda2) {
  pvalues = c()
  for (i in c(1:times)){
    X = rexp(n,lambda1)
    pvalues[i] = pgamma(sum(X), n, rate=lambda2, lower.tail=FALSE)
  }
  return(pvalues)
}

pvalues_for_20 = pv_generator(times=1000, n=20, lambda1=5, lambda2=5)
pvalues_for_200 = pv_generator(times=1000, n=200, lambda1=5, lambda2=5)

par(mfrow=c(1,2))
hist(pvalues_for_20, main="Histogram of pvalues for n=20",
     xlab="p-value", col="cadetblue", breaks=10)
hist(pvalues_for_200, main="Histogram of pvalues for n=200",
     xlab="p-value", col="cadetblue", breaks=10)

par(mfrow=c(1,2))
qqplot(qunif(seq(from=0, to=1 ,by=1/1000)), pvalues_for_20, main = "Q-Q plot of p-values for n=20", 
       xlab='quantiles with uniform distribution', ylab='quantiles with p-value', col="cadetblue" )
qqplot(qunif(seq(from=0, to=1, by=1/1000 )) , pvalues_for_200, main = "Q-Q plot of p-values for n=200", 
       xlab='quantiles with uniform distribution', ylab='quantiles with p-value', col="cadetblue")


przedział ufności dla odsetka 
#eii - 
confidence_interval = function(dane, alpha) {
  m = sum(dane<0.05)
  n = length(dane)
  d = m/n
  return(c(d,qnorm(1-alpha/2)*sqrt(d*(1-d)/n))) 
  }

confidence_interval(pvalues_for_20, 0.05)
confidence_interval(pvalues_for_200, 0.05)



#fi - dlaczego lambda1=3? czemu bierzemy dwie rożne lambdy? 
pvalues_for_20 = pv_generator(times=1000, n=20, lambda1=3, lambda2=5)
pvalues_for_200 = pv_generator(times=1000, n=200, lambda1=3, lambda2=5)

par(mfrow=c(2,2))
hist(pvalues_for_20, main="Histogram of pvalues for n=20",
     xlab="p-value", col="cadetblue", breaks=10)
hist(pvalues_for_200, main="Histogram of pvalues for n=200",
     xlab="p-value", col="cadetblue", breaks=10)

#par(mfrow=c(1,2))
qqplot(qunif(seq(from=0, to=1 ,by=1/1000)), pvalues_for_20, main = "Q-Q plot of p-values for n=20", 
       xlab='quantiles with uniform distribution', ylab='quantiles with p-value', col="cadetblue" )
qqplot(qunif(seq(from=0, to=1, by=1/1000 )) , pvalues_for_200, main = "Q-Q plot of p-values for n=200", 
       xlab='quantiles with uniform distribution', ylab='quantiles with p-value', col="cadetblue")

#fii
power = function(alpha, n, lambda1, lambda2) {
     return(pgamma(qgamma(1-alpha,n, rate=lambda1), n, rate=lambda2, lower.tail=FALSE))
}

confidence_interval(pvalues_for_20, 0.05)
confidence_interval(pvalues_for_200, 0.05)

power(0.05, 20, 5, 3)
power(0.05, 200, 5, 3)