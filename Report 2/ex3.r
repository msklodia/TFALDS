#A
n = 100
lambda = 5
X = rpois(n, lambda)
ppois(sum(X), n*lambda, lower.tail=FALSE)

#B
lambda = 5
i = 100
pvalues = c()
for(j in 1:1000){
    X = rpois(i, lambda)
    pvalues[j] = ppois(sum(X), i*lambda, lower.tail=FALSE)
}

hist(pvalues, main="Histogram of pvalues", xlab="p-value", col="cadetblue")


#TEST BONFERRONIEGO 
BonferroniTest = function(n, lambda0, alpha=0.05, k=1000) {
    p = 0
    for(j in 1:k){
        pvalues = c()

        for(i in 1:1000){
            X = rpois(n, lambda0)
            pvalues[i] = ppois(sum(X), n*lambda0), lower.tail=FALSE)
        }

        if(min(pvalues) <= alpha/1000){
            p = p+1
        }
    }
    return(p/k)
}
BonferroniTest(n=100, lambda0=5)


#TEST FISHERA
FisherTest = function(n, lambda0, alpha = 0.05, k=1000) {
    p = 0
    for(j in 1:k){
        pvalues = c()

        for(i in 1:1000){
            X = rpois(n, lambda0)
            pvalues[i] = ppois(sum(X), n*lambda0, lower.tail=FALSE)
        }

        T = -sum(2*log(pvalues))

        if (T > qchisq(1-alpha, 2*length(pvalues))) {
            p = p+1
        }
    }
    return(p/k)

}

FisherTest(n=100, lambda0=5)




#C
#Moc testu Bonferroniego - coś zle
Bonferroni_power = function(n, m, lambda0, lambdaA, alpha=0.05, k=1000) {
  p = 0
  for(j in 1:k){
    pvalues = c()
    
    for(j in 1:m){
      X = rpois(m, lambdaA)
      pvalues[j] =  ppois(sum(X), m*lambda0, lower.tail=FALSE)
    }
    
    for(i in (m+1):(n+m)){
      Y =  rpois(n, lambda0)
      pvalues[i] = ppois(sum(Y), n*lambda0, lower.tail=FALSE)
    }
    
    if(min(pvalues)<alpha/(n+m)){
      p = p+1
    }
  }
  return(p/k)
}

Bonferroni_power(n=999, m=1, lambda0=5, lambdaA=7)
Bonferroni_power(n=900, m=100, lambda0=5, lambdaA=5.5)


#Moc testu Fishera 
Fisher_power = function(n, m, lambda0, lambdaA, k=1000, alpha = 0.05) {
  p = 0
  for(z in 1:k){
    pvalues = c()
    
    for( j in 1:m){
      X = rpois(n, lambdaA)
      pvalues[j] =  ppois(sum(X), n*lambda0, lower.tail=FALSE)
    }
    
    for(i in (m+1):(n+m)){
      Y =  rpois(n, lambda0)
      pvalues[i] = ppois(sum(Y), lambda0*n, lower.tail=FALSE)
    }
    
    T = -sum(2*log(pvalues))
    if(T > qchisq(1-alpha, 2*(n+m))){
      p = p+1
    }
  }
  return(p/k)
}

Fisher_power(n=999, m=1, lambda0=5, lambdaA=7)
Fisher_power(n=900, m=100, lambda0=5, lambdaA=5.5)
