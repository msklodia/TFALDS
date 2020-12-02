options("scipen"=100, "digits"=4)


gamma = function(n, epsilon=0.1) {
   return((1-epsilon)*sqrt(2*log(n)))
}


L = function(n, Y, epsilon=0.1){
    return(mean(exp(Y*gamma(n,epsilon) - (gamma(n,epsilon)^2)/2)))
}


hatL = function(n, Y, epsilon=0.1){
    return(mean(exp(gamma(n,epsilon)*Y*ifelse(Y<sqrt(2*log(n)),1,0) - (gamma(n,epsilon)^2)/2)))
}


general = function(n, epsilon=0.1, k=1000) {
    L = c()
    hatL = c()
    p = 0
    for(i in 1:k){
        Y = rnorm(n,0,1)

        L[i]=L(n, Y, epsilon) 

        hatL[i]=hatL(n, Y, epsilon) 

        if (L[i]==hatL[i]) {
           p=p+1
        } 
    }
    return(c(L, hatL, p/k))
}

g = general(n=1000)
L1000 = g[1:1000]
var(L1000)
hatL1000 = g[1001:2000]
var(hatL1000)

g = general(n=10000)
L10000 = g[1:1000]
var(L10000)
hatL10000 =g[1001:2000]
var(hatL10000)

g = general(n=100000)
L100000 = g[1:1000]
var(L100000)
hatL100000 =g[1001:2000]
var(hatL100000)

par(mfrow=c(1,3))
hist(L1000, main="Histogram of L for n=1000",xlim=c(0,3), breaks = 200, xlab="L", col= "cadetblue" )
hist(L10000, main="Histogram of L for n=10000",xlim=c(0,3), breaks = 200, xlab="L", col = "cadetblue4")
hist(L100000, main="Histogram of L for n=100000",xlim=c(0,3), breaks = 200, xlab="L", col = "darkslategray4")

par(mfrow=c(1,3))
hist(hatL1000, main="Histogram approximation of L for n=1000",xlim=c(0,3), breaks = 25, xlab="L", col= "cadetblue" )
hist(hatL10000, main="Histogram approximation of L for n=10000",xlim=c(0,3), breaks = 25, xlab="L", col = "cadetblue4")
hist(hatL100000, main="Histogram approximation of L for n=100000",xlim=c(0,3), breaks = 20, xlab="L", col = "darkslategray4")

###########
general(n=1000, k=10000)[20001]
general(n=10000, k=10000)[20001]
general(n=100000,k=10000)[20001]

