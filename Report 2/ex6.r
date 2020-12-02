options("scipen"=1000, "digits"=4)


gamma = function(n, epsilon) {
   return((1-epsilon)*sqrt(2*log(n)))
}

#gamma(500, -0.05)



general = function(epsilon, n, k=1000) {
    L = c()
    for(i in 1:k){
        Y  = rnorm(n,0,1)
        L[i]=(1/n*sum(exp(gamma(n, epsilon)*Y-gamma(n, epsilon)^2/2)))
    }
    return(L)
}

#general(-0.05, 500)



critical_value_NeymanPearson = function(n, epsilon, k=10000) {
    L = general(epsilon, n, k)
    critical_value = quantile(L, probs = seq(0.95, 0.95, 0))
    return(critical_value)
}





#moc testu Neymana
NeymanPower = function(n, epsilon, k=10000) {
    cv = critical_value_NeymanPearson(n, epsilon)
    L = c()
    for(i in 1:k){
        Y = c(rnorm(1, gamma(n,epsilon), 1), rnorm(n-1, 0, 1))
        L[i]=(1/n*sum(exp(gamma(n, epsilon)*Y-gamma(n, epsilon)^2/2)))
    }
    sum(L>cv)/k
}    





#moc testu Bonferroniego
BonferroniPower = function(n, epsilon, k=10000) {
    p = 0
    for(i in 1:k){
        Y = c(rnorm(1, gamma(n, epsilon) ,1), rnorm(n-1, 0, 1))
        if (max(abs(Y))>qnorm(1-0.05/(2*n)) ){
           p = p+1
        }
    }
    return(p/k)
}







# A
cv_500 = critical_value_NeymanPearson(n=500, epsilon=-0.05)
cv_5000 = critical_value_NeymanPearson(n=5000, epsilon=-0.05)
cv_50000 = critical_value_NeymanPearson(n=50000, epsilon=-0.05)


NeymanPower(n=500, epsilon=-0.05)    
BonferroniPower(500, -0.05)

NeymanPower(n=5000, epsilon=-0.05) 
BonferroniPower(5000, -0.05)

NeymanPower(n=50000, epsilon=-0.05) 
BonferroniPower(50000, -0.05)






#B 
cv_500 = critical_value_NeymanPearson(500, -0.2)
cv_5000 = critical_value_NeymanPearson(5000, -0.2)
cv_50000 = critical_value_NeymanPearson(50000, -0.2)


NeymanPower(n=500, epsilon=-0.2)    
BonferroniPower(500, -0.2)

NeymanPower(n=5000, epsilon=-0.2) 
BonferroniPower(5000, -0.2)

NeymanPower(n=50000, epsilon=-0.2) 
BonferroniPower(50000, -0.2)