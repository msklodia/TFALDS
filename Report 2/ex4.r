options("scipen"=100, "digits"=4)

n = 100000 
X <- rnorm(n, mean = 0, sd = 1)
plot(X, pch = 20, col = "olivedrab4", ylab = "Observation value", xlab = "n", main = "Observations of N(0,1)")

f = function(n) {
    X <- rnorm(n, mean = 0, sd = 1)
    R = c()
    for(i in 2:n){
        R[i] = max(X[1:i])/sqrt(2*log(i))}
    return(R)}

R1 = f(n)
R2 = f(n)
R3 = f(n)
R4 = f(n)
R5 = f(n)
R6 = f(n)
R7 = f(n)
R8 = f(n)
R9 = f(n)
R10 = f(n)

plot(x=c(1:n), y=R1, col = "olivedrab4", ylim=c(0,1.5), type='l', main='Function R', xlab="i", ylab="R")
lines(x=c(1:n),y=R2, col="blue")
lines(x=c(1:n),y=R3, col="maroon4")
lines(x=c(1:n),y=R4, col="orange3")
lines(x=c(1:n),y=R5, col="red")
lines(x=c(1:n),y=R6, col="rosybrown3")
lines(x=c(1:n),y=R7, col="turquoise1")
lines(x=c(1:n),y=R8, col="snow4")
lines(x=c(1:n),y=R9, col="mistyrose4")
lines(x=c(1:n),y=R10, col="mediumorchid4")


