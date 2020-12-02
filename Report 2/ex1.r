curve(pnorm(x,0,1), xlim=c(-5,5),ylim=c(0,1), type="l", col="red", main="Cumulative distribution function", xlab='X', ylab=' ')
curve(pt(x,1), col="maroon4", add=TRUE)
curve(pt(x,3), col="olivedrab4", add=TRUE)
curve(pt(x,5), col="orange3", add=TRUE)
curve(pt(x,10), col="turquoise3", add=TRUE)
curve(pt(x,100), col="red", add=TRUE)
legend(-4.5,1,c("N(0,1)","k=1","k=3","k=5","k=10", "k=100"),
       col=c("red","red","green","orange3", "turquoise3", "blue"), lty=1, cex=1,
       title="Line types")