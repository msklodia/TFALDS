degree = c(1, 3, 5, 10, 100)
t = seq(from=-4, to=4, by=0.0001)
color = c("red","green","orange3", "turquoise3", "blue")
plot(t, pnorm(t,0,1), type="l", xlab="x",  main="Cumulative distribution function")
for(df in degree){
       i = which(degree==df)
       chi = t*sqrt(2*df) + df
       y=pchisq(chi,df) 
       lines(t, y, col=color[i],type="l")
}
legend(-4,1,c("N(0,1)","k=1","k=3","k=5","k=10", "k=100"),
       col=c("black","red","green","orange3", "turquoise3", "blue"),lty=1, cex=1,
       title="Line types")