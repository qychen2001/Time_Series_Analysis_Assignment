library("TSA")
data("SP")
plot.ts(SP,type = "o")
log.SP = log(SP);
plot.ts(log.SP,type = "o")
pct.change = (tail(SP,-1)-head(SP,-1))/head(SP,-1)
diff.log = log(tail(SP,-1))-log(head(SP,-1))

plot(pct.change,type = "o",pch = 15,col = "blue",
     xlab = "Time",ylab = "Value")
points(1:167,diff.log,pch = 16,col = "red")
lines(diff.log,col="red",lty = 2)
legend(100,0.38,c("pct.change","diff.log"),col=c("blue","red"),
       text.col=c("blue","red"),pch=c(15,16),lty=c(1,2))