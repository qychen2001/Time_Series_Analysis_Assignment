library("TSA")
data("winnebago")
plot.ts(winnebago,type = "o")
log.winnebago = log(winnebago);
plot.ts(log.winnebago,type = "o")
pct.change = (tail(winnebago,-1)-head(winnebago,-1))/head(winnebago,-1)
diff.log = log(tail(winnebago,-1))-log(head(winnebago,-1))

plot(pct.change,type = "o",pch = 15,col = "blue",
     xlab = "Time",ylab = "Value")
points(1:63,diff.log,pch = 16,col = "red")
lines(diff.log,col="red",lty = 2)
legend(45,1.8,c("pct.change","diff.log"),col=c("blue","red"),
       text.col=c("blue","red"),pch=c(15,16),lty=c(1,2))