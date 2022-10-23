library("TSA")
data("airpass")
plot.ts(airpass,type = "o")
log.airpass = log(airpass);
plot.ts(log.airpass,type = "o")
pct.change = (tail(airpass,-1)-head(airpass,-1))/head(airpass,-1)
diff.log = log(tail(airpass,-1))-log(head(airpass,-1))

plot(pct.change,type = "o",pch = 15,col = "blue",
     xlab = "Time",ylab = "Value")
points(1:143,diff.log,pch = 16,col = "red")
lines(diff.log,col="red",lty = 2)
legend(110,0.26,c("pct.change","diff.log"),col=c("blue","red"),
       text.col=c("blue","red"),pch=c(15,16),lty=c(1,2))