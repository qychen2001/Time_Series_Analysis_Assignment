library(TSA)
data("airpass")
plot(airpass, type = "o")

log_SP = log(airpass)
plot(log_SP, type = "o")

change_rate=(tail(airpass,-1)-head(airpass,-1))/head(airpass,-1)
diff_log=log(tail(airpass,-1))-log(head(airpass,-1))
cor(change_rate,diff_log)
# 输出：[1] 0.9963424
plot(change_rate,type = "o",pch = 15,xlab = "Time",ylab = "Value")
plot(diff_log,type = "o",pch = 15,xlab = "Time",ylab = "Value")

# 画在一起
plot(change_rate,type = "o",pch = 15,col = "blue",
     xlab = "Time",ylab = "Value")

points(1:143, diff_log, pch = 16, col = "red")
lines(diff_log, col = "red", lty = 3)
legend (100,0.3,c("change_rate", "diff_log"),col = c("blue", "red"),
        text.col = c("blue", "red"),lty = c(1, 2))
