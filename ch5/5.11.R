library(TSA)
data("winnebago")
plot(winnebago, type = "o")

log_winnebago = log(winnebago)
plot(log_winnebago, type = "o")
change_rate = (tail(winnebago, -1) - head(winnebago, -1)) / head(winnebago, -1)
diff_log = log(tail (winnebago, -1)) - log(head(winnebago, -1))

plot(
  pct.change,
  type = "o",
  pch = 15,
  col = "blue",
  xlab = "Time",
  ylab = "Value"
)
points(1:63, diff_log, pch = 16, col = "red")
lines(diff_log, col = "red", lty = 3)
legend (
  45,
  1.8,
  c("change_rate", "diff_log"),
  col = c("blue", "red"),
  text.col = c("blue", "red"),
  pch = c(15, 16),
  lty = c(1, 2)
)
