# 3.5
# a
data("wages")
win.graph(width = 8,
          height = 8,
          pointsize = 10)
plot(
  x = zlag(wages),
  y = wages,
  xlab = "Previous Year Wages",
  ylab = "Wages"
)
win.graph(width = 12,
          height = 6,
          pointsize = 10)
plot(wages, type = 'o', pch = 21)
# b
x = time(wages)
y = wages

result = linear_model(x, y)
y_hat = result[[2]]
y_num = as.numeric(y) # 转换为数值
e = y_num - y_hat
# c
win.graph(12, 6, pointsize = 10)
plot(ts(e, start = start(wages), frequency = 12),
     type = 'o',
     pch = 21)
# d
lm2 = lm(wages ~ time(wages) + I(time(wages) ^ 2), singular.ok = FALSE)
x = time(wages)
y = wages
result = quadratic_model(x, y)
y_hat = result[[2]]
y_num = as.numeric(y) # 转换为数值
e = y_num - y_hat
# e
win.graph(12, 6, pointsize = 10)
plot(ts(e, start = start(wages), frequency = 12),
     type = 'o',
     pch = 21)