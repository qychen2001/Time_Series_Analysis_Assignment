# 3.5
library(TSA)
library(matlib)

linear_model = function(x, y) {
  ones = rep(1, length(x)) # 构造截距项
  x = cbind(ones, x) # 将截距项和原数据合并
  mu = solve(t(x) %*% x) %*% t(x) %*% y # 求参数
  y_hat = x %*% mu # 预测
  return(list(mu, y_hat))
}

quadratic_model = function(x, y) {
  ones = rep(1, length(x))
  x_quad = x ^ 2
  x = cbind(ones, x, x_quad)
  
  mu = qr.solve(x, y) # 使用qr分解来求解矩阵的逆
  
  y_hat = x %*% mu # 预测
  return(list(mu, y_hat))
}

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
mean(e)
var(e)

e = scale(e)


# c
win.graph(12, 6, pointsize = 10)
plot(ts(e, start = start(wages), frequency = 12),
     type = 'o',
     pch = 21)
# d
x = time(wages)
y = wages

result = quadratic_model(x, y)
y_hat = result[[2]]
y_num = as.numeric(y) # 转换为数值
e = y_num - y_hat

mean(e)
var(e)

e = scale(e)
# e
win.graph(12, 6, pointsize = 10)
plot(ts(e, start = start(wages), frequency = 12),
     type = 'o',
     pch = 21)