linear_model = function(x, y) {
  ones = rep(1, length(x)) # 构造截距项
  x = cbind(ones, x) # 将截距项和原数据合并
  mu = solve(t(x) %*% x) %*% t(x) %*% y # 求参数
  y_hat = x %*% mu # 预测
  return(list(mu, y_hat))
}

season_model_with_intercept = function(x, y) {
  x = model.matrix( ~ x, x)
  mu = solve(t(x) %*% x) %*% t(x) %*% y # 求参数
  y_hat = x %*% mu # 预测
  return(list(mu, y_hat))
}

season_model_without_intercept = function(x, y) {
  x = model.matrix( ~ x - 1, x) # 转化为one-hot编码
  mu = solve(t(x) %*% x) %*% t(x) %*% y # 求参数
  y_hat = x %*% mu # 预测
  return(list(mu, y_hat))
}

library(TSA)

# a
data("winnebago")
win.graph(width = 8,
          height = 8,
          pointsize = 10)
plot(
  x = zlag(winnebago),
  y = winnebago,
  xlab = "Previous Year Wages",
  ylab = "Wages"
) # 绘制前一年和后一年的散点图
win.graph(width = 12,
          height = 6,
          pointsize = 10)
plot(winnebago, type = 'o', pch = 21) # 时间序列图
# b
x = time(winnebago)
y = winnebago

result = linear_model(x, y)
y_hat = result[[2]]
y_num = as.numeric(y) # 转换为数值
e = y_num - y_hat

win.graph(12, 6, pointsize = 10)
plot(ts(e, start = start(winnebago), frequency = 12),
     type = 'o',
     pch = 21)
# c
win.graph(width = 12,
          height = 6,
          pointsize = 10)
plot(log(winnebago),
     type = 'o',
     pch = 21)
# d
x = time(winnebago)
y = log(winnebago) # 添加对数

result = linear_model(x, y)
y_hat = result[[2]]
y_num = as.numeric(y) # 转换为数值
e = y_num - y_hat

win.graph(12, 6, pointsize = 10)
plot(ts(e, start = start(winnebago), frequency = 12),
     type = 'o',
     pch = 21)
# e
x = season(winnebago)
x = model.matrix( ~ x, x) # 将类别转化为设计矩阵
x = cbind(x, time(log(winnebago))) # 添加时间
y = log(winnebago) # 添加对数
mu = solve(t(x) %*% x) %*% t(x) %*% y # 线性模型拟合
y_hat = x %*% mu # 预测
y_num = as.numeric(y) # 转换为数值
e = y_num - y_hat
# f
win.graph(12, 6, pointsize = 10)
plot(ts(e, start = start(winnebago), frequency = 12),
     type = 'o',
     pch = 21)
