library(MASS)

ave_model = function(x, y) {
  mu = sum(y) / length(y)
  y_hat = mu
  return(list(mu, y_hat))
}

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
  #print(dim(x))
  mu = solve(t(x) %*% x) %*% t(x) %*% y # 求参数
  y_hat = x %*% mu # 预测
  return(list(mu, y_hat))
}

season_model_with_intercept = function(x, y) {
  x = model.matrix(~ x, x)
  mu = solve(t(x) %*% x) %*% t(x) %*% y # 求参数
  y_hat = x %*% mu # 预测
  return(list(mu, y_hat))
}

season_model_without_intercept = function(x, y) {
  x = model.matrix(~ x - 1, x) # 转化为one-hot编码
  mu = solve(t(x) %*% x) %*% t(x) %*% y # 求参数
  y_hat = x %*% mu # 预测
  return(list(mu, y_hat))
}

cosine_model = function(x, y, f = 12) {
  x = as.numeric(x) - 1 #转换为数字，此处需要-1使得从0开始
  f = 1 / f
  cos_x = cos(2 * pi * f * x)
  sin_x = sin(2 * pi * f * x)
  ones = rep(1, length(x))
  x = cbind(ones, cos_x, sin_x)
  mu = solve(t(x) %*% x) %*% t(x) %*% y # 求参数
  y_hat = x %*% mu # 预测
  return(list(mu, y_hat))
}


library(TSA)
data(rwalk)
data(tempdub)

x = time(rwalk)
y = rwalk
linear_model(x, y) # 验证线性模型正确性


x = season(tempdub)
y = tempdub

season_model_with_intercept(x, y) # 验证有截距情况
season_model_without_intercept(x, y) # 验证没有截距情况
# x=harmonic(tempdub,1) # 书本做法
cosine_model(x, y)



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


