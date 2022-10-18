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
  x_quad = x * x
  x = cbind(ones, x, x_quad)
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
