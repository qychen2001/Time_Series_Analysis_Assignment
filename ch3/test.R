library(TSA)
library(matlib)
data(wages)

quadratic_model = function(x, y) {
  ones = rep(1, length(x))
  x_quad = x ^ 2
  x = cbind(ones, x, x_quad)
  #print(dim(x))
  I_matrix = diag(dim(x)[2]) * 0.5
  
  mu = inv(t(x) %*% x) %*% t(x) %*% y # 求参数
  mu = solve(t(x) %*% x + I_matrix) %*% t(x) %*% y # 求参数
  y_hat = x %*% mu # 预测
  return(list(mu, y_hat))
}

lm2 = lm(wages ~ time(wages) + I(time(wages) ^ 2), singular.ok = FALSE)
x = time(wages)
y = wages
result = quadratic_model(x, y)


qr(x)$rank