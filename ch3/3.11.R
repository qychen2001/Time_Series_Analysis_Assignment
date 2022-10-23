library(TSA)
library(matlib)

data(wages)

quadratic_model = function(x, y) {
  ones = rep(1, length(x))
  x_quad = x ^ 2
  x = cbind(ones, x, x_quad)
  #print(dim(x))
  mu = qr.solve(x, y) # 求参数
  y_hat = x %*% mu # 预测
  return(list(mu, y_hat))
}

# a
x = time(wages)
y = wages

result = quadratic_model(x, y)
y_hat = result[[2]]
y_num = as.numeric(y) # 转换为数值
e = y_num - y_hat

mean(e)
var(e)

# b

e=scale(e) # 标准化
runs(e)

# c
acf_result=acf(e)
acf_result

# d
# 此处的残差已经经过标准化

#正态性检验，原假设为符合正态分布
shapiro.test(e)
win.graph(width = 8,
          height = 8,
          pointsize = 10)
qqnorm(e)
qqline(e)
win.graph(width = 8,
          height = 8,
          pointsize = 10)
hist(e)

