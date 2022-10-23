library(TSA)
data("retail")

x = season(retail)
x = model.matrix(~ x, x) # 将类别转化为设计矩阵
x = cbind(x, time(retail)) # 添加时间
y = retail # 添加对数
mu = solve(t(x) %*% x) %*% t(x) %*% y # 线性模型拟合
y_hat = x %*% mu # 预测
y_num = as.numeric(y) # 转换为数值
e = y_num - y_hat

e = scale(e) # 对残差进行标准化

# b
runs(e)
# c
acf_result = acf(e)
acf_result

# 此处的残差已经经过标准化

# 正态性检验，原假设为符合正态分布
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
