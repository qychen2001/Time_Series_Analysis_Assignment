lbtest_result=matrix(nrow = 6,ncol = 3); #初始化随机性检验结果存储矩阵

windows(45,30); #设置绘图参数
par(mfrow = c(2,3)); #将Graph Device分屏为2×3的大小
for (i in 1:6) #模拟6次
{
  x <- ts(rnorm(48)); #生成48个服从标准正态分布的随机数
  plot.ts(x,ylab = "Value",xlab = "Time",main = "独立（标准）正态分布过程模拟"); #在当前屏幕绘制时间序列图
  points(x,pch = 21); #设置点型
  y1 <- Box.test(x,lag = 6,type = "Ljung-Box"); #对数据进行Ljung-Box随机性检验
  lbtest_result[i,]=c(y1[[1]],y1[[2]],y1[[3]]); #存储随机性检验结果
}