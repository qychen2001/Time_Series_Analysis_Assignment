windows(40,20); #设置图窗参数
split.screen(c(1,2)); #将Graph Device分屏为1×2的大小
screen(1); #设置当前绘图屏幕
n <- 2;x <- numeric(100);x[1] <- rnorm(1,0,1); #初始化随机游走序列
while (n<=100){
  x[n] <- x[n-1]+rnorm(1,0,1);
  n <- n+1;
} #生成长度为100的随机游走序列
y1=ts(x);plot.ts(y1,xlab = "Time",ylab = "Value",main = "随机游走模拟"); #绘制时间序列图像
points(y1,pch = 21); #设置点型
screen(2); #设置当前绘图屏幕
x <- rnorm(101,0,1); #生成101个独立同分布于标准正态分布的随机数
y <- numeric(100); #初始化滑动平均序列
for (i in 1:100){
  y[i] <- (x[i]+x[i+1])/2;
} #通过随机数向量x生成长度为100的滑动平均序列
y2=ts(y);plot.ts(y2,xlab = "Time",ylab = "Value",main = "滑动平均模拟"); #绘制时间序列图像
points(y2,pch = 21); #设置点型
close.screen(all = TRUE); #退出split.screen模式

library("TSA") #加载程辑包
windows(40,20); #设置图窗参数
split.screen(c(1,2)); #将Graph Device分屏为1×2的大小
screen(1); #设置当前绘图屏幕
plot(y1,x=zlag(y1,1),xlab = "Previous Value",ylab = "Value",main = "随机游走序列的一阶滞后自相关图"); 
#绘制随机游走序列的一阶滞后自相关图
screen(2); #设置当前绘图屏幕
plot(y2,x=zlag(y2,1),xlab = "Previous Value",ylab = "Value",main = "滑动平均序列的一阶滞后自相关图")
#绘制滑动平均序列的一阶滞后自相关图