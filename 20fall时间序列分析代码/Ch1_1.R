library("TSA"); #加载程辑包

data(larain); #获取larain数据

windows(3,3,8); #设置绘图参数
plot(y = larain,x = zlag(larain),ylab = 'Inches',xlab = 'Previous Year Inches') #绘制图像