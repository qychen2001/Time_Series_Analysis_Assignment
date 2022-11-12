for (i in c(1:6)){
  data<-ts(rt(48,df=5)) # df的含义是自由度
  png( 
    filename = paste0(i, "_", ".jpg"), # 文件名称
    width = 480,           # 宽
    height = 360,          # 高
    units = "px",          # 单位
    bg = "white",          # 背景颜色
    res = 72)              # 分辨率
  plot(data)
  dev.off()
  print(Box.test(data,lag = 6))
  print(shapiro.test(data))
}
