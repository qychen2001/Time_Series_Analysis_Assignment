library(TSA)

data("deere1")
# (a)
plot.ts(deere1,type='o')
# (b)
source("acf_func.R")
(acf=acf_func(deere1))
draw_acf(acf[1:20],type="样本自相关",n=length(acf))
# (c)
ts_replace=deere1
ts_replace[27]=(ts_replace[26]+ts_replace[28])/2
plot.ts(ts_replace,type='o')
(acf=acf_func(deere1))
draw_acf(acf[1:20],type="样本自相关",n=length(acf))
# (d)
pacf=pacf_func(ts_replace)
draw_pacf(pacf,n=length(ts_replace))