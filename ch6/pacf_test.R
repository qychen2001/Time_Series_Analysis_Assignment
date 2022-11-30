source("./acf_func.R")
library(TSA)
data(ar2.s)

pacf_func(ar2.s)
pacf_func_1(ar2.s)
pacf(ar2.s)$acf #书本结果
