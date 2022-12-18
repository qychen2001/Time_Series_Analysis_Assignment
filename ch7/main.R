library(TSA)
source("./ch7.R")
source("./sm_arma.R")

# 1
series = ARMA_func(c(0.1, 0.7), NULL, 100)
# 自己的结果
(MM.AR(series, 2))
(LS.AR(series, 2))
# 书本的结果
ar(series, order.max = 2, AIC = F, method = 'yw')
ar(series, order.max = 2, AIC = F, method = 'ols')


series = ARMA_func(c(0.2, 0.4, 0.6), NULL, 100)
(MM.AR(series, 3))
(LS.AR(series, 3))
ar(series, order.max = 3, AIC = F, method = 'yw')
ar(series, order.max = 3, AIC = F, method = 'ols')

# 2
data("ar1.s")
data("ar1.2.s")
source("./mlear1.R")
source("./ulsar1.R")
MLE.AR1(ar1.s)
ULS.AR1(ar1.s,LearningRate = 0.01)

MLE.AR1(ar1.2.s)
ULS.AR1(ar1.2.s,LearningRate = 0.01)


# 3
set.seed(666)
series = ARMA_func(NULL, c(0.1, 0.7), 1000)
LS.MA(series,2,lr=1e-4)$theta
arima(series,order = c(0,0,2),method="CSS")$coef

series = ARMA_func(NULL, c(0.2, 0.4, 0.6), 1000)
LS.MA(series,3,lr=1e-4)$theta
arima(series,order = c(0,0,3),method="CSS")$coef

# 4
# 书本结果
data("arma11.s")
MM.ARMA11(arma11.s)
LS.ARMA11(arma11.s,lr=0.1)

set.seed(666)
# 模拟数据
series = ARMA_func(c(0.7), c(0.5), 1000)
MM.ARMA11(series)
LS.ARMA11(series, lr = 0.1)

series = ARMA_func(c(0.3), c(0.6), 10000)
MM.ARMA11(series)
LS.ARMA11(series, lr = 0.1)
