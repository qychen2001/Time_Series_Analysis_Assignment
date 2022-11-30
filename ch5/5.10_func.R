ARMA_func = function(AR_params, MA_params, seq_length) {
  AR_params_length = length(AR_params)
  MA_params_length = length(MA_params)
  if (AR_params_length == 0 & MA_params_length == 0) {
    print("parameter error!") # p和q都是0需要报错
    return(0)
  }
  Y = c(rep(0, AR_params_length + MA_params_length),
        rep(NA, seq_length))
  e = rnorm(n = (seq_length + AR_params_length + MA_params_length))
  for (i in seq(AR_params_length + MA_params_length + 1, length(Y))) {
    AR = 0
    MA = 0
    if (AR_params_length != 0) {
      AR = (Y[(i - 1):(i - AR_params_length)] %*% AR_params)[1, 1]
    }
    if (MA_params_length != 0) {
      # 是一个矩阵，取第一个元素
      MA = (e[(i - 1):(i - MA_params_length)] %*% MA_params)[1, 1]
    }
    Y[i] = AR - MA + e[i]
  }
  return(Y[(AR_params_length + MA_params_length + 1):length(Y)]) # 忽略前几项
}

ARIMA_func = function(AR_params, MA_params, seq_length, diff) {
  w = ARMA_func(AR_params, MA_params, seq_length)
  w = c(0, w)
  # cat("ARMA series length:",length(w),'\n')
  d = diff
  while (d > 0) {
    w = cumsum(w) #逆差分
    d = d - 1
  }
  y = w
  # cat("ARIMA series length:",length(y),'\n')
  return(y)
}

draw_ARIMA = function(AR_params,
                      MA_params,
                      seq_length,
                      diff,
                      batch) {
  # 绘图
  results = list(NULL)
  max_num = 0
  min_num = 0
  for (i in 1:batch) {
    result = ARIMA_func(AR_params, MA_params, seq_length, diff)
    if (max(result) > max_num)
      max_num = max(result)
    if (min(result) < min_num)
      min_num = min(result)
    results[[i]] = result
  }
  plot(
    results[[1]],
    type = 'o',
    ylim = c(min_num, max_num),
    pch = 16,
    xlab = "time",
    ylab = "value"
  )
  for (i in 2:batch) {
    points(1:(seq_length + 1),
           results[[i]],
           pch = 16)
    lines(results[[i]])
  }
}
