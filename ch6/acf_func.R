acf_func = function(series) {
  # 样本自相关函数计算函数
  n = length(series)
  series_mean = mean(series)
  series = series - series_mean
  acf_result = rep(0, n - 1)
  sum = series %*% series
  for (k in 1:(n - 1)) {
    acf_result[k] = (series[(k + 1):n] %*% series[1:(n - k)]) / sum
  }
  return(acf_result)
}

pacf_func_1 = function(series, max_lag = 20) {
  # 偏自相关的另外一种实现 6.2.9
  rho = acf_func(series) # 得到样本自相关函数
  phi = matrix(0, nrow = max_lag, ncol = max_lag) #phi是一个函数
  phi[1, 1] = rho[1]
  for (k in 2:max_lag) {
    a = rho[k] - phi[(k - 1), 1:(k - 1)] %*% rho[(k - 1):1]
    b = 1 - phi[(k - 1), 1:(k - 1)] %*% rho[1:(k - 1)]
    phi[k, k] = a / b
    for (j in 1:(k - 1)) {
      phi[k, j] = phi[k - 1, j] - phi[k, k] * phi[k - 1, k - j]
    }
  }
  return(diag(phi))
}

pacf_func = function(series, max_lag = 20) {
  # 6.2.8的实现
  rho = acf_func(series) # 得到样本自相关函数
  M = matrix(1, nrow = max_lag, ncol = max_lag) #phi是一个函数
  for (i in 1:(max_lag - 1)) {
    for (j in (i + 1):max_lag) {
      M[i, j] = rho[j - i]
      M[j, i] = rho[j - i] #对称矩阵
    }
  }
  # 得到完整的矩阵
  phi_kk = rep(rho[1], max_lag)
  for (k in 2:max_lag) {
    result = qr.solve(M[1:k, 1:k], rho[1:k])
    phi_kk[k] = result[k]
  }
  return(phi_kk)
}



MA_thm_acf = function(MA_params) {
  q = length(MA_params)
  rho0 = 1 + (MA_params %*% MA_params)
  rho = rep(0, (q + 1))
  if (q > 1) {
    for (k in 1:(q - 1)) {
      rho[k] = (-MA_params[k] + (MA_params[1:(q - k)] %*%
                                   MA_params[(k + 1):q])) / rho0
    }
  }
  rho[q] = (-MA_params[q]) / rho0
  return(rho)
}

draw_acf = function(acf_series, type = "acf") {
  plot(acf_series, type = 'h')
  abline(h = 0)
}
