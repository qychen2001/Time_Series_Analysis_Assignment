acf_func = function(series) {
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

pacf_func = function(series) {
  n = length(series)
  t = n
  for (k in 1:(n-1)) {
    
  }
}



MA_thm_acf = function(MA_params) {
  q=length(MA_params)
  rho0=1+(MA_params%*%MA_params)
  rho = rep(0,(q+1))
  if (q > 1){
    for (k in 1:(q-1)){
      rho[k]=(-MA_params[k]+(MA_params[1:(q-k)]%*%MA_params[(k+1):q]))/rho0
    }
  }
  rho[q]= (-MA_params[q])/rho0
  return(rho)
}




# 
# library(TSA)
# data(ima22.s)
# acf_func(ima22.s)
# acf(ima22.s)
# acf(ima22.s)$acf
# 
# source("./sm_series.R")
# data("ma1.1.s")
# length(ma1.1.s)
# acf(ma1.1.s)$acf
# acf_func(ma1.1.s)
# sm_ma1=ARMA_func(NULL,c(0.9),120)
# acf(sm_ma1)$acf
