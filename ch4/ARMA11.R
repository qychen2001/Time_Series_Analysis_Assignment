AutoCor_ARMA11 = function(phi, theta, max.lag = 30) {
  rho = rep(NA, max.lag)
  for (k in 1:max.lag) {
    rho[k] = (phi^(k-1))*(1-theta*phi)*(phi-theta)/(1-2*phi*theta+theta^2)
  }
  rho
}
