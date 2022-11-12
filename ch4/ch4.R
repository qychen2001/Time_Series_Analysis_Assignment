AutoCor_MA = function(theta) {
  q = length(theta)
  B = 1 + sum(theta ^ 2)
  A = rep(NA, q)
  for (k in seq(q - 1)) {
    A[k] = -theta[k] + sum(theta[seq(1, q - k)] * theta[seq(k + 1, q)])
  }
  A[q] = -theta[q]
  
  rho = A / B
  rho
}


AutoCor_AR2 = function(phi1, phi2, max.lag = 20) {
  rho = req(NA, max.lag)
  rho[1] = phi1 / (1 - phi2)
  rho[2] = (phi2 * (1 - phi2) + phi1 ^ 2) / (1 - phi2)
  for (k in 3:max.lag) {
    rho[k] = phi1 * rho[k - 1] + phi2 * rho[k - 2]
  }
}


theta = c(1, -0.6, 0.3)

Y = rep(NA, n)
e = rnorm(n, 0, 1)
q = length(theta)
e1 = c(rep(0, q), e)
for (i in seq(n)) {
  Y[i] = e[i + q] - e[i + q - 1]
}