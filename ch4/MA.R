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
