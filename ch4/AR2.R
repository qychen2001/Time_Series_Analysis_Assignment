AutoCor_AR2 = function(phi1, phi2, max.lag = 30) {
  rho = rep(NA, max.lag)
  rho[1] = phi1 / (1 - phi2)
  rho[2] = (phi2 * (1 - phi2) + phi1 ^ 2) / (1 - phi2)
  for (k in 3:max.lag) {
    rho[k] = phi1 * rho[k - 1] + phi2 * rho[k - 2]
  }
  rho
}

plot_rho = function(rho) {
  plot(
    c(1:30),
    rho,
    ylab = "自相关函数值",
    xlab = "滞后数",
    col = "red",
    cex = 1,
    pch = 16
  )
}


Charac_func = function(phi1, phi2) {
  delta = sqrt(complex(real = phi1 ^ 2 + 4 * phi2))
  if (Im(delta) == 0) {
    # 虚部为0，换言之delta大于0
    roots = c((phi1 - Re(delta)) / (-2 * phi2), (phi1 + Re(delta)) / (-2 * phi2))
    return(roots)
  }
  else{
    roots = c((phi1 - delta) / (-2 * phi2), (phi1 + delta) / (-2 * phi2))
    R = sqrt(-phi2) # 阻尼因子
    theta = acos(phi1/(2*R)) # 频率
    return(list(roots,R,theta))
  }
}