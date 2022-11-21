# 4.10
source("./ARMA11.R")
(rho1 = AutoCor_ARMA11(phi = 0.7, theta = 0.4))
plot(c(1:30),rho1,ylab = "自相关函数值",xlab = "滞后数",
  col = "red",cex = 1,pch = 16)

(rho2 = AutoCor_ARMA11(phi = 0.7, theta = -0.4))
plot(c(1:30),rho2,ylab = "自相关函数值",xlab = "滞后数",
  col = "red",cex = 1,pch = 16
)

# 4.9
source("./AR2.R")
# a
rho=AutoCor_AR2(0.6,0.3)
(root=Charac_func(0.6,0.3))
plot_rho(rho)

# b
rho=AutoCor_AR2(-0.4,0.5)
(root=Charac_func(-0.4,0.5))
plot_rho(rho)

# c
rho=AutoCor_AR2(1.2,-0.7)
(root=Charac_func(1.2,-0.7))
plot_rho(rho)

# d
rho=AutoCor_AR2(-1,-0.6)
(root=Charac_func(-1,-0.6))
plot_rho(rho)

# e
rho=AutoCor_AR2(0.5,-0.9)
(root=Charac_func(0.5,-0.9))
plot_rho(rho)

# f
rho=AutoCor_AR2(-0.5,-0.6)
(root=Charac_func(-0.5,-0.6))
plot_rho(rho)