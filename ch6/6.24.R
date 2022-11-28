source("./sm_series.R") # 导入模拟生成序列的函数
source("./acf_func.R") # 导入计算理论、样本自相关函数

set.seed(1111) # 设置随机种子以保证结果可复现

# (a)
sm_series1 = ARMA_func(AR_params = NULL,
                       MA_params = c(0.7),
                       seq_length = 24)
(rho1 = MA_thm_acf(c(0.7)))
r1 = acf_func(sm_series1)
r1[1]

# (b)
sm_series2 = ARMA_func(AR_params = NULL,
                       MA_params = c(0.7),
                       seq_length = 60)
(rho2 = MA_thm_acf(c(0.7)))
r2 = acf_func(sm_series2)
r2[1]

# (c)
sm_series3 = ARMA_func(AR_params = NULL,
                       MA_params = c(0.7),
                       seq_length = 120)
(rho3 = MA_thm_acf(c(0.7)))
r3 = acf_func(sm_series3)
r3[1]