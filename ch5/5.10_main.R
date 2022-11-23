source("./5.10_func.R")

# IMA11
AR_params = NULL
MA_params_list = list(c(1), c(0.3), c(-0.3), c(-1))
seq_length = 9; diff = 1; batch = 10;


j = 1
for (MA_params in MA_params_list) {
  png(
    filename = paste("MA11_", MA_params[1], ".png", collapse = NULL),
    width = 480,
    height = 480,
    units = "px",
    bg = "white",
    res = 72
  )
  draw_ARIMA(AR_params, MA_params, seq_length, diff, batch)
  dev.off()
  j = j + 1
}


# IMA22
AR_params = NULL
MA_params_list = list(c(1, -0.6), c(-1, 0.6), c(2, -1.2), c(-2, 1.2))
seq_length = 9
diff = 2
batch = 10


j = 1
for (MA_params in MA_params_list) {
  png(
    filename = paste("MA22_", MA_params[1], "_", MA_params[2], ".png", collapse = NULL),
    width = 480,
    height = 480,
    units = "px",
    bg = "white",
    res = 72
  )
  draw_ARIMA(AR_params, MA_params, seq_length, diff, batch)
  dev.off()
  j = j + 1
}
