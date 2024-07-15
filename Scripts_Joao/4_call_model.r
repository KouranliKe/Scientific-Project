rm(list = ls())
library(tidyverse)

source("2_data_prep.r")
source("3_rolling_window.r")
source("functions.r")


#####
## The file with the forecasts will be saved with model_name
model_name <- "f_arima_var_win"
## The function called to run models is model_function, which is a function from functions.R
model_function <- f_arima_var_win
#####

load("data_joao.rda")
dates <- data$date
data <- data %>%
  select(-date) %>%
  as.matrix()
rownames(data) <- as.character(dates)

####### run rolling window ##########

nwindows <- 52 # num. de janelas ou num. de previsões
y_out <- tail(data[, "CPIAUCSL"], nwindows)
model_list <- list()
for_ind <- c(1, 3, 6) # 1:6

for (i in for_ind) {
  model <- rolling_window_var_win(
    fn = model_function,
    df = data,
    nwindow = nwindows + i - 1,
    horizon = i,
    variable = "CPIAUCSL",
    #n_lags = 6, # Comentar para arima
    w_min = 0.1,
    inc = 1
  )
  model_list[[i]] <- model
  cat(i, "\n")
}

forecasts_mean <- Reduce(
  f = cbind,
  x = lapply(model_list, function(x) head(x$forecast_mean, nwindows))
) %>% as.matrix()

forecasts_tail <- Reduce(
  f = cbind,
  x = lapply(model_list, function(x) head(x$forecast_tail, nwindows))
) %>% as.matrix()

# plot de previsoes x valores reais
plot.ts(y_out)
lines(forecasts_mean[, 1], col = 2)
lines(forecasts_tail[, 1], col = 3)

# RMSE
f_rmse <- function(x, y) {
  sqrt(mean((x - y)^2))
}

rmse_mean <- apply(forecasts_mean, 2, f_rmse, y = y_out) %>% print()
rmse_tail <- apply(forecasts_tail, 2, f_rmse, y = y_out) %>% print()

# CSFE

f_csfe <- function(x, y_bench, y_real) {
  error_bench <- (y_bench - y_real)^2
  error_x <- (x - y_real)^2
  result <- cumsum(error_bench - error_x)
  return(result)
}

for (i in seq_len(ncol(forecasts_tail))) {
  csfe <- f_csfe(
    x = forecasts_mean[, i],
    y_bench = forecasts_tail[, i],
    y_real = y_out
  )
  if (i == 1) {
    plot.ts(csfe)
  } else {
    lines(csfe, col = i)
  }
}
abline(h = 0)

# Saving some results

save(
  forecasts_mean,
  forecasts_tail,
  y_out,
  file = paste(
    model_name,
    ".rda",
    sep = ""
  )
) 
