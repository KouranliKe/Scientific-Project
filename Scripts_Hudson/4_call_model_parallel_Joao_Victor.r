rm(list = ls())

library(dplyr)
library(future)
library(future.apply)
library(parallel)

source("1_data_prep.r")
source("2_boosting.r")
source("3_rolling_window.r")

# Loading the dataset

load("data_joao.rda")

dates <- data$date

data <- select(data, -c(date)) %>% as.matrix()

#####
## The file with the forecasts will be saved with model_name
model_name <- "Joao_boosting"
## The function called to run models is model_function, which is a function from functions.R
model_function <- f_boosting_var_win
## Dependent variable
y_var <- "CPIAUCSL"

####### run rolling window ##########

nwindows <- 168 # num. de janelas ou num. de previsões
y_out <- tail(data[, y_var], nwindows)
for_ind <- c(1, 3, 6) # 1:12

f_loop <- function(i) {
  model <- rolling_window(
    fn = model_function,
    df = data,
    nwindow = nwindows,
    horizon = i,
    variable = y_var,
    n_lags = 4,
    w_min = 0.2,
    inc = 1
  )
  return(model)
}

# loop #

n_cores <- detectCores(logical = FALSE)

n_workers <- min(n_cores, length(for_ind))

plan(multicore, workers = n_workers)

ind <- as.matrix(for_ind)

model_list <- future_apply(
  X = ind,
  MARGIN = 1,
  FUN = f_loop,
  simplify = FALSE,
  future.seed = TRUE
)

forecasts_mean <- Reduce(
  f = cbind,
  x = lapply(model_list, function(x) x$forecast_mean)
) %>% as.matrix()

forecasts_tail <- Reduce(
  f = cbind,
  x = lapply(model_list, function(x) x$forecast_tail)
) %>% as.matrix()

plot.ts(y_out)
for (i in seq_len(ncol(forecasts_tail))) {
  lines(forecasts_mean[, i], col = i + 1, lty = 1)
  lines(forecasts_tail[, i], col = i + 1, lty = 2)
}

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
    "results/",
    model_name,
    ".rda",
    sep = ""
  )
)
