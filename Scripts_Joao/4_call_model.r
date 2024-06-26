rm(list = ls())

source("13_data_prep.r")
source("functions.r")
source("15_rolling_window.r")

#####
## The file with the forecasts will be saved with model_name
model_name <- "boosting_var_win"
## The function called to run models is model_function, which is a function from functions.R
model_function <- f_boosting_var_win
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
for_ind <- 1 #c(1, 3, 6, 12) # 1:12

for (i in for_ind) {
  model <- rolling_window(
    fn = model_function,
    df = data,
    nwindow = nwindows + i - 1,
    horizon = i,
    variable = "CPIAUCSL",
    n_lags = 4,
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

plot.ts(y_out)
lines(forecasts_mean[, 1], col = 2)
lines(forecasts_tail[, 1], col = 3)

f_rmse <- function(x, y) {
  sqrt(mean((x - y)^2))
}

rmse_mean <- apply(forecasts_mean, 2, f_rmse, y = y_out) %>% print()
rmse_tail <- apply(forecasts_tail, 2, f_rmse, y = y_out) %>% print()
