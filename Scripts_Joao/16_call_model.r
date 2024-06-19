rm(list = ls())

source("13_data_prep.r")
source("2_boosting.r")
source("15_rolling_window.r")

#####
## The file with the forecasts will be saved with model_name
model_name <- "boosting"
## The function called to run models is model_function, which is a function from functions.R
model_function <- get_boosting
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
for_ind <- c(1, 3, 6, 12) # 1:12

for (i in for_ind) {
  model <- rolling_window(
    fn = model_function,
    df = data,
    nwindow = nwindows + i - 1,
    horizon = i,
    variable = "CPIAUCSL",
    n_lags = 4
  )
  model_list[[i]] <- model
  cat(i, "\n")
}

forecasts <- Reduce(
  f = cbind,
  x = lapply(model_list, function(x) head(x$forecast, nwindows))
) %>% as.matrix()

plot.ts(y_out)
lines(forecasts[, 1], col = 2)

f_rmse <- function(x, y) {
  sqrt(mean((x - y)^2))
}

rmse <- apply(forecasts, 2, f_rmse, y = y_out) %>% print()
