dataprep <- function(ind, df, variable, horizon, n_lags = 6) {
  
  # For ARIMA
  if (identical(model_function, f_arima_var_win)) {
    df <- df[ind, ]
    y <- df[, variable]
    return(list(y_in = y))
  } else {
    # For the rest
    
    if (n_lags <= 1) {
      stop("n_lags deve ser um inteiro maior do que 1.")
    }
    
    df <- df[ind, ]
    y <- df[, variable]
    x_aux <- df
  
    x <- embed(as.matrix(x_aux), n_lags)
  
    names_x <- NULL
    for (i in seq_len(n_lags)) {
      names_x <- c(
        names_x,
        paste(colnames(x_aux), "_lag_", horizon + i - 1, sep = "")
      )
  }
  colnames(x) <- names_x
  
  x_in <- x[-c((nrow(x) - horizon + 1):nrow(x)), ]
  x_out <- x[nrow(x), ]
  x_out <- t(as.vector(x_out))
  y_in <- tail(y, nrow(x_in))

  return(list(x_in = x_in, x_out = x_out, y_in = y_in))
  }
}
# # Exemplo
# 
# x <- matrix(1:150, 30, 5)
# colnames(x) <- paste("x", 1:5, sep = "")
# 
# teste <- dataprep(1:25, x, "x1", 1, 2)
# print(teste)
