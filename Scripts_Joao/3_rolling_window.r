rolling_window <- function(fn, df, nwindow = 1, horizon, variable, ...) {
  # nwindow: num. de janelas ou num. de previsões
  # ind <- seq_len(nrow(df))
  window_size <- nrow(df) - nwindow
  indmat <- matrix(NA, window_size, nwindow)
  indmat[1, ] <- seq_len(ncol(indmat))
  for (i in 2:nrow(indmat)) {
    indmat[i, ] <- indmat[i - 1, ] + 1
  }
  rw <- apply(
    X = indmat,
    MARGIN = 2,
    FUN = fn,
    df = df,
    horizon = horizon,
    variable = variable,
    ...
  )
  forecast <- unlist(lapply(rw, function(x) x$forecast))
  outputs <- lapply(rw, function(x) x$outputs)
  return(
    list(
      forecast = forecast,
      outputs = outputs))
}

rolling_window_var_win <- function(fn, df, nwindow = 1, horizon, variable, ...) {
  # nwindow: num. de janelas ou num. de previsões
  # ind <- seq_len(nrow(df))
  window_size <- nrow(df) - nwindow
  indmat <- matrix(NA, window_size, nwindow)
  indmat[1, ] <- seq_len(ncol(indmat))
  for (i in 2:nrow(indmat)) {
    indmat[i, ] <- indmat[i - 1, ] + 1
  }
  rw <- apply(
    X = indmat,
    MARGIN = 2,
    FUN = fn,
    df = df,
    horizon = horizon,
    variable = variable,
    ...
  )
  forecast_mean <- unlist(lapply(rw, function(x) x$forecast_mean))
  forecast_tail <- unlist(lapply(rw, function(x) x$forecast_tail))
  outputs <- lapply(rw, function(x) x$outputs)
  return(
    list(
      forecast_mean = forecast_mean, 
      forecast_tail = forecast_tail, 
      outputs = outputs))
}