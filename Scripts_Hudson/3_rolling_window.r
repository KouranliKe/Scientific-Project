rolling_window <- function(fn, df, nwindow = 1, horizon, variable, ...) {
  # nwindow: num. de janelas ou num. de previsões
  # ind <- seq_len(nrow(df))
  window_size <- nrow(df) - nwindow - horizon + 1
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

  return(
    list(
      forecast_mean = forecast_mean,
      forecast_tail = forecast_tail,
      forecast = forecast_tail
    )
  )
}
