# Boosting
get_boosting <- function(ind, df, variable, horizon, n_lags) {
  library(mboost)
  library(forecast)

  # INICIALIZACAO DE VARIAVEIS
  set.seed(100)

  data_in <- dataprep(
    ind = ind,
    df = df,
    variable = variable,
    horizon = horizon,
    n_lags = n_lags
  )

  y_in <- data_in$y_in
  x_in <- data_in$x_in
  x_out <- data_in$x_out

  # AJUSTE DO MODELO DE BOOSTING
  reg_full <- glmboost(
    y = y_in,
    x = as.matrix(x_in),
    # offset = 0,
    center = TRUE,
    control = boost_control(mstop = 100, nu = 0.1)
  )

  # DETERMINACAO DO NUMERO OTIMO DE ITERACOES
  cv5f <- cv(model.weights(reg_full), type = "kfold", B = 5)
  cv_seq <- cvrisk(reg_full, folds = cv5f, papply = lapply)
  m_opt <- mstop(cv_seq)

  # AJUSTE DO MODELO COM O NUMERO OTIMO DE ITERACOES

  reg_opt <- reg_full[m_opt]

  # PREVISAO PARA A JANELA DE TESTE
  opt_boosting <- predict(
    object = reg_opt,
    newdata = matrix(x_out, nrow = 1)
  )

  # RESULTADOS
  results <- list(
    forecast = opt_boosting,
    outputs = list(
      m_opt = m_opt,
      reg_opt = reg_opt
    )
  )
  return(results)
}

# Random Forest
get_random_forest <- function(ind, df, variable, horizon, n_lags) {
  library(randomForest)
  library(forecast)
  
  # INICIALIZACAO DE VARIAVEIS
  set.seed(100)
  
  data_in <- dataprep(
    ind = ind,
    df = df,
    variable = variable,
    horizon = horizon,
    n_lags = n_lags
  )
  
  y_in <- data_in$y_in
  x_in <- data_in$x_in
  x_out <- data_in$x_out
  
  # AJUSTE DO MODELO DE RANDOM FOREST
  reg_rf <- randomForest(
    y = y_in,
    x = as.matrix(x_in),
    ntree = 500
  )
  
  # PREVISAO PARA A JANELA DE TESTE
  rf_forecast <- predict(
    object = reg_rf,
    newdata = as.matrix(x_out)
  )
  
  # RESULTADOS
  results <- list(
    forecast = rf_forecast,
    outputs = list(
      reg_rf = reg_rf
    )
  )
  return(results)
}

# Lasso
get_lasso <- function(ind, df, variable, horizon, n_lags) {
  library(glmnet)
  library(forecast)
  
  # INICIALIZACAO DE VARIAVEIS
  set.seed(100)
  
  data_in <- dataprep(
    ind = ind,
    df = df,
    variable = variable,
    horizon = horizon,
    n_lags = n_lags
  )
  
  y_in <- data_in$y_in
  x_in <- data_in$x_in
  x_out <- data_in$x_out
  
  # CROSS VALIDATION
  cv_lasso <- cv.glmnet(
    x = x_in,
    y = y_in,
    alpha = 1
  )
  
  # AJUSTE DO MODELO COM O MELHOR LAMBDA APOS CV
  lasso_opt <- glmnet(
    x = x_in,
    y = y_in,
    alpha = 1,
    lambda = cv_lasso$lambda.min
  )
  
  # PREVISAO PARA A JANELA DE TESTE
  opt_lasso <- predict(
    object = lasso_opt,
    newx = matrix(x_out, nrow = 1)
  )
  
  # RESULTADOS
  results <- list(
    forecast = opt_lasso,
    outputs = list(
      lasso_opt = lasso_opt,
      lambda_min = cv_lasso$lambda.min
    )
  )
  return(results)
}

# ARIMA
# todo

##################################################################
# SCRIPTS COM JANELAS DE TAMANHO VARIAVEL

# Boosting de janela variavel
f_boosting_var_win <- function(
    ind,
    df,
    variable,
    horizon = 1,
    n_lags = 4,
    w_min = 0.1,
    inc = 1
) {
  
  library(mboost)
  library(forecast)
  library(magrittr)
  
  # INICIALIZACAO DE VARIAVEIS
  set.seed(100)
  
  # jbes 2011
  
  t_tot <- length(ind)
  m_max <- t_tot * (1 - w_min) + 1
  ind_vw <- seq(from = 1, to = m_max, by = inc)
  w_i <- w_min + (ind_vw - 1) / t_tot
  w_len <- w_i * t_tot
  
  if (!all.equal(round(w_len, 0), w_len)) {
    stop("'w_len' must be integer.")
  }
  
  for_boosting <- m_opt_seq <- NULL
  
  for (i in seq_along(w_len)) {
    
    if (w_len[i] <= (n_lags + horizon)) {
      stop("too many lags!")
    }
    
    data_in <- dataprep(
      ind = tail(ind, w_len[i]),
      df = df,
      variable = variable,
      horizon = horizon,
      n_lags = n_lags
    )
    
    y_in <- data_in$y_in
    x_in <- data_in$x_in
    x_out <- data_in$x_out
    
    # AJUSTE DO MODELO DE BOOSTING
    reg_full <- glmboost(
      y = y_in,
      x = as.matrix(x_in),
      # offset = 0,
      center = TRUE,
      control = boost_control(mstop = 100, nu = 0.1)
    )
    
    # DETERMINACAO DO NUMERO OTIMO DE ITERACOES
    cv5f <- cv(model.weights(reg_full), type = "kfold", B = 5)
    cv_seq <- cvrisk(reg_full, folds = cv5f, papply = lapply)
    m_opt <- mstop(cv_seq)
    
    m_opt_seq <- c(m_opt_seq, m_opt)
    
    # AJUSTE DO MODELO COM O NUMERO OTIMO DE ITERACOES
    
    reg_opt <- reg_full[m_opt]
    
    # PREVISAO PARA A JANELA DE TESTE
    for_aux <- predict(
      object = reg_opt,
      newdata = matrix(x_out, nrow = 1)
    ) %>% as.vector()
    
    for_boosting <- c(for_boosting, for_aux)
    
  }
  
  # RESULTADOS
  results <- list(
    forecast_mean = mean(for_boosting),
    forecast_tail = tail(for_boosting, 1),
    m_opt_seq = m_opt_seq
  )
  return(results)
}
