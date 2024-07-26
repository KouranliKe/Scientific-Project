# SCRIPTS COM JANELAS DE TAMANHO VARIAVEL

# ARIMA DE JANELA VARIAVEL
f_arima_var_win <- function(
    ind,
    df,
    variable,
    horizon = 1,
    n_lags = 12,
    w_min = 0.1,
    inc = 1
) {
  
  library(forecast) # Certifique-se de ter a biblioteca forecast instalada
  
  # INICIALIZACAO DE VARIAVEIS
  # set.seed(100) 
   
  # jbes 2011
  
  t_tot <- length(ind)
  m_max <- t_tot * (1 - w_min) + 1
  ind_vw <- seq(from = 1, to = m_max, by = inc)
  w_i <- w_min + (ind_vw - 1) / t_tot
  w_len <- w_i * t_tot
  
  # if (!all.equal(round(w_len, 0), w_len)) {
  #   stop("'w_len' must be integer.")
  # }
  
  for_arima <- NULL
  
  for (i in seq_along(w_len)) {
    set.seed(100)
    print(paste('tam janela:', w_len[i]))
    
    # Dados de entrada
    data_in <- dataprep(
      ind = tail(ind, w_len[i]),
      df = df,
      variable = variable,
      horizon = horizon,
      n_lags = n_lags
    )
    
    y_in <- data_in$y_in
    
    # AJUSTE DO MODELO DE ARIMA
    reg_arima <- auto.arima(
      y = y_in,
      ic = 'bic')
    
    # PREVISAO PARA A JANELA DE TESTE
    for_aux <- forecast(reg_arima,
                        h = horizon)$mean[horizon] %>% as.vector()
    for_arima <- c(for_arima, for_aux)
    
  }
  
  # RESULTADOS
  results <- list(
    forecast_mean = mean(for_arima),
    forecast_tail = tail(for_arima, 1)
  )
  return(results)
}


# Boosting de janela variavel
f_boosting_var_win <- function(
    ind,
    df,
    variable,
    horizon = 1,
    n_lags = 12,
    w_min = 0.1,
    inc = 1
) {
  
  library(mboost)
  library(forecast)
  library(magrittr)
  
  # INICIALIZACAO DE VARIAVEIS
  # set.seed(100)
  
  # jbes 2011
  
  t_tot <- length(ind)
  m_max <- t_tot * (1 - w_min) + 1
  ind_vw <- seq(from = 1, to = m_max, by = inc)
  w_i <- w_min + (ind_vw - 1) / t_tot
  w_len <- w_i * t_tot
  
  # if (!all.equal(round(w_len, 0), w_len)) {
  #   stop("'w_len' must be integer.")
  # }
  
  for_boosting <- m_opt_seq <- NULL
  
  for (i in seq_along(w_len)) {
    set.seed(100)
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
      control = boost_control(mstop = 300, nu = 0.05)
    )
    
    # DETERMINACAO DO NUMERO OTIMO DE ITERACOES
    cv10f <- cv(model.weights(reg_full), type = "kfold", B = 10)
    cv_seq <- cvrisk(reg_full, folds = cv10f, papply = lapply)
    m_opt <- mstop(cv_seq)
    m_opt_seq <- c(m_opt_seq, m_opt)
    # print(paste('m_opt:', m_opt, 'tam janela:', w_len[i])) # print m otimo (depuracao)
    
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
    outputs = m_opt_seq
  )
  return(results)
}


# LASSO DE JANELA VARIAVEL
f_lasso_var_win <- function(
    ind,
    df,
    variable,
    horizon = 1,
    n_lags = 4,
    w_min = 0.1,
    inc = 1
) {

  library(glmnet)
  library(forecast)

  # INICIALIZACAO DE VARIAVEIS
  # set.seed(100)

  # jbes 2011

  t_tot <- length(ind)
  m_max <- t_tot * (1 - w_min) + 1
  ind_vw <- seq(from = 1, to = m_max, by = inc)
  w_i <- w_min + (ind_vw - 1) / t_tot
  w_len <- w_i * t_tot

  # if (!all.equal(round(w_len, 0), w_len)) {
  #   stop("'w_len' must be integer.")
  # }

  for_lasso <- lambda_min_seq <- NULL

  for (i in seq_along(w_len)) {
    set.seed(100)

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

    # CROSS VALIDATION
    cv_lasso <- cv.glmnet(
      x = x_in,
      y = y_in,
      alpha = 1
    )
    lambda_min <- cv_lasso$lambda.min
    # print(paste('lambda_opt:',lambda_min ,'Tam janela:', w_len[i]))
    
    # AJUSTE DO MODELO COM O MELHOR LAMBDA APOS CV
    lasso_opt <- glmnet(
      x = x_in,
      y = y_in,
      alpha = 1,
      lambda = lambda_min
    )

    # PREVISAO PARA A JANELA DE TESTE
    for_aux <- predict(
      object = lasso_opt,
      newx = matrix(x_out, nrow = 1)
    ) %>% as.vector()

    for_lasso <- c(for_lasso, for_aux)

  }

  # RESULTADOS
  results <- list(
    forecast_mean = mean(for_lasso),
    forecast_tail = tail(for_lasso, 1),
    outputs = lambda_min_seq
  )
  return(results)
}

# RANDOM FOREST DE JANELA VARIAVEL
rf_var_win <- function(
    ind,
    df,
    variable,
    horizon = 1,
    n_lags = 4,
    w_min = 0.1,
    inc = 1
) {
  
  library(randomForest)
  library(forecast)
  library(magrittr)
  
  # INICIALIZACAO DE VARIAVEIS
  #set.seed(100)
  
  # jbes 2011
  
  t_tot <- length(ind)
  m_max <- t_tot * (1 - w_min) + 1
  ind_vw <- seq(from = 1, to = m_max, by = inc)
  w_i <- w_min + (ind_vw - 1) / t_tot
  w_len <- w_i * t_tot
  
  # if (!all.equal(round(w_len, 0), w_len)) {
  #   stop("'w_len' must be integer.")
  # }
  
  for_rf <- NULL
  
  for (i in seq_along(w_len)) {
    print(paste('tam janela:', w_len[i]))
    set.seed(100)
    
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
    
    # AJUSTE DO MODELO DE RANDOM FOREST
    reg_rf <- randomForest(
      y = y_in,
      x = as.matrix(x_in),
      ntree = 500
    )
    
    # PREVISAO PARA A JANELA DE TESTE
    for_aux <- predict(
      object = reg_rf,
      newdata = matrix(x_out, nrow = 1)
    ) %>% as.vector()
    
    for_rf <- c(for_rf, for_aux)
    
  }
  
  # RESULTADOS
  results <- list(
    forecast_mean = mean(for_rf),
    forecast_tail = tail(for_rf, 1)
  )
  return(results)
}
