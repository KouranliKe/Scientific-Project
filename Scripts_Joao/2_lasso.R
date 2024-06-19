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
      lasso_opt = lasso_opt
    )
  )
  return(results)
}

##################################################################

