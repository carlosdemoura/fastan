fiat_init = function(model, chains, param = 1) {
  if ( (length(param) == 1) & is.numeric(param) ) {
    x = param
    param = list()
    for (p in c("alpha", "lambda", "sigma2", "pred")) {
      param[[p]] = rep(x, chains)
    }
  }

  init = list()

  for (i in 1:chains) {
    init[[i]] = list(alpha  = matrix(param$alpha[i] , nrow = model$dim$al_row,  ncol = model$dim$al_fac),
                     lambda = matrix(param$lambda[i], nrow = model$dim$al_fac,  ncol = model$dim$al_col),
                     sigma2 = matrix(param$sigma2[i], nrow = model$dim$al_row,  ncol = 1)
                     #,
                     #pred   = matrix(param$pred[i]  , nrow = model$dim$obs_row, ncol = model$dim$obs_col)
    )

    if(is.null(T)) {
      init[[1]]$pred = matrix(as.numeric(pred), nrow = data$obs_row, ncol = data$obs_col)
    }
  }

  return(init)
}




fiat_init = function(model, chains, pred, const = 1) {
  if ( (length(param) == 1) & is.numeric(param) ) {
    x = param
    param = list()
    for (p in c("alpha", "lambda", "sigma2", "pred")) {
      param[[p]] = rep(x, chains)
    }
  }

  init = list()

  for (i in 1:chains) {
    init[[i]] = list(alpha  = matrix(param$alpha[i] , nrow = model$dim$al_row,  ncol = model$dim$al_fac),
                     lambda = matrix(param$lambda[i], nrow = model$dim$al_fac,  ncol = model$dim$al_col),
                     sigma2 = matrix(param$sigma2[i], nrow = model$dim$al_row,  ncol = 1)
                     #,
                     #pred   = matrix(param$pred[i]  , nrow = model$dim$obs_row, ncol = model$dim$obs_col)
    )

    if(is.null(T)) {
      init[[1]]$pred = matrix(as.numeric(pred), nrow = data$obs_row, ncol = data$obs_col)
    }
  }

  return(init)
}
