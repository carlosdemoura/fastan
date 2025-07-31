#' Create sample from CmdStan
#'
#' @param fit CmdStan fit objecy
#'
#' @returns list
#' 
#' @export
extract_cmd = function(fit) {
  fac  = fit$metadata$stan_variable_sizes$alpha[2]
  row  = fit$metadata$stan_variable_sizes$alpha[1]
  col  = fit$metadata$stan_variable_sizes$lambda[2]
  pred = fit$metadata$stan_variable_sizes$pred[1]
  iter = fit$metadata$iter_sampling
  
  samp = list(
    alpha  = array(dim=c(iter,row,fac)),
    lambda = array(dim=c(iter,fac,col)),
    sigma2 = array(dim=c(iter,row,1))
  )
  
  if (pred > 1) {  ## INCORRETO
    samp$pred = array(dim=c(iter,pred,1))
    for (i in 1:pred) {
      samp$pred[,i,1] = fit$post_warmup_draws[,,paste0("pred[", i , ",1]")] |> c()
    }
  }
  
  for (k in 1:fac) {
    for (i in 1:row) {
      if (k == 1) {
        samp$sigma2[,i,1] = fit$post_warmup_draws[,,paste0("sigma2[", i , ",1]")] |> c()
      }
      samp$alpha[,i,k] = fit$post_warmup_draws[,,paste0("alpha[", i, ",", k, "]")] |> c()
    }
    for (j in 1:col) {
      samp$lambda[,k,j] = fit$post_warmup_draws[,,paste0("lambda[", k, ",", j, "]")] |> c()
    }
  }
  
  samp
}
