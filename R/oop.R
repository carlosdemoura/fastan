#' Title
#'
#' @param data .
#' @param info .
#'
#' @export
fa_project = function(data, info = "") {
  proj =
    list(
      info    = info,
      data    = data,
      model   = NA,
      fit     = NA,
      summary = NA
    )
  class(proj) = "fastanProject"
  return(proj)
  #model = process_conf(data)
  #{\(.) c(., list(fit = run_stan(.$model))) }() |>
  #{\(.) c(., list(summary = .$fit |> rstan::extract() |> summary_matrix() ))}()
}
