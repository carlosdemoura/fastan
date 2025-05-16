i = 1
for (i in 1:5){

  col = 50
  xxx = 1:col-col/2

  if (i == 1) {
    lambda = scale(xxx) |> {\(.) sin(.*pi-1.5)}() |> as.vector()
  } else if (i == 2) {
    lambda = xxx^2 |> scale() |> as.vector()
  } else if (i == 3) {
    lambda = (5*xxx^3 + col*xxx) |> scale() |> as.vector()
  } else if (i == 4) {
    lambda = log(abs(xxx)) |> {\(.) -1*replace(., (.==0)|(.==-Inf), .5)}() |> scale() |> as.vector()
  } else if (i == 5) {
    lambda = xxx[xxx > 0] |> {\(.) c(.,.)[1:col]}() |> log() |> scale() |> as.vector()
  }

  lambda =
    lambda |>
    {\(.) if(min(.) < 0) . + abs(1.1*min(.)) else . }()

  plot(lambda, type = "l")


}



p =ggplot(reglin::wdi, aes(x=pib, y=expectativa, label=pais)) +
  geom_point() +
  geom_text(, data = reglin::wdi)
p|>plotly::ggplotly()





lambda = rep(0, col)
for (i in 2:col) {
  lambda[i] = rnorm(1, lambda[i-1])
}
lambda =
  lambda |>
  scale() |>
  {\(.) if(min(.) <= 0) . + abs(min(.)) + .1 else . }()

plot(lambda, type="l")
