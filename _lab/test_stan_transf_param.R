library(rstan)
set.seed(12345)

modelo = stan_model(model_code =
"data {
  int<lower=0> N;
  int<lower=0> P;
  matrix[N, P] X;
  vector[N] y;
  vector[P] mean;
  matrix[P, P] cov_beta;
  matrix[N, N] cov_X;
}

parameters {
  vector[P] beta_;  // Coeficientes
  beta_[1] =0;
}

transformed parameters {
  vector[P] beta;
  beta = beta_;
}


model {
  // Priors
  beta ~ multi_normal(mean, cov_beta);
  //beta_[1] ~ uniform(0, 0.1);
  //beta_[1] =0;

  // Likelihood
  y ~ multi_normal(X * beta, cov_X);
}
"
)

N <- 100  # número de observações
P <- 3    # número de preditores
X <- matrix(rnorm(N * P), N, P)  # Matriz de preditores
beta_true <- c(1, -2, 3)  # coeficientes verdadeiros
y <- X %*% beta_true + rnorm(N)

cov_beta = diag(1, P)
#cov_beta[1,1] = 0
fit <-
  sampling(
    modelo,
    data = list(N = N, P = P, X = X, y = c(y), mean = rep(0, P), cov_beta = cov_beta, cov_X = diag(1, N)),
    iter = 1000,
    pars = c("beta", "beta_"),
    #pars = c("beta_"),
    chain=1
    )

rstan::traceplot(fit, par="beta")

print(fit)

beta_samples <- extract(fit)$beta
apply(beta_samples, c(2, 3), mean)


