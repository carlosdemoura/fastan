data {
  int<lower=1> n_row;
  int<lower=1> n_col;
  int<lower=1> n_fac;

  real<lower=0> sigma2_shape;
  real<lower=0> sigma2_rate;
  vector[n_row] alpha_mean[n_fac];
  matrix[n_row, n_row] alpha_cov[n_fac];
  vector[n_col] lambda_mean[n_fac];
  matrix[n_col, n_col] lambda_cov[n_fac];

  int<lower=1> n_obs;
  vector[n_obs] obs;
  int obs_coor[n_obs, 2];

  int<lower=0> n_pred;
  int pred_coor[max(n_pred, 1), 2];
}

parameters {
  matrix[n_row, n_fac] alpha;
  matrix[n_fac, n_col] lambda;
  matrix[n_row, 1] sigma2;
  matrix[n_pred, 1] pred;
}

model {
  matrix[n_row, n_col] alpha_lambda;
  alpha_lambda = alpha * lambda;

  // Likelihood
  for(i in 1:n_obs){
    obs[i] ~ normal( alpha_lambda[obs_coor[i,1], obs_coor[i,2]], sqrt(sigma2[obs_coor[i,1]]) );
  }

  // Missings
  if (n_pred > 0) {
    for(i in 1:n_pred){
      pred[i] ~ normal( alpha_lambda[pred_coor[i,1], pred_coor[i,2]], sqrt(sigma2[pred_coor[i,1]]) );
    }
  }

  // Priors
  for(i in 1:n_row) {
    sigma2[i,1] ~ gamma(sigma2_shape, sigma2_rate);
  }
  for(k in 1:n_fac) {
    alpha[,k] ~ multi_normal(alpha_mean[k], alpha_cov[k]);
    lambda[k,] ~ multi_normal(lambda_mean[k], lambda_cov[k]);
  }

}
