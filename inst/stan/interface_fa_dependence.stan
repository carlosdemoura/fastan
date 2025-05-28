// Factor Analysis STAN Scrpit
functions {
  matrix lambda_cov(int ncol, real rho) {
    matrix[ncol, ncol] D;
    matrix[ncol, ncol] W;
    matrix[ncol, ncol] A;

    // Inicializa D como matriz identidade multiplicada por 2
    D = diag_matrix(rep_vector(2.0, ncol));
    D[1, 1] = 1.0;
    D[ncol, ncol] = 1.0;

    // Inicializa W como matriz de zeros
    W = rep_matrix(0.0, ncol, ncol);
    W[1, 2] = 1.0;
    W[ncol, ncol - 1] = 1.0;
    for (i in 2:(ncol - 1)) {
      W[i, i - 1] = 1.0;
      W[i, i + 1] = 1.0;
    }

    // Calcula a matriz A = inv(D - rho * W)
    A = inverse(D - rho * W);
    return A;
  }

  vector lambda_mean(int ncol) {
    return rep_vector(0.0, ncol);
  }
}

data {
  int<lower=1> al_row;
  int<lower=1> al_col;
  int<lower=1> al_fac;
  matrix[al_row, al_fac] var_alpha_prior;

  int<lower=1> obs_n;
  vector[obs_n] obs;
  int obs_coor[obs_n, 2];

  int<lower=0> pred_n;
  int pred_coor[max(pred_n, 1), 2];
}

parameters {
  matrix[al_row, al_fac] alpha;
  matrix[al_fac, al_col] lambda;
  matrix[al_row, 1] sigma2;
  matrix[pred_n, 1] pred;
}

model {
  matrix[al_row, al_col] alpha_lambda;
  alpha_lambda = alpha * lambda;

  // Likelihood
  for(i in 1:obs_n){
    obs[i] ~ normal( alpha_lambda[obs_coor[i,1], obs_coor[i,2]], sqrt(sigma2[obs_coor[i,1]]) );
  }

  // Missings
  if (pred_n > 0) {
    for(i in 1:pred_n){
      pred[i] ~ normal( alpha_lambda[pred_coor[i,1], pred_coor[i,2]], sqrt(sigma2[pred_coor[i,1]]) );
    }
  }

  // Priors
  for(i in 1:al_row) {
    sigma2[i,1] ~ gamma(0.1, 0.1);
  }

  for(k in 1:al_fac) {
    for(i in 1:al_row) {
      alpha[i,k] ~ normal(0, var_alpha_prior[i,k]);
    }
    lambda[k,] ~ multi_normal(lambda_mean(al_col), lambda_cov(al_col, 0.95));;
  }

}
