// Factor Analysis STAN Scrpit
data{
  int<lower=1> al_row;
  int<lower=1> al_col;
  int<lower=1> al_fac;
  int<lower=1> obs_row;
  int<lower=1> obs_col;
  vector[obs_row] obs_arg;
  int obs_coor[obs_row, 2];
  matrix[al_row, al_fac] var_alpha_prior;
  int sentinel;
}

transformed data {
  vector[obs_row] obs;

  for(i in 1:obs_row) {
    obs[i] = obs_arg[i];
  }
}

parameters{
  matrix[al_row, al_fac] alpha;
  matrix[al_fac, al_col] lambda;
  matrix[al_row, 1] sigma2;
  //matrix[lines, columns] pred2;
}

model{

  matrix[al_row, al_col] alpha_lambda;
  alpha_lambda = alpha * lambda;

  // Likelihood
  for(i in 1:obs_row){
    obs[i] ~ normal( alpha_lambda[obs_coor[i,1], obs_coor[i,2]], sqrt( sigma2[obs_coor[i,1]] ) );
  }

  // Priors
  for(i in 1:al_row) {
    sigma2[i,1] ~ gamma(0.1, 0.1);
  }

  for(k in 1:al_fac) {
    for(i in 1:al_row) {
      alpha[i,k] ~ normal(0, var_alpha_prior[i,k]);
    }
    for(j in 1:al_col){
      lambda[k,j] ~ normal(0, 1);
    }
  }


}
