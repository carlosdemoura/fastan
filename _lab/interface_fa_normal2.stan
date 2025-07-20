functions {
  int[] seq(int a, int b) {
    int len = b - a + 1;
    int result[len];
    for (i in 1:len) {
      result[i] = a + i - 1;
    }
    return result;
  }

  int[] concat(int[] a, int[] b) {
    int n1 = size(a);
    int n2 = size(b);
    int result[n1 + n2];
    for (i in 1:n1) {
      result[i] = a[i];
    }
    for (i in 1:n2) {
      result[n1 + i] = b[i];
    }
    return result;
  }

  // vector concat(vector a, vector b) {
  //   int n1 = num_elements(a);
  //   int n2 = num_elements(b);
  //   vector[n1 + n2] result;
  //
  //   for (i in 1:n1) {
  //     result[i] = a[i];
  //   }
  //
  //   for (i in 1:n2) {
  //     result[n1 + i] = b[i];
  //   }
  //
  //   return result;
  // }
}

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

  int<lower=0> semi_conf;
  int<lower=0> alpha_nullify;
  int<lower=1> n_groups;
  int group_lim[n_groups, 2];
  int group_size[n_groups];

  int<lower=1> n_obs;
  vector[n_obs] obs;
  int obs_coor[n_obs, 2];

  int<lower=0> n_pred;
  int pred_coor[max(n_pred, 1), 2];
}

parameters {
  matrix[n_row, n_fac] alpha_;
  matrix[n_fac, n_col] lambda;
  matrix[n_row, 1] sigma2;
  matrix[n_pred, 1] pred;
}

transformed parameters {
  matrix[n_row, n_fac] alpha;
  //alpha = alpha_;
  if (alpha_nullify == 1) {
    for (i in 1:n_row) {for (j in 1:n_fac) {
      if (alpha_cov[j][i,i] == .01) {
        alpha[i,j] = 0;
      } else {
        alpha[i,j] =  alpha_[i,j];
      }
    }}
  }
}

model {
  // if (alpha_nullify == 1) {
  //   for (i in 1:n_row) {for (j in 1:n_fac) {
  //     if (alpha_cov[j][i,i] == .01) {
  //       alpha_[i,j] ~ uniform(-2,2);
  //     }
  //   }}
  // }
  //to_vector(alpha_) ~ uniform(-2, 2);
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
    if ((alpha_nullify == 1) && (semi_conf == 0)) {
      int rows_alpha[group_size[k]];
      rows_alpha = seq(group_lim[k,1],group_lim[k,2]);
      alpha[rows_alpha,k] ~ multi_normal(alpha_mean[k][rows_alpha], alpha_cov[k][rows_alpha,rows_alpha]);
    } else if ((alpha_nullify == 1) && (semi_conf == 1)) {
      int rows_alpha[group_size[k] + group_size[n_fac + 1]];
      rows_alpha = concat(seq(group_lim[k,1],group_lim[k,2]), seq(group_lim[n_fac+1,1],group_lim[n_fac+1,2]));
      alpha[rows_alpha,k] ~ multi_normal(alpha_mean[k][rows_alpha], alpha_cov[k][rows_alpha,rows_alpha]);
    } else if (alpha_nullify == 0) {
      int rows_alpha[n_row];
      rows_alpha = seq(1, n_row);
      alpha[rows_alpha,k] ~ multi_normal(alpha_mean[k][rows_alpha], alpha_cov[k][rows_alpha,rows_alpha]);
    }

    //alpha[rows_alpha,k] ~ multi_normal(alpha_mean[k][rows_alpha], alpha_cov[k][rows_alpha,rows_alpha]);
    lambda[k,] ~ multi_normal(lambda_mean[k], lambda_cov[k]);
  }

}
