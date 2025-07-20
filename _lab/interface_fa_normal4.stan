functions {
  // seq
  int[] seq(int a, int b) {
    int len = b - a + 1;
    int result[len];
    for (i in 1:len) {
      result[i] = a + i - 1;
    }
    return result;
  }
  // concat
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
  // rows_alpha_not_zero
  int[] rows_alpha_not_zero(int k, int alpha_nullify, int semi_conf, int[,] group_lim, int[] group_size, int n_fac, int n_row) {
    if ((alpha_nullify == 1) && (semi_conf == 0)) {
      int out[group_size[k]];
      int start = group_lim[k, 1];
      for (i in 1:group_size[k])
        out[i] = start + i - 1;
      return out;
    } else if ((alpha_nullify == 1) && (semi_conf == 1)) {
      int size1 = group_size[k];
      int size2 = group_size[n_fac + 1];
      int out[size1 + size2];
      int start1 = group_lim[k, 1];
      for (i in 1:size1)
        out[i] = start1 + i - 1;
      int start2 = group_lim[n_fac + 1, 1];
      for (i in 1:size2)
        out[size1 + i] = start2 + i - 1;
      return out;
    } else {  // alpha_nullify == 0
      int out[n_row];
      for (i in 1:n_row)
        out[i] = i;
      return out;
    }
  }
  // rows_alpha_not_zero_size
  int rows_alpha_not_zero_size(int k, int alpha_nullify, int semi_conf, int[] group_size, int n_fac, int n_row) {
    if ((alpha_nullify == 1) && (semi_conf == 0)) {
      return group_size[k];
    } else if ((alpha_nullify == 1) && (semi_conf == 1)) {
      return group_size[k] + group_size[n_fac + 1];
    } else {
      return n_row;
    }
  }
}
data {
  // size args
  int<lower=1> n_row;
  int<lower=1> n_col;
  int<lower=1> n_fac;
  // prior args
  real<lower=0> sigma2_shape;
  real<lower=0> sigma2_rate;
  vector[n_row] alpha_mean[n_fac];
  matrix[n_row, n_row] alpha_cov[n_fac];
  vector[n_col] lambda_mean[n_fac];
  matrix[n_col, n_col] lambda_cov[n_fac];
  // alpha nullify args
  int<lower=0> semi_conf;
  int<lower=0> alpha_nullify;
  int<lower=1> n_groups;
  int<lower=0> n_alpha;
  int group_lim[n_groups, 2];
  int group_size[n_groups];
  int alpha_in_group[n_row, n_fac];
  // obs args
  int<lower=1> n_obs;
  vector[n_obs] obs;
  int obs_coor[n_obs, 2];
  // pred args
  int<lower=0> n_pred;
  int pred_coor[max(n_pred, 1), 2];
}
parameters {
  vector[n_alpha] alpha_;
  matrix[n_fac, n_col] lambda;
  matrix[n_row, 1] sigma2;
  matrix[n_pred, 1] pred;
}
transformed parameters {
  matrix[n_row, n_fac] alpha;
  for (i in 1:n_row) { for (j in 1:n_fac) {
    if ((alpha_in_group[i,j] == 0) && (alpha_nullify == 1) ) {
      alpha[i,j] = 0;
    } else {
      alpha[i,j] = alpha_[alpha_in_group[i,j]];
    }
  }}
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
    int rows_alpha[rows_alpha_not_zero_size(k, alpha_nullify, semi_conf, group_size, n_fac, n_row)];
    rows_alpha = rows_alpha_not_zero(k, alpha_nullify, semi_conf, group_lim, group_size, n_fac, n_row);
    alpha[rows_alpha,k] ~ multi_normal(alpha_mean[k][rows_alpha], alpha_cov[k][rows_alpha,rows_alpha]);
    lambda[k,] ~ multi_normal(lambda_mean[k], lambda_cov[k]);
  }
}
