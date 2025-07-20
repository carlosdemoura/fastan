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
  int[] rows_alpha_not_zero(int k, int omit_alpha0, int semi_conf, int[,] group_lim, int n_fac, int n_row) {
    if ((omit_alpha0 == 1) && (semi_conf == 0)) {
      int size = group_lim[k, 2] - group_lim[k, 1] + 1;
      int out[size];
      int start = group_lim[k, 1];
      for (i in 1:size)
        out[i] = start + i - 1;
      return out;

    } else if ((omit_alpha0 == 1) && (semi_conf == 1)) {
      int size1 = group_lim[k, 2] - group_lim[k, 1] + 1;
      int size2 = group_lim[n_fac + 1, 2] - group_lim[n_fac + 1, 1] + 1;
      int out[size1 + size2];

      int start1 = group_lim[k, 1];
      for (i in 1:size1)
        out[i] = start1 + i - 1;

      int start2 = group_lim[n_fac + 1, 1];
      for (i in 1:size2)
        out[size1 + i] = start2 + i - 1;

      return out;

    } else {  // omit_alpha0 == 0
      int out[n_row];
      for (i in 1:n_row)
        out[i] = i;
      return out;
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
  int<lower=0> omit_alpha0;
  int<lower=1> n_groups;
  int group_lim[n_groups,2];
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
  vector[alpha_in_group[n_row,n_fac]] alpha_;
  matrix[n_fac, n_col] lambda;
  matrix[n_row, 1] sigma2;
  matrix[n_pred, 1] pred;
}
transformed parameters {
  matrix[n_row, n_fac] alpha;
  for (i in 1:n_row) { for (j in 1:n_fac) {
    if ((alpha_in_group[i,j] == 0) && (omit_alpha0 == 1) ) {
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
    int rows_alpha[num_elements(rows_alpha_not_zero(k, omit_alpha0, semi_conf, group_lim, n_fac, n_row))];
    rows_alpha = rows_alpha_not_zero(k, omit_alpha0, semi_conf, group_lim, n_fac, n_row);
    alpha[rows_alpha,k] ~ multi_normal(alpha_mean[k][rows_alpha], alpha_cov[k][rows_alpha,rows_alpha]);
    lambda[k,] ~ multi_normal(lambda_mean[k], lambda_cov[k]);
  }
}
