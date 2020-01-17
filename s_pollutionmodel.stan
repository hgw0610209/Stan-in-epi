data 
{
int K; // number of covariates
int<lower=0> Nrow; //number of paired observations 
int<lower=0> Ncol; //number of pollutant types
int<lower=0> Ncomp; // number of non-missing observations
int<lower=0> Nmiss; // number of missing observations
real dat_complete[Ncomp];   // Vector of non-missing observations
int ind_pres[Ncomp, 2];     // non-missing value array indices
int ind_miss[Nmiss, 2];     // missing value array indices
matrix[Nrow,K] data_cov[Ncol];// the design matrices
}

parameters 
{
vector[K] betas[Ncol]; // regression parameters
cholesky_factor_corr[Ncol] L1; //cholesky factor of covariance
real ymiss[Nmiss]; // unknown missing observations   
vector<lower=0>[Ncol] sigma; // variance parameter for regression parameters
}

transformed parameters 
{
  vector[Ncol] mu[Nrow]; // mean of the normal likelihood model
  vector[Ncol] y[Nrow];  // both non-missing and missing observations 
  
  for(n in 1:Ncomp) 
   {
    y[ind_pres[n,1]][ind_pres[n,2]] = dat_complete[n]; 
    // assign non-missing observations to y
   }
  for(n in 1:Nmiss)
  {
   y[ind_miss[n,1]][ind_miss[n,2]] = ymiss[n]; 
   // assign missing observations to y
  }
  for(n in 1:Ncol)
  {
    for(i in 1:Nrow)
    {
    mu[i,n] = data_cov[n,i,] * betas[n,]; 
    //calculate the mean of the normal likelihood model
    }
  }
}

model 
{
  L1 ~ lkj_corr_cholesky(1); //prior
  
  for(i in 1:Ncol)
  {
  betas[i] ~ normal(0,10); //prior
  }
  sigma ~ exponential(0.5);//prior
  for(i in 1:Nrow)
   {
    y[i] ~ multi_normal(mu[i,], quad_form_diag(tcrossprod(L1), sigma));// likelihood model
   }
}
