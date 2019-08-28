### a function to estimate  E(Y1-Y2|Y1>a) by bootstrap 
# the two columns of dt must be named as Y1 and Y2
Delta_RSRR_extreme_by_bootstrap = function(dt,a,num_boot_replication=2000,seed=123)
{ 
  set.seed(seed)
  nobs = nrow(dt)
  Delta_RSRR_extreme = rep(NA,num_boot_replication)
  for(k in 1:num_boot_replication)
  {
    index = sample(1:nobs,size=nobs,replace=TRUE)
    bootstrap_sample = dt[index]
    bootstrap_sample[,Y1_minus_Y2:=(Y1-Y2)]
    Delta_RSRR_extreme[k] = mean(bootstrap_sample[Y1>a,Y1_minus_Y2])- mean(bootstrap_sample[,Y1_minus_Y2])
  }
  list(Delta_RSRR_extreme_estimate=median(Delta_RSRR_extreme),Delta_RSRR_extreme_L95=quantile(Delta_RSRR_extreme,probs = 0.025),Delta_RSRR_extreme_H95=quantile(Delta_RSRR_extreme,probs = 0.975),Delta_RSRR_extreme=Delta_RSRR_extreme)
}


#### a toy data example 
library(data.table)
library(MASS)
set.seed(1)
rou=0.4; sigma2=0.07^2
dt = mvrnorm(n = 4000, mu=c(1,1), Sigma=cbind(c(sigma2,rou*sigma2),c(rou*sigma2,sigma2)), tol = 1e-6, empirical = FALSE, EISPACK = FALSE); dt=as.data.table(dt);   
names(dt)=c('Y1','Y2')
dt[,Y1:=(Y1*19.7)]
dt[,Y2:=(Y2*17.0)]

bivariate_normal_test = Delta_RSRR_extreme_by_bootstrap(dt,a=19.7)
round(bivariate_normal_test$Delta_RSRR_extreme_estimate,4)  
round(bivariate_normal_test$Delta_RSRR_extreme_L95,4)  
round(bivariate_normal_test$Delta_RSRR_extreme_H95,4)  

  