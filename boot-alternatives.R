
# function to compute confidence interval 
# by default, a bootstrap confidence interval is computed
# statistic is a function that computes the desired statistic. tau is a function specifying the rate of convergence
# b is the resample size. R is the number of replicates

resample.ci <- function(x, statistic, tau,  b = length(x), alpha = 0.05, R = 1000, replace = TRUE){
  theta.hat <- statistic(x) # observed value of the statistic
  theta.star <- numeric(R)
  for (r in 1:R){
    theta.star[r] <- tau(b)*(statistic(sample(x, size = b, replace = replace)) - theta.hat)
  }
  ci <- theta.hat - quantile(theta.star, probs = c(1-alpha/2, alpha/2))/tau(n)
  return(ci)
}

# function to compute multiple confidence intervals
# the output is a matrix with columns as follows:
#   lower limit, upper limit, indicator of if the true value is in the interval (1) or not (0), interval length
# statistic, tau, and b are as before

multiple.ci <- function(n, M = 1000, true.value = 0, dist = c('t','unif'), statistic, tau, b, replace){
  result <- matrix(nrow = M, ncol = 4)
  colnames(result) <- c('lwr', 'upr', 'y/n', 'len')
  
  if (dist == 't'){
    for (m in 1:M){
      x <- rt(n, df = 2) # generate data
      ci <- resample.ci(x, statistic = statistic, tau = tau, b = b, replace = replace) # compute confidence interval
      result[m,'lwr'] <- ci[1]
      result[m,'upr'] <- ci[2]
      # is the true value in the interval?
      result[m,'y/n'] <- ifelse( (result[m,'lwr'] < true.value && true.value < result[m,'upr']), 1, 0)
      result[m,'len'] <- result[m,'upr'] - result[m,'lwr']
    }
  }
  else{
    for (m in 1:M){
      x <- runif(n, min = 0, max = true.value) # generate data
      ci <- resample.ci(x, statistic = statistic, tau = tau, b = b, replace = replace) # compute confidence interval
      result[m,'lwr'] <- ci[1]
      result[m,'upr'] <- ci[2]
      # is the true value in the interval?
      result[m,'y/n'] <- ifelse( (result[m,'lwr'] < true.value && true.value < result[m,'upr']), 1, 0)
      result[m,'len'] <- result[m,'upr'] - result[m,'lwr']
    }
  }
  
  return(result)
}


# Example use of the above functions. Here we consider X1,...,Xn i.i.d. t distribution with 
# 2 degrees of freedom and a sample size of 100


n <- 100


# m out of n bootstrap
b <- floor( c(n^(1/6), n^(1/3), n^(1/2), n^(2/3), n^(5/6)) )

mboot1 <- multiple.ci(n, dist = 't', statistic = mean, tau = sqrt, b = b[1], replace = TRUE)
mboot2 <- multiple.ci(n, dist = 't', statistic = mean, tau = sqrt, b = b[2], replace = TRUE)
mboot3 <- multiple.ci(n, dist = 't', statistic = mean, tau = sqrt, b = b[3], replace = TRUE)
mboot4 <- multiple.ci(n, dist = 't', statistic = mean, tau = sqrt, b = b[4], replace = TRUE)
mboot5 <- multiple.ci(n, dist = 't', statistic = mean, tau = sqrt, b = b[5], replace = TRUE)

# average coverage and average length
mboot.cov <- c(mean(mboot1[,'y/n']), mean(mboot2[,'y/n']), mean(mboot3[,'y/n']), mean(mboot4[,'y/n']), mean(mboot5[,'y/n']))
mboot.len <- c(mean(mboot1[,'len']), mean(mboot2[,'len']), mean(mboot3[,'len']), mean(mboot4[,'len']), mean(mboot5[,'len']))


# subsampling

sub1 <- multiple.ci(n, dist = 't', statistic = mean, tau = sqrt, b = b[1], replace = FALSE)
sub2 <- multiple.ci(n, dist = 't', statistic = mean, tau = sqrt, b = b[2], replace = FALSE)
sub3 <- multiple.ci(n, dist = 't', statistic = mean, tau = sqrt, b = b[3], replace = FALSE)
sub4 <- multiple.ci(n, dist = 't', statistic = mean, tau = sqrt, b = b[4], replace = FALSE)
sub5 <- multiple.ci(n, dist = 't', statistic = mean, tau = sqrt, b = b[5], replace = FALSE)

# average coverage and average length
sub.cov <- c(mean(sub1[,'y/n']), mean(sub2[,'y/n']), mean(sub3[,'y/n']), mean(sub4[,'y/n']), mean(sub5[,'y/n']))
sub.len <- c(mean(sub1[,'len']), mean(sub2[,'len']), mean(sub3[,'len']), mean(sub4[,'len']), mean(sub5[,'len']))

