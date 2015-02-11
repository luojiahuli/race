

################################################################################
# Calculates the maximum likelihood of the parameters from a multivariate
# normal distribution
#
# x       : a [n x p] matrix with the data
#
# OUTPUT
# mean : the estimated mean vector
# cov  : the estimated covariance matrix
# ll   : -2ln L (deviance)
#
# EXAMPLE
# library(mvtnorm)
# mu = c(-0.1, 0.1)
# Sigma = matrix(c(1, 0, 0, 1), 2)
# x = rmvnorm(5000, mu, Sigma)
# a = mle.mnorm(x)
################################################################################
# Author  : Thomas Debray
# Version : 3 aug 2011
# http://www.netstorm.be/home/mle#mleMnormChol
################################################################################
mle.mnorm <- function(x)
{
  # Define the log likelihood function
  logl <- function (theta, x)
  {
    mu = theta[1:dim(x)[2]]
    s.vct = theta[(dim(x)[2]+1):length(theta)]
    s.cov = vector2cov(s.vct,d=dim(x)[2])
    p = nrow(s.cov)
    
    ed = eigen(s.cov, symmetric = TRUE)
    ev = ed$values
    if (!all(ev >= -1e-06 * abs(ev[1])))
      stop("The covariance matrix is not positive definite")
    ss = if (!is.matrix(mu)) {
      x - rep(mu, each = nrow(x))
    } else {
      x - mu
    }
    inv.Sigma = ed$vectors %*% (t(ed$vectors)/ev)
    quad = 0.5 * rowSums((ss %*% inv.Sigma) * ss)
    fact = -0.5 * (p * log(2 * pi) + sum(log(ev)))
    
    -sum(fact - quad)
  }
  
  # Parameterize a covariance matrix into a vector
  cov2vector <- function (sigma)
  {
    T = dim(sigma)[1]
    sv.dim = ((T*T)+T)/2
    sv = array(NA,dim=sv.dim)
    
    k=1
    for (i in 1:T)
    {
      for (j in i:T)
      {
        sv[k] = sigma[j,i]
        k = k+1
      }
    }
    sv
  }
  
  # Reconverts a vectorized covariance matrix
  vector2cov <- function (vct,d)
  {
    sigma = array(NA,dim=c(d,d))
    indexes = array(NA,dim=c(length(vct),3))
    k=1
    for (i in 1:d)
    {
      for (j in i:d)
      {
        indexes[k,1] = i
        indexes[k,2] = j
        indexes[k,3] = vct[k]
        k = k+1
      }
    }
    for (i in 1:dim(indexes)[1])
    {
      sigma[indexes[i,1],indexes[i,2]] = indexes[i,3]
      sigma[indexes[i,2],indexes[i,1]] = indexes[i,3]
    }
    sigma
  }
  
  # Define initial values for the parameters
  m.start = array(0,dim=dim(x)[2])
  s.start = array(0,dim=c(dim(x)[2],dim(x)[2]))
  diag(s.start)=1
  s.vct = cov2vector(s.start)
  
  # Calculate the maximum likelihood
  theta.start = c(m.start,s.vct)
  mle = optim(theta.start,logl,x=x,hessian=F,method="L-BFGS-B")
  mle.mu  = mle$par[1:length(m.start)]
  mle.cov = vector2cov(mle$par[(length(m.start)+1):length(mle$par)],
                       d=length(m.start))
  mle.ll = 2*mle$value
  out <- list(mean=mle.mu,cov=mle.cov,ll=mle.ll)
}
################################################################################
