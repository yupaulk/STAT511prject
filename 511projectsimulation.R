library(MASS)
library(LaplacesDemon)
K = 3

N = rpois(K,100)

M = 20

n = rpois(M,20)

theta = 2 + 2*seq(1,3,1)

generate_am = function(iter){
  randoms = c()
  for(i in 1:iter){
  z = rnorm(1)
  if(z < 0){
    randoms[i] = runif(1,-2,-0.2)
  } else{
    randoms[i] = runif(1,0.2,2)
  }
  }
  return(randoms)
}

mu1m = generate_am(20)*theta[1] + rnorm(20)
mu2m = generate_am(20)*theta[2] + rnorm(20)
mu3m = generate_am(20)*theta[3] + rnorm(20)

sigma1 = 3



X1m = mvrnorm(n = N[1], mu = mu1m, Sigma = 3*diag(20))
X2m = mvrnorm(n = N[2], mu = mu2m, Sigma = 3*diag(20))
X3m = mvrnorm(n = N[3], mu = mu3m, Sigma = 3*diag(20))

data = matrix(ncol = sum(N), nrow = sum(n))
j=1
for(k in 1:3){
  
  for(i in 1:N[k])
    {
    datam = c()
    for(m in 1:20){
      phi = 0.5*diag(n[m]) + 0.5*matrix(1, n[m], n[m])
      Sigma = rinvwishart(60,phi)
      v = 1/sqrt(diag(Sigma))
      Sigma = Sigma*outer(v,v)
    
    if(k==1){
        dat = mvrnorm(n=1, mu = rep(X1m[i,m],n[m]), Sigma = Sigma)
    }
    else if(k==2){
      dat = mvrnorm(n=1, mu = rep(X2m[i,m],n[m]), Sigma = Sigma)
    }
    else{
      dat = mvrnorm(n=1, mu = rep(X3m[i,m],n[m]), Sigma = Sigma)
    }
    datam = c(datam, dat)
    }
    data[,j] = datam
    j = j+1
  }
}




