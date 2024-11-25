library(MASS)
library(GuidedSparseKmeans)
library(aricode)
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
am = generate_am(20)

mu1m = am*theta[1] + rnorm(20)
mu2m = am*theta[2] + rnorm(20)
mu3m = am*theta[3] + rnorm(20)

s2 = 9



X1m = mvrnorm(n = N[1], mu = mu1m, Sigma = s2*diag(20))
X2m = mvrnorm(n = N[2], mu = mu2m, Sigma = s2*diag(20))
X3m = mvrnorm(n = N[3], mu = mu3m, Sigma = s2*diag(20))

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

V = 4

R = 20

for(v in 1:4){
nr = rpois(R, 20)

mu1mr = generate_am(20)*theta[1] + rnorm(20)
mu2mr = generate_am(20)*theta[2] + rnorm(20)
mu3mr = generate_am(20)*theta[3] + rnorm(20)

X1mr = mvrnorm(n = N[1], mu = mu1mr, Sigma = s2*diag(20))
X2mr = mvrnorm(n = N[2], mu = mu2mr, Sigma = s2*diag(20))
X3mr = mvrnorm(n = N[3], mu = mu3mr, Sigma = s2*diag(20))



tertdata = matrix(ncol = sum(N), nrow = sum(nr))

j=1
for(k in 1:3){
  
  for(i in 1:N[k])
  {
    datam = c()
    for(m in 1:20){
      phi = 0.5*diag(nr[m]) + 0.5*matrix(1, nr[m], nr[m])
      Sigma = rinvwishart(60,phi)
      v = 1/sqrt(diag(Sigma))
      Sigma = Sigma*outer(v,v)
      
      if(k==1){
        dat = mvrnorm(n=1, mu = rep(X1mr[i,m],nr[m]), Sigma = Sigma)
      }
      else if(k==2){
        dat = mvrnorm(n=1, mu = rep(X2mr[i,m],nr[m]), Sigma = Sigma)
      }
      else{
        dat = mvrnorm(n=1, mu = rep(X3mr[i,m],nr[m]), Sigma = Sigma)
      }
      datam = c(datam, dat)
    }
    tertdata[,j] = sample(datam, length(datam))
    j = j+1
  }
}

data = rbind(data, tertdata)
}


mug = runif(n = 8000, min = 4, max = 8)

data3 = mvrnorm(n = sum(N), mu = mug, Sigma = diag(8000))

data = t(rbind(data, t(data3)))

Y = vector(length = sum(N))

Y[1:N[1]] = rnorm(N[1], mean = theta[1], sd = 64)
Y[(N[1]+1):(N[2]+N[1])] = rnorm(N[2], mean = theta[2], sd = 64)
Y[(N[2]+N[1]+1):sum(N)] = rnorm(N[3], mean = theta[3], sd = 64)


ours = GSKm(data, Y, 3, 13.5, 1.5)

theirs = GuidedSparseKmeans(data, Y, 3, 13.5, 1.5, "linear")

true = c(rep(1, N[1]), rep(2,N[2]), rep(3,N[3]))

(compare = ARI(ours$clusters, theirs[[1]]$clusters))

(ourARI = ARI(ours$clusters, true))
