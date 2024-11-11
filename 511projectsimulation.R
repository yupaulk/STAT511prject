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

