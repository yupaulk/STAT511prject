#Implementation of optimization algorithm
GSKm = function(x,y, K, s, lam, nstart = 20, maxiter = 15){
    
    R2 = as.vector(cor(x,y))^2
    
    nonzero = order(R2, decreasing = F)[1:400]
    
    Ur = rep(0, ncols(x))
    
    Ur[1:400] = R2[nonzero]
    
    Urnormalized = abs(Ur/sum(Ur))
    
    w = Urnormalized * s
    
    w.prev = rep(1, ncol(x))
    
    iter = 0
    
    while(norm(w-w.prev, mode = "L1")/norm(w.prev, mode = "L!") > 1e-4 && iter < maxiter){
        
        
        iter = iter + 1
        
        w.prev = w
        
        
    }
    
}
