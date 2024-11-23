#Implementation of optimization algorithm

softthres = function(x,y){
    if(x-y <= 0){
        return(0)
    } else{
        return(x-y)
    }

}
findb = function(obj, s){
    b1 = 0
    b2 = max(abs(obj))

    while(b2 - b1 > 1e-4){
        w = softthres(obj, (b1 + b2)/2)/norm(softthres(obj, (b1+b2)/2))
        #If our norm is less than s, we need a smaller b and vice verse
        if(norm(w,1) < s){
            b2 = (b1+b2)/2
        } else{
            b1 = (b1+b2)/2
        }
    }

    return((b1+b2)/2)

}


GSKm = function(x,y, K, s, lam, nstart = 20, maxiter = 15){
    
    R2 = as.vector(cor(x,y))^2
    
    nonzero = order(R2, decreasing = T)[1:400]
    
    Ur = rep(0, ncol(x))
    
    Ur[1:400] = R2[nonzero]
    
    Urnormalized = abs(Ur/sum(Ur))
    
    w = Urnormalized * s
    
    w.prev = rep(1, ncol(x))
    
    iter = 0
    
    while(norm(w-w.prev, mode = "L1")/norm(w.prev, mode = "L!") > 1e-4 && iter < maxiter){
        
        
        iter = iter + 1
        
        w.prev = w
        

        #As per the discussion in the paper we can essentially do this as
        # an optimization on our original data scaled to the sqrt of the 
        #weights as the BCSS has a squared term giving us back the initial weights
        cluster.dat = sweep(x[, w!=0], 2, sqrt(w[w!=0]), "*")

        curr.means = kmeans(cluster.dat, K, nstart = nstart)

        c.cur = curr.means$cluster

        
        
    }
    
}


