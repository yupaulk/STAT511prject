#Implementation of optimization algorithm
library(pracma)

#Bisection method to find the proper b, initializing at endpoints
findb = function(obj, s, tol){
    b1 = 0
    b2 = max(abs(obj))

    while(b2 - b1 > tol){
        w = pmax(obj - (b1 + b2)/2, 0)/Norm(pmax(obj - (b1+b2)/2, 0))
        #If our norm is less than s, we need a smaller b and vice verse
        if(Norm(w,p=1) < s){
            b2 = (b1+b2)/2
        } else{
            b1 = (b1+b2)/2
        }
    }

    return((b1+b2)/2)

}

SS = function(X, clust){
    s.X = scale(X, scale = F)
    tscols = colSums(s.X^2)
    
    wcsscols = rep(0,ncol(X))
    
    for(i in unique(clust)){
        redind = which(clust == i)
        
        if(length(redind) == 1){
            wcsscols = wcsscols + scale(X[redind,], center = T, scale = F)^2
        } else{
            wcsscols = wcsscols + colSums(scale(X[redind,], center = T, scale = F)^2)
        }
    }
    
    bcsscols = tscols - wcsscols
    
    return(list(wcsscol = wcsscols, tsscol = tscols, bcsscol = bcsscols, wcss = sum(wcsscols), tss = sum(tscols), bcss = sum(bcsscols)))
    
}


GSKm = function(x,y, K, s, lam, nstart = 20, maxiter = 15, tol = 1e-6){
    
    R2 = as.vector(cor(x,y))^2
    
    nonzero = order(R2, decreasing = T)[1:400]
    
    Ur = rep(0, ncol(x))
    
    Ur[nonzero] = R2[nonzero]
    
    Urnormalized = abs(Ur/sum(Ur))
    
    w = Urnormalized * s
    
    w.prev = rep(0, ncol(x))
    
    iter = 0
    
    while(Norm(w-w.prev, p = 1)/Norm(w.prev, p = 1) > tol && iter < maxiter){
        
        
        iter = iter + 1
        
        w.prev = w
        

        #As per the discussion in the paper we can essentially do this as
        # an optimization on our original data scaled to the sqrt of the
        #weights as the BCSS has a squared term giving us back the initial weights
        cluster.dat = sweep(x[, w!=0], 2, sqrt(w[w!=0]), "*")

        curr.means = kmeans(cluster.dat, K, nstart = nstart)

        c.cur = curr.means$cluster

        SOS = SS(x,c.cur)
        
        obj = SOS$bcsscol/SOS$tsscol + lam*R2
        
        b = findb(obj, s, tol)
        
        w = pmax(obj - b,0)/Norm(pmax(obj-b,0))
        
    }
    return(list(clusters = c.cur, weights = w))
}



