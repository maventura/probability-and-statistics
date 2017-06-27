Bmed<-function(X)
{
    sX = sort(X)
    n = length(X)
    median = X[n/2]
    return(2*median)
}

Bmom<-function(X)
{
    mean = 0
    for(i in 1:length(X))
    { 
        mean = mean + X[i];
    }
    mean = mean/length(X)
    return(2*mean)
}

Bmv<-function(X)
{
    return(max(X))
}

simulacion_mom<-function(b,n)
{
    
    nrep = 1000;
    Bmoms = double();
    X = double();
    
    for (i in 1:nrep){
        X = runif(n, 0.0, b);
        Bmoms[i] <- Bmom(X);
    }
    
    momMean = mean(Bmoms);
    momErr = abs(b-momMean);
    momVar = var(Bmoms)
    ECMBmom = momVar + (momErr^2);
    return(ECMBmom);
}

simulacion_med<-function(b,n)
{
    
    nrep = 1000;
    Bmeds = double();
    X = double();
    
    for (i in 1:nrep){
        X = runif(n, 0.0, b);
        Bmeds[i] <- Bmed(X);
        
    }
    
    medMean = mean(Bmeds);
    medErr = abs(b-medMean);
    medVar = var(Bmeds);
    ECMBmed = medVar + (medErr^2);
    return(ECMBmed);
}

simulacion_mv<-function(b,n)
{
    
    nrep = 1000;
    Bmvs = double();
    X = double();
    
    for (i in 1:nrep){
        X = runif(n, 0.0, b);
        Bmvs[i] <- Bmv(X);
    }
    
    mvMean = mean(Bmvs);
    mvErr = abs(b-mvMean);
    mvVar = var(Bmvs);
    ECMBmv  = mvVar + (mvErr^2);
    return(ECMBmv);
}