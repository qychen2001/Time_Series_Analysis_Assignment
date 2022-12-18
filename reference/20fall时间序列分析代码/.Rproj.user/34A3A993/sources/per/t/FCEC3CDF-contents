simARIMA <- function(AR.para = integer(0), MA.para = integer(0), diff, noise.sd = 1, ARMA.mean = 0, seq.length = 10){
  
  l = seq.length+1;
  d = diff;
  if ((length(AR.para)>0)&(length(MA.para)>0)){
    m = length(MA.para);n = length(AR.para);mode = 1
  }else if ((length(AR.para)>0)&(length(MA.para)==0)){
    n = length(AR.para);mode = 2;
  }else if ((length(AR.para)==0)&(length(MA.para)>0)){
    m = length(MA.para);mode = 3;
  }else if ((length(AR.para)==0)&(length(MA.para)==0)){
    mode = 4;
  }
  
  if (mode==1){
    e = rnorm(n = l+m-1, mean = 0,sd = noise.sd);
    X = matrix(0, nrow = l-1, ncol = m);
    for (i in 1:m){
      X[,i] = e[(m-i+1):(l+m-i-1)]
    }
    W.MA = e[(m+1):(l+m-1)]-X%*%MA.para
    
    W.AR = c(rnorm(n = n, mean = 0,sd = noise.sd), vector("numeric",length = l-1))
    p = 0;
    while (p<(l-1)){
      W.AR[n+1+p] = crossprod(rev(W.AR[(1+p):(n+p)]),AR.para)+W.MA[p+1]
      p = p+1
    }
    
    W = W.AR[(n+1):(n+l-1)]
    
  }else if (mode==2){
    e = rnorm(n = l-1, mean = 0,sd = noise.sd);
    
    W.AR = c(rnorm(n = n, mean = 0,sd = noise.sd), vector("numeric",length = l-1))
    p = 0;
    while (p<(l-1)){
      W.AR[n+1+p] = crossprod(rev(W.AR[(1+p):(n+p)]),AR.para)+e[p+1]
      p = p+1
    }
    
    W = W.AR[(n+1):(n+l-1)]
    
  }else if (mode==3){
    e = rnorm(n = l+m-1, mean = 0,sd = noise.sd);
    X = matrix(0, nrow = l-1, ncol = m);
    for (i in 1:m){
      X[,i] = e[(m-i+1):(l+m-i-1)]
    }
    W = e[(m+1):(l+m-1)]-X%*%MA.para
  }else if (mode==4){
    W = rnorm(n = l-1, mean = 0,sd = noise.sd);
  }
  W = W+ARMA.mean
  
  if (d>0){
    while (d>0){
    dY = cumsum(W);
    d = d-1;
    }
    Y = c(0, dY)
  }else{
    Y = c(0, W)
  }
  
  
  return(Y)
  
}