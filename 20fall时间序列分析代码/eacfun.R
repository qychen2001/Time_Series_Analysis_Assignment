eacfun = function(data, order){
  l = length(data)
  data = data-mean(data)
  Y = list()
  for (i in 1:(order+1)){
    A = matrix(0, nrow = l-i+1, ncol = i)
    for (j in 1:i){
      A[,(i-j+1)] = rev(data[j:(l-i+j)])
    }
    Y[[i]] = A
  }
  M = matrix(NA, ncol = order+1, nrow = order+1)
  for (i in 0:order){
    for (j in 0:order){
      m = i
      n = j
      A = Y[[i+1]]
      if (m!=0){
        y = as.matrix(A[,1])
        X = as.matrix(A[,2:(m+1)])
        XX = X
        fit = lm(y~XX-1)
        if (n!=0){
          res = as.matrix(fit$residuals)
          Res = tail(res, -1)
          for (k in 1:n){
            y = head(y, -1)
            XX = cbind(X[1:length(y),], Res)
            fit = lm(y~XX-1)
            res = as.matrix(fit$residuals)
            Res = cbind(res, Res)
            Res = Res[2:NROW(Res),]
          }
        }
        para.AR = as.matrix(fit$coefficients[1:m])
        W = as.matrix(A[,1])-as.matrix(A[,2:(m+1)])%*%para.AR
      }else{
        W = data
      }
      acfval = acfun(data = W, lag.max = n+1, plot = FALSE)
      v = tail(acfval$acfval, 1)
      if (abs(v)>(1.96/sqrt(l-m-n))){
        M[m+1, n+1] = "x"
      }else{
        M[m+1, n+1] = "o"
      }
      #M[m+1,n+1]=abs(v)
    }
  }
  rn = rep(c("AR"), order+1)
  cn = rep(c("MA"), order+1)
  for (i in (1:(order+1))){
    rn[i] = paste(rn[i],as.character(i-1))
    cn[i] = paste(cn[i],as.character(i-1))
  }
  rownames(M) = rn
  colnames(M) = cn
  return(M)
}