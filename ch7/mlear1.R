MLE.AR1 = function(data, intercept = T,
                   tol = 1e-04,
                   maxstep = 1e+04,
                   LearningRate = 1){
  #arima,order=c(1,0,0),method="ML"
  if (intercept==T){
    initial.ar = 0
    initial.mu = 0
    initial.sigma2 = 1
    l = length(data)
    G = 0
    e = vector("numeric", length = l)
    for (i in 2:l){
      e[i] = data[i]-
        initial.ar*data[i-1]+initial.mu*(initial.ar-1)
    }
    L.new = -(sum(e^2)+
                (1-initial.ar^2)*(data[1]-initial.mu)^2)/2/initial.sigma2+
      -l/2*log(2*pi)-l/2*log(initial.sigma2)+1/2*log(1-initial.ar^2)
    L.new = -L.new
    dEdAR = matrix(0, ncol = l, nrow = 1)
    dEdMU = matrix(0, ncol = l, nrow = 1)
    for (i in 2:l){
      dEdAR[i] = initial.mu-data[i-1]
      dEdMU[i] = initial.ar-1
    }
    dLdAR = -initial.ar/(1-initial.ar^2)+
      initial.ar*(data[1]-initial.mu)^2/initial.sigma2-
      dEdAR%*%e/initial.sigma2
    dLdMU = (1-initial.ar^2)*(data[1]-initial.mu)/initial.sigma2-
      dEdMU%*%e/initial.sigma2
    dLdS2 = -l/2/initial.sigma2+
      (sum(e^2)+(1-initial.ar^2)*(data[1]-initial.mu)^2)/2/initial.sigma2^2
    dL = c(-dLdAR, -dLdMU, -dLdS2)
    G = G+sum(dL^2)
    delta = -dL/sqrt(G)*LearningRate
    ar.new = initial.ar+delta[1]
    mu.new = initial.mu+delta[2]
    s2.new = initial.sigma2+delta[3]
    p = 0
    while (T){
      e = vector("numeric", length = l)
      for (i in 2:l){
        e[i] = data[i]-ar.new*data[i-1]+mu.new*(ar.new-1)
      }
      L = L.new
      L.new = -(sum(e^2)+
                  (1-ar.new^2)*(data[1]-mu.new)^2)/2/s2.new+
        -l/2*log(2*pi)-l/2*log(s2.new)+1/2*log(1-ar.new^2)
      L.new = -L.new
      if (abs(L-L.new)<tol&sum(dL^2)<tol){
        para = c(ar1 = ar.new, intercept = mu.new)
        print(as.symbol("Local minimum found."))
        break
      }
      dEdAR = matrix(0, ncol = l, nrow = 1)
      dEdMU = matrix(0, ncol = l, nrow = 1)
      for (i in 2:l){
        dEdAR[,i] = mu.new-data[i-1]
        dEdMU[i] = ar.new-1
      }
      dLdAR = -ar.new/(1-ar.new^2)+
        ar.new*(data[1]-mu.new)^2/s2.new-
        dEdAR%*%e/s2.new
      dLdMU = (1-ar.new^2)*(data[1]-mu.new)/s2.new-
        dEdMU%*%e/s2.new
      dLdS2 = -l/2/s2.new+
        (sum(e^2)+(1-initial.ar^2)*(data[1]-initial.mu)^2)/2/s2.new^2
      dL = c(-dLdAR, -dLdMU, -dLdS2)
      G = G+sum(dL^2)
      delta = -dL/sqrt(G)*LearningRate
      ar.new = ar.new+delta[1]
      mu.new = mu.new+delta[2]
      s2.new = s2.new+delta[3]
      p = p+1
      if (p>maxstep){
        para = c(ar1 = ar.new, intercept = mu.new)
        print(as.symbol("Number of iterations exceeded options."))
        break
      }
    }
  }else{
    initial.ar = 0
    initial.sigma2 = 1
    l = length(data)
    G = 0
    e = vector("numeric", length = l)
    for (i in 2:l){
      e[i] = data[i]-initial.ar*data[i-1]
    }
    L.new = -(sum(e^2)+
                (1-initial.ar^2)*data[1]^2)/2/initial.sigma2+
      -l/2*log(2*pi)-l/2*log(initial.sigma2)+1/2*log(1-initial.ar^2)
    L.new = -L.new
    dEdAR = matrix(0, ncol = l, nrow = 1)
    for (i in 2:l){
      dEdAR[i] = -data[i-1]
    }
    dLdAR = -initial.ar/(1-initial.ar^2)+
      initial.ar*data[1]^2/initial.sigma2-
      dEdAR%*%e/initial.sigma2
    dLdS2 = -l/2/initial.sigma2+
      (sum(e^2)+(1-initial.ar^2)*data[1]^2)/2/initial.sigma2^2
    dL = c(-dLdAR, -dLdS2)
    G = G+sum(dL^2)
    delta = -dL/sqrt(G)*LearningRate
    ar.new = initial.ar+delta[1]
    s2.new = initial.sigma2+delta[2]
    p = 0
    while (T){
      e = vector("numeric", length = l)
      for (i in 2:l){
        e[i] = data[i]-ar.new*data[i-1]
      }
      L = L.new
      L.new = -(sum(e^2)+
                  (1-ar.new^2)*data[1]^2)/2/s2.new+
        -l/2*log(2*pi)-l/2*log(s2.new)+1/2*log(1-ar.new^2)
      L.new = -L.new
      if (abs(L-L.new)<tol&sum(dL^2)<tol){
        para = c(ar1 = ar.new)
        print(as.symbol("Local minimum found."))
        break
      }
      dEdAR = matrix(0, ncol = l, nrow = 1)
      for (i in 2:l){
        dEdAR[,i] = -data[i-1]
      }
      dLdAR = -ar.new/(1-ar.new^2)+
        ar.new*data[1]^2/s2.new-
        dEdAR%*%e/s2.new
      dLdS2 = -l/2/s2.new+
        (sum(e^2)+(1-initial.ar^2)*data[1]^2)/2/s2.new^2
      dL = c(-dLdAR, -dLdS2)
      G = G+sum(dL^2)
      delta = -dL/sqrt(G)*LearningRate
      ar.new = ar.new+delta[1]
      s2.new = s2.new+delta[2]
      p = p+1
      if (p>maxstep){
        para = c(ar1 = ar.new)
        print(as.symbol("Number of iterations exceeded options."))
        break
      }
    }
  }
  return(para)
}