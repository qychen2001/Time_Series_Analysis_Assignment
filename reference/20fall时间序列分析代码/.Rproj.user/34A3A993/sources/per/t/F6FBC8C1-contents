ulse.ar1 = function(data, intercept = T,
                    tol = 1e-04,
                    maxstep = 1e+04,
                    LearningRate = 1){
  if (intercept==T){
    initial.ar = 0
    initial.mu = 0
    l = length(data)
    G = 0
    e = vector("numeric", length = l)
    for (i in 2:l){
      e[i] = data[i]-
        initial.ar*data[i-1]+initial.mu*(initial.ar-1)
    }
    L.new = sum(e^2)+(1-initial.ar^2)*(data[1]-initial.mu)^2
    dEdAR = matrix(0, ncol = l, nrow = 1)
    dEdMU = matrix(0, ncol = l, nrow = 1)
    for (i in 2:l){
      dEdAR[i] = initial.mu-data[i-1]
      dEdMU[i] = initial.ar-1
    }
    dE = rbind(dEdAR, dEdMU)
    de = rbind(-2*initial.ar*(data[1]-initial.mu)^2,
               2*(1-initial.ar^2)*(initial.mu-data[1]))
    d = 2*dE%*%e+de
    G = G+sum(d^2)
    delta = -d/sqrt(G)*LearningRate
    p = 0
    ar.new = initial.ar+delta[1]
    mu.new = initial.mu+delta[2]
    while (T){
      e = vector("numeric", length = l)
      for (i in 2:l){
        e[i] = data[i]-ar.new*data[i-1]+mu.new*(ar.new-1)
      }
      L = L.new
      L.new = sum(e^2)+(1-ar.new^2)*(data[1]-mu.new)^2
      if (abs(L-L.new)<tol&sum(d^2)<tol){
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
      dE = rbind(dEdAR, dEdMU)
      de = rbind(-2*ar.new*(data[1]-mu.new),
                 2*(1-ar.new^2)*(mu.new-data[1]))
      d = 2*dE%*%e+de
      G = G+sum(d^2)
      delta = -d/sqrt(G)*LearningRate
      ar.new = ar.new+delta[1]
      mu.new = mu.new+delta[2]
      p = p+1
      if (p>maxstep){
        para = c(ar1 = ar.new, intercept = mu.new)
        print(as.symbol("Number of iterations exceeded options."))
        break
      }
    }
  }else{
    initial.ar = 0
    l = length(data)
    G = 0
    e = vector("numeric", length = l)
    for (i in 2:l){
      e[i] = data[i]-initial.ar*data[i-1]
    }
    L.new = sum(e^2)+(1-initial.ar^2)*data[1]^2
    dEdAR = matrix(0, ncol = l, nrow = 1)
    for (i in 2:l){
      dEdAR[i] = -data[i-1]
    }
    de = -2*initial.ar*data[1]^2
    d = 2*dEdAR%*%e+de
    G = G+d^2
    delta = -d/sqrt(G)*LearningRate
    p = 0
    ar.new = initial.ar+delta
    while (T){
      e = vector("numeric", length = l)
      for (i in 2:l){
        e[i] = data[i]-ar.new*data[i-1]
      }
      L = L.new
      L.new = sum(e^2)+(1-ar.new^2)*data[1]^2
      if (abs(L-L.new)<tol&sum(d^2)<tol){
        para = c(ar1 = ar.new)
        print(as.symbol("Local minimum found."))
        break
      }
      dEdAR = matrix(0, ncol = l, nrow = 1)
      for (i in 2:l){
        dEdAR[,i] = -data[i-1]
      }
      de = -2*ar.new*data[1]
      d = 2*dEdAR%*%e+de
      G = G+d^2
      delta = -d/sqrt(G)*LearningRate
      ar.new = ar.new+delta
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