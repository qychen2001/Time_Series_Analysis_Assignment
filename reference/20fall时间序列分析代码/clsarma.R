clse.arma = function(data, order, intercept = T,
                     tol = 1e-04,
                     maxstep = 1e+04,
                     LearningRate = 0.1){
  #arima,method="CSS"
  if (intercept==T){
    order.ar = order[1]
    order.ma = order[2]
    initial.ar = as.numeric(integer(order[1]))
    initial.ma = as.numeric(integer(order[2]))
    initial.mu = 0
    l = length(data)
    G = 0
    if (order.ma==0){
      e = vector("numeric", length = l)
      for (i in (order.ar+1):l){
        e[i] = data[i]-
          initial.ar%*%rev(data[(i-order.ar):(i-1)])+
          initial.mu*(sum(initial.ar)-1)
      }
      L.new = sum(e^2)
      dEdAR = matrix(0, ncol = l, nrow = order.ar)
      dEdMU = matrix(0, ncol = l, nrow = 1)
      for (i in (order.ar+1):l){
        dEdAR[,i] = rep(initial.mu, order.ar)-
          rev(data[(i-order.ar):(i-1)])
        dEdMU[i] = sum(initial.ar)-1
      }
      dE = rbind(dEdAR, dEdMU)
      d = 2*dE%*%e
      G = G+sum(d^2)
      delta = -d/sqrt(G)*LearningRate
      p = 0
      ar.new = initial.ar+delta[1:order.ar]
      mu.new = initial.mu+delta[1+order.ar]
      while (T){
        e = vector("numeric", length = l)
        for (i in (order.ar+1):l){
          e[i] = data[i]-
            ar.new%*%rev(data[(i-order.ar):(i-1)])+
            mu.new*(sum(ar.new)-1)
        }
        L = L.new
        L.new = sum(e^2)
        if (abs(L-L.new)<tol&sum(d^2)<tol){
          name = NULL
          for (i in 1:order.ar){
            name[i] = paste("ar", as.character(i), sep = "")
          }
          names(ar.new) = name
          para = c(ar.new, intercept = mu.new)
          print(as.symbol("Local minimum found."))
          break
        }
        dEdAR = matrix(0, ncol = l, nrow = order.ar)
        dEdMU = matrix(0, ncol = l, nrow = 1)
        for (i in (order.ar+1):l){
          dEdAR[,i] = 
            rep(mu.new, order.ar)-rev(data[(i-order.ar):(i-1)])
          dEdMU[i] = sum(ar.new)-1
        }
        dE = rbind(dEdAR, dEdMU)
        d = 2*dE%*%e
        G = G+sum(d^2)
        delta = -d/sqrt(G)*LearningRate
        ar.new = ar.new+delta[1:order.ar]
        mu.new = mu.new+delta[1+order.ar]
        p = p+1
        if (p>maxstep){
          name = NULL
          for (i in 1:order.ar){
            name[i] = paste("ar", as.character(i), sep = "")
          }
          names(ar.new) = name
          para = c(ar.new, intercept = mu.new)
          print(as.symbol("Number of iterations exceeded options."))
          break
        }
      }
    }else if (order.ar==0){
      e = vector("numeric", length = l+order.ma)
      for (i in 1:l){
        e[i+order.ma] = data[i]+
          initial.ma%*%rev(e[i:(i+order.ma-1)])-
          initial.mu
      }
      L.new = sum(e^2)
      dEdMA = matrix(0, ncol = l+order.ma, nrow = order.ma)
      dEdMU = matrix(0, ncol = l+order.ma, nrow = 1)
      for (i in 1:l){
        dEdMA[,(i+order.ma)] = 
          dEdMA[,(i:(i+order.ma-1))]%*%rev(initial.ma)+
          rev(e[i:(i+order.ma-1)])
        dEdMU[i+order.ma] = -1+
          rev(initial.ma)%*%dEdMU[i:(i+order.ma-1)]
      }
      dEdMA = dEdMA[,(1+order.ma):(l+order.ma)]
      dEdMU = dEdMU[(1+order.ma):(l+order.ma)]
      dE = rbind(dEdMA, dEdMU)
      d = 2*dE%*%e[(1+order.ma):(l+order.ma)]
      G = G+sum(d^2)
      delta = -d/sqrt(G)*LearningRate
      p = 0
      ma.new = initial.ma+delta[1:order.ma]
      mu.new = initial.mu+delta[1+order.ma]
      while (T){
        e = vector("numeric", length = l+order.ma)
        for (i in 1:l){
          e[i+order.ma] = data[i]+
            ma.new%*%rev(e[i:(i+order.ma-1)])-
            mu.new
        }
        L = L.new
        L.new = sum(e^2)
        if (abs(L-L.new)<tol&sum(d^2)<tol){
          name = NULL
          for (i in 1:order.ma){
            name[i] = paste("ma", as.character(i), sep = "")
          }
          names(ma.new) = name
          para = c(ma.new, intercept = mu.new)
          print(as.symbol("Local minimum found."))
          break
        }
        dEdMA = matrix(0, ncol = l+order.ma, nrow = order.ma)
        dEdMU = matrix(0, ncol = l+order.ma, nrow = 1)
        for (i in 1:l){
          dEdMA[,(i+order.ma)] = 
            dEdMA[,(i:(i+order.ma-1))]%*%rev(ma.new)+
            rev(e[i:(i+order.ma-1)])
          dEdMU[i+order.ma] = -1+
            rev(ma.new)%*%dEdMU[i:(i+order.ma-1)]
        }
        dE = rbind(dEdMA, dEdMU)
        dE = dE[,(1+order.ma):(l+order.ma)]
        d = 2*dE%*%e[(1+order.ma):(l+order.ma)]
        G = G+sum(d^2)
        delta = -d/sqrt(G)*LearningRate
        ma.new = ma.new+delta[1:order.ma]
        mu.new = mu.new+delta[1+order.ma]
        p = p+1
        if (p>maxstep){
          name = NULL
          for (i in 1:order.ma){
            name[i] = paste("ma", as.character(i), sep = "")
          }
          names(ma.new) = name
          para = c(ma.new, intercept = mu.new)
          print(as.symbol("Number of iterations exceeded options."))
          break
        }
      }
    }else{
      e = vector("numeric", length = l+order.ma)
      for (i in (order.ar+1):l){
        e[i+order.ma] = data[i]+
          initial.ma%*%rev(e[i:(i+order.ma-1)])-
          initial.ar%*%rev(data[(i-order.ar):(i-1)])+
          initial.mu*(sum(initial.ar)-1)
      }
      L.new = sum(e^2)
      dEdAR = matrix(0, ncol = l+order.ma, nrow = order.ar)
      dEdMU = matrix(0, ncol = l+order.ma, nrow = 1)
      dEdMA = matrix(0, ncol = l+order.ma, nrow = order.ma)
      for (i in (order.ar+1):l){
        dEdAR[,(i+order.ma)] = 
          dEdAR[,(i:(i+order.ma-1))]%*%rev(initial.ma)+
          rep(initial.mu, order.ar)-rev(data[(i-order.ar):(i-1)])
        dEdMU[i+order.ma] = sum(initial.ar)-1+
          rev(initial.ma)%*%dEdMU[i:(i+order.ma-1)]
        dEdMA[,(i+order.ma)] = 
          dEdMA[,(i:(i+order.ma-1))]%*%rev(initial.ma)+
          rev(e[i:(i+order.ma-1)])
      }
      dEdAR = dEdAR[,(1+order.ma):(l+order.ma)]
      dEdMA = dEdMA[,(1+order.ma):(l+order.ma)]
      dEdMU = dEdMU[(1+order.ma):(l+order.ma)]
      dE = rbind(dEdAR, dEdMA, dEdMU)
      d = 2*dE%*%e[(1+order.ma):(l+order.ma)]
      G = G+sum(d^2)
      delta = -d/sqrt(G)*LearningRate
      p = 0
      ar.new = initial.ar+delta[1:order.ar]
      ma.new = initial.ma+delta[(1+order.ar):(order.ma+order.ar)]
      mu.new = initial.mu+delta[1+order.ma+order.ar]
      while (T){
        e = vector("numeric", length = l+order.ma)
        for (i in (order.ar+1):l){
          e[i+order.ma] = data[i]+
            ma.new%*%rev(e[i:(i+order.ma-1)])-
            ar.new%*%rev(data[(i-order.ar):(i-1)])+
            mu.new*(sum(ar.new)-1)
        }
        L = L.new
        L.new = sum(e^2)
        if (abs(L-L.new)<tol&sum(d^2)<tol){
          name.ar = NULL
          for (i in 1:order.ar){
            name.ar[i] = paste("ar", as.character(i), sep = "")
          }
          names(ar.new) = name.ar
          name.ma = NULL
          for (i in 1:order.ma){
            name.ma[i] = paste("ma", as.character(i), sep = "")
          }
          names(ma.new) = name.ma
          para = c(ar.new, ma.new, intercept = mu.new)
          print(as.symbol("Local minimum found."))
          break
        }
        dEdAR = matrix(0, ncol = l+order.ma, nrow = order.ar)
        dEdMA = matrix(0, ncol = l+order.ma, nrow = order.ma)
        dEdMU = matrix(0, ncol = l+order.ma, nrow = 1)
        for (i in (order.ar+1):l){
          dEdAR[,(i+order.ma)] = 
            dEdAR[,(i:(i+order.ma-1))]%*%rev(ma.new)+
            rep(mu.new, order.ar)-rev(data[(i-order.ar):(i-1)])
          dEdMU[i+order.ma] = sum(ar.new)-1+
            rev(ma.new)%*%dEdMU[i:(i+order.ma-1)]
          dEdMA[,(i+order.ma)] = 
            dEdMA[,(i:(i+order.ma-1))]%*%rev(ma.new)+
            rev(e[i:(i+order.ma-1)])
        }
        dE = rbind(dEdAR, dEdMA, dEdMU)
        dE = dE[,(1+order.ma):(l+order.ma)]
        d = 2*dE%*%e[(1+order.ma):(l+order.ma)]
        G = G+sum(d^2)
        delta = -d/sqrt(G)*LearningRate
        ar.new = ar.new+delta[1:order.ar]
        ma.new = ma.new+delta[(1+order.ar):(order.ma+order.ar)]
        mu.new = mu.new+delta[1+order.ma+order.ar]
        p = p+1
        if (p>maxstep){
          name.ar = NULL
          for (i in 1:order.ar){
            name.ar[i] = paste("ar", as.character(i), sep = "")
          }
          names(ar.new) = name.ar
          name.ma = NULL
          for (i in 1:order.ma){
            name.ma[i] = paste("ma", as.character(i), sep = "")
          }
          names(ma.new) = name.ma
          para = c(ar.new, ma.new, intercept = mu.new)
          print(as.symbol("Number of iterations exceeded options."))
          break
        }
      }
    }
  }else{
    order.ar = order[1]
    order.ma = order[2]
    initial.ar = as.numeric(integer(order[1]))
    initial.ma = as.numeric(integer(order[2]))
    l = length(data)
    G = 0
    if (order.ma==0){
      e = vector("numeric", length = l)
      for (i in (order.ar+1):l){
        e[i] = data[i]-
          initial.ar%*%rev(data[(i-order.ar):(i-1)])
      }
      L.new = sum(e^2)
      dEdAR = matrix(0, ncol = l, nrow = order.ar)
      for (i in (order.ar+1):l){
        dEdAR[,i] = -rev(data[(i-order.ar):(i-1)])
      }
      dE = dEdAR
      d = 2*dE%*%e
      G = G+sum(d^2)
      delta = -d/sqrt(G)*LearningRate
      p = 0
      ar.new = initial.ar+delta[1:order.ar]
      while (T){
        e = vector("numeric", length = l)
        for (i in (order.ar+1):l){
          e[i] = data[i]-
            ar.new%*%rev(data[(i-order.ar):(i-1)])
        }
        L = L.new
        L.new = sum(e^2)
        if (abs(L-L.new)<tol&sum(d^2)<tol){
          name = NULL
          for (i in 1:order.ar){
            name[i] = paste("ar", as.character(i), sep = "")
          }
          names(ar.new) = name
          para = ar.new
          print(as.symbol("Local minimum found."))
          break
        }
        dEdAR = matrix(0, ncol = l, nrow = order.ar)
        for (i in (order.ar+1):l){
          dEdAR[,i] = -rev(data[(i-order.ar):(i-1)])
        }
        dE = dEdAR
        d = 2*dE%*%e
        G = G+sum(d^2)
        delta = -d/sqrt(G)*LearningRate
        ar.new = ar.new+delta[1:order.ar]
        p = p+1
        if (p>maxstep){
          name = NULL
          for (i in 1:order.ar){
            name[i] = paste("ar", as.character(i), sep = "")
          }
          names(ar.new) = name
          para = ar.new
          print(as.symbol("Number of iterations exceeded options."))
          break
        }
      }
    }else if (order.ar==0){
      e = vector("numeric", length = l+order.ma)
      for (i in 1:l){
        e[i+order.ma] = data[i]+
          initial.ma%*%rev(e[i:(i+order.ma-1)])
      }
      L.new = sum(e^2)
      dEdMA = matrix(0, ncol = l+order.ma, nrow = order.ma)
      for (i in 1:l){
        dEdMA[,(i+order.ma)] = 
          dEdMA[,(i:(i+order.ma-1))]%*%rev(initial.ma)+
          rev(e[i:(i+order.ma-1)])
      }
      dEdMA = dEdMA[,(1+order.ma):(l+order.ma)]
      dE = dEdMA
      d = 2*dE%*%e[(1+order.ma):(l+order.ma)]
      G = G+sum(d^2)
      delta = -d/sqrt(G)*LearningRate
      p = 0
      ma.new = initial.ma+delta[1:order.ma]
      while (T){
        e = vector("numeric", length = l+order.ma)
        for (i in 1:l){
          e[i+order.ma] = data[i]+
            ma.new%*%rev(e[i:(i+order.ma-1)])
        }
        L = L.new
        L.new = sum(e^2)
        if (abs(L-L.new)<tol&sum(d^2)<tol){
          name = NULL
          for (i in 1:order.ma){
            name[i] = paste("ma", as.character(i), sep = "")
          }
          names(ma.new) = name
          para = ma.new
          print(as.symbol("Local minimum found."))
          break
        }
        dEdMA = matrix(0, ncol = l+order.ma, nrow = order.ma)
        for (i in 1:l){
          dEdMA[,(i+order.ma)] = 
            dEdMA[,(i:(i+order.ma-1))]%*%rev(ma.new)+
            rev(e[i:(i+order.ma-1)])
        }
        dE = dEdMA
        dE = dE[,(1+order.ma):(l+order.ma)]
        d = 2*dE%*%e[(1+order.ma):(l+order.ma)]
        G = G+sum(d^2)
        delta = -d/sqrt(G)*LearningRate
        ma.new = ma.new+delta[1:order.ma]
        p = p+1
        if (p>maxstep){
          name = NULL
          for (i in 1:order.ma){
            name[i] = paste("ma", as.character(i), sep = "")
          }
          names(ma.new) = name
          para = ma.new
          print(as.symbol("Number of iterations exceeded options."))
          break
        }
      }
    }else{
      e = vector("numeric", length = l+order.ma)
      for (i in (order.ar+1):l){
        e[i+order.ma] = data[i]+
          initial.ma%*%rev(e[i:(i+order.ma-1)])-
          initial.ar%*%rev(data[(i-order.ar):(i-1)])
      }
      L.new = sum(e^2)
      dEdAR = matrix(0, ncol = l+order.ma, nrow = order.ar)
      dEdMA = matrix(0, ncol = l+order.ma, nrow = order.ma)
      for (i in (order.ar+1):l){
        dEdAR[,(i+order.ma)] = 
          dEdAR[,(i:(i+order.ma-1))]%*%rev(initial.ma)-
          rev(data[(i-order.ar):(i-1)])
        dEdMA[,(i+order.ma)] = 
          dEdMA[,(i:(i+order.ma-1))]%*%rev(initial.ma)+
          rev(e[i:(i+order.ma-1)])
      }
      dEdAR = dEdAR[,(1+order.ma):(l+order.ma)]
      dEdMA = dEdMA[,(1+order.ma):(l+order.ma)]
      dE = rbind(dEdAR, dEdMA)
      d = 2*dE%*%e[(1+order.ma):(l+order.ma)]
      G = G+sum(d^2)
      delta = -d/sqrt(G)*LearningRate
      p = 0
      ar.new = initial.ar+delta[1:order.ar]
      ma.new = initial.ma+delta[(1+order.ar):(order.ma+order.ar)]
      while (T){
        e = vector("numeric", length = l+order.ma)
        for (i in (order.ar+1):l){
          e[i+order.ma] = data[i]+
            rev(e[i:(i+order.ma-1)])%*%ma.new-
            ar.new%*%rev(data[(i-order.ar):(i-1)])
        }
        L = L.new
        L.new = sum(e^2)
        if (abs(L-L.new)<tol&sum(d^2)<tol){
          name.ar = NULL
          for (i in 1:order.ar){
            name.ar[i] = paste("ar", as.character(i), sep = "")
          }
          names(ar.new) = name.ar
          name.ma = NULL
          for (i in 1:order.ma){
            name.ma[i] = paste("ma", as.character(i), sep = "")
          }
          names(ma.new) = name.ma
          para = c(ar.new, ma.new)
          print(as.symbol("Local minimum found."))
          break
        }
        dEdAR = matrix(0, ncol = l+order.ma, nrow = order.ar)
        dEdMA = matrix(0, ncol = l+order.ma, nrow = order.ma)
        for (i in (order.ar+1):l){
          dEdAR[,(i+order.ma)] = 
            dEdAR[,(i:(i+order.ma-1))]%*%rev(ma.new)-
            rev(data[(i-order.ar):(i-1)])
          dEdMA[,(i+order.ma)] = 
            dEdMA[,(i:(i+order.ma-1))]%*%rev(ma.new)+
            rev(e[i:(i+order.ma-1)])
        }
        dE = rbind(dEdAR, dEdMA)
        dE = dE[,(1+order.ma):(l+order.ma)]
        d = 2*dE%*%e[(1+order.ma):(l+order.ma)]
        G = G+sum(d^2)
        delta = -d/sqrt(G)*LearningRate
        ar.new = ar.new+delta[1:order.ar]
        ma.new = ma.new+delta[(1+order.ar):(order.ma+order.ar)]
        p = p+1
        if (p>maxstep){
          name.ar = NULL
          for (i in 1:order.ar){
            name.ar[i] = paste("ar", as.character(i), sep = "")
          }
          names(ar.new) = name.ar
          name.ma = NULL
          for (i in 1:order.ma){
            name.ma[i] = paste("ma", as.character(i), sep = "")
          }
          names(ma.new) = name.ma
          para = c(ar.new, ma.new)
          print(as.symbol("Number of iterations exceeded options."))
          break
        }
      }
    }
  }
  return(para)
}