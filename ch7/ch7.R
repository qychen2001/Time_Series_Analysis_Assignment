MM.AR <- function(Y, q){
  n = length(Y)
  
  r = my.acf(Y, q, plot = F)$acf
  rr = c(1, r[seq(q-1)])
  
  idx.mat = matrix(NA,q,q)
  idx.mat[1,] =  seq(0,q-1)
  for(i in seq(2,q)){
    idx.mat[i,] = idx.mat[i-1,]-1
  }
  idx.mat = abs(idx.mat)
  idx.mat = idx.mat +1
  A = matrix(NA,q,q)
  for(i in seq(q)){
    A[i,] = rr[idx.mat[i,]]
  }
  
  b = r[seq(q)]
  phi = solve(A,b)
  
  phi
}






my.acf = function(Y, lag.max=NULL, plot=TRUE, ci.type = "white"){
  
  n = length(Y)
  if(is.null(lag.max)){
    lag.max = 20
  }
  if(max(lag.max)>=n){
    stop("The largest lag.max need be smaller than n")
  }
  
  if(min(lag.max)<=0){
    stop("The smallest lag.max need be large than 0")
  }
  
  Y.mean = Y - mean(Y)
  
  acf.Y = rep(NA,lag.max)
  for(k in seq(lag.max)){
    acf.Y[k] = sum(Y.mean[1:(n-k)]*Y.mean[(k+1):n])
  }
  acf = acf.Y/sum(Y.mean*Y.mean)
  
  
  #plot1.acf
  if(plot){
    
    ylim <- c(min(acf)-0.1, max(acf)+0.1)
    plot(y = acf, x = 1:lag.max,type = "h", xlab = "Lag", ylab = "ACF", ylim = ylim)
    abline(h = 0,col="black")
    if(ci.type == "ma"){
      wt <- sqrt(cumsum(c(1, 2*acf^2)))
      wt <- wt[-length(wt)]
      lines(y = 1.96/sqrt(n)*wt, x = 1:lag.max,col="red",lty = 2)
      lines(y = -1.96/sqrt(n)*wt, x = 1:lag.max,col="red",lty = 2)
    }
    
    if(ci.type == "white"){
      abline(h = 1.96/sqrt(n),col="red",lty = 2)
      abline(h = -1.96/sqrt(n),col="red",lty = 2)
    }
    
  }
  out = list(acf=acf, Y= Y, lag.max=lag.max)
}



LS.AR <- function(Y, p){
  n = length(Y)
  mu = mean(Y)
  Y.mean = Y - mu
  X = matrix(0, n-p, p+1)
  for (i in seq(n-p)){
    X[i,] = c(1,Y.mean[seq(i+p-1,i,-1)])
  }
  y = Y.mean[(p+1):n]
  phi = solve(t(X)%*%X,t(X)%*%y)
  Intercept = phi[1]
  phi = phi[-1]
  result = list(mu=mu, phi=phi, Intercept=Intercept)
}


LS.MA1 <- function(Y, lr=0.1, maxit = 500){
  n = length(Y)
  
  theta.old = 0
  loss = c()
  for (i in seq(maxit)){
    e = rep(NA,n)
    e.grad = rep(NA, n)
    e[1] = Y[1]
    e[2] = Y[2] + theta.old*e[1]
    e.grad[1] = 0
    e.grad[2] = e[1]
    
    for(j in seq(3,n)){
      e[j] = Y[j] + theta.old*e[j-1]
      e.grad[j] = e[j-1] + theta.old*e.grad[j-1]
    }
    
    grad = 2*sum(e*e.grad)
    
    theta = theta.old - lr*grad
    
    loss[i] = sum(e*e)
    if (abs(theta-theta.old)<1e-6)
      break
    
    theta.old = theta
    
  }
  
  result = list(theta=theta, loss=loss)
  
  result
}



LS.MA <- function(Y, q, lr=0.1, maxit = 500){
  
  n = length(Y)
  N = n + q +1
  
  theta.old = rep(0, q)
  loss = c()
  for (i in seq(maxit)){
    
    
    e = rep(0,N)
    e.grad = matrix(0, q, N)
    
    grad = rep(0,q)
    
    for(j in seq(q+2,N)){
      
      e[j] = Y[j-(q+1)] + sum(e[seq(j-1,j-q,-1)]*theta.old)
      e.grad[,j] = e[seq(j-1,j-q,-1)] +  e.grad[,seq(j-1,j-q,-1)]%*%theta.old 
      
      grad = grad + 2*e[j]*e.grad[,j]
    }
    
    
    theta = theta.old - lr*grad
    #print(grad)
    
    loss[i] = sum(e*e)
    #print(sum(abs(theta-theta.old)))
    if (sum(abs(theta-theta.old)) < 1e-6)
      break
    
    theta.old = theta
    
  }
  
  result = list(theta=theta, loss=loss)
  
  result
}


LS.ARMA11 =  function(Y, lr=0.1, maxit = 500){
  n = length(Y)
  mu = mean(Y)
  Y = Y - mu
  
  theta.old = 0
  phi.old = 0
  loss = c()

  grad.phi.total =1
  grad.phi.theta =1
  
  for (i in seq(maxit)){
    
    e = rep(NA,n)
    e.grad.phi = rep(NA, n)
    e.grad.theta = rep(NA, n)
    
    e[1] = Y[1]
    e[2] = Y[2] - phi.old*Y[1] + theta.old*e[1]
    e.grad.phi[1] = 0
    e.grad.theta[1] = 0
    
    e.grad.phi[2] = -Y[1]
    e.grad.theta[2] = e[1]
    
    for(j in seq(3,n)){
      e[j] = Y[j] - phi.old*Y[j-1] + theta.old*e[j-1]
      e.grad.phi[j] = -Y[j-1] + theta.old*e.grad.phi[j-1]
      e.grad.theta[j] = e[j-1] + theta.old*e.grad.theta[j-1]
    }
    
    grad.phi = 2*mean(e*e.grad.phi)
    grad.theta = 2*mean(e*e.grad.theta)
    
    grad.phi.total = grad.phi.total + grad.phi^2
    grad.phi.theta = grad.phi.theta + grad.theta^2
    
    phi = phi.old - lr*grad.phi/sqrt(grad.phi.total)
    theta = theta.old - lr*grad.theta/sqrt(grad.phi.theta)
    loss[i] = mean(e*e)
    if ((abs(phi-phi.old)+abs(theta-theta.old))<1e-5)
      break
    theta.old = theta
    phi.old = phi
  }
  result = list(phi = phi, theta=theta, loss=loss)
  result
}





MM.ARMA11 = function(data) {
  result.acf = my.acf(data - mean(data), lag.max = 2, plot = FALSE)
  acfval = result.acf$acf
  phi = acfval [2] / acfval [1]
  r1 = acfval [1]
  a = r1 - phi
  b = phi ^ 2 + 1 - 2 * r1 * phi
  c = r1 - phi
  x1 = (-b + sqrt(b ^ 2 - 4 * a * c)) / 2 / a
  x2 = (-b - sqrt(b ^ 2 - 4 * a * c)) / 2 / a
  theta = c(x1, x2)
  theta = theta[which(abs(1 / theta) > 1)]
  para = c(ar1 = phi, ma1 = theta)
  return(para)
}
  