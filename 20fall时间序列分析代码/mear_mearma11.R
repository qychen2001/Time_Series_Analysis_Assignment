me.ar = function(data, order){
  #ar.yw
  result.acf = acfun(data = data-mean(data), lag.max = order,
                     plot = FALSE)
  acfval = result.acf$acfval
  phi = integer(order)
  for (i in 1:order){
    if (i==1){
      RHO = 1
    }else{
      RHO = rbind(c(1, acfval[1:(i-1)]), cbind(acfval[1:(i-1)], RHO))
    }
  }
  phi = as.numeric(qr.solve(RHO, acfval))
  name = NULL
  for (i in 1:order){
    name[i] = paste("ar", as.character(i), sep = "")
  }
  names(phi) = name
  para = phi
  return(para)
}

me.arma11 = function(data){
  result.acf = acfun(data = data-mean(data), lag.max = 2, plot = FALSE)
  acfval = result.acf$acfval
  phi = acfval[2]/acfval[1]
  r1 = acfval[1]
  a = r1-phi; b = phi^2+1-2*r1*phi; c = r1-phi
  x1 = (-b+sqrt(b^2-4*a*c))/2/a
  x2 = (-b-sqrt(b^2-4*a*c))/2/a
  theta = c(x1, x2)
  theta = theta[which(abs(1/theta)>1)]
  para = c(ar1 = phi, ma1 = theta)
  return(para)
}