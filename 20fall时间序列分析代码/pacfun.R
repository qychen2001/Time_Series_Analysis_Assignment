pacfun = function(data = NULL, lag.max, plot = TRUE, method = NULL,
                  model = NULL, model.para = NULL){
  library(ggplot2)
  if (is.null(model)){
    result.acf = acfun(data = data, lag.max = lag.max, plot = FALSE)
    acfval = result.acf$acfval
    pacfval = integer(lag.max)
    if (is.null(method)|isTRUE(method=="system")){
      for (i in 1:lag.max){
        if (i==1){
          RHO = 1
          pacfval[1] = acfval[1]
        }else{
          RHO = rbind(c(1, acfval[1:(i-1)]), cbind(acfval[1:(i-1)], RHO))
          phi = qr.solve(RHO, acfval[1:i])
          pacfval[i] = tail(phi, 1)
        }
      }
    }else if (method=="iteration"){
      for (i in 1:lag.max){
        if (i==1){
          pacfval[1] = acfval[1]
          PHI = acfval[1]
        }else{
          pacfval[i] = (acfval[i]-PHI%*%rev(acfval[1:(i-1)]))/
            (1-PHI%*%acfval[1:(i-1)])
          PHI = PHI-pacfval[i]*rev(PHI)
          PHI = c(PHI, pacfval[i])
        }
      }
    }
    n = length(data)
    pacfstd = sqrt(1/n)
    maxpacf = round(10000*max(abs(pacfval)))/10000
  
    if (plot==TRUE){
      data = data.frame(lags = 1:lag.max, pacfval = pacfval)
      pacfplot = ggplot(data, aes(x = lags,y = pacfval))+
        geom_area(aes(x = lags, y = qnorm(0.975)*pacfstd), fill = "#B9CFE7") +
        geom_area(aes(x = lags, y = -qnorm(0.975)*pacfstd), fill = "#B9CFE7") +
        geom_segment(aes(x = lags, xend = lags, y = 0, yend = pacfval))+
        geom_point(size = 2, color = "red")+
        scale_x_continuous("lag", breaks = seq(0,lag.max,2))+
        scale_y_continuous("val", breaks = seq(-maxpacf,maxpacf,maxpacf/5))+
        ggtitle("PACF plot")
      result = list(pacfval = pacfval, pacfplot = pacfplot)
    }else{
      result = list(pacfval = pacfval)
    }
  }else{
    result.acf = acfun(model = model, model.para = model.para,
                       lag.max = lag.max)
    acfval = result.acf$acfval
    pacfval = integer(lag.max)
    l = length(model.para)
    for (i in 1:lag.max){
      if (i==1){
        RHO = 1
        pacfval[1] = acfval[1]
      }else{
        RHO = rbind(c(1, acfval[1:(i-1)]), cbind(acfval[1:(i-1)], RHO))
        phi = qr.solve(RHO, acfval[1:i])
        pacfval[i] = tail(phi,1)
      }
    }
    maxpacf = round(10000*max(abs(pacfval)))/10000
    if (l==1){
      para = as.character(model.para)
    }else{
      for (i in 1:(l-1)){
        if (i==1){
          para = as.character(model.para[i])
        }else{
          para = paste(para, as.character(model.para[i]), sep = ",")
        }
      }
      para = paste(para, as.character(model.para[l]))
    }
    
    if (plot==TRUE){
      data = data.frame(lags = 1:lag.max, pacfval = pacfval)
      pacfplot = ggplot(data, aes(x = lags,y = pacfval))+
        geom_segment(aes(x = lags, xend = lags, y = 0, yend = pacfval))+
        geom_point(size = 2, color = "red")+
        scale_x_continuous("lag", breaks = seq(0,lag.max,2))+
        scale_y_continuous("val", breaks = seq(-maxpacf,maxpacf,maxpacf/5))+
        ggtitle(paste(c("PACF plot of"), model, c("["),
                      para, c("]")))
      result = list(pacfval = pacfval, pacfplot = pacfplot)
    }else{
      result = list(pacfval = pacfval)
    }
  }
  
  return(result)
}