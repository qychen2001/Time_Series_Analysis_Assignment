acfun = function(data = NULL, model = NULL, model.para = NULL,
                 lag.max, plot = TRUE, lag0 = FALSE){
  library(ggplot2)
  if (is.null(model)){
    data = data-mean(data)
    n = length(data)
    SST = t(data)%*%data
    gamma = integer(lag.max)
    for (i in 1:lag.max){
      gamma[i] = t(head(data,-i))%*%tail(data,-i)
    }
    acfval = gamma/as.numeric(SST)
    acfstd = sqrt(1/n*(1+2*cumsum(acfval^2)))
    maxacf = round(10000*max(abs(acfval)))/10000
  
    if (plot==TRUE){
      if (lag0==TRUE){
        data = data.frame(lags = 0:lag.max, acfval = c(1, acfval))
        acfstd = c(0, acfstd)
        acfplot = ggplot(data, aes(x = lags,y = acfval))+
          geom_area(aes(x = lags, y = qnorm(0.975)*acfstd), fill = "#B9CFE7") +
          geom_area(aes(x = lags, y = -qnorm(0.975)*acfstd), fill = "#B9CFE7") +
          geom_segment(aes(x = lags, xend = lags, y = 0, yend = acfval))+
          geom_point(size = 2, color = "red")+
          scale_x_continuous("滞后数", breaks = seq(0,lag.max,2))+
          scale_y_continuous("自相关函数值", breaks = seq(-1,1,0.2))+
          ggtitle("ACF plot")
      }else{
        data = data.frame(lags = 1:lag.max, acfval = acfval)
        acfplot = ggplot(data, aes(x = lags,y = acfval))+
          geom_area(aes(x = lags, y = qnorm(0.975)*acfstd), fill = "#B9CFE7") +
          geom_area(aes(x = lags, y = -qnorm(0.975)*acfstd), fill = "#B9CFE7") +
          geom_segment(aes(x = lags, xend = lags, y = 0, yend = acfval))+
          geom_point(size = 2, color = "red")+
          scale_x_continuous("滞后数", breaks = seq(0,lag.max,2))+
          scale_y_continuous("自相关函数值", breaks = seq(-maxacf,maxacf,maxacf/5))+
          ggtitle("ACF plot")
      }
      result = list(acfval = acfval, acfplot = acfplot)
    }else{
      result = list(acfval = acfval)
    }
  }else{
    if (model=="AR"){
      l = length(model.para)
      if (l>1){
        M = matrix(NA, nrow = l, ncol = l+1)
        for (i in 1:l){
          v = integer(2*l-1)
          v[(l+1-i):(2*l-i)] = model.para
          M[i,] = c(v[l], rev(v[1:(l-1)])+v[(l+1):(2*l-1)], 0)
        }
        N = -M[,1]
        M = M[,2:(l+1)]-diag(x = 1, nrow = l)
        acfval = t(as.matrix(qr.solve(M, N)))
      }else{
        acfval = model.para
      }
      ar = as.matrix(rev(model.para))
      if (lag.max>l){
        for (i in (l+1):lag.max){
          acfval = cbind(acfval, t(tail(t(acfval), l))%*%ar)
        }
        acfval = as.vector(acfval)
      }else{
        acfval = acfval[1:lag.max]
      }
      
      maxacf = round(10000*max(abs(acfval)))/10000
    }else if (model=="MA"){
      l = length(model.para)
      a = 1+sum(model.para^2)
      rho = integer(lag.max)
      for (i in 1:lag.max){
        if (i<l){
          v = c(-1, model.para[1:(l-i)])
          u = model.para[i:l]
          rho[i] = sum(v*u)/a
        }else if (i==l){
          rho[i] = -tail(model.para, 1)/a
        }else{
          rho[i] = 0
        }
      }
      acfval = rho
      maxacf = round(10000*max(abs(acfval)))/10000
      
    }
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
      if (lag0==TRUE){
        data = data.frame(lags = 0:lag.max, acfval = c(1, acfval))
        acfplot = ggplot(data, aes(x = lags,y = acfval))+
          geom_segment(aes(x = lags, xend = lags, y = 0, yend = acfval))+
          geom_point(size = 2, color = "red")+
          scale_x_continuous("滞后数", breaks = seq(0,lag.max,2))+
          scale_y_continuous("自相关函数值", breaks = seq(-1,1,0.2))+
          ggtitle(paste(c("ACF plot of"), model, c("["),
                        as.character(model.para), c("]")))
      }else{
        data = data.frame(lags = 1:lag.max, acfval = acfval)
        acfplot = ggplot(data, aes(x = lags,y = acfval))+
          geom_segment(aes(x = lags, xend = lags, y = 0, yend = acfval))+
          geom_point(size = 2, color = "red")+
          scale_x_continuous("滞后数", breaks = seq(0,lag.max,2))+
          scale_y_continuous("自相关函数值", breaks = seq(-maxacf,maxacf,maxacf/5))+
          ggtitle(paste(c("ACF plot of"), model, c("["),
                        para, c("]")))
      }
      result = list(acfval = acfval, acfplot = acfplot)
    }else{
      result = list(acfval = acfval)
    }
  }
  return(result)
}