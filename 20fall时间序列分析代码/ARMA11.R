ARMA11 <- function(phi,theta,lag,plot = FALSE){
  k <- lag;
  x <- 0:(k-1);
  rhos <- (1-theta*phi)*(phi-theta)*(phi^x)/(1-2*theta*phi+theta^2);
  
  if (plot == TRUE){
    data <- data.frame(lags = 1:k, acfval = rhos);
    p <- ggplot(data, aes(x = lags,y = acfval))+
    geom_segment(aes(x = lags, xend = lags, y = 0, yend = acfval))+geom_point(size = 3, color = "red")+
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8, vjust = 0.9))+
    scale_x_continuous("滞后数", breaks = seq(2,k,2))+theme(legend.position="none")+      
    scale_y_continuous("自相关函数值", breaks = seq(-1,1,0.2))
  }else{
    p = NA;
  }
  
  results <- list(ACFval = rhos, Lag = k, ACFplot = p);
  return(results);
  
}