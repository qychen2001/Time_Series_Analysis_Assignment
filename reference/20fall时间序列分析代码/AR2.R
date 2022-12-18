AR2 <- function(phi1, phi2, lag, plot = FALSE, root = FALSE){
  
  library("ggplot2");
  
  k <- lag;
  rhos <- integer(k);
  rhos[1] <- phi1/(1-phi2);rhos[2] <- phi1*rhos[1]+phi2;
  for (i in 3:k){
    rhos[i] <- phi1*rhos[i-1]+phi2*rhos[i-2];
  }
  
  if (plot == TRUE){
    data <- data.frame(lags = 1:k, acfval = rhos);
    p <- ggplot(data, aes(x = lags,y = acfval))+
    geom_segment(aes(x = lags, xend = lags, y = 0, yend = acfval))+geom_point(size = 3, color = "red")+
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8, vjust = 0.9))+
    scale_x_continuous("滞后数", breaks = seq(2,k,2))+theme(legend.position="none")+
    scale_y_continuous("自相关函数值", breaks = seq(-1,1,0.2))
  }else{
    p <- NA;
  }
  
  if (root == TRUE){
    Delta <- sqrt(complex(real = phi1^2+4*phi2));
    if (Im(Delta) == 0){
      roots <- c((phi1-Re(Delta))/(-2*phi2),(phi1+Re(Delta))/(-2*phi2));
      results <- list(ACFval = rhos, Lag = k, ACFplot = p, Roots = roots);
    }else{
      roots <- c((phi1-Delta)/(-2*phi2),(phi1+Delta)/(-2*phi2));
      R <- sqrt(-phi2);
      Theta <- acos(phi1/2/R);
      results <- list(ACFval = rhos, Lag = k, ACFplot = p, Roots = roots, Damping_Factor = R, Frequency = Theta);
    }
  }else{
    results <- list(ACFval = rhos, Lag = k, ACFplot = p);
  }
  return(results)
  
}