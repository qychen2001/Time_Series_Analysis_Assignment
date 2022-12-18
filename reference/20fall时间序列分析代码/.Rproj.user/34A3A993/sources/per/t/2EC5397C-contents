plot.simARIMA <- function(AR.para = integer(0), MA.para=integer(0),
                          diff, noise.sd = 1, ARMA.mean = 0, seq.length = 10, batch = 9){
  
  library(ggplot2)
  library(reshape2)
  
  data = matrix(NA, nrow = seq.length+1, ncol = batch+1)
  if ((length(AR.para)>0)&(length(MA.para)>0)){
    para = AR.para; para.char = "AR ["; m = length(para)
    for (i in 1:m){
      if (i == m){
        para.char = paste(para.char, para[i])
      }else{
        para.char = paste(para.char, para[i],",")
      }
    }
    para.char = paste(para.char, "], ")
    para = MA.para; para.char = paste(para.char, "MA ["); m = length(para)
    for (i in 1:m){
      if (i == m){
        para.char = paste(para.char, para[i])
      }else{
        para.char = paste(para.char, para[i],",")
      }
    }
    para.char = paste(para.char, "]")
  }else if ((length(AR.para)>0)&(length(MA.para)==0)){
    para = AR.para; para.char = "AR ["; m = length(para)
    for (i in 1:m){
      if (i == m){
        para.char = paste(para.char, para[i])
      }else{
        para.char = paste(para.char, para[i],",")
      }
    }
    para.char = paste(para.char, "]")
  }else if ((length(AR.para)==0)&(length(MA.para)>0)){
    para = MA.para; para.char = "MA ["; m = length(para)
    for (i in 1:m){
      if (i == m){
        para.char = paste(para.char, para[i])
      }else{
        para.char = paste(para.char, para[i],",")
      }
    }
    para.char = paste(para.char, "]")
  }else if ((length(AR.para)==0)&(length(MA.para)==0)){
    para = "No parameters";
  }
  
  for (i in 1:batch){
    data[,i] = simARIMA(AR.para, MA.para, diff, noise.sd, ARMA.mean, seq.length)
  }
  data[,batch+1] = 0:(seq.length);
  name = c("serie1","serie2","serie3","serie4",
           "serie5","serie6","serie7","serie8",
           "serie9","x");
  data = as.data.frame(data);names(data) = name[c(1:batch,10)];
  data.melt=melt(data,id.vars = c("x"),variable.name = c("V"),value.name = c("y"));
  
  max.data = ceiling(max(data.melt[,3]));min.data = floor(min(data.melt[,3]));
  max.abs.data = max(abs(min.data),max.data)
  if (max.data-min.data>80){
    sz = 8;by.x = 5
  }else if(max.data-min.data>60){
    sz = 10;by.x = 2
  }else{
    sz = 12;by.x = 2
  }
  if (seq.length>100){
    by.y = 10
  }else{
    by.y = 5
  }
  
  p_line = ggplot(data.melt)+
    geom_line(aes(x=data.melt[,1],y=data.melt[,3],color=data.melt[,2]),size=1)+
    scale_color_brewer(palette = "Set1")+
    scale_x_continuous(breaks = seq(0, seq.length, by.y))+
    scale_y_continuous(breaks = seq(-max.abs.data, max.abs.data, by.x))+
    labs(x="Time",y="Value",color="Group")+
    ggtitle(paste("Parameter(s) : ",para.char))+
    theme(
      legend.title = element_text(size = 15, face = "bold"),
      legend.text = element_text(size = 15, face = "bold"),
      legend.position = "right",
      legend.key.size=unit(0.6,'cm'),       
      axis.ticks.x=element_blank(),
      axis.text.x=element_text(size = 12,face = "bold", vjust = 0.5, hjust = 0.5),
      axis.text.y=element_text(size = sz,face = "bold", vjust = 0.5, hjust = 0.5),
      axis.title.x = element_text(size = 15,face = "bold", vjust = -0.5, hjust = 0.5),
      axis.title.y = element_text(size = 15,face = "bold", vjust = 1.2, hjust = 0.5),      
      panel.background = element_rect(fill = "transparent",colour = "black"), 
      panel.grid.minor = element_line(color="lightgrey",size=0.1),
      panel.grid.major = element_line(color="lightgrey",size=0.1),
      plot.background = element_rect(fill = "transparent",colour = "white"),
      plot.title = element_text(size = 18,face = "bold", vjust = 0.5, hjust = 0.5));
  
  return(p_line);
  
}