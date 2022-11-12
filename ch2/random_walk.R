y<-array(0,dim=100)
e<-rnorm(100)
for (i in 1:100){
  y [i+1]<-y[i]+e[i+1]
  }
y<-ts(y[1:100])
plot(y)