DTTSM <- function(Data,Model,Intercept){
  Time <- time(Data);n <- length(Data);
  if (length(grep(Model,"Constant"))==1){
    X <- cbind(Intercept = rep(1,n));
    Para <- qr.solve(X,Data);
  }else if (length(grep(Model,"Linear"))==1){
    X <- cbind(Intercept = rep(1,n),Time);
    Para <- qr.solve(X,Data);
  }else if (length(grep(Model,"Quadratic"))==1){
    Time <- time(Data);n <- length(Data);
    X <- cbind(Intercept = rep(1,n),Time,Time^2);
    Para <- qr.solve(X,Data);
  }else if (length(grep(Model,"Cosine"))==1){
    X <- cbind(Intercept = rep(1,n),Cos = cos(2*pi*Time),Sin = sin(2*pi*Time));
    Para <- qr.solve(X,Data);
  }else if (length(grep(Model,"SeasonC"))==1){
    t <- Time;Time <- integer(0);FirstMonth <- (t[1]-floor(t[1]))*12+1;
    m1 <- FirstMonth-12;m2 <- floor((n+m1-1)/12);
    if (m2 == -1){
      Time <- FirstMonth:(FirstMonth+n-1);
    }else if (FirstMonth == 1){
      Time <- c(FirstMonth:12,rep(1:12,m2));
    }else{
      Time <- c(FirstMonth:12,rep(1:12,m2),1:(n+FirstMonth-13-12*m2));
    }
    Time <- round(Time);
    dimnames = list(rep("0",n),c("Jan","Feb","Mar","Apr",
                                 "May","June","July","Aug",
                                 "Sept","Oct","Nov","Dec"))
    X <- matrix(0,nrow = n,ncol = 12,dimnames = dimnames);
    if (Intercept){
      X[,1] <- rep(1,n);
      for (i in 2:12){
        idx <- which(Time == i);
        u <- rep(0,n);
        u[idx] <- 1;
        X[,i] <- u;
      }
    }else{
      for (i in 1:12){
        idx <- which(Time == i);
        u <- rep(0,n);
        u[idx] <- 1;
        X[,i] <- u;
      }
    }
    Para <- qr.solve(X,Data);
  }else if (length(grep(Model,"SeasonL"))==1){
    t <- Time;Time <- integer(0);FirstMonth <- (t[1]-floor(t[1]))*12+1;
    m1 <- FirstMonth-12;m2 <- floor((n+m1-1)/12);
    if (m2 == -1){
      Time <- FirstMonth:(FirstMonth+n-1);
    }else if (FirstMonth == 1){
      Time <- c(FirstMonth:12,rep(1:12,m2));
    }else{
      Time <- c(FirstMonth:12,rep(1:12,m2),1:(n+FirstMonth-13-12*m2));
    }
    Time <- round(Time);
    dimnames = list(rep("0",n),c("Jan","Feb","Mar","Apr",
                                 "May","June","July","Aug",
                                 "Sept","Oct","Nov","Dec",
                                 "Slope"))
    X <- matrix(0,nrow = n,ncol = 13,dimnames = dimnames);
    if (Intercept){
      X[,1] <- rep(1,n);
      for (i in 2:12){
        idx <- which(Time == i);
        u <- rep(0,n);
        u[idx] <- 1;
        X[,i] <- u;
      }
    }else{
      for (i in 1:12){
        idx <- which(Time == i);
        u <- rep(0,n);
        u[idx] <- 1;
        X[,i] <- u;
      }
    }
    X[,13] <- t;
    Para <- qr.solve(X,Data);
  }
  qrr <-qr(X);
  R <- qr.R(qrr);
  invXtX <- solve(R)%*%solve(t(R));
  Fitval <- X%*%Para;Res <- Data-Fitval;Fitval <- Data-Res;m <- ncol(X);
  SE <- sqrt((t(Res)%*%Res)/(n-m));SEPara <- sqrt(as.vector((SE^2))*diag(invXtX));
  if (length(grep(Model,"SeasonC"))==1 || length(grep(Model,"SeasonL"))==1 && Intercept==FALSE){
    tStatistic <- as.vector(Para/SEPara);t <- qt(p = 0.975,df = n-m);
    SSR <- sum(Fitval^2);SSE <-SE^2*(n-m);SST <- SSR+SSE;
    FStatistic <- as.vector(SSR*(n-m)/SSE/m);F <-qf(0.95,m,n-12);
    tTest <- cbind(tStatistic,t);FTest <- cbind(FStatistic,F);R2 <- as.vector(SSR/SST);
    Hat <- X%*%invXtX%*%t(X);StuRes <- Res/as.vector(SE)/sqrt(diag(diag(1,n)-Hat));
    EStuRes <- StuRes*sqrt((n-m-1)/(n-m-StuRes^2));
  }else if (m!=1){
    tStatistic <- as.vector(Para/SEPara);t <- qt(p = 0.975,df = n-m);
    MeanData <- mean(Data);
    SSR <- sum((Fitval-MeanData)^2);SSE <-SE^2*(n-m);SST <- SSR+SSE;
    FStatistic <- as.vector(SSR*(n-m)/SSE/(m-1));F <-qf(0.95,m-1,n-m);
    tTest <- cbind(tStatistic,t);FTest <- cbind(FStatistic,F);R2 <- as.vector(SSR/SST);
    Hat <- X%*%invXtX%*%t(X);StuRes <- Res/as.vector(SE)/sqrt(diag(diag(1,n)-Hat));
    EStuRes <- StuRes*sqrt((n-m-1)/(n-m-StuRes^2));
  }else{
    tStatistic <- as.vector(Para/SEPara);t <- qt(p = 0.975,df = n-m);
    tTest <- cbind(tStatistic,t);FTest <- integer(0);R2 <- integer(0);EStuRes <- integer(0);
  }
  mdl <- list(Coefficients = Para,Fitted_Value = Fitval,Designed_Matrix = X,Residuals = Res,
              External_Studentized_Residuals = EStuRes,Standard_Error = SE,
              Coefficients_Standard_Error = SEPara,t_Test_Result = tTest,
              F_Test_Result = FTest,Rsquare = R2,Model_Type = Model,Data = Data)
  return(mdl)
}