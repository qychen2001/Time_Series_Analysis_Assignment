library("TSA");library("lawstat");
data("winnebago");
source('D:/R Files/TSAcourse/DTTSM.R')
plot.ts(winnebago,type = "o");
lmdl = DTTSM(winnebago,"Linear");
lCoef = lmdl$Coefficients;
ltTest = lmdl$t_Test_Result;lFTest = lmdl$F_Test_Result;
lR2 = lmdl$Rsquare;
lESRE = lmdl$External_Studentized_Residuals;
plot.ts(lESRE,type = "o",ylab = "Studentized Residuals");abline(h = c(-3,3));

plot.ts(log(winnebago),type = "o");
lglmdl = DTTSM(log(winnebago),"Linear");
lglCoef = lglmdl$Coefficients;
lgltTest = lglmdl$t_Test_Result;lglFTest = lglmdl$F_Test_Result;
lglR2 = lglmdl$Rsquare;
lglESRE = lglmdl$External_Studentized_Residuals;
plot.ts(lglESRE,type = "o",ylab = "Studentized Residuals");abline(h = c(-3,3));

lgslmdl = DTTSM(log(winnebago),"SeasonL",TRUE);
lgslCoef = lgslmdl$Coefficients;
lgsltTest = lgslmdl$t_Test_Result;lgslFTest = lgslmdl$F_Test_Result;
lgslR2 = lgslmdl$Rsquare;
lgslESRE = lgslmdl$External_Studentized_Residuals;
plot.ts(lgslESRE,type = "o",ylab = "Studentized Residuals");abline(h = c(-3,3));
lgslRun = runs.test(lgslESRE);
acf(lgslESRE);hist(lgslESRE);qqnorm(lgslESRE);qqline(lgslESRE);
lgslSW = shapiro.test(lgslESRE);
idx = abs(lgslESRE)<3;
lgslDmdl = DTTSM(log(winnebago[idx]),"SeasonL",TRUE);
lgslDSW = shapiro.test(lgslDmdl$External_Studentized_Residuals)
