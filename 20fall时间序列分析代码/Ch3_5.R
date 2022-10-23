library("TSA");library("lawstat");
source('D:/R Files/TSAcourse/DTTSM.R');
data(wages);
plot.ts(wages,type = "o");
lmdl = DTTSM(wages,"Linear");
lCoef = lmdl$Coefficients;
ltTest = lmdl$t_Test_Result;lFTest = lmdl$F_Test_Result;
lR2 = lmdl$Rsquare;
lESRE = lmdl$External_Studentized_Residuals;
plot.ts(lESRE,type = "o",ylab = "Studentized Residuals");abline(h = c(-3,3));

qmdl = DTTSM(wages,"Quadratic");
qCoef = qmdl$Coefficients;
qtTest = qmdl$t_Test_Result;qFTest = qmdl$F_Test_Result;
qR2 = qmdl$Rsquare;
qESRE = qmdl$External_Studentized_Residuals
plot.ts(qESRE,type = "o",ylab = "Studentized Residuals");abline(h = c(-3,3));
qRun = runs.test(qESRE);
acf(qESRE);hist(qESRE);qqnorm(qESRE);qqline(qESRE);
qSW = shapiro.test(qESRE)