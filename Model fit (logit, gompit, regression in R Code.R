
###################### Make one plot (y vs t) ###########################################
attach(CMS)
plot(y_CMS ~ t_CMS, data=CMS, #Plot y vs t
     xlab = "Time (t)", #Legend title for X-axis
     ylab = "CMS (y)", #Legend title for y-axis
     pch = 16, #Shape of the scatterplot points
     main = "CMS (y vs. t)") #Plot title
lines(CMS$t_CMS, CMS$y_CMS, type="l") #Add lines between the data points
###################### Make one plot (dydtCMS vs t) ###########################################
plot(dydtCMS ~ t_CMS, data=CMS, #Plot y vs t
     xlab = "Time (t)", #Legend title for X-axis
     ylab = "CMS (y)", #Legend title for y-axis
     pch = 16, #Shape of the scatterplot points
     main = "CMS (dy/dtCMS vs. t)") #Plot title
lines(CMS$t_CMS, dydtCMS, type="l") #Add lines between the data points

###################### Get SLR and OLS output (y vs t) ##################################
SLR_CMS <- lm (CMS$y_CMS ~ CMS$t_CMS) #Find the SLR for this plot (y vs t)
summary(SLR_CMS) #Show the SLR output for this plot (y vs t)
anova(SLR_CMS) #Get the ANOVA summary statistics
###################### Plot residuals (y vs t) ##########################################
CMS$SLR_CMS.res = resid(SLR_CMS) #Create residuals for y vs t
plot(SLR_CMS.res ~ t_CMS, data=CMS, #Plot the residuals vs t
     ylab="CMS (y)", xlab="Time (t)",
     main="Y vs. T Residuals")
abline(0, 0) #Add the line for residuals at y=0
###################### Make new plot (natural log of y vs t) ##############################################
CMS$ln_CMS <- log(CMS$y_CMS) #Add a new column to the dataset, then plot it
#compare: CMS$log10_CMS <-log10(CMS$y_CMS) #NOTE: in R, log=natural log, use log10=log base 10
plot(ln_CMS ~ t_CMS, data=CMS,
     xlab = "Time (t)",
     ylab = "CMS (ln y)",
     pch = 16,
     main = "CMS (ln y vs. t)")
lines(CMS$t_CMS, CMS$ln_CMS, type="l")
###################### Get SLR and OLS output (natural log of y vs t) ##################################
SLRln_CMS <- lm (CMS$ln_CMS ~ CMS$t_CMS) #Find the SLR for this plot (y vs t)
summary(SLRln_CMS) #Show the SLR output for this plot (y vs t)
anova(SLRln_CMS) #Get the ANOVA summary statistics
###################### Plot residuals (natural log of y vs t) ##########################################
CMS$SLRln_CMS.res = resid(SLRln_CMS) #Create residuals for y vs t
plot(SLRln_CMS.res ~ t_CMS, data=CMS, #Plot the residuals vs t
     ylab="CMS (y)", xlab="Time (t)",
     main="natural log of y vs. T Residuals")
abline(0, 0) #Add the line for residuals at y=0
##################### Make new plot (logit of y vs t) ##############################################
library(faraway)
CMS$logit_CMS <- logit(CMS$y_CMS) #Add a new column to the dataset, then plot it
#compare: CMS$log10_CMS <-log10(CMS$y_CMS) #NOTE: in R, log=natural log, use log10=log base 10
plot(logit_CMS ~ t_CMS, data=CMS,
     xlab = "Time (t)",
     ylab = "CMS (ln y)",
     pch = 16,
     main = "CMS (logit y vs. t)")
lines(CMS$t_CMS, CMS$logit_CMS, type="l")
###################### Get SLR and OLS output (natural log of y vs t) ##################################
SLRlogit_CMS <- lm (CMS$logit_CMS ~ CMS$t_CMS) #Find the SLR for this plot (y vs t)
summary(SLRlogit_CMS) #Show the SLR output for this plot (y vs t)
anova(SLRlogit_CMS) #Get the ANOVA summary statistics
###################### Plot residuals (natural log of y vs t) ##########################################
CMS$SLRlogit_CMS.res = resid(SLRlogit_CMS) #Create residuals for y vs t
plot(SLRlogit_CMS.res ~ t_CMS, data=CMS, #Plot the residuals vs t
     ylab="CMS (y)", xlab="Time (t)",
     main=" logit of y vs. T Residuals")
abline(0, 0) #Add the line for residuals at y=0
##################### Make new plot (gompit  of y vs t) ##############################################
CMS$gomgit_CMS <- -log( -(log(y_CMS))) #Add a new column to the dataset, then plot it
#compare: CMS$log10_CMS <-log10(CMS$y_CMS) #NOTE: in R, log=natural log, use log10=log base 10
plot(CMS$gomgit_CMS ~ t_CMS, data=CMS,
     xlab = "Time (t)",
     ylab = "CMS (ln y)",
     pch = 16,
     main = "CMS (gompit  of y vs. t)")
lines(CMS$t_CMS, CMS$gomgit_CMS, type="l")
###################### Get SLR and OLS output (natural log of y vs t) ##################################
SLRgompit_CMS <- lm (CMS$gomgit_CMS ~ CMS$t_CMS) #Find the SLR for this plot (y vs t)
summary(SLRgompit_CMS) #Show the SLR output for this plot (y vs t)
anova(SLRgompit_CMS) #Get the ANOVA summary statistics
###################### Plot residuals (natural log of y vs t) ##########################################
CMS$SLRgompit_CMS.res = resid(SLRgompit_CMS) #Create residuals for y vs t
plot(SLRgompit_CMS.res ~ t_CMS, data=CMS, #Plot the residuals vs t
     ylab="CMS (y)", xlab="Time (t)",
     main=" gompit of y vs. T Residuals")
abline(0, 0) #Add the line for residuals at y=0
###################### Plot a collection ##########################################
par(mfrow=c(2,2))
plot(y_CMS ~ t_CMS, data=CMS, #Plot y vs t
     xlab = "Time (t)", #Legend title for X-axis
     ylab = "CMS (y)", #Legend title for y-axis
     pch = 16, #Shape of the scatterplot points
     main = "CMS (y vs. t)") #Plot title
lines(CMS$t_CMS, CMS$y_CMS, type="l") #Add lines between the data points
plot(ln_CMS ~ t_CMS, data=CMS,
     xlab = "Time (t)",
     ylab = "CMS (ln y)",
     pch = 16,
     main = "CMS (ln y vs. t)")
lines(CMS$t_CMS, CMS$ln_CMS, type="l")
plot(logit_CMS ~ t_CMS, data=CMS,
     xlab = "Time (t)",
     ylab = "CMS (ln y)",
     pch = 16,
     main = "CMS (logit y vs. t)")
lines(CMS$t_CMS, CMS$logit_CMS, type="l")
plot(CMS$gomgit_CMS ~ t_CMS, data=CMS,
     xlab = "Time (t)",
     ylab = "CMS (ln y)",
     pch = 16,
     main = "CMS (gompit  of y vs. t)")
lines(CMS$t_CMS, CMS$gomgit_CMS, type="l")
###################### Plot residuals (y vs t) ##########################################
CMS$SLR_CMS.res = resid(SLR_CMS) #Create residuals for y vs t
plot(SLR_CMS.res ~ t_CMS, data=CMS, #Plot the residuals vs t
     ylab="CMS (y)", xlab="Time (t)",
     main="Y vs. T Residuals")
abline(0, 0) #Add the line for residuals at y=0
CMS$SLRln_CMS.res = resid(SLRln_CMS) #Create residuals for y vs t
plot(SLRln_CMS.res ~ t_CMS, data=CMS, #Plot the residuals vs t
     ylab="CMS (y)", xlab="Time (t)",
     main="natural log of y vs. T Residuals")
abline(0, 0) #Add the line for residuals at y=0
CMS$SLRlogit_CMS.res = resid(SLRlogit_CMS) #Create residuals for y vs t
plot(SLRlogit_CMS.res ~ t_CMS, data=CMS, #Plot the residuals vs t
     ylab="CMS (y)", xlab="Time (t)",
     main=" logit of y vs. T Residuals")
abline(0, 0) #Add the line for residuals at y=0
plot(SLRgompit_CMS.res ~ t_CMS, data=CMS, #Plot the residuals vs t
     ylab="CMS (y)", xlab="Time (t)",
     main=" gompit of y vs. T Residuals")
abline(0, 0) #Add the line for residuals at y=0
######################################################################################################
par(mfrow=c(2,2))
plot(y_CMS ~ t_CMS, data=CMS, #Plot y vs t
     xlab = "Time (t)", #Legend title for X-axis
     ylab = "CMS(y)", #Legend title for y-axis
     pch = 16, #Shape of the scatterplot points
     main = "CMS(y vs. t)") #Plot title
abline (SLR_CMS, lty = 1) #Adds the regression line
r2 <- round(summary(SLR_CMS)$r.squared, 2)
rmse <- round(sqrt(deviance(SLR_CMS)/df.residual(SLR_CMS)), 4)
eqn <- bquote(r^2 == .(r2)*"," ~~ SSE == .(rmse))
text(2, 0.8, eqn,pos = 4)

plot(ln_CMS~ t_CMS, data=CMS,
     xlab = "Time (t)",
     ylab = "CMS(ln y)",
     pch = 16,
     main = "CMS(ln y vs. t)")
abline (SLRln_CMS, lty = 1) #Adds the regression line
r2 <- round(summary(SLRln_CMS)$r.squared, 2)
rmse <- round(sqrt(deviance(SLRln_CMS)/df.residual(SLRln_CMS)), 4)
eqn <- bquote(r^2 == .(r2)*"," ~~ SSE == .(rmse))
text(2, -0.5, eqn,pos = 4)

plot(logit_CMS~ t_CMS, data=CMS,
     xlab = "Time (t)",
     ylab = "CMS(ln y)",
     pch = 16,
     main = "CMS(logit y vs. t)")
abline (SLRlogit_CMS, lty = 1) #Adds the regression line
r2 <- round(summary(SLRlogit_CMS)$r.squared, 2)
rmse <- round(sqrt(deviance(SLRlogit_CMS)/df.residual(SLRlogit_CMS)), 4)
eqn <- bquote(r^2 == .(r2)*"," ~~ SSE == .(rmse))
text(2, 2, eqn,pos = 4)

plot(CMS$gomgit_CMS~ t_CMS, data=CMS,
     xlab = "Time (t)",
     ylab = "CMS(ln y)",
     pch = 16,
     main = "CMS(gompit  of y vs. t)")
abline (SLRgompit_CMS, lty = 1) #Adds the regression line
r2 <- round(summary(SLRgompit_CMS)$r.squared, 2)
rmse <- round(sqrt(deviance(SLRgompit_CMS)/df.residual(SLRgompit_CMS)), 4)
eqn <- bquote(r^2 == .(r2)*"," ~~ SSE == .(rmse))
text(2, 2, eqn,pos = 4)




