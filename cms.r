cms <- read.csv("C:/Users/vijay/Desktop/Spring 2018/EPIDEMIO7002/Assingment-2/cms.csv")
View(cms)
attach(cms)
names(cms)




par(mfrow=c(2,3))
q<-plot(y_CMS ~ t_CMS, data=cms, #Plot y vs t
        xlab = "Time (t)", #Legend title for X-axis
        ylab = "CMS (y)", #Legend title for y-axis
        pch = 16, #Shape of the scatterplot points
        main = "CMS (y vs. t)") #Plot title
r<-q+lines(cms$t_CMS, cms$y_CMS, type="l") #Add lines between the data points
###################### Make new plot (natural log of y vs t) ##############################################
cms$ln_CMS <- log(cms$y_CMS) #Add a new column to the dataset, then plot it
#compare: CMS$log10_CMS <-log10(CMS$y_CMS) #NOTE: in R, log=natural log, use log10=log base 10
s<-plot(y. ~ t_CMS, data=cms,
        xlab = "Time (t)",
        ylab = "CMS (log y)",
        pch = 16,
        main = "CMS (log y vs. t)")
t<-s+lines(cms$t_CMS, cms$y., type="l")


u<-plot(ln_CMS ~ t_CMS, data=cms,
     xlab = "Time (t)",
     ylab = "CMS (ln y)",
     pch = 16,
     main = "CMS (ln y vs. t)")
v<-u+lines(cms$t_CMS, cms$ln_CMS, type="l")


w<-plot(gomp ~ t_CMS, data=cms,
     xlab = "Time (t)",
     ylab = "CMS (gomp y)",
     pch = 16,
     main = "CMS (gompit y vs. t)")
x<-w+lines(cms$t_CMS, cms$gomp, type="l")

y<-plot(mono ~ t_CMS, data=cms,
     xlab = "Time (t)",
     ylab = "CMS (mono y)",
     pch = 16,
     main = "CMS (mono y vs. t)")
z<-y+lines(cms$t_CMS, cms$mono, type="l")


par(mfrow=c(2,3))
###################### Get SLR and OLS output (y vs t) ##################################
SLR_CMS1 <- lm (cms$y_CMS ~ cms$t_CMS) #Find the SLR for this plot (y vs t)
summary(SLR_CMS1) #Show the SLR output for this plot (y vs t)
anova(SLR_CMS1) #Get the ANOVA summary statistics
###################### Plot residuals (y vs t) ##########################################
cms$SLR_CMS1.res = resid(SLR_CMS1) #Create residuals for y vs t
plot(SLR_CMS1.res ~ t_CMS, data=cms, #Plot the residuals vs t
     ylab="CMS (y)", xlab="Time (t)",
     main="Y vs. T Residuals")
abline(0, 0) #Add the line for residuals at y=0


SLR_CMS2 <- lm (cms$y. ~ cms$t_CMS) #Find the SLR for this plot (y vs t)
summary(SLR_CMS2) #Show the SLR output for this plot (y vs t)
anova(SLR_CMS2) #Get the ANOVA summary statistics
###################### Plot residuals (y vs t) ##########################################
cms$SLR_CMS2.res = resid(SLR_CMS2) #Create residuals for y vs t
plot(SLR_CMS2.res ~ t_CMS, data=cms, #Plot the residuals vs t
     ylab="CMS (log y)", xlab="Time (t)",
     main="log Y vs. T Residuals")
abline(0, 0) #Add the line for residuals at y=0


SLR_CMS3 <- lm (cms$gomp ~ cms$t_CMS) #Find the SLR for this plot (y vs t)
summary(SLR_CMS3) #Show the SLR output for this plot (y vs t)
anova(SLR_CMS3) #Get the ANOVA summary statistics
###################### Plot residuals (y vs t) ##########################################
cms$SLR_CMS3.res = resid(SLR_CMS3) #Create residuals for y vs t
plot(SLR_CMS3.res ~ t_CMS, data=cms, #Plot the residuals vs t
     ylab="CMS (gomp y)", xlab="Time (t)",
     main="gomp Y vs. T Residuals")
abline(0, 0) #Add the line for residuals at y=0


SLR_CMS4 <- lm (cms$expo ~ cms$t_CMS) #Find the SLR for this plot (y vs t)
summary(SLR_CMS4) #Show the SLR output for this plot (y vs t)
anova(SLR_CMS4) #Get the ANOVA summary statistics
###################### Plot residuals (y vs t) ##########################################
cms$SLR_CMS4.res = resid(SLR_CMS4) #Create residuals for y vs t
plot(SLR_CMS4.res ~ t_CMS, data=cms, #Plot the residuals vs t
     ylab="CMS (expo y)", xlab="Time (t)",
     main="expo Y vs. T Residuals")
abline(0, 0) #Add the line for residuals at y=0

SLR_CMS5 <- lm (cms$mono ~ cms$t_CMS) #Find the SLR for this plot (y vs t)
summary(SLR_CMS5) #Show the SLR output for this plot (y vs t)
anova(SLR_CMS5) #Get the ANOVA summary statistics
###################### Plot residuals (y vs t) ##########################################
cms$SLR_CMS5.res = resid(SLR_CMS5) #Create residuals for y vs t
plot(SLR_CMS5.res ~ t_CMS, data=cms, #Plot the residuals vs t
     ylab="CMS (mono y)", xlab="Time (t)",
     main="mono Y vs. T Residuals")
abline(0, 0) #Add the line for residuals at y=0



par(mfrow=c(2,3))
k<-plot(dy.dt ~ t_CMS, data=cms, #Plot y vs t
        xlab = "Time (t)", #Legend title for X-axis
        ylab = "dy/dt", #Legend title for y-axis
        pch = 16, #Shape of the scatterplot points
        main = "CMS (dy/dt vs. t)") #Plot title
l<-k+lines(cms$t_CMS, cms$dy.dt, type="l") #Add lines between the data points
###################### Make new plot (natural log of y vs t) ##############################################
m<-plot(Exp_dy.dt ~ t_CMS, data=cms,
        xlab = "Time (t)",
        ylab = "CMS (log dy/dt)",
        pch = 16,
        main = "CMS (log dy/dt vs. t)")
n<-m+lines(cms$t_CMS, cms$Exp_dy.dt, type="l")

o<-plot(mono_dy.dt ~ t_CMS, data=cms,
        xlab = "Time (t)",
        ylab = "CMS (mono_dy/dt)",
        pch = 16,
        main = "CMS (mono_dy/dt vs. t)")
h<-o+lines(cms$t_CMS, cms$mono_dy.dt, type="l")


f<-plot(Logi_dy.dt ~ t_CMS, data=cms,
        xlab = "Time (t)",
        ylab = "CMS (Logi_dy/dt)",
        pch = 16,
        main = "CMS (Logi_dy/dt vs. t)")
g<-f+lines(cms$t_CMS, cms$Logi_dy.dt, type="l")

i<-plot(Gomp_dy.dt ~ t_CMS, data=cms,
        xlab = "Time (t)",
        ylab = "CMS (Gomp_dy/dt)",
        pch = 16,
        main = "CMS (Gomp_dy/dt vs. t)")
j<-i+lines(cms$t_CMS, cms$Gomp_dy.dt, type="l")

names(wmv)


plot(expo ~ T, data=wmv,
     xlab = "Time (t)",
     ylab = "wmv(ln y)",
     pch = 16,
     main = "wmv(Exponential  of y vs. t)")
SLRexp_CMS<- lm (expo ~ T, data = wmv)
abline (SLRexp_CMS, lty = 1) #Adds the regression line
r2 <- round(summary(SLRexp_CMS)$r.squared, 2)
rmse <- round(sqrt(deviance(SLRexp_CMS)/df.residual(SLRexp_CMS)), 4)
eqn <- bquote(r^2 == .(r2)*"," ~~ SSE ==.(rmse))
text(2, 2, eqn,pos = 4)

plot(y. ~ T, data=wmv,
     xlab = "Time (t)",
     ylab = "wmv(ln y)",
     pch = 16,
     main = "wmv(logit  of y vs. t)")
SLRlogit_CMS<- lm (y. ~ T, data = wmv)
abline (SLRlogit_CMS, lty = 1) #Adds the regression line
r2 <- round(summary(SLRlogit_CMS)$r.squared, 2)
rmse <- round(sqrt(deviance(SLRlogit_CMS)/df.residual(SLRlogit_CMS)), 4)
eqn <- bquote(r^2 == .(r2)*"," ~~ SSE ==.(rmse))
text(2, 2, eqn,pos = 4)

plot(gomp ~ T, data=wmv,
     xlab = "Time (t)",
     ylab = "wmv(ln y)",
     pch = 16,
     main = "wmv(gompit  of y vs. t)")
SLRgompit_CMS<- lm (gomp ~ T, data = wmv)
abline (SLRgompit_CMS, lty = 1) #Adds the regression line
r2 <- round(summary(SLRgompit_CMS)$r.squared, 2)
rmse <- round(sqrt(deviance(SLRgompit_CMS)/df.residual(SLRgompit_CMS)), 4)
eqn <- bquote(r^2 == .(r2)*"," ~~ SSE ==.(rmse))
text(2, 2, eqn,pos = 4)
 
plot(y. ~ T, data=wmv,
     xlab = "Time (t)",
     ylab = "wmv(ln y)",
     pch = 16,
     main = "wmv(logit  of y vs. t)")
SLRlogit_CMS<- lm (y. ~ T, data = wmv)
abline (SLRlogit_CMS, lty = 1) #Adds the regression line
r2 <- round(summary(SLRlogit_CMS)$r.squared, 2)
rmse <- round(sqrt(deviance(SLRlogit_CMS)/df.residual(SLRlogit_CMS)), 4)
eqn <- bquote(r^2 == .(r2)*"," ~~ SSE ==.(rmse))
text(2, 2, eqn,pos = 4)

plot(gomp ~ T, data=wmv,
     xlab = "Time (t)",
     ylab = "wmv(ln y)",
     pch = 16,
     main = "wmv(gompit  of y vs. t)")
SLRgompit_wmv<- lm (gomp ~ T, data = wmv)
abline (SLRgompit_wmv, lty = 1) #Adds the regression line
r2 <- round(summary(SLRgompit_wmv)$r.squared, 2)
rmse <- round(sqrt(deviance(SLRgompit_wmv)/df.residual(SLRgompit_wmv)), 4)
rmse
eqn <- bquote(r^2 == .(r2)*"," ~~ SSE ==.(rmse))
eqn
text(2, 2, eqn, pos = 4)


