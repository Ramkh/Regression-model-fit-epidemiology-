
wmv <- read.csv("C:/Users/vijay/Desktop/Spring 2018/EPIDEMIO7002/Assingment-2/wmv.csv")
wmv <- read.csv("C:/Users/vijay/Desktop/Spring 2018/EPIDEMIO7002/Assingment-2/wmv.csv")
View(wmv)
detach(cms)
attach(wmv)
names(wmv)

names (cms)


par(mfrow=c(2,3))
q<-plot(y.WMV.2. ~ T, data=wmv, #Plot y vs t
        xlab = "Time (t)", #Legend title for X-axis
        ylab = "wmv (y)", #Legend title for y-axis
        pch = 16, #Shape of the scatterplot points
        main = "wmv (y vs. t)") #Plot title
r<-q+lines(wmv$T, wmv$y.WMV.2., type="l") #Add lines between the data points
###################### Make new plot (natural log of y vs t) ##############################################
wmv$ln_wmv <- log(wmv$y.WMV.2.) #Add a new column to the dataset, then plot it
#compare: wmv$log10_wmv <-log10(wmv$y.WMV.2.) #NOTE: in R, log=natural log, use log10=log base 10
s<-plot(y. ~ T, data=wmv,
        xlab = "Time (t)",
        ylab = "wmv (log y)",
        pch = 16,
        main = "wmv (log y vs. t)")
t<-s+lines(wmv$T, wmv$y., type="l")

u<-plot(ln_wmv ~ T, data=wmv,
        xlab = "Time (t)",
        ylab = "wmv (ln y)",
        pch = 16,
        main = "wmv (ln y vs. t)")
v<-u+lines(wmv$T, wmv$ln_wmv, type="l")


w<-plot(gomp ~ T, data=wmv,
        xlab = "Time (t)",
        ylab = "wmv (gomp y)",
        pch = 16,
        main = "wmv (gompit y vs. t)")
x<-w+lines(wmv$T, wmv$gomp, type="l")

y<-plot(mono ~ T, data=wmv,
        xlab = "Time (t)",
        ylab = "wmv (mono y)",
        pch = 16,
        main = "wmv (mono y vs. t)")
z<-y+lines(wmv$T, wmv$mono, type="l")


par(mfrow=c(2,3))
###################### Get SLR and OLS output (y vs t) ##################################
SLR_wmv1 <- lm (wmv$y.WMV.2. ~ wmv$T) #Find the SLR for this plot (y vs t)
summary(SLR_wmv1) #Show the SLR output for this plot (y vs t)
anova(SLR_wmv1) #Get the ANOVA summary statistics
###################### Plot residuals (y vs t) ##########################################
wmv$SLR_wmv1.res = resid(SLR_wmv1) #Create residuals for y vs t
plot(SLR_wmv1.res ~ T, data=wmv, #Plot the residuals vs t
     ylab="wmv (y)", xlab="Time (t)",
     main="Y vs. T Residuals")
abline(0, 0) #Add the line for residuals at y=0


SLR_wmv2 <- lm (wmv$y. ~ wmv$T) #Find the SLR for this plot (y vs t)
summary(SLR_wmv2) #Show the SLR output for this plot (y vs t)
anova(SLR_wmv2) #Get the ANOVA summary statistics
###################### Plot residuals (y vs t) ##########################################
wmv$SLR_wmv2.res = resid(SLR_wmv2) #Create residuals for y vs t
plot(SLR_wmv2.res ~ T, data=wmv, #Plot the residuals vs t
     ylab="wmv (log y)", xlab="Time (t)",
     main="log Y vs. T Residuals")
abline(0, 0) #Add the line for residuals at y=0


SLR_wmv3 <- lm (wmv$gomp ~ wmv$T) #Find the SLR for this plot (y vs t)
summary(SLR_wmv3) #Show the SLR output for this plot (y vs t)
anova(SLR_wmv3) #Get the ANOVA summary statistics
sd<-sd(wmv$gomp)

wmv$Varg <-  (1-wmv$y)/((wmv$y)*log(wmv$y)^2) #Add a new column to the dataset, then plot it
wmv$sd<-sd(wmv$gomp)

model.2 <- lm(wmv$gomp ~ wmv$T, weights=1/sd(wmv$gomp)^2)
summary(model.2)
model.2 <- lm(wmv$gomp ~ wmv$T, weights=1/var)
summary(model.2)


wmv$Varg <-  (1-wmv$y.)/((wmv$y.)*log(wmv$y.)^2) #Add a new column to the dataset, then plot it
model.2 <- lm(y. ~ t, weights=1/Varg)
summary(model.2)
coef(model.2)

Weighted_CMS$Var
anova(model.2)
###################### Plot residuals (y vs t) ##########################################
wmv$SLR_wmv3.res = resid(SLR_wmv3) #Create residuals for y vs t
plot(SLR_wmv3.res ~ T, data=wmv, #Plot the residuals vs t
     ylab="wmv (gomp y)", xlab="Time (t)",
     main="gomp Y vs. T Residuals")
abline(0, 0) #Add the line for residuals at y=0


SLR_wmv4 <- lm (wmv$expo ~ wmv$T) #Find the SLR for this plot (y vs t)
summary(SLR_wmv4) #Show the SLR output for this plot (y vs t)
anova(SLR_wmv4) #Get the ANOVA summary statistics
###################### Plot residuals (y vs t) ##########################################
wmv$SLR_wmv4.res = resid(SLR_wmv4) #Create residuals for y vs t
plot(SLR_wmv4.res ~ T, data=wmv, #Plot the residuals vs t
     ylab="wmv (expo y)", xlab="Time (t)",
     main="expo Y vs. T Residuals")
abline(0, 0) #Add the line for residuals at y=0

SLR_wmv5 <- lm (wmv$mono ~ wmv$T) #Find the SLR for this plot (y vs t)
summary(SLR_wmv5) #Show the SLR output for this plot (y vs t)
anova(SLR_wmv5) #Get the ANOVA summary statistics
###################### Plot residuals (y vs t) ##########################################
wmv$SLR_wmv5.res = resid(SLR_wmv5) #Create residuals for y vs t
plot(SLR_wmv5.res ~ T, data=wmv, #Plot the residuals vs t
     ylab="wmv (mono y)", xlab="Time (t)",
     main="mono Y vs. T Residuals")
abline(0, 0) #Add the line for residuals at y=0



par(mfrow=c(2,3))
k<-plot(dy.dt ~ T, data=wmv, #Plot y vs t
        xlab = "Time (t)", #Legend title for X-axis
        ylab = "dy/dt", #Legend title for y-axis
        pch = 16, #Shape of the scatterplot points
        main = "wmv (dy/dt vs. t)") #Plot title
l<-k+lines(wmv$T, wmv$dy.dt, type="l") #Add lines between the data points
###################### Make new plot (natural log of y vs t) ##############################################
m<-plot(Exp_dy.dt ~ T, data=wmv,
        xlab = "Time (t)",
        ylab = "wmv (log dy/dt)",
        pch = 16,
        main = "wmv (log dy/dt vs. t)")
n<-m+lines(wmv$T, wmv$Exp_dy.dt, type="l")

o<-plot(mono_dy.dt ~ T, data=wmv,
        xlab = "Time (t)",
        ylab = "wmv (mono_dy/dt)",
        pch = 16,
        main = "wmv (mono_dy/dt vs. t)")
h<-o+lines(wmv$T, wmv$mono_dy.dt, type="l")


f<-plot(Logi_dy.dt ~ T, data=wmv,
        xlab = "Time (t)",
        ylab = "wmv (Logi_dy/dt)",
        pch = 16,
        main = "wmv (Logi_dy/dt vs. t)")
g<-f+lines(wmv$T, wmv$Logi_dy.dt, type="l")

i<-plot(Gomp_dy.dt ~ T, data=wmv,
        xlab = "Time (t)",
        ylab = "wmv (Gomp_dy/dt)",
        pch = 16,
        main = "wmv (Gomp_dy/dt vs. t)")
j<-i+lines(wmv$T, wmv$Gomp_dy.dt, type="l")

