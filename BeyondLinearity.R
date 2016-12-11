#####################################################################################
# Author : Yogesh Gongati                                                           #
# Date : 11/27/2016                                                                 #
# Course : Data Analysis Using R by Professor Ram Gopal                             #
# Topic : Concepts beyond Linearity: Splines                                        #
# Packages Explored : ISLR, splines, gam                                            #
#####################################################################################

## Have fun running the code##

# removing all other environment variables if required
rm(list = ls())

#install.packages("ISLR")
library(ISLR)
library(splines)
data(Auto)
attach(Auto)
str(Auto)
#View(Auto)
#install.packages("gam")
library(gam)

##The below command gives us the plot of displacement and mpg and a corresponding linear fit 
plot(Auto$displacement, Auto$mpg)
abline(lm(mpg~displacement))

##Below is a fucntion that we created to predict the outcomes of 
##vaious non linear models., calculate 95% confidence intervals, and 
##then plot the fit and confidence intervals

allparam<-function(model)
{
  Disp =range(Auto$displacement) # range of displacement
  Dispgrid=seq(from=Disp [1],to=Disp [2]) 
  predictions<-predict(model, newdata=list(displacement=Dispgrid), se=T) # predictions for range thourgh steps
  standarderror=cbind(predictions$fit +2* predictions$se.fit ,predictions$fit -2* predictions$se.fit) # calculating standard errors
  plot(Auto$displacement ,Auto$mpg ,xlim=Disp ,cex =.5,col=" black ") # plot displacement and mpg
  lines(Dispgrid ,predictions$fit,lwd=2,col="blue") # Plot fit line 
  matlines(Dispgrid ,standarderror ,lwd=1, col=" blue",lty=3) # plot confidence intervals
}

Model1=lm(mpg ~ poly(displacement) ,data=Auto) # polynomial of 0 degree
(summary (Model1)) # summary of model fit
allparam(Model1) # calling the defined function

Model2=lm(mpg ~ poly(displacement ,2) ,data=Auto) #polynomial of 2 degree
(summary (Model2))# summary of model fit
allparam(Model2) # calling the defined function

Model3=lm(mpg ~ poly(displacement ,3) ,data=Auto) #polynomial of 2 degree
(summary (Model3)) # summary of model fit
allparam(Model3) # calling the defined function
anova(Model1,Model2,Model3) # model comparison

#Stepwise Function
table(cut(displacement ,4)) # cut the region in to 4 parts
Stepwise<-lm(mpg~cut(displacement,4)) # Stepwise model with 4 cuts
summary(Stepwise) 
allparam(Stepwise)

#Regression splines

quantile(displacement) # finding the quantiles
splinemodel<-lm(mpg ~ bs(displacement, knots = c(105,151,275)), data = Auto) #regression spline with 3 knots at quantiles
allparam(splinemodel)

##Smoothing splines

smooth<-smooth.spline(mpg,displacement , df = 15) # smooth spline with 15 degrees of freedom
smooth2=smooth.spline (mpg ,displacement ,cv=TRUE) # smooth spline with Cross validation
plot(mpg,displacement ,cex =.5,col=" darkgrey ") #Plot mpg and displacement 
lines(smooth ,col="red",lwd =2) #Plot smooth fit
lines(smooth2,col="blue",lwd=2) # plot smooth2 fit

#Range of values for variable displacement
dislimits=range(displacement)

#Forming a grid of data from minimum value to maximum value of displacment, increment by 1 (Integer data)
displacement.grid=seq(from=dislimits[1],to=dislimits[2])

par(mfrow=c(1,1))
plot(Auto$displacement,Auto$mpg,xlim=dislimits ,cex =.5,col=" black ")

########################################################################
########################### LOCAL REGRESSION ###########################
########################################################################

#Fitting Local Regression Splines on the Auto data for mpg vs displacement

#At span=0.11, fitting local regression on displacement with degree 2 polynomial and with 95% confidence intervals (=2 standard deviations) 
localreg1=loess(Auto$mpg~Auto$displacement,degree=2,span=0.11)
p1=predict(localreg1,displacement.grid,se=T)
stderr1=cbind(p1$fit +2* p1$se.fit ,p1$fit -2* p1$se.fit)

#At span=0.4, fitting local regression on displacement with degree 2 polynomial and with 95% confidence intervals (=2 standard deviations) 
localreg2=loess(Auto$mpg~Auto$displacement,degree=2,span=0.3)
p2=predict(localreg2,displacement.grid,se=T)
stderr2=cbind(p2$fit +2* p2$se.fit ,p2$fit -2* p2$se.fit)

#At span=0.8, fitting local regression on displacement with degree 2 polynomial and with 95% confidence intervals (=2 standard deviations) 
localreg3=loess(Auto$mpg~Auto$displacement,degree=2,span=0.8)
p3=predict(localreg3,displacement.grid,se=T)
stderr3=cbind(p3$fit +2* p3$se.fit ,p3$fit -2* p3$se.fit)

#Plotting and Comparing Baseline model, Simple Linear Regression and Local Regression with different Spans

par(mfrow=c(2,3))

#Original data points
plot(Auto$displacement,Auto$mpg,xlim=dislimits ,cex =.5,col=" black",
     main="mpg Vs displacement",xlab="Engine Displacement",ylab="Miles per Gallon")

#Baseline model graph (mean of mpg)
plot(Auto$displacement,Auto$mpg,xlim=dislimits ,cex =.5,col=" black",
     main="BaseLine Model curve",xlab="Engine Displacement",ylab="Miles per Gallon")
abline(h=mean(Auto$mpg,col="gray"))

#Simple Linear Regression plot
plot(Auto$displacement,Auto$mpg,xlim=dislimits ,cex =.5,col=" black",
     main="Simple Linear Regression",xlab="Engine Displacement",ylab="Miles per Gallon")
abline(lm(Auto$mpg~Auto$displacement),col="pink")

#Local Regression Splines plot with two standard deviations (95% confidence interval) at span=0.11
plot(Auto$displacement,Auto$mpg,xlim=dislimits ,cex =.5,col=" black",
     main="Local Regression at span=0.11",xlab="Engine Displacement",ylab="Miles per Gallon")
lines(displacement.grid,p1$fit,col="red",lwd=1)
matlines(displacement.grid ,stderr1 ,lwd=1, col=" red",lty=3)

#Local Regression Splines plot with two standard deviations (95% confidence interval) at span=0.4
plot(Auto$displacement,Auto$mpg,xlim=dislimits ,cex =.5,col=" black",
     main="Local Regression at span=0.4",xlab="Engine Displacement",ylab="Miles per Gallon")
lines(displacement.grid,p2$fit,col="blue",lwd=1)
matlines(displacement.grid ,stderr2 ,lwd=1, col=" blue",lty=3)

#Local Regression Splines plot with two standard deviations (95% confidence interval) at span=0.8
plot(Auto$displacement,Auto$mpg,xlim=dislimits ,cex =.5,col=" black",
     main="Local Regression at span=0.8",xlab="Engine Displacement",ylab="Miles per Gallon")
lines(displacement.grid,p3$fit,col="green",lwd=1)
matlines(displacement.grid ,stderr3 ,lwd=1, col=" green",lty=3)

#We see that Local Regression Splines are much better fit than Baseline model and Simple Linear Regression lines

#Let us overlap and see the best fit among Local Regression with different spans in one graph
################

par(mfrow=c(1,1))

#Adding lables,title,legend and other features to the plot
plot(Auto$displacement,Auto$mpg,xlim=dislimits,xlab="Engine Displacement", ylab="Miles Per Gallon",
     main="Local Regression Splines" ,cex =.5,col=" black")
lines(displacement.grid,p1$fit,col="red",lwd=1)
lines(displacement.grid,p2$fit,col="blue",lwd=1)
lines(displacement.grid,p3$fit,col="green",lwd=1)
legend(350,48,c("Span=0.11","Span=0.4","Span=0.8"),lty=c(1,1,1),lwd=c(1,1,1),col=c("red","blue","green"),cex=0.7,pt.cex=5,bty='n')

#We see that as span value increases curve becomes more smooth, that is in bias-variance trade off, variance increases as bias reduces
#Too less span overfits the curve as shown in red coloured lines
#So we select the optimum trade-off value for span at 0.3, and go ahead with ******localreg2******* model.

####################################################################################
############################## General Additive Model ##############################
####################################################################################

#Baseline model assuming linear relationship between response variable and predictors
baseline=lm(mpg~.,data = Auto)
summary(baseline)

####Test Correlation among the predictors
#Subset the datset with numeric values using sapply function
Auto_num=sapply(Auto,is.numeric)
Auto_cont=Auto[,Auto_num]
cor(Auto_cont)

#Displacement and weight; Displacement and cylinders have >90% of correlation, thus weight and cylinders can be ignored while building the model

########### Function to display the plot model function fitting, standard errors and model summary ##########

gamoutput<-function(gammodel)
{
  library(gam)
  par(mfrow=c(2,3))
  plot.gam(gammodel,se=TRUE,col="blue")#plot method for GAM objects
  return(summary(gammodel))
}
#This function could be used to obtain model summary and plots for GAM models built

################### GAM using natural splines ####################################
#Load gam package to use spline functions available in this package
library(gam)
#lm() function is used with appropriate choice of natural spline functions for the predictors
gam1=lm(mpg~(ns(horsepower,df=4)+ns(acceleration,df=3)+ns(displacement,df=4)+ns(year,df=4)+ns(origin)),data=Auto)
gamoutput(gam1)

############################GAM using smoothening splines and gam function######################

#gam() function used to fit GAM using smoothening splines
#s() funtion fits smoothening spline model 
gam.s1=gam(mpg~s(acceleration,3)+s(horsepower,df=3)+s(displacement,df=3)+s(year,df=3),data=Auto)
gamoutput(gam.s1)

############## Local regression fits can be used as a building block in GAM ##################
#lo() function is used to fit local regression with a specified span value

gam.lo=gam(mpg~s(acceleration,3)+s(horsepower,3)+lo(displacement,span=0.7)+lo(year,span=0.7),data=Auto)
gamoutput(gam.lo)

#Anova test is performed to determine the best models among the models built till now
anova(baseline,gam1,gam.s1,gam.lo,test="F")

#Anova test shows that gam.s1 is better model among the gam models built
#########################################################################
############################ End of the code ############################
#########################################################################