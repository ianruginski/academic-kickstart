## ---- message=FALSE, warning=FALSE---------------------------------------
##this calls the packages required. use install.packages('packagename') if not installed. We will focus on using ggplot2 for plotting, along with an extension GGally, car, and MASS for outlier detection.
library(ggplot2)
library(GGally)
library(car)
library(MASS)


## ------------------------------------------------------------------------
feardata <- read.csv('feardata.csv') #read-in datafile
fit <- lm(HeightEstimate ~ StateFear_c + TraitFear_c,feardata) #assign regression results to fit



## ----fig.align='center'--------------------------------------------------
theme_set(theme_bw(base_size = 12)) #changes default theme to black and white with bigger font sizes
ggpairs(feardata, columns = 7:5) #this will create a scatterplot matrix of each of our X variables (trait and state fear) against Y (height estimates).  


## ----fig.align='center'--------------------------------------------------
ggplot(feardata, aes(TraitFear_c,HeightEstimate)) + stat_smooth(method="loess") + stat_smooth(method="lm", color="red", fill="red", alpha=.25) + geom_point()
ggplot(feardata, aes(StateFear_c,HeightEstimate)) + stat_smooth(method="loess") + stat_smooth(method="lm", color="red", fill="red", alpha=.25) + geom_point()


## ----fig.align='center'--------------------------------------------------
spreadLevelPlot(fit)


## ----fig.align='center'--------------------------------------------------
# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot") # distribution of studentized residuals


## ----fig.align='center'--------------------------------------------------
#Use MASS package to generate histogram of residual distribution
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
   main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)


## ------------------------------------------------------------------------
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 #checks if the VIF's in your model are > 2, usually indicating a problem if TRUE


## ----fig.align='center'--------------------------------------------------
feardata$leverage <- hatvalues(fit)
ggplot(feardata, aes(X, leverage)) + geom_point() + ylim(0,1) + xlab('Case')


## ------------------------------------------------------------------------
# list the observations with large hat value
fit.hat <- hatvalues(fit)
id.fit.hat <- which(fit.hat > (2*(4+1)/nrow(feardata))) ##  hatvalue > #2*(k+1)/n
fit.hat[id.fit.hat]


## ----fig.align='center'--------------------------------------------------
feardata$studres <- studres(fit) #adds the studendized residuals to our dataframe
ggplot(feardata, aes(X,studres)) + geom_point() + geom_hline(color="red", yintercept=0) + xlab('Case')


## ----fig.align='center'--------------------------------------------------
#identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(feardata)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)


## ----fig.align='center'--------------------------------------------------
feardata$dffits <- dffits(fit)
ggplot(feardata, aes(X,dffits)) + geom_point() + geom_hline(color="red", yintercept=0) + xlab('Case') + ylim(-5,5)


## ----fig.align='center'--------------------------------------------------
dfbetas <- dfbetas(fit)
feardata$dfbeta_int <- dfbetas[,1]
feardata$dfbeta_statefear <- dfbetas[,2]
feardata$dfbeta_traitfear <- dfbetas[,3]
ggplot(feardata, aes(X,dfbeta_int)) + geom_point() + geom_hline(color="red", yintercept=0) + xlab('Case') + ylim(-5,5)
ggplot(feardata, aes(X,dfbeta_statefear)) + geom_point() + geom_hline(color="red", yintercept=0) + xlab('Case') + ylim(-5,5)
ggplot(feardata, aes(X,dfbeta_traitfear)) + geom_point() + geom_hline(color="red", yintercept=0) + xlab('Case') + ylim(-5,5)


## ----fig.align='center'--------------------------------------------------
influence.measures(fit)


## ------------------------------------------------------------------------
summary(fit) #first, let's remind ourself of the results with 38 in the model
feardata.no38 <- feardata[-38,] #remove case 38 from the dataframe and assign this to a new dataframe
fit.no38 <- lm(HeightEstimate ~ StateFear_c + TraitFear_c, feardata.no38) #new model without 38
summary(fit.no38) #did our results change?


## ----fig.align='center'--------------------------------------------------
plot(fit)


## ----warning=FALSE-------------------------------------------------------
library(gvlma)
gvlma(fit)

