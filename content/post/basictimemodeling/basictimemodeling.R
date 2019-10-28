## ----include=FALSE-------------------------------------------------------
require(knitr)
opts_chunk$set(message=FALSE)
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)
options(scipen=999)


## ----warning=FALSE, message=FALSE, dpi = 300-----------------------------
x <- c(1, 2, 3, 4)
y <- c(0, 0, 0, 0)
marks <- c(1, 2, 3, 4)

plot(x, y, xaxt = "n", 
     xlab='Time', 
     main = "Discrete units of time") + 
  lines(x, y) + 
  axis(1, at = marks, labels = marks) 


## ----warning=FALSE, message=FALSE, dpi = 300-----------------------------

distance <- c(0, 2.5, 3.625, 4.25, 4.5625, 4.71875, 4.796875, 4.835938, 4.855469, 4.865235, 4.870118)
step <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

df <- data.frame(distance, step)

plot(distance, step, main = "Let's get to the wall!", ylim=c(0, 10)) + 
  lines(smooth.spline(distance, step)) + axis(side=2,at=seq(0,10,1)) + abline(v = 5, lty = 3, lwd = 2)


## ----message=FALSE, dpi = 300--------------------------------------------
require(Hmisc) #for easily making lags and leads in your data
set.seed(999)
anxiety <- as.data.frame(filter(rnorm(1000, 200, 8), filter=rep(1,1), circular=T)) #simulate gaussian noise stationary time series 

colnames(anxiety) <- c("HR")
anxiety$HR <- as.numeric(anxiety$HR)

anxiety$HRlag <- as.numeric(Lag(anxiety$HR, 50)) #where the second number indicates amount to lag by. Note you can also use negative numbers, such as -1, to indicate a lead variable. Usually you'll want to assign this to a new variable so you don't lose your original data.

anxiety$HRlead <- as.numeric(Lag(anxiety$HR, -50)) #lets create a lead as well. 

attach(anxiety)
plot(HRlead, HR,main = "Autoregressive Model for Heart Rate", xlab = "Current Heart Rate", ylab = "Future Heart Rate (lag 50)") + abline(a = 198.204993, b = 0.007827, col = "blue", lwd = 2) + abline(a = 0, b = 1)



## ----message=FALSE, dpi=300----------------------------------------------
anxiety$HRdiff <- anxiety$HR - anxiety$HRlag

plot(anxiety$HR, anxiety$HRdiff, main = "Difference Model for Heart Rate", xlab = "Current Heart Rate", ylab = "Difference between Future and Current Heart Rate") + 
  abline(a = -198.20499, b = 0.99217, col = "blue", lwd = 2) + 
  abline(a = 0, b = 0)



## ----warning=FALSE-------------------------------------------------------
Embed <- function(x,E,tau) {
  len <- length(x)
  out <- x[1:(len-(E*tau)+tau)]
  for(i in 2:E) { out <- cbind(out,x[(1+((i-1)*tau)):(len-(E*tau)+(i*tau))]) }
  return(out)
}

GLLA <- function(x, embed, tau, deltaT, order=2) {
  L <- rep(1,embed)
  for(i in 1:order) {
    L <- cbind(L,(((c(1:embed)-mean(1:embed))*tau*deltaT)^i)/factorial(i))
  }
  
  W <- solve(t(L)%*%L)%*%t(L)
  EMat <- Embed(x,embed,1)
  Estimates <- EMat%*%t(W)
  
  return(Estimates)
}


## ----warning=FALSE, dpi = 300, message=FALSE-----------------------------
GLLAHR <- GLLA(x=anxiety$HR,embed=4,tau=1, deltaT = .25, order=1) #makes new dataframe with original variable (HR) as first column, and 1st order derivative (velocity) as second column
GLLAHR <- as.data.frame(GLLAHR) #make into dataframe
colnames(GLLAHR) <- c("HRposition", "HRvelocity") #rename columns for ease of tracking variables
plot(GLLAHR$HRposition, GLLAHR$HRvelocity, main = "GLLA Model for Heart Rate", xlab = "Heart Rate Position", ylab = "Heart Rate Velocity estimated using GLLA") + lines(lowess(GLLAHR$HRposition, GLLAHR$HRvelocity, f = 50, delta = 10), col = "blue", lwd = 2) + abline(a = 0, b = 0) #plot results
 


## ----warning=FALSE-------------------------------------------------------
 ContrastsGOLD <- function(Time,max) {
         Xi <- matrix(NA,max+1,length(Time))
       	  	for(r in 0:max) {
          	    	Xi[r+1,] <- (Time^r)
          	    	if(r>0) {
                   	for(p in 0:(r-1)) {
                       	 Xi[r+1,] <- Xi[r+1,] -
                      	       (Xi[p+1,]*(sum(Xi[p+1,]*(Time^r)))/(sum(Xi[p+1,]*(Time^p))))
                       	 }}}
         DXi <- diag(1/factorial(c(0:max)))%*%Xi
	 	 t(DXi)%*%solve(DXi%*%t(DXi))
         }
    
	GOLD <- function(x, Time, max,tau) {
          W <- ContrastsGOLD(Time,max)
          X <- Embed(x,length(Time),tau)
          Est <- X%*%W
          return(Est)          
          }
         
    Embed <- function(x,E,tau) {
         len <- length(x)
         out <- x[1:(len-(E*tau)+tau)]
         for(i in 2:E) {
              out <- cbind(out,x[(1+((i-1)*tau)):(len-(E*tau)+(i*tau))])
              }
         return(out)
         }

