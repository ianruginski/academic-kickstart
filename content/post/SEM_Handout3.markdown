---
title: "Structural Equation Modeling in R Tutorial 3: Path Analysis using R"
authors: ["admin"]
date: 2017-09-13
tags: ['structural equation modeling', 'regression', 'path analysis', 'R']
categories: ['structural equation modeling', 'regression', 'path analysis', 'R']
output: blogdown::html_page
---

{{% toc %}}

Made for Jonathan Butner's Structural Equation Modeling Class, Fall 2017, University of Utah. This handout begins by showing how to import a matrix into R. Then, we will overview how path models can be conducted simply as a series of regressions. such as transposing and inversing matrices. This syntax imports the 4 variable dataset from datafile pathmodel example 3.txt.



## Data Input
First, we will read in datafile using Fortran and clean up the datafile a little bit.

```r
library(psych)  #for descriptives
# make sure to set your working directory to where the file
# is located, e.g. setwd('D:/Downloads/Handout 1 -
# Regression') you can do this in R Studio by going to the
# Session menu - 'Set Working Directory' - To Source File
# Location
socdata <- read.fortran("pathmodel example 3.txt", c("F12.0", 
    "3F13.0"))
# note: #The format for a field is of one of the following
# forms: rFl.d, rDl.d, rXl, rAl, rIl, where l is the number
# of columns, d is the number of decimal places, and r is the
# number of repeats. F and D are numeric formats, A is
# character, I is integer, and X indicates columns to be
# skipped. The repeat code r and decimal place code d are
# always optional. The length code l is required except for X
# formats when r is present.

socdata <- read.table("pathmodel example 3.txt")  #this does the same thing as the read.fortran statement above!

# add column names
names(socdata) <- c("Behavior", "Snorm", "Belief", "Intent")

# get descriptives for the data
describe(socdata)
```

```
##          vars   n  mean   sd median trimmed  mad   min  max range  skew
## Behavior    1 180  0.09 2.24   0.11    0.12 2.55 -5.85 5.68 11.53 -0.09
## Snorm       2 180 -0.04 2.02   0.02   -0.01 2.03 -5.43 4.23  9.66 -0.13
## Belief      3 180 -0.01 0.96  -0.01    0.01 0.95 -2.40 2.33  4.72 -0.09
## Intent      4 180  0.00 1.01   0.03    0.02 1.02 -3.02 2.06  5.09 -0.29
##          kurtosis   se
## Behavior    -0.27 0.17
## Snorm       -0.31 0.15
## Belief      -0.21 0.07
## Intent      -0.34 0.08
```
<br>
## Path Models
Now, we'll complete a series of regressions. First, we will test a reduced model, where beliefs and social norms cause intentions and intentions cause behaviors. Our first model will regress intent onto belief and social norms, the first portion of the path model. If you remember from earlier in class, this is Azjen and Fishbein's theory of planned behavior. 


![Reduced path model](img1.png)


![Reduced model regression 1](img2.png)



```r
model.reduced.1 <- lm(Intent ~ Belief + Snorm, socdata)
summary(model.reduced.1)
```

```
## 
## Call:
## lm(formula = Intent ~ Belief + Snorm, data = socdata)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.76241 -0.39316  0.06305  0.39973  1.65727 
## 
## Coefficients:
##             Estimate Std. Error t value             Pr(>|t|)    
## (Intercept)  0.01400    0.04560   0.307                0.759    
## Belief      -0.31713    0.06871  -4.616           0.00000751 ***
## Snorm        0.49472    0.03260  15.178 < 0.0000000000000002 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.6117 on 177 degrees of freedom
## Multiple R-squared:  0.6382,	Adjusted R-squared:  0.6341 
## F-statistic: 156.1 on 2 and 177 DF,  p-value: < 0.00000000000000022
```
You'l want to note the .638 R-squared value, which will be utilized in the Q*W calculation for model construction. Each of the R-squared values in the following regressions will be used to calculate Q. 

Model 2, below, contains the remaining portion of the reduced model.

![Reduced model regression 2](img3.png)


```r
model.reduced.2 <- lm(Behavior ~ Intent, socdata)
summary(model.reduced.2)
```

```
## 
## Call:
## lm(formula = Behavior ~ Intent, data = socdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.7855 -1.0341 -0.0224  0.9642  5.1768 
## 
## Coefficients:
##             Estimate Std. Error t value            Pr(>|t|)    
## (Intercept)  0.09307    0.11790   0.789               0.431    
## Intent       1.56615    0.11691  13.396 <0.0000000000000002 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.582 on 178 degrees of freedom
## Multiple R-squared:  0.502,	Adjusted R-squared:  0.4992 
## F-statistic: 179.4 on 1 and 178 DF,  p-value: < 0.00000000000000022
```


Next, we will run the fully saturated model where we've allowed beliefs and social norms to also cause behaviors without having to go through intentions. There will be a final regression that accounts for intentions, social norms, and beliefs all predicting behavior simultaneously.

![Fully saturated path model](img4.png)


![Fully saturated model regression](img5.png)




```r
model.full.1 <- lm(Behavior ~ Intent + Snorm + Belief, socdata)
summary(model.full.1)
```

```
## 
## Call:
## lm(formula = Behavior ~ Intent + Snorm + Belief, data = socdata)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.89945 -0.58227 -0.01066  0.63923  2.38706 
## 
## Coefficients:
##             Estimate Std. Error t value            Pr(>|t|)    
## (Intercept)  0.12582    0.06414   1.962              0.0514 .  
## Intent       0.01864    0.10570   0.176              0.8603    
## Snorm        0.97677    0.06954  14.047 <0.0000000000000002 ***
## Belief       0.11631    0.10227   1.137              0.2570    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.8601 on 176 degrees of freedom
## Multiple R-squared:  0.8544,	Adjusted R-squared:  0.8519 
## F-statistic: 344.3 on 3 and 176 DF,  p-value: < 0.00000000000000022
```
Note how we need only one more regression equation (the equation above) to complete the fully saturated model. We will use the R-squared value from this regression to calculate Q below. 

<br>
## Models Using Q and W Indices

![Comparing Models Using Q and W Indices](img6.png)





