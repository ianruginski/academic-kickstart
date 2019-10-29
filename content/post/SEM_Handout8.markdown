---
title: "Structural Equation Modeling in R Tutorial 8: Introduction to Stacked Models and Factorial Measurement Invariance Testing"
authors: ["admin"]
date: 2017-12-01
tags: ['structural equation modeling', 'measurement', 'R', 'lavaan']
categories: ['structural equation modeling', 'measurement', 'R', 'lavaan']
commentable: true
output: blogdown::html_page
---

{{% toc %}}

Made for Jonathan Butner's Structural Equation Modeling Class, Fall 2017, University of Utah. This handout will focus on implementing stacked models in lavaan, which allow us to test a model for two different groups (for example, control vs. intervention). This syntax imports the X variable, 192 person dataset called HW9 2017.csv, which is an comma separated  data file. The data are for 10 items on the SF36 scale collected at 2 timepoints (20 total observed items for the scale), as well as a condition variable where 0 is control and 1 is intervention. There is also a diagnosis variable with 3 diagnoses. There is missing data coded as 99 in the original datafile. See http://lavaan.ugent.be/tutorial/groups.html for more on stacked models in lavaan.



## Data Input
First, we will read in the datafile and clean up the datafile a little bit, as well as load required packages.

```r
library(lavaan)  #for doing the SEM
library(dplyr)  #for subsetting data quickly if needed

# make sure to set your working directory to where the file
# is located, e.g. setwd('D:/Downloads/Handout 1 -
# Regression') you can do this in R Studio using menus by
# going to the Session menu - 'Set Working Directory' - To
# Source File Location
semdat <- read.csv("HW9 2017.csv", na.strings = 99)
# note that this data is in free format so we use read.table

# fix first column name
names(semdat)[1] <- c("condition")

# create dummy variables for diagnosis (for demonstration - i
# won't show any models with diagnosis)
semdat$d1[semdat$diag == 2] <- 0
semdat$d1[semdat$diag == 1] <- 1
semdat$d2[semdat$diag == 2] <- 1
semdat$d2[semdat$diag == 1] <- 0

head(semdat)  #take a look at the beginning of our datafile
```

```
##   condition diag sf3 sf4 sf5 sf6 sf7 sf8 sf9 sf10 sf11 sf12 sf3_1 sf4_1
## 1         1    2   1   1   1   1   1   1   1    1    1    2     1     1
## 2         1    2   2   2   2   2   2   3   1    1    2    3     2     2
## 3         1    3   1   1   1   1   1   1   1    1    1    2     1     2
## 4         0    2   1   1   1   1   1   2   1    2    3    2     1     1
## 5         1    2   1   2   2   1   1   2   1    2    3    3     1     1
## 6         1    3   1   1   1   1   1   1   1    1    1    1    NA    NA
##   sf5_1 sf6_1 sf7_1 sf8_1 sf9_1 sf10_1 sf11_1 sf12_1 d1 d2
## 1     1     1     1     1     1      1      1      1  0  1
## 2     2     2     2     1     3      3      2      2  0  1
## 3     1     2     3     2     1      1      1      3 NA NA
## 4     4     4     4     3     4      3      3      3  0  1
## 5     2     1     2     3     1      2      3      3  0  1
## 6    NA    NA    NA    NA    NA     NA     NA     NA NA NA
```
<br>
## Stacked Models in Lavaan
Your group variable should be specified using the `group = 'variablename'` argument. Notice that lavaan, unlike Mplus, does not automatically assume equality of lambdas (regression parameters) and taus (intercepts) across groups when generating stacked models.  Note that here we add an additional argument called `group.equal = c("loadings")`, which equates lambdas across groups. Think of this as assuming that there is no interaction with group (effect is same across groups). You can also extend this to include `group.equal = c('loadings', 'intercepts', 'residuals')`, amongst other arguments depending on which equality constraints you want to include in your model. Note that we can compare models where we have equal parameters across groups to a model where the parameters are different across groups. Note you can do the same with intercepts. You would then do a chi-sq difference test where you compare a model with equality constrants to a model where. This concept is called factorial measurement invariance testing (see the next section for how to do this). The bit of code specifying `missing = "fiml"` implements full information maximum likelihood estimation to handle missing data, which assumes that your data is missing at random (MAR) or missing completely at random (MCAR). 


```r
sem.model.equality <- "physfun =~ sf3+sf4+sf5+sf6+sf7+sf8+sf9+sf10+sf11+sf12"
sem.fit.equality <- sem(sem.model.equality, data = semdat, group = "condition", 
    group.equal = c("loadings"), missing = "fiml")
# missing = fiml implements full information ML for
# missingness
summary(sem.fit.equality, fit.measures = TRUE)
```

```
## lavaan 0.6-5 ended normally after 73 iterations
## 
##   Estimator                                         ML
##   Optimization method                           NLMINB
##   Number of free parameters                         60
##   Number of equality constraints                     9
##   Row rank of the constraints matrix                 9
##                                                       
##   Number of observations per group:                   
##     1                                              101
##     0                                               91
##   Number of missing patterns per group:               
##     1                                                1
##     0                                                1
##                                                       
## Model Test User Model:
##                                                       
##   Test statistic                               295.364
##   Degrees of freedom                                79
##   P-value (Chi-square)                           0.000
##   Test statistic for each group:
##     1                                          186.914
##     0                                          108.450
## 
## Model Test Baseline Model:
## 
##   Test statistic                               802.850
##   Degrees of freedom                                90
##   P-value                                        0.000
## 
## User Model versus Baseline Model:
## 
##   Comparative Fit Index (CFI)                    0.696
##   Tucker-Lewis Index (TLI)                       0.654
## 
## Loglikelihood and Information Criteria:
## 
##   Loglikelihood user model (H0)              -1387.924
##   Loglikelihood unrestricted model (H1)      -1240.242
##                                                       
##   Akaike (AIC)                                2877.848
##   Bayesian (BIC)                              3043.980
##   Sample-size adjusted Bayesian (BIC)         2882.428
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.169
##   90 Percent confidence interval - lower         0.149
##   90 Percent confidence interval - upper         0.190
##   P-value RMSEA <= 0.05                          0.000
## 
## Standardized Root Mean Square Residual:
## 
##   SRMR                                           0.099
## 
## Parameter Estimates:
## 
##   Information                                 Observed
##   Observed information based on                Hessian
##   Standard errors                             Standard
## 
## 
## Group 1 [1]:
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)
##   physfun =~                                          
##     sf3               1.000                           
##     sf4     (.p2.)    5.427    1.377    3.941    0.000
##     sf5     (.p3.)    5.955    1.530    3.893    0.000
##     sf6     (.p4.)    3.330    0.952    3.498    0.000
##     sf7     (.p5.)    6.114    1.612    3.793    0.000
##     sf8     (.p6.)    4.679    1.305    3.587    0.000
##     sf9     (.p7.)    3.383    0.916    3.695    0.000
##     sf10    (.p8.)    4.235    1.127    3.758    0.000
##     sf11    (.p9.)    6.192    1.626    3.808    0.000
##     sf12    (.10.)    4.373    1.241    3.523    0.000
## 
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)
##    .sf3               1.050    0.032   32.428    0.000
##    .sf4               1.366    0.067   20.489    0.000
##    .sf5               1.515    0.072   21.010    0.000
##    .sf6               1.149    0.055   21.019    0.000
##    .sf7               1.416    0.076   18.544    0.000
##    .sf8               1.663    0.076   21.822    0.000
##    .sf9               1.109    0.048   22.897    0.000
##    .sf10              1.238    0.053   23.296    0.000
##    .sf11              1.604    0.072   22.383    0.000
##    .sf12              2.327    0.073   32.077    0.000
##     physfun           0.000                           
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)
##    .sf3               0.098    0.014    7.015    0.000
##    .sf4               0.223    0.037    6.073    0.000
##    .sf5               0.252    0.042    6.054    0.000
##    .sf6               0.216    0.034    6.453    0.000
##    .sf7               0.301    0.050    6.034    0.000
##    .sf8               0.418    0.062    6.746    0.000
##    .sf9               0.149    0.023    6.380    0.000
##    .sf10              0.147    0.025    6.001    0.000
##    .sf11              0.224    0.040    5.589    0.000
##    .sf12              0.384    0.057    6.696    0.000
##     physfun           0.008    0.004    1.936    0.053
## 
## 
## Group 2 [0]:
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)
##   physfun =~                                          
##     sf3               1.000                           
##     sf4     (.p2.)    5.427    1.377    3.941    0.000
##     sf5     (.p3.)    5.955    1.530    3.893    0.000
##     sf6     (.p4.)    3.330    0.952    3.498    0.000
##     sf7     (.p5.)    6.114    1.612    3.793    0.000
##     sf8     (.p6.)    4.679    1.305    3.587    0.000
##     sf9     (.p7.)    3.383    0.916    3.695    0.000
##     sf10    (.p8.)    4.235    1.127    3.758    0.000
##     sf11    (.p9.)    6.192    1.626    3.808    0.000
##     sf12    (.10.)    4.373    1.241    3.523    0.000
## 
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)
##    .sf3               1.022    0.022   46.600    0.000
##    .sf4               1.297    0.056   23.220    0.000
##    .sf5               1.396    0.064   21.672    0.000
##    .sf6               1.132    0.055   20.714    0.000
##    .sf7               1.440    0.079   18.298    0.000
##    .sf8               1.747    0.072   24.220    0.000
##    .sf9               1.088    0.043   25.389    0.000
##    .sf10              1.165    0.049   23.804    0.000
##    .sf11              1.527    0.074   20.704    0.000
##    .sf12              2.396    0.073   32.749    0.000
##     physfun           0.000                           
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)
##    .sf3               0.039    0.006    6.591    0.000
##    .sf4               0.135    0.026    5.266    0.000
##    .sf5               0.198    0.036    5.530    0.000
##    .sf6               0.216    0.034    6.426    0.000
##    .sf7               0.374    0.062    6.079    0.000
##    .sf8               0.363    0.058    6.275    0.000
##    .sf9               0.109    0.018    6.081    0.000
##    .sf10              0.127    0.022    5.825    0.000
##    .sf11              0.302    0.051    5.944    0.000
##    .sf12              0.390    0.060    6.458    0.000
##     physfun           0.005    0.003    1.940    0.052
```

You don't have to constrain everything at once, either, if you have a theoretical reason not to. You can constrain individaul parameters across groups if you don't want to constrain all loadings, for example. I do this here, where I constrain just certain indicators to be the same across groups. Though I don't show it in this example, you can also fix parameters in one group, but not the other group through syntax such as `c(1,NA)*sf4`. This will fix the loading of the sf4 item to 1 in the first group but not the second group, where `NA` tells lavaan to freely estimate the parameter for that group. Note that this syntax for fixing and equating variables can be extended to more than two groups simply by increasing the number of items in the list. For example, if you wanted to equate parameters across three groups in the example below you would use `c(v6,v6,v6)*sf6`. This syntax can be extended to any parameters in your model that you want to fix or equate, not just loadings of observed variables on latent factors. 


```r
sem.model.partequality <- "physfun =~ c(v3,v3)*sf3+sf4+sf5+c(v6,v6)*sf6+sf7+sf8+sf9+sf10+sf11+sf12"
sem.fit.partequality <- sem(sem.model.partequality, data = semdat, 
    group = "condition", missing = "fiml")
# missing = fiml implements full information ML for
# missingness
summary(sem.fit.partequality, fit.measures = TRUE)
```

```
## lavaan 0.6-5 ended normally after 109 iterations
## 
##   Estimator                                         ML
##   Optimization method                           NLMINB
##   Number of free parameters                         60
##   Number of equality constraints                     1
##   Row rank of the constraints matrix                 1
##                                                       
##   Number of observations per group:                   
##     1                                              101
##     0                                               91
##   Number of missing patterns per group:               
##     1                                                1
##     0                                                1
##                                                       
## Model Test User Model:
##                                                       
##   Test statistic                               288.803
##   Degrees of freedom                                71
##   P-value (Chi-square)                           0.000
##   Test statistic for each group:
##     1                                          184.417
##     0                                          104.386
## 
## Model Test Baseline Model:
## 
##   Test statistic                               802.850
##   Degrees of freedom                                90
##   P-value                                        0.000
## 
## User Model versus Baseline Model:
## 
##   Comparative Fit Index (CFI)                    0.694
##   Tucker-Lewis Index (TLI)                       0.613
## 
## Loglikelihood and Information Criteria:
## 
##   Loglikelihood user model (H0)              -1384.643
##   Loglikelihood unrestricted model (H1)      -1240.242
##                                                       
##   Akaike (AIC)                                2887.286
##   Bayesian (BIC)                              3079.479
##   Sample-size adjusted Bayesian (BIC)         2892.585
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.179
##   90 Percent confidence interval - lower         0.158
##   90 Percent confidence interval - upper         0.200
##   P-value RMSEA <= 0.05                          0.000
## 
## Standardized Root Mean Square Residual:
## 
##   SRMR                                           0.091
## 
## Parameter Estimates:
## 
##   Information                                 Observed
##   Observed information based on                Hessian
##   Standard errors                             Standard
## 
## 
## Group 1 [1]:
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)
##   physfun =~                                          
##     sf3       (v3)    1.000                           
##     sf4               4.443    1.276    3.481    0.000
##     sf5               4.932    1.427    3.456    0.001
##     sf6       (v6)    3.250    0.936    3.472    0.001
##     sf7               5.238    1.518    3.450    0.001
##     sf8               3.396    1.130    3.005    0.003
##     sf9               2.955    0.909    3.250    0.001
##     sf10              3.905    1.158    3.372    0.001
##     sf11              5.534    1.600    3.458    0.001
##     sf12              3.840    1.232    3.116    0.002
## 
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)
##    .sf3               1.050    0.033   31.969    0.000
##    .sf4               1.366    0.065   20.937    0.000
##    .sf5               1.515    0.071   21.377    0.000
##    .sf6               1.149    0.056   20.425    0.000
##    .sf7               1.416    0.076   18.691    0.000
##    .sf8               1.663    0.073   22.745    0.000
##    .sf9               1.109    0.048   22.957    0.000
##    .sf10              1.238    0.054   22.710    0.000
##    .sf11              1.604    0.073   22.013    0.000
##    .sf12              2.327    0.073   31.915    0.000
##     physfun           0.000                           
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)
##    .sf3               0.099    0.014    6.994    0.000
##    .sf4               0.228    0.038    6.067    0.000
##    .sf5               0.258    0.042    6.070    0.000
##    .sf6               0.211    0.034    6.296    0.000
##    .sf7               0.298    0.050    5.914    0.000
##    .sf8               0.422    0.062    6.821    0.000
##    .sf9               0.146    0.023    6.242    0.000
##    .sf10              0.143    0.025    5.835    0.000
##    .sf11              0.222    0.041    5.392    0.000
##    .sf12              0.385    0.058    6.664    0.000
##     physfun           0.010    0.006    1.796    0.073
## 
## 
## Group 2 [0]:
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)
##   physfun =~                                          
##     sf3       (v3)    1.000                           
##     sf4               6.849    2.161    3.169    0.002
##     sf5               7.606    2.427    3.134    0.002
##     sf6       (v6)    3.250    0.936    3.472    0.001
##     sf7               7.416    2.473    2.998    0.003
##     sf8               6.880    2.353    2.924    0.003
##     sf9               3.915    1.340    2.922    0.003
##     sf10              4.409    1.524    2.893    0.004
##     sf11              6.604    2.317    2.850    0.004
##     sf12              5.034    1.922    2.619    0.009
## 
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)
##    .sf3               1.022    0.022   47.330    0.000
##    .sf4               1.297    0.057   22.714    0.000
##    .sf5               1.396    0.066   21.238    0.000
##    .sf6               1.132    0.053   21.504    0.000
##    .sf7               1.440    0.080   18.094    0.000
##    .sf8               1.747    0.075   23.147    0.000
##    .sf9               1.088    0.043   25.313    0.000
##    .sf10              1.165    0.047   24.626    0.000
##    .sf11              1.527    0.072   21.276    0.000
##    .sf12              2.396    0.073   32.956    0.000
##     physfun           0.000                           
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)
##    .sf3               0.039    0.006    6.606    0.000
##    .sf4               0.127    0.026    4.903    0.000
##    .sf5               0.184    0.036    5.165    0.000
##    .sf6               0.214    0.033    6.507    0.000
##    .sf7               0.377    0.063    5.974    0.000
##    .sf8               0.347    0.057    6.100    0.000
##    .sf9               0.113    0.019    6.056    0.000
##    .sf10              0.133    0.023    5.900    0.000
##    .sf11              0.311    0.052    5.978    0.000
##    .sf12              0.389    0.060    6.441    0.000
##     physfun           0.004    0.002    1.624    0.104
```

<br>
## Model Comparison Using lavaan
Note that models that are compared using most fit statistics must be nested in order for the tests to be valid. Nested models are models that contain at least all of the same exact observed variables contained in the less complicated model. For nested models, you can only free or fix parameter(s), not do both. The underlying data matrix must be the exact same between models. The code below compares the reduced model with more df (paths between physical functioning and observed indicators equated to be the same) to the more saturated model with less df (paths between physical functioning and indicators allowed to differ, and thus estimated differently for both conditions). Note that you can compare models directly in order to have an explicit test of factorial measurement invariance. If we retain the null hypothesis, our less saturated model with more parameters fixed is a better fit ot the data, and we can claim that the loadings are equivalent across groups (we have achieved one level of factorial measurement invariance). 

First, we'll fit our model where we allow the lambdas to be different across conditions, which is the default in lavaan.

```r
sem.model.diff <- "physfun =~ sf3+sf4+sf5+sf6+sf7+sf8+sf9+sf10+sf11+sf12"
sem.fit.diff <- sem(sem.model.diff, data = semdat, group = "condition", 
    missing = "fiml")
# missing = fiml implements full information ML for
# missingness
```

Now, we'll compare the models (the model we just made to the model earlier where we equated lambdas - the paths from our latent variables to our observed variables - to be the same across groups) using a chi-squared difference of fit test. This is considered a test of what's typically referred to as weak invariance.

```r
anova(sem.fit.equality, sem.fit.diff)  #model fit comparison
```

```
## Chi-Squared Difference Test
## 
##                  Df    AIC    BIC  Chisq Chisq diff Df diff Pr(>Chisq)
## sem.fit.diff     70 2888.7 3084.2 288.26                              
## sem.fit.equality 79 2877.8 3044.0 295.36     7.1058       9     0.6261
```
Since we retain the null hypothesis using the chi-squared difference of fit test, this means that the less saturated model with less parameters estimated (in this case, when the groups have factor loadings equated) fits the data better than the more free model with those factor loadings allowed to differ across groups. This means that we have met weak invariance. You could now keep building on this model, next testing whether intercepts are the same across groups (all the way up to means and residuals constrained to the same across groups, etc.), which would be a test of strong invariance. Note in these models you would keep the factor loadings constrained to be the same since you already know this reflects your data more accurately.  
