---
title: "Structural Equation Modeling in R Tutorial 2: Matrix algebra using R"
authors: ["admin"]
date: 2017-09-01
tags: ['structural equation modeling', 'regression', 'matrix algebra', 'R']
categories: ['structural equation modeling', 'regression', 'matrix algebra', 'R']
output: blogdown::html_page
---

{{% toc %}}

Made for Jonathan Butner's Structural Equation Modeling Class, Fall 2017, University of Utah. This handout begins by showing how to import a matrix into R. Then, we go through how to create matrices and vectors in R as well as perform a few matrix algebra operations, such as transposing and inversing matrices. This syntax imports the 6x6 matrix from datafile Matrix.txt. Note that a helpful resource for working with matrices in R can be found at http://www.statmethods.net/advstats/matrix.html. 



## Data Input
First, we will read in datafile using Fortran and clean up the datafile a little bit. Most matrix functions are built into base R, so you won't need many outside packages. 

```r
# make sure to set your working directory to where the file
# is located, e.g. setwd('D:/Downloads/Handout 1 -
# Regression') you can do this in R Studio by going to the
# Session menu - 'Set Working Directory' - To Source File
# Location
A <- read.fortran("MatrixA.txt", c("6F5.3"))
```

```
## Warning in readLines(file, n = thisblock): incomplete final line found on
## 'MatrixA.txt'
```

```r
# note: #The format for a field is of one of the following
# forms: rFl.d, rDl.d, rXl, rAl, rIl, where l is the number
# of columns, d is the number of decimal places, and r is the
# number of repeats. F and D are numeric formats, A is
# character, I is integer, and X indicates columns to be
# skipped. The repeat code r and decimal place code d are
# always optional. The length code l is required except for X
# formats when r is present.

# add column names
names(A) <- c("a1", "a2", "a3", "a4", "a5", "a6")

# make into a matrix
A <- data.matrix(A)

# print the matrix
A
```

```
##          a1     a2     a3     a4     a5    a6
## [1,]  0.219 -0.062 -0.064 -0.063 -0.047 0.010
## [2,] -0.062  0.647  0.555  0.529  0.113 0.174
## [3,] -0.064  0.555  0.703  0.581  0.074 0.225
## [4,] -0.063  0.529  0.581  0.671  0.095 0.215
## [5,] -0.047  0.113  0.074  0.095  0.761 0.034
## [6,]  0.010  0.174  0.225  0.215  0.034 0.939
```
<br>
## Creating Matrices and Vectors
Now, we'll show how to make Matrices and Vectors from scratch. Note the first comment, which indicates the structure of the function used to generate matrices. 

```r
# computing Hat matrix matrix(data, nrow, ncol, byrow)
# #matrix function template where data is a vector or list,
# nrow is number of rows, ncol is number of columns, byrow is
# whether or not to enter the data by row or by column
# (default is false, or by column)

# B and C are 3x2 matrices
B <- matrix(c(1, 2, 3, 4, 5, 6), 3, 2, byrow = TRUE)
C <- matrix(c(5, 3, 5, 4, 9, 6), 3, 2, byrow = TRUE)

# D is a 2x2 matrix
D <- matrix(c(5, 4, 4, 9), 2, 2, byrow = TRUE)

# E and F are vectors
E <- matrix(c(2, 3, 4, 5, 9, 6), 6, 1, byrow = TRUE)
F <- matrix(c(2, 3, 4, 5, 9, 6), 1, 6, byrow = TRUE)

# G is a 3x3 matrix
G <- matrix(c(2, 3, 5, 3, 9, 6, 5, 9, 1), 3, 3, byrow = TRUE)

# print the results of our matrices
B
```

```
##      [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## [3,]    5    6
```

```r
C
```

```
##      [,1] [,2]
## [1,]    5    3
## [2,]    5    4
## [3,]    9    6
```

```r
D
```

```
##      [,1] [,2]
## [1,]    5    4
## [2,]    4    9
```

```r
E
```

```
##      [,1]
## [1,]    2
## [2,]    3
## [3,]    4
## [4,]    5
## [5,]    9
## [6,]    6
```

```r
F
```

```
##      [,1] [,2] [,3] [,4] [,5] [,6]
## [1,]    2    3    4    5    9    6
```

```r
G
```

```
##      [,1] [,2] [,3]
## [1,]    2    3    5
## [2,]    3    9    6
## [3,]    5    9    1
```
<br>
## Operations Using Matrices

We now have the matrices entered that we need, so now we can do matrix algebra. First, we'll compute the inverse of matrix A. Next we will compute the determinant of matrix A. Then, we will compute the trace of matrix A. Note that the trace of a (square) matrix is just the sum of the diagonal elements. Each will be assigned to new objects.


```r
Ainv <- solve(A)  #inverse of matrix A
Adet <- det(A)  #determinant of matrix A
Atrace <- sum(diag(A))  #trace of matrix A

# print the results
Ainv
```

```
##          [,1]        [,2]       [,3]       [,4]        [,5]        [,6]
## a1  4.7776403  0.13338608  0.2053442  0.1882573  0.23968517 -0.17658428
## a2  0.1333861  5.54968915 -2.7149454 -1.9970797 -0.30650066  0.08911192
## a3  0.2053442 -2.71494538  6.3914572 -3.3162808  0.22078070 -0.27927262
## a4  0.1882573 -1.99707969 -3.3162808  6.0391954 -0.11361085 -0.21596570
## a5  0.2396852 -0.30650066  0.2207807 -0.1136109  1.36808050 -0.02218296
## a6 -0.1765843  0.08911192 -0.2792726 -0.2159657 -0.02218296  1.16750112
```

```r
Adet
```

```
## [1] 0.003263043
```

```r
Atrace
```

```
## [1] 3.94
```

Now we will compute some simpler operations with matrices, such as addition, multiplication, and transposing. The transpose of a matrix is a new matrix whose rows are the columns of the original. (This makes the columns of the new matrix the rows of the original). Note how the multiplication symbol for matrix multiplication differs from the typical multiplication symbol. 

```r
H <- B + C  #add matrices B and C
I <- B - C  #subtract matrix C from matrix B
J <- B %*% D  #multiply matrices B and D
Etrans = t(E)  #compute the transpose of matrix E.

# print the results
H
```

```
##      [,1] [,2]
## [1,]    6    5
## [2,]    8    8
## [3,]   14   12
```

```r
I
```

```
##      [,1] [,2]
## [1,]   -4   -1
## [2,]   -2    0
## [3,]   -4    0
```

```r
J
```

```
##      [,1] [,2]
## [1,]   13   22
## [2,]   31   48
## [3,]   49   74
```

```r
Etrans
```

```
##      [,1] [,2] [,3] [,4] [,5] [,6]
## [1,]    2    3    4    5    9    6
```

Now we will compute some more complex operations with matrices. First, we will multiply matrix F by the inverse of matrix A by the transpose of F. Note that you have not previously transposed F, so you will perform that operation as part of your calculation.

```r
K <- F %*% Ainv %*% t(F)  #multiply matrix F by the inverse of matrix A by the transpose of F
# print the results
K
```

```
##          [,1]
## [1,] 194.0346
```

Next, we will multiply a matrix by a scalar value of 3. 

```r
L <- D * 3  #multiply by a scalar of 3
# print the results
L
```

```
##      [,1] [,2]
## [1,]   15   12
## [2,]   12   27
```


Here, we will decompose matrix A into its eigenvalues and eigenvectors using the eigen() function. Note that `Aeigen$val` will hold the eigenvalues of A while `Aeigen$vec` will hold the eigenvectors of A. 

```r
Aeigen <- eigen(A)  #Note that `Aeigen$val` will hold the eigenvalues of A while `Aeigen$vec` will hold the eigenvectors of A.
O <- kronecker(D, G)  #compute the Kronecker product of D and G

# print the results
Aeigen$val  #print eigenvalues
```

```
## [1] 1.9435231 0.8230922 0.7327585 0.2073069 0.1301322 0.1031870
```

```r
Aeigen$vec  #print eigenvectors
```

```
##             [,1]        [,2]        [,3]        [,4]        [,5]
## [1,]  0.06073943 -0.08831644  0.03145381  0.99351915 -0.02003508
## [2,] -0.51719017  0.19459842  0.10577726  0.06299467  0.79032474
## [3,] -0.55541800  0.13590932  0.16768996  0.03192490 -0.19090408
## [4,] -0.53832277  0.14411829  0.12841283  0.03365550 -0.57945783
## [5,] -0.13942382  0.30744711 -0.93765924  0.06458513 -0.03549710
## [6,] -0.33332489 -0.90585126 -0.25299645 -0.05126629  0.03885382
##             [,6]
## [1,] -0.00631456
## [2,]  0.23425506
## [3,] -0.77939464
## [4,]  0.57969353
## [5,] -0.03743187
## [6,]  0.01352591
```

```r
O  #kronecker product of D and G
```

```
##      [,1] [,2] [,3] [,4] [,5] [,6]
## [1,]   10   15   25    8   12   20
## [2,]   15   45   30   12   36   24
## [3,]   25   45    5   20   36    4
## [4,]    8   12   20   18   27   45
## [5,]   12   36   24   27   81   54
## [6,]   20   36    4   45   81    9
```
