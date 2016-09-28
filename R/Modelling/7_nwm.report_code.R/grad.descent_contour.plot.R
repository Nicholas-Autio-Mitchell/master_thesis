###################### =========================================================== ######################
######################  Create simple contour plot to illustrate gradient descent  ######################
###################### =========================================================== ######################


# definition of the 2D Rosenbrock function, optima is at (1,1)
rosenbrock <- function(v) { 
(1 - v[1])^2 + 100 * (v[2] - v[1]*v[1])^2
}
 
# definition of the gradient of the 2D Rosenbrock function
derivative <- function(v) {
c(-400 * v[1] * (v[2] - v[1]*v[1]) - 2 * (1 - v[1]), 
200 * (v[2] - v[1]*v[1]))
}
 
# locate the minimum of the function using the Conjugate Gradient method
result <- optim(
c(runif(1,-3,3), runif(1,-3,3)), # start at a random position
rosenbrock, # the function to minimize
derivative, # no function gradient 
method="CG", # use the Conjugate Gradient method
control=c( # configure Conjugate Gradient
maxit=100, # maximum iterations of 100
reltol=1e-8, # response tolerance over-one step
type=2)) # use the Polak-Ribiere update method
 
# summarize results
print(result$par) # the coordinate of the minimum
print(result$value) # the function response of the minimum
print(result$counts) # the number of function calls performed
 
# display the function as a contour plot
x <- seq(-2, 3, length.out=1000)
y <- seq(-3, 3, length.out=1000)
z <- rosenbrock(expand.grid(y, x))
contour(y, x, matrix(log10(z), length(x)), xlab=TeX("f(X_1)"), ylab=TeX("f(X_2)"))
# draw the optima as a point
points(0, 3, col="red", pch=19)
points(2.5, -2, col="blue", pch=19)

# draw a square around the optima to highlight it
rect(result$par[1]-0.2, result$par[2]-0.2, result$par[1]+0.2, result$par[2]+0.2, lwd=2)



## ================ ##
##  first attempts  ##
## ================ ##

# define a 2D basin function, optima is at (0,0)
basin <- function(x) {
4*x[1]^2 + x[2]^2
}
 
# define the derivative for a 2D basin function
derivative <- function(x) {
c(2*x[1], 2*x[2])
}
 
# definition of the gradient descent method in 2D
gradient_descent <- function(func, derv, start, step=0.05, tol=1e-8) {
pt1 <- start
grdnt <- derv(pt1)
pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
while (abs(func(pt1)-func(pt2)) > tol) {
pt1 <- pt2
grdnt <- derv(pt1)
pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
print(func(pt2)) # print progress
}
pt2 # return the last point
}
 
# locate the minimum of the function using the Gradient Descent method
result <- gradient_descent(
basin, # the function to optimize
derivative, # the gradient of the function
c(runif(1,-3,3), runif(1,-3,3)), # start point of the search 
0.05, # step size (alpha)
1e-8) # relative tolerance for one step
 
# display a summary of the results
print(result) # coordinate of fucntion minimum
print(basin(result)) # response of fucntion minimum
 
# display the function as a contour plot
x <- seq(-3, 3, length.out=100)
y <- seq(-3, 3, length.out=100)
z <- basin(expand.grid(x, y))
contour(x, y, matrix(z, length(x)), xlab=TeX("$f(X_1)$"),ylab=TeX("$f(X_2)$"))
# draw the optima as a point
points(result[1], result[2], col="red", pch=19)
points(2.67, 2.5, col="blue", pch=19)
# draw a square around the optima to highlight it
rect(result[1]-0.2, result[2]-0.2, result[1]+0.2, result[2]+0.2, lwd=2)





## define a 2D basin function, optima is at (0,0)
basin <- function(x) {
    x[1]^2 + x[2]^2
}

## define the derivative for a 2D basin function
derivative <- function(x) {
    c(2*x[1], 2*x[2])
}

## definition of the gradient descent method in 2D
gradient_descent <- function(func, derv, start, step=0.05, tol=1e-8) {
    pt1 <- start
    grdnt <- derv(pt1)
    pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
    while (abs(func(pt1)-func(pt2)) > tol) {
        pt1 <- pt2
        grdnt <- derv(pt1)
        pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
        print(func(pt2)) # print progress
    }
    pt2 # return the last point
}

## locate the minimum of the function using the Gradient Descent method
result <- gradient_descent(
    basin, # the function to optimize
    derivative, # the gradient of the function
    c(runif(1,-3,3), runif(1,-3,3)), # start point of the search 
    0.05, # step size (alpha)
    1e-8) # relative tolerance for one step

## display a summary of the results
print(result) # coordinate of fucntion minimum
print(basin(result)) # response of fucntion minimum

## display the function as a contour plot
x <- seq(-2, 3, length.out=100)
y <- seq(-3, 2.5, length.out=100)
z <- basin(expand.grid(x, y))
contour(x, y, matrix(z, length(x)), xlab="x" ,ylab="y")
## draw the optima as a point
points(result[1], result[2], col="red", pch=19)
## draw a square around the optima to highlight it
rect(result[1]-0.2, result[2]-0.2, result[1]+0.2, result[2]+0.2, lwd=2)
