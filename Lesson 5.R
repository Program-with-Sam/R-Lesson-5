# Lesson 5

# Dummy values
vals<-seq(0,20,1)

# Declare a function
f<-function(x) {
y = (12 * x) - 4
}

# Plot the function
plot(x=vals, y=f(vals), type='l')

# Import the library
library(deSolve)
# Create the system
eqsystem <- function (Time, State, Pars) {
	x <- State[1]
	y <- State[2]
    with(as.list(c(Pars)), {
        dx = x * (a - g*x - b*y)
        dy = y * (-c + d*x)
        return(list(c(dx, dy)))
    })
}
# Create the parameters
Pars <- c(a = 5, b = 0.01, c = 100, d = 0.01, g = 0.0001)
State <- c(x = 10000, y = 60)
Time <- seq(0, 5, by = 0.1)
# Perform ODE
out <-ode(func = eqsystem, y = State, parms = Pars, times = Time)
#Plot
matplot(out[,1], (out[,2:3]), type = "l", xlab = "time", ylab = "population")

# Matrix
mat<-matrix(c(1:9),nrow=3, byrow=TRUE) 
mat
# Covariance
covmat<-cov(mat)
covmat
# SVD
svdres<-svd(mat)
svdres
