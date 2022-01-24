
rm(list = ls())

######################
## Maximum likelihood estimate using optim function :

set.seed(18)
dat <- rexp(100, rate = 5)
datn <- rnorm(100, mean = 10, sd = 5)
#mean(dat)

ll_norm <- function(par, y) {
  sd1 <- sum(y - par)^2/(length(y) - 1)
  -sum(dnorm(x = y, mean = par, sd = sd1, log = T))
}


ll <- function(par, y){
  #n <- length(y)
  -sum(dexp(x = y, rate = 1/par, log = T))
  #return((n*log(par)) + (sum(y)/par))
}
##
optim(par = 0.1, fn = ll, y = dat, method = "BFGS")
optim(par = 1, fn = ll_norm, y = datn, method = "BFGS")

#############################################################
##  Maximum likelihood estimate using score and hessian :

score <- function(theta, dat){
  u <- -(100/theta) + (sum(dat)/(theta^2))
  return(u)
}

hess <- function(theta, dat){
  h <- (100/(theta^2)) - ((2*sum(dat)) / (theta^3))
  return(h)
}

#theta0=as.double(readline(prompt = "Enter a point on function:"))

my_optim <- function(theta0, dat){
n=1
while(n<1000){
  n=n+1
  uth=score(theta0, dat)
  hes=hess(theta0, dat)
  if(uth<0.00001){
    break(0)
  }
  theta1=theta0-(uth/hes)
  if(abs((theta1-theta0))<0.00001){
    print("Convergent")
    
    break(0)
  }
  
  theta0=theta1
  print(paste("Iteration number:",n,"Solve",theta0))
}
return(theta0)
}



#####
data=rpois(3,.2)

score_f <- function(theta, dat){
  n <- length(dat)
  u <- -(n/theta) + (sum(dat)/(theta^2))
  return(u)
}
score_f(.01,data)

hess_f <- function(theta, dat){
  n <- length(dat)
  h <- (n/(theta^2)) - ((2*sum(dat))/(theta^3))
  return(h)
}

my_optim <- function(par, dat) {
  ###
  for(r in 1:100) {
    U <- score_f(par, dat)
    H <- hess_f(par, dat)
    par1 <- par - U/H 
    change <- abs(par - par1)
    cat("r=", r, "par=", par1, "change=", change, "\n")
    if (change < .000001) return(par)
    else par <- par1
  }
  cat("program did not converge!")
}

my_optim(.01, data)
data


repeat{
  U <- score_f(par, dat)
  H <- hess_f(par, dat)
  par1 <- par - U/H 
  change <- abs(par - par1)
  cat("r=", r, "par=", par1, "change=", change, "\n")
  if (change < .000001) return(par)
  else par <- par1
  r=r+1
}



newton_r<- function(par, dat,tol) {
  r=0
  repeat{
    U <- score_f(par, dat)
    H <- hess_f(par, dat)
    par1 <- par - U/H 
    change <- abs(par - par1)
    cat("r=", r, "par=", par1, "change=", change, "\n")
    if (change < tol) return(par)
    else par <- par1
    r=r+1
  }
  cat("program did not converge!")
}

my_opti(.01, dat=rpois(1000,3),.0000000000000000000006)
rm(list = ls())



