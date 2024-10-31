library(alr4)
install.packages("reshape2") 
library(reshape2)  

llr <- function(x,y,z,omega){
  
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

compute_f_hat <- function(z,x,y,omega) {
  
  Wz = make_weight_matrix(z, x, omega) 
  X = make_predictor_matrix(x)
  f_hat = c(1, z) %*% solve(t(X) %*% apply(X, 2, function(col, i){col * Wz}, Wz)) %*% t(X) %*% (mapply('*',y,Wz))
  return(f_hat)
  
}


make_weight_matrix  <- function(z,x,omega){
  n = length(x)
  p <- c(rep(0,n))
  for (i in 1:n){
    r <- abs(x[i]-z)/omega
    p[i] <- w_fun(r)
  }
  return(p)
}

w_fun <- function(r){
  ifelse(abs(r)<1, (1- (abs(r))^3)^3, 0)
}

make_predictor_matrix <- function(x) {
  n <- length(x)
  a <- matrix(rep(1, n * 2), n, 2)
  a[, 2] <- x
  return(a)
}


data(french_fries) 
french_fries = french_fries[complete.cases(french_fries),] 
z = seq(0, 15, length.out = 100) 
fits = llr(z = z, x = french_fries$potato, y = french_fries$buttery, omega = 3) 
plot(z, fits)