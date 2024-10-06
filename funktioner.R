
p_var <- function(theta, X){
  ## Pi
  print(dim(t(X)))
  print(dim(theta))
  print(dim(-t(X)))
  
  print(ncol(-t(X)))
  print(nrow(theta))
  #p_res <- 1 / (1 + exp(-t(X)%*%theta))
  p_res <- 1 / (1 + exp(-X%*%theta)) ## removed transpose from X because it didnt work, check it later
  return(p_res)
}

L <- function(theta, y, X){
  ## likelihood
  p_var <- p_var(theta = theta, X = X)
  likelihood <- prod(p_var^y * (1 - p_var)^(1 - y))
  return(likelihood)
}

l <- function(theta, y, X){ ## Same thing as before, but log likelihood
  ## log likelihood
  p_var <- p_var(theta = theta, X = X)
  log_likelihood <- sum(y * log(p_var) + (1 - y) * log(1 - p_var))
  return(log_likelihood)
}

S <- function(theta, y, X){
  ## Score function
  p_var <- p_var(theta = theta, X = X)
  score <- t(X) %*% (y-p_var)
  return(score)
}

v <- function(theta, X){
  ## Vi
  p_var <- p_var(theta = theta, X = X)
  v_res <- p %*%(1-p)
  return(v_res)
}

I <- function(theta, y, X){
  ## fisher information
  v_var = v(theta = theta, X = X)
  D <- diag(as.vector(v_var))
  fisher <- t(X) %*% D %*% X
  return(fisher)
  }

NR <- function(theta0, niter, y, X){
  # The likelihood can not be maximized analytically, and we need a numerical 
  # method to compute the ML-estimates. Write a 
  # function NR <- function(theta0, niter, y, X){...} 
  # that applies Newton-Raphson?s algorithm in order to compute the ML-estimates 
  # in a logistic regression model (see e.g. the textbook, page 356, and note that 
  # Fisher?s information matrix is the negative Hessian). Given a starting value 
  # theta0 the function should perform niter iterations of the algorithm and 
  # provide a numerical value (vector) for the ML-estimate as output. 
  # The function should make full use of the functions for score and information 
  # coded in the previous task.
  
  
  #Note: this might break if the matrix dim from x changes
    theta <- matrix(theta0, nrow = 4, ncol = 1)
  
  for (i in 1:niter){
    score <- S(theta = theta, X = X, y = y)
    log_likelihood <- L(theta = theta, X = X, y = y)
    theta <- theta - (log_likelihood/score)
    print(i)
  }
  
  return(theta)
}