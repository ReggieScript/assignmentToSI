
p_var <- function(theta, X){
  # function to compute probabiloty recieving theta and X
  p_res <- 1 / (1 + exp(-X%*%theta)) 
  return(p_res)
}

L <- function(theta, y, X){
  ## likelihood
  p_var <- p_var(theta = theta, X = X)
  likelihood <- prod(p_var^y * (1 - p_var)^(1 - y))
  return(likelihood)
}

l <- function(theta, y, X){ 
  ## Same thing as before, but log likelihood
  ## log likelihood
  p_var <- p_var(theta = theta, X = X)
  log_likelihood <- sum(y * log(p_var) + (1 - y) * log(1 - p_var))
  # log_likelihood <- dbninom()
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
  p <- p_var(theta = theta, X = X)
  v_res <- p * (1-p)
  return(v_res)
}

I <- function(theta, y, X){
  ## fisher information
  v_var = v(theta,X)
  D <- diag(as.vector(v_var))
  fisher <- t(X) %*% D %*% X
  return(fisher)
  }

NR <- function(theta0, niter, y, X){
  # function that applies Newton-Raphson?s algorithm in order to compute the 
  # ML-estimates in a logistic regression model, in a certain number of n
  # iterations.
  
  #Note: this might break if the matrix dim from x changes
    theta <- matrix(theta0, nrow = 4, ncol = 1)
  
  for (i in 1:niter){
    score <- S(theta, y, X)
    log_likelihood <- L(theta, y, X)
    #theta <- theta + (log_likelihood/score)
    theta <- theta + solve(I(theta, y, X)) %*% score # its a plus and not a 
    # minus because of how we obtain the derivative of the score function
  }
  
  return(theta)
}
 
