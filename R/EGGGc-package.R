# Loading distribution specifics into the environment

EGExpG <- function(x, a, b, p, lambda){
  (a*b*lambda*(1-p)*exp(-(lambda*a*x))*(1-exp(-(lambda*a*x)))^(b-1))/(1-p*(1-(1-exp(-(lambda*a*x)))^b))^2
}

cdf_EGExpG <- function(par,x){
 a = par[1]
 b = par[2]
 p = par[3]
 lambda = par[4]
 (1-exp(-lambda*a*x))^b/(1-p*(1-(1-exp(-lambda*a*x))^b))
}

pdf_EGExpG <- function(par,x){
 a = par[1]
 b = par[2]
 p = par[3]
 lambda = par[4]
 (a*b*lambda*(1-p)*exp(-(lambda*a*x))*(1-exp(-(lambda*a*x)))^(b-1))/(1-p*(1-(1-exp(-(lambda*a*x)))^b) )^2
}
