#' Produces a sample from the distribution.
#'
#' Produces a sample from the distribution.
#' @param n_sample Sample size
#' @param a Parameter a>0
#' @param b Parameter b>0
#' @param p Parameter 0<p<1
#' @param lambda Parameter lambda>0
#' @param density Probability Density Function of the sample
#' @param limsup The numbers generated are in the interval (0, limsup+1]. This makes possible fine control over distributions on the real line.
#' @export
#' @examples
#' produce_sample(100, 2, 2, 0.5, 2, EGExpG)
produce_sample <- function (n_sample, a, b, p, lambda, density, debug=FALSE) {

  if (!suppressWarnings(require("pracma"))) {
    stop("Please make pracma package available. ")
  }

  sample<-rep(NaN,n_sample)
  cont<-0

  for(i in 1:n_sample) {
    #x<-runif(1,max=limsup+1)
    x<-runif(1,max=1000)
    u<-runif(1)

    if (debug) {
      cat("Entering rejection loop...\n")
      cat("x: ", x, "\n")
      cat("a: ", a, "\n")
      cat("b: ", b, "\n")
      cat("p: ", p, "\n")
      cat("lambda: ", lambda, "\n")
      cat("density: ", density(x, a, b, p, lambda), "\n")
      flush.console()
    }
    while(u>density(x, a, b, p, lambda)) {
      #x<-runif(1, max = limsup+1)
      x<-runif(1,max=1.1)
      u<-runif(1)
      cont<-cont+1
      if (debug) {
        cat("Iterating...\n")
        cat("x: ", x, "\n")
        cat("a: ", a, "\n")
        cat("b: ", b, "\n")
        cat("p: ", p, "\n")
        cat("lambda: ", lambda, "\n")
        cat("density: ", density(x, a, b, p, lambda), "\n")
        flush.console()
      }
    }
    sample[i]<-x
  }

  # cat("\ntaxa de aceitação: ",n_sample/cont)
  return(sample)
}
