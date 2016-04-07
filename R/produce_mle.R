#' Maximum Likelihood estimation and Goodness of Fit measures.
#'
#' Based on code by PRD Marinho (2013) for the AdequacyModel package version 1.0.8. Link: https://cran.r-project.org/web/packages/AdequacyModel/ (acessed 04/01/2016).
#' @param pdf Probability Density Function to be used.
#' @param cdf Cumulative probability Distribution Function to be used.
#' @param initialGuess Initial guesses for parameters.
#' @param data Data to be used on the construction of the likelihood function.
#' @param method Optimization method used. Currently only SANN+L-BFGS-B available. TO-DO: Implement the method of "Dealing with Flat Likelihood Functions" by Frery, Alejandro C., Francisco Cribari-Neto, and Marcelo O. de Souza.
#' @param domain
#' @param mle
#' @param lower
#' @param upper
#' @param control
#' @param debug
#' @export
#' @example
#' produce_mle()
produce_mle <- function(pdf, cdf, initialGuess=NULL, data, method,
           lower=c(1e-10,1e-10,1e-10,1e-10), upper=c(Inf,Inf,Inf,Inf,Inf), control = list(type = 2), debug=FALSE){

    if (debug) {
      traceLevel <- 2
      options(warn = 2)

      print("Data used:")
      print(data)
      flush.console()
    } else {
      traceLevel <- 0
    }

    # if(!is.null(lower)||!is.null(upper)) {
    #   print("Optimization bounds found. Defaulting to Simulated Annealing+L-BFGS-B.")
    #   flush.console()
    #   method = "L-BFGS-B"
    # }



    if (is.null(initialGuess)) {
        sannResults <- optim(
                        par = initialGuess,
                        fn = likelihood,
                        x = data,
                        method = "SANN",
                        hessian = TRUE)
        initialGuess <- sannResults$par
    } else {
      sannResults <- NULL
    }

    optimResults <- optim(
                      par = initialGuess,
                      fn = likelihood,
                      x = data,
                      lower=lower,
                      upper=upper,
                      method = "L-BFGS-B",
                      hessian = TRUE)

    if (optimResults$convergence!=1) {
      print(paste0(optimResults$message, " - Code: ", optimResults$convergence))
      print(paste0("Parameter values: ", optimResults$par))
      stop("Convergence failure.")
    }

    attr(optimResults, "initialGuess") <- initialGuess
    attr(optimResults, "sannResults") <- sannResults

    return(optimResults)
}
