likelihood <- function(par, x){
  pdf_vec <- pdf(par, x)
  product <- prod(pdf_vec)
  retval<--(log(product))

  if (debug) {
    print("Likelihood function called.")
    print(paste0("par = ", par))
    print(paste0("prod(pdf) = ", product))
    print(paste0("min(pdf) = ", min(pdf_vec)))
    print(paste0("max(pdf) = ", max(pdf_vec)))
    print(paste0("ll = ", retval))

    flush.console()
  }

  return(retval)
}
