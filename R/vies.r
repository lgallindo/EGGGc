calculate <- function(a, b, p, lambda, random, density1, density2, cumulate, n_replica, n_sample){
  result_sample = c()
  Dn = c()
  EQM = c()
  variance = c()

  for(i in 1:n_replica){
    sample = get_sample(n_sample, a, b, p, lambda, density1)
    a_b_p_lambda_estimado = goodness.fit(pdf=density2, cdf=cumulate, starts = c(1.2,2,0.5,1.5),data = sample, method="N",domain=c(0,Inf),mle=NULL, upper=c(Inf, Inf, 1, Inf))$mle
    result_sample[i] = a_b_p_lambda_estimado
    s2<-sample
    
    empirical_CDF <- function(y){
      retval<-rep(0,length(y))
      
      for(i in 1:length(y)) {
        retval[i]<-length(s2[s2<y[i]])/length(s2)
      }
      return(retval)
    }
    
    # Dn[i] <- max(abs(SinExp_CDF(sample, lambda_estimado)-empirical_CDF(sample)))
    # Dn[i] <- max(abs(SinExp_CDF(sample, lambda_estimado)-EGExpG_CDF(sample, lambda)))
    #The K-S test is based on the maximum distance between these two curves.
    #Dn[i] <- ks.test(SinExp_CDF(sample, lambda_estimado),
    #SinExp_CDF(sample, lambda))$statistic
    #variance[i] <- J(lambda_estimado,sample)
    #}
    
    ERROR=mean(Dn)
    media = mean(result_sample)
    variance<-mean(variance)
    bias1 = mean(result_sample[1]) - a 
    bias2 = mean(result_sample[2]) - b
    bias3 = mean(result_sample[3]) - p
    bias4 = mean(result_sample[4]) - lambda
    EQM1 =  mean((result_sample[1] - a)^2)
    #mean((result_sample[1])^2)-2*1*mean(result_sample[1])- nreplicas*1^2
    EQM2 = mean((result_sample[2] - b)^2)
    #mean((result_sample[2])^2)-2*1*mean(result_sample[2])- nreplicas*2^2
    EQM3 = mean((result_sample[3] - p)^2)
    #mean((result_sample[3])^2)-2*1*mean(result_sample[3])- nreplicas*2.5^2
    EQM4 = mean((result_sample[4] - lambda)^2)
    #mean((result_sample[4])^2)-2*1*mean(result_sample[4])- nreplicas*1.5^2	
    #EQM = variance+bias^2
  }
  return(c(a,b,p,lambda, media, bias1, bias2, bias3, bias4, EQM1, EQM2, EQM3, EQM4))
}

EGExpG = function(x, a, b, p, lambda){
  (a*b*lambda*(1-p)*exp(-(lambda*a*x))*(1-exp(-(lambda*a*x)))^(b-1))/(1-p*(1-(1-exp(-(lambda*a*x)))^b))^2
}

# funcao acumulada
EGExpG_CDF = function(x,a, b, p, lambda){
  (1-exp(-lambda*a*x))^b/(1-p*(1-(1-exp(-lambda*a*x))^b))         
}



# funcao random, utilizada para o limsup
# basta passar ela por parametro
rEGExpG = function(U, a, b, p, lambda){
  (-1/lambda)*log((1-((U*(1-p))/(1-U*p))^(1/b))^(1/a))                                              
}
#utilizada no Adequacy
# pdf distribution function.
pdf_EGExpG <- function(par,x){
  a = par[1]
  b = par[2]
  p = par[3]
  lambda = par[4]
  (a*b*lambda*(1-p)*exp(-(lambda*a*x))*(1-exp(-(lambda*a*x)))^(b-1))/(1-p*(1-(1-exp(-(lambda*a*x)))^b) )^2
}
#utilizada no Adequacy
# cdf distribution function
cdf_EGExpG <- function(par,x){
  a = par[1]
  b = par[2]
  p = par[3]
  lambda = par[4]
  (1-exp(-lambda*a*x))^b/(1-p*(1-(1-exp(-lambda*a*x))^b))
}

# Testando
n_sample <- 10
a <- 2
b <- 2
p <- 0.5
#limsup <- 1000
density1 = EGExpG

#get_sample(n_sample, a, b, p, lambda, density1, limsup)

#sample = get_sample(n_sample, a, b, p, lambda, density1, limsup)

#goodness.fit(pdf = pdf_EGExpG, cdf = cdf_EGExpG, starts = c(1,1,0.9,1),data = sample, method="N",domain=c(0,Inf),mle=NULL)

calculate(1.2, 2, 0.5, 1.5, random = rEGExpG,
          density1 = EGExpG, density2 = pdf_EGExpG,
          cumulate = cdf_EGExpG,
          n_replica = 100, n_sample = 100)