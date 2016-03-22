
#Fisher
J<-function(lambda, x){
  soma<-0
  for (i in 1:length(x)) {
    soma=+4*cos((1/2)*pi*(-1+exp(-lambda*x[i])))^2*lambda^2/(2*cos((1/2)*pi*
                                                                     (-1+exp(-lambda*x[i])))*sin((1/2)*pi*(-1+exp(-lambda*x[i])))*pi
                                                             *exp(-lambda*x[i])*lambda^2*x[i]^2+pi^2
                                                             *exp(-2*lambda*x[i])*lambda^2*x[i]^2+4*cos((1/2)*pi*(-1+exp(-lambda*x[i])))^2)
  }
  return(soma)
}
#criando matriz e salvando em arquivo
amostra = c(5,10,15,20)
nreplicas = 10
a = c(0.5)
b = c(0.3)
p = c(0.02)
lambda = c(2)
resultado_final = matrix(nrow=length(lambdas), ncol=7)
colnames(resultado_final) = list('Amostra', 'Lambda', 'Media',
                                 'Variancia', 'Bias', 'EQM','ERROR')
result<-NULL
for(k in 1:length(amostra)){
  for(i in 1:length(lambdas)){
    result = rbind(result, calculate(a, b, p, lambda, random = rEGExpG,
                                     density1 = EGExpG, density2 =pdf_EGExpG,
                                     cumulate = cdf_EGExpG,
                                     n_replica = nreplicas, n_sample = amostra[k]))
    #resultado_final[i,1] = amostra[k]
    #resultado_final[i,2] = lambdas[i]
    #resultado_final[i,3] = round(result[3],6)
    #resultado_final[i,4] = round(result[2],6)
    #resultado_final[i,5] = abs(round(result[4],6))
    #resultado_final[i,6] = round(result[5],6)
    #resultado_final[i,7] = round(result[6],6)
    #}
  }
  #write.table(x = resultado_final, sep = ';',
  #file = "C:/users/Frank/Dropbox/Article_Yousof_Manoel_Gomes-Silva/teste1.txt")