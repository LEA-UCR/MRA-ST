d.pc.prior<-function(Phi,sigma=Sigma,U_Phi=10,a_Phi=0.9,U_sigma=2,a_sigma=0.9,log=TRUE){
  #equation 14 from Franco-Villoria2019
  #sigma instead of tau
  lambda_Phi=-log(a_Phi)*U_Phi
  lambda_sigma=-log(a_sigma)/U_sigma  
  pi <-lambda_Phi/Phi^2*exp(-lambda_Phi/Phi)*lambda_sigma/2*sigma^(-3/2)*exp(-lambda_sigma/sigma^(-1/2))
  return(ifelse(log,log(pi),pi))
}
