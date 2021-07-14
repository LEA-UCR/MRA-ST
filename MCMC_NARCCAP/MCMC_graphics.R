library(tidyverse)

load('sim_res/chainTempSVCExponentialNARCCAP.Rdata')
burn <- 500

betas <- chain$chain_beta[-(1:burn),]
colnames(betas) <- paste0('Beta',0:(dim(betas)[2]-1))
betas <- data.frame(betas) %>% mutate(Index=1:n())

betas_plot <- betas %>% pivot_longer(-Index,names_to = 'Variable',values_to='Betas')

grafico_betas <- ggplot(data = betas_plot,mapping = aes(x = Index,y = Betas))+
  geom_line()+facet_wrap(vars(Variable),nrow = dim(betas)[2],scales = 'free')+
  theme_bw()


Phis <- chain$chain_Phi[-(1:burn),]
colnames(Phis) <- paste0('Phi',0:(dim(Phis)[2]-1))
Phis <- data.frame(Phis) %>% mutate(Index=1:n())
Phis_plot <- Phis %>% pivot_longer(-Index,names_to = 'Variable',values_to='Phi')

grafico_Phis <- ggplot(data = Phis_plot,mapping = aes(x = Index,y = Phi))+
  geom_line()+facet_wrap(vars(Variable),nrow = dim(betas)[2],scales = 'free')+
  theme_bw()


elements_A <- function(i){
  elements <- c(diag(chain$chain_A[[i]]),chain$chain_A[[i]][upper.tri(chain$chain_A[[1]])])
  return(elements)
}

As <- purrr::map(1:2000,~elements_A(.x))
As <- do.call(rbind,As)
As <- As[-(1:burn),]
#colnames(As) <- paste0('A',1:dim(As)[2])
colnames(As) <- c('A11','A22','A12')
As <- data.frame(As) %>% mutate(Index=1:n())

A_plot <- As %>% pivot_longer(-Index,names_to = 'Variable',values_to='A')

grafico_A <- ggplot(data = A_plot,mapping = aes(x = Index,y = A))+
  geom_line()+facet_wrap(vars(Variable),nrow = dim(As)[2]-1,scales = 'free')+
  theme_bw()


taus <- chain$chain_tau[-(1:burn)]
taus <- data.frame(Tau=taus) %>% mutate(Index=1:n())

grafico_tau <- ggplot(data = taus,mapping = aes(x = Index,y = Tau))+
  geom_line()+
  theme_bw()


