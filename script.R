######################### MIRT - Modelo Logistico de 1 a 4 Parametros ###########################
if(!require(mirt)) install.packages("mirt"); library(mirt)  
if(!require(irtoys)) install.packages("irtoys"); library(irtoys)  
if(!require(data.table)){install.packages("data.table");library(data.table)}
if(!require("bit64")){install.packages("bit64");library(bit64)}
if(!require("psych")){install.packages("psych");library(psych)}
if(!require("dplyr")){install.packages("dplyr");library(dplyr)}
if(!require("ggplot2")){install.packages("ggplot2");library(ggplot2)}
if(!require("stringr")){install.packages("stringr");library(stringr)}
if(!require("latticeExtra")){install.packages("latticeExtra");library(latticeExtra)}
if(!require(nortest)){install.packages("nortest"); library(nortest)}

# Carregar banco
banco <- read.table(file.choose(),header = T, sep = ";")
BANCO1 = banco[,2:21]
fator = principal(BANCO1, nfactors = 1, rotate = "Quartimin");fator;fator$loadings
modelo<-mirt(BANCO1, model=1, itemtype = "graded",  technical = list(NCYCLES=5000))
par.MGRM=coef(modelo,simplify=TRUE)$items
par.MGRM.PAR=-par.MGRM[,2:11]/par.MGRM[,1]
par.MGRM.FINAL = cbind(par.MGRM[,1],par.MGRM.PAR)
escore = fscores(modelo)

# Distribuicao dos escores
ggplot(data=BANCO1 , aes(x=escore[,1])) + 
  geom_histogram(alpha = .7, fill ="gray", color="black",  binwidth=.5)+theme_bw()+
  scale_x_continuous(breaks=seq(-50, 50, .5))+xlab("Indicador")+ylab("Frequencia")+
  theme(axis.text = element_text(size=15), legend.text = element_text(size=18))

# Plots
plot(modelo, which.items = 1:NCOL(BANCO1), type = "trace", main = ' ')
plot(modelo, type = "infotrace", main = "")
plot(modelo, type = 'trace', facet_items=FALSE, which.items = 1:NCOL(BANCO1), main = ' ')
itemplot(modelo, type = "infotrace", item = 20, main = "Item 20")

