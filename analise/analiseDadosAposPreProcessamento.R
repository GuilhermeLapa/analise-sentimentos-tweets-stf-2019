

#load("~/dataframe PARTE3.RData")

library("tm")
library("stringr")
library("dplyr")
library("ggplot2")


############### Analise de dados apos o Pre-processamento - PARTE 1  ###############

#Resumo dos dados iniciais
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#1.00      1.00      1.00     21.17      4.00 23784.00
summary(dataFrameFrequencia$frequencia)

#82485 palavras possuem frequencia igual a 1
totalPalavrasComFrequencia1 <- count(filter(dataFrameFrequencia, frequencia == 1))
#56% das palavras da tabela possuem frequência igual a 1
porcentagemPalavrasComFrequencia1 <- (totalPalavrasComFrequencia1/length(dataFrameFrequencia$frequencia)) * 100

#resumo dos dados desconsiderando as palavras com ocorrencia 1
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#2.00      2.00      5.00     47.34     15.00 23784.00 
summary(filter(dataFrameFrequencia, frequencia != 1)$frequencia)


#########GRAFICO SCATTERPLOT
filter(dataFrameFrequencia, frequencia>=5000) %>% 
  ggplot(aes(x= palavra, y= frequencia)) +
  geom_point() + 
  scale_y_continuous(limits = c(5000,220000), breaks = seq(5000,220000,10000)) + 
  theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1)) + 
  xlab("Palavras") + ylab("Frequência") + 
  ggtitle("Palavras com frequência maior que 5000")




############### Analise de dados apos o Pre-processamento - PARTE 2  ###############
#resumo dos dados desconsiderando as palavras com ocorrencia 1 e as palavras 'stf' e 'stfoficial'
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#2.00     2.00     5.00    47.34    15.00 23784.00 
summary(filter(filter(dataFrameFrequencia, frequencia != 1), palavra != 'stf', palavra != 'stfoficial')$frequencia)

dataFrameFrequencia <- filter(filter(dataFrameFrequencia, palavra != 'stf', palavra != 'stfoficial'), frequencia>1)


#########GRAFICO SCATTERPLOT
filter(dataFrameFrequencia, frequencia>=5000) %>% 
  ggplot(aes(x= palavra, y= frequencia)) +
  geom_point() + 
  scale_y_continuous(limits = c(5000,25000), breaks = seq(5000,25000,1000)) + 
  theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1)) + 
  xlab("Palavras") + ylab("Frequência") + 
  ggtitle("Palavras com frequência maior que 5000")
