

#load("~/dataframe PARTE6.RData")

library("tm")
library("stringr")
library("dplyr")


#total de tweets no ano
count(filter(DF_analisePorTweet, mes >= 1, mes <= 12))
#total de tweets ate setembro
count(filter(DF_analisePorTweet, mes >= 1, mes < 9))
#total de tweets entre setembro e dezembro
count(filter(DF_analisePorTweet, mes >= 9, mes <= 12))


i <- 1
qtdPorMes <- c(1)
while(i <= 12) {
  aux <- as.integer(count(filter(DF_analisePorTweet, mes == i)))
  qtdPorMes <- c(qtdPorMes, aux)
  i <- i+1
}
qtdPorMes <- qtdPorMes[2:13]#[13:24]


#media da quantidade de tweets de janeiro ate agosto
mean(qtdPorMes[1:8])
#quantidade de tweets de agosto
qtdPorMes[8]
#quantidade de tweets de setembro
qtdPorMes[9]
#variacao da quantidade de tweets entre agosto e setembro
qtdPorMes[9] - qtdPorMes[8]
#media da quantidade de tweets de setembro ate dezembro
mean(qtdPorMes[9:12])
#resumos estatisticos
summary(qtdPorMes[1:12])
summary(qtdPorMes[1:8])
summary(qtdPorMes[9:12])


#total de palavras positivas e negativas nos tweets
pontuacaoPorGrupo


#total de tweets classificados como negativo em todo o ano 2019
count(filter(DF_analisePorTweet, classificacao == 'negativo'))
#total de tweets classificados como positivos em todo o ano 2019
count(filter(DF_analisePorTweet, classificacao == 'positivo'))
#total de tweets classificados como negativos entre janeiro e agosto
count(filter(filter(DF_analisePorTweet, mes >= 1, mes <= 8), classificacao == 'negativo'))
#total de tweets classificados como positivos entre janeiro e agosto
count(filter(filter(DF_analisePorTweet, mes >= 1, mes <= 8), classificacao == 'positivo'))
#total de tweets classificados como negativos entre setembro e dezembro
count(filter(filter(DF_analisePorTweet, mes >= 9, mes <= 12), classificacao == 'negativo'))
#total de tweets classificados como positivos entre setembro e dezembro
count(filter(filter(DF_analisePorTweet, mes >= 9, mes <= 12), classificacao == 'positivo'))


rm(i)
rm(aux)
rm(qtdPorMes)



#Exemplo de positivo
filter(DF_analisePorTweet, classificacao == 'negativo')$text[212]
filter(DF_analisePorTweet, classificacao == 'positivo')$text[41]


#Exemplo de negativo
filter(DF_analisePorTweet, classificacao == 'negativo')$text[200]
filter(DF_analisePorTweet, classificacao == 'negativo')$text[206]
filter(DF_analisePorTweet, classificacao == 'negativo')$text[207]


#Positivo mas nao direcionados ao STF
filter(DF_analisePorTweet, classificacao == 'positivo')$text[32]
filter(DF_analisePorTweet, classificacao == 'positivo')$text[35]


#Exemplos de ironia
filter(DF_analisePorTweet, classificacao == 'positivo')$text[9]
filter(DF_analisePorTweet, classificacao == 'positivo')$text[10]
filter(DF_analisePorTweet, classificacao == 'positivo')$text[14]
filter(DF_analisePorTweet, classificacao == 'positivo')$text[31]
filter(DF_analisePorTweet, classificacao == 'positivo')$text[33]
filter(DF_analisePorTweet, classificacao == 'positivo')$text[39]
filter(DF_analisePorTweet, classificacao == 'negativo')$text[204]



i <- 1
qtdPositivos <- c(7873,3680,3419,7200,6622,4096,6184,5647,9343,12485,25041,32052)
qtdNegativos <- c(7661,3294,3654,8795,7327,4089,5758,6574,10444,15181,26403,32989)
vProporcaoPolaridade <- c(0)
while(i <= 12) {
  aux <- qtdPositivos[i]-qtdNegativos[i]
  if(aux < 0) {
    aux <- aux*(-1)
  }
  
  vProporcaoPolaridade <- c(vProporcaoPolaridade,aux)
  
  i <- i+1
}
vProporcaoPolaridade <- vProporcaoPolaridade[2:13]

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#7.0   348.2   816.0   882.4  1166.2  2696.0  
summary(vProporcaoPolaridade)

rm(aux)
rm(i)
rm(qtdNegativos)
rm(qtdPositivos)
rm(vProporcaoPolaridade)



##########################################################################################
##########################################################################################
##########################################################################################
calcularQtdPosNegOcorrencia <- function(indices) {
  qtdPositivos <- 0
  qtdNegativos <- 0
  i <- 1
  tamanho <- length(indices)
  while(i <= tamanho) {
    if(DF_analisePorTweet$classificacao[indices[i]] == "negativo") {
      qtdNegativos <- qtdNegativos + 1
    }
    else if(DF_analisePorTweet$classificacao[indices[i]] == "positivo") {
      qtdPositivos <- qtdPositivos + 1
    }
    
    i <- i+1
  }
  print(paste0("As palavras aparecem juntas em: ", length(indices), " tweets.")) #437
  print(paste0("Dos quais ", qtdPositivos, " sao positivos e ", qtdNegativos, " são negativos."))
  
  rm(tamanho)
  rm(i)
  rm(qtdNegativos)
  rm(qtdPositivos)
}


##########################################################################################
#Quantos tweets e em qual classificacao a palavra aborto aparece?
length(which(str_count(DF_analisePorTweet$text, "aborto") >= 1))

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(str_count(DF_analisePorTweet$text[i], "aborto") >= 1) {
    
    qtd <- qtd + 1
    indices <- c(indices, i)
  }
  
  i <- i+1
}
indices <- c(indices[2:length(indices)])
calcularQtdPosNegOcorrencia(indices)

rm(indices)


##########################################################################################
#Quantos tweets e em qual classificacao as palavras salario e aumento aparecem juntas?
length(which(str_count(DF_analisePorTweet$text, "salario") >= 1))
length(which(str_count(DF_analisePorTweet$text, "aumento") >= 1))

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(((str_count(DF_analisePorTweet$text[i], "salario") >= 1) && (str_count(DF_analisePorTweet$text[i], "aumento") >= 1)) || 
     ((str_count(DF_analisePorTweet$text[i], "salário") >= 1) && (str_count(DF_analisePorTweet$text[i], "aumento") >= 1))) {
    
    qtd <- qtd + 1
    indices <- c(indices, i)
  }
  
  i <- i+1
}
indices <- c(indices[2:length(indices)])
calcularQtdPosNegOcorrencia(indices)

rm(indices)


##########################################################################################
#Quantos tweets e em qual classificacao as palavras segunda e instancia aparecem juntas?
length(which(str_count(DF_analisePorTweet$text, "instancia") >= 1))
length(which(str_count(DF_analisePorTweet$text, "segunda") >= 1))
length(which(str_count(DF_analisePorTweet$text, "2a") >= 1))
length(which(str_count(DF_analisePorTweet$text, "2ª") >= 1))

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(((str_count(DF_analisePorTweet$text[i], "segunda") >= 1) && (str_count(DF_analisePorTweet$text[i], "instancia") >= 1)) || 
     #((str_count(DF_analisePorTweet$text[i], "2a") >= 1) && (str_count(DF_analisePorTweet$text[i], "instancia") >= 1)) || 
     #((str_count(DF_analisePorTweet$text[i], "2ª") >= 1) && (str_count(DF_analisePorTweet$text[i], "instancia") >= 1)) || 
     ((str_count(DF_analisePorTweet$text[i], "2") >= 1) && (str_count(DF_analisePorTweet$text[i], "instancia") >= 1))) {
    
    qtd <- qtd + 1
    indices <- c(indices, i)
  }
  
  i <- i+1
}
indices <- c(indices[2:length(indices)])
calcularQtdPosNegOcorrencia(indices)

rm(indices)


##########################################################################################
#Quantos tweets e em qual classificacao a palavra presidente aparece?
length(which(str_count(DF_analisePorTweet$text, "presidente") >= 1))

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(str_count(DF_analisePorTweet$text[i], "presidente") >= 1) {
    
    qtd <- qtd + 1
    indices <- c(indices, i)
  }
  
  i <- i+1
}
indices <- c(indices[2:length(indices)])
calcularQtdPosNegOcorrencia(indices)

rm(indices)


##########################################################################################
#Quantos tweets e em qual classificacao a palavra congresso aparece?
length(which(str_count(DF_analisePorTweet$text, "congresso") >= 1))

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(str_count(DF_analisePorTweet$text[i], "congresso") >= 1) {
    
    qtd <- qtd + 1
    indices <- c(indices, i)
  }
  
  i <- i+1
}
indices <- c(indices[2:length(indices)])
calcularQtdPosNegOcorrencia(indices)

rm(indices)


##########################################################################################
#Quantos tweets e em qual classificacao a palavra lula aparece?
length(which(str_count(DF_analisePorTweet$text, "lula") >= 1))

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(str_count(DF_analisePorTweet$text[i], "lula") >= 1) {
    
    qtd <- qtd + 1
    indices <- c(indices, i)
  }
  
  i <- i+1
}
indices <- c(indices[2:length(indices)])
calcularQtdPosNegOcorrencia(indices)

rm(indices)


##########################################################################################
#Quantos tweets e em qual classificacao a palavra moro aparece?
length(which(str_count(DF_analisePorTweet$text, "moro") >= 1))

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(str_count(DF_analisePorTweet$text[i], "moro") >= 1) {
    
    qtd <- qtd + 1
    indices <- c(indices, i)
  }
  
  i <- i+1
}
indices <- c(indices[2:length(indices)])
calcularQtdPosNegOcorrencia(indices)

rm(indices)


##########################################################################################
#Quantos tweets e em qual classificacao a palavra jairbolsonaro aparece?
length(which(str_count(DF_analisePorTweet$text, "jairbolsonaro") >= 1))

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(str_count(DF_analisePorTweet$text[i], "jairbolsonaro") >= 1) {
    
    qtd <- qtd + 1
    indices <- c(indices, i)
  }
  
  i <- i+1
}
indices <- c(indices[2:length(indices)])
calcularQtdPosNegOcorrencia(indices)

rm(indices)



rm(tamanho)
rm(i)
rm(qtd)
rm(calcularQtdPosNegOcorrencia)
