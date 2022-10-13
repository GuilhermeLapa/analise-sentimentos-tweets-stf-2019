

library("tm")
library("stringr")
library("dplyr")


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




####################
##### toffoli
length(which(str_count(DF_analisePorTweet$text, "toffoli") >= 1))
filter(dataFrameFrequencia, palavra == "toffoli")

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(str_count(DF_analisePorTweet$text[i], "toffoli") >= 1) {
    
    qtd <- qtd + 1
    indices <- c(indices, i)
  }
  
  i <- i+1
}
indices <- c(indices[2:length(indices)])
calcularQtdPosNegOcorrencia(indices)

rm(indices)

####################
##### gilmar
length(which(str_count(DF_analisePorTweet$text, "gilmar") >= 1))
filter(dataFrameFrequencia, palavra == "gilmar")

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(str_count(DF_analisePorTweet$text[i], "gilmar") >= 1) {
    
    qtd <- qtd + 1
    indices <- c(indices, i)
  }
  
  i <- i+1
}
indices <- c(indices[2:length(indices)])
calcularQtdPosNegOcorrencia(indices)

rm(indices)

####################
##### carmen
length(which(str_count(DF_analisePorTweet$text, "carmen") >= 1))
filter(dataFrameFrequencia, palavra == "carmen")

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(str_count(DF_analisePorTweet$text[i], "carmen") >= 1) {
    
    qtd <- qtd + 1
    indices <- c(indices, i)
  }
  
  i <- i+1
}
indices <- c(indices[2:length(indices)])
calcularQtdPosNegOcorrencia(indices)

rm(indices)

####################
##### fachin
length(which(str_count(DF_analisePorTweet$text, "fachin") >= 1))
filter(dataFrameFrequencia, palavra == "fachin")

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(str_count(DF_analisePorTweet$text[i], "fachin") >= 1) {
    
    qtd <- qtd + 1
    indices <- c(indices, i)
  }
  
  i <- i+1
}
indices <- c(indices[2:length(indices)])
calcularQtdPosNegOcorrencia(indices)

rm(indices)

####################
##### barroso
length(which(str_count(DF_analisePorTweet$text, "barroso") >= 1))
filter(dataFrameFrequencia, palavra == "barroso")

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(str_count(DF_analisePorTweet$text[i], "barroso") >= 1) {
    
    qtd <- qtd + 1
    indices <- c(indices, i)
  }
  
  i <- i+1
}
indices <- c(indices[2:length(indices)])
calcularQtdPosNegOcorrencia(indices)

rm(indices)

####################
##### mello
length(which(str_count(DF_analisePorTweet$text, "mello") >= 1))
filter(dataFrameFrequencia, palavra == "mello")

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(str_count(DF_analisePorTweet$text[i], "mello") >= 1) {
    
    qtd <- qtd + 1
    indices <- c(indices, i)
  }
  
  i <- i+1
}
indices <- c(indices[2:length(indices)])
calcularQtdPosNegOcorrencia(indices)

rm(indices)

####################
##### weber
length(which(str_count(DF_analisePorTweet$text, "weber") >= 1))
filter(dataFrameFrequencia, palavra == "weber")

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(str_count(DF_analisePorTweet$text[i], "weber") >= 1) {
    
    qtd <- qtd + 1
    indices <- c(indices, i)
  }
  
  i <- i+1
}
indices <- c(indices[2:length(indices)])
calcularQtdPosNegOcorrencia(indices)

rm(indices)

####################
##### fux
length(which(str_count(DF_analisePorTweet$text, "fux") >= 1))
filter(dataFrameFrequencia, palavra == "fux")

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(str_count(DF_analisePorTweet$text[i], "fux") >= 1) {
    
    qtd <- qtd + 1
    indices <- c(indices, i)
  }
  
  i <- i+1
}
indices <- c(indices[2:length(indices)])
calcularQtdPosNegOcorrencia(indices)

rm(indices)

####################
##### lewandowski
length(which(str_count(DF_analisePorTweet$text, "lewandowski") >= 1))
filter(dataFrameFrequencia, palavra == "lewandowski")

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(str_count(DF_analisePorTweet$text[i], "lewandowski") >= 1) {
    
    qtd <- qtd + 1
    indices <- c(indices, i)
  }
  
  i <- i+1
}
indices <- c(indices[2:length(indices)])
calcularQtdPosNegOcorrencia(indices)

rm(indices)

####################
##### moraes
length(which(str_count(DF_analisePorTweet$text, "moraes") >= 1))
filter(dataFrameFrequencia, palavra == "moraes")

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(str_count(DF_analisePorTweet$text[i], "moraes") >= 1) {
    
    qtd <- qtd + 1
    indices <- c(indices, i)
  }
  
  i <- i+1
}
indices <- c(indices[2:length(indices)])
calcularQtdPosNegOcorrencia(indices)

rm(indices)



####################
##### marco
length(which(str_count(DF_analisePorTweet$text, "marco aurelio") >= 1))
filter(dataFrameFrequencia, palavra == "marco")
filter(dataFrameFrequencia, palavra == "aurelio")

qtd <- 0
i <- 1
indices <- c(0)
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  if(str_count(DF_analisePorTweet$text[i], "marco") >= 1 && (str_count(DF_analisePorTweet$text[i], "aurelio") >= 1)) {
    
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
