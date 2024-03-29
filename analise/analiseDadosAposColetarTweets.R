

#load("~/dataframe PARTE1.RData")

library("tm")
library("stringr")
library("dplyr")


#######################################################################################
######################### Analise dos dados da Fase de Coleta #########################
#######################################################################################
which(str_count(tweets$text, "sTF") == 1)
which(str_count(tweets$text, "StF") == 1)
which(str_count(tweets$text, "STf") == 1)
which(str_count(tweets$text, "Stf") == 1)
which(str_count(tweets$text, "sTf") == 1)
which(str_count(tweets$text, "stF") == 1)
which(str_count(tweets$text, "STF") == 1)
which(str_count(tweets$text, "stf") == 1)

length(which(str_count(tweets$text, "sTF") == 1))
length(which(str_count(tweets$text, "StF") == 1))
length(which(str_count(tweets$text, "STf") == 1))
length(which(str_count(tweets$text, "Stf") == 1))
length(which(str_count(tweets$text, "sTf") == 1))
length(which(str_count(tweets$text, "stF") == 1))
length(which(str_count(tweets$text, "STF") == 1))
length(which(str_count(tweets$text, "stf") == 1))

#ocorrencias da palavra "stf" escrita de diferentes formas
31 + 53 + 109 + 1112 + 7 + 11 + 223049 + 9906 # = 234278

#total de tweets coletados menos todas as ocorrencias
length(tweets$user_id) - 234278 # = 21546

#21546 tweets nao contem a palavra "stf"??
#textos com fontes diferentes mesmo que possuam a palavra "stf"
#nao sao contabilizados pela funcao str_count()
tweets$text[50384]
tweets$screen_name[50384]
str_detect(tweets$text[50384], "sTF")
str_detect(tweets$text[50384], "StF")
str_detect(tweets$text[50384], "STf")
str_detect(tweets$text[50384], "Stf")
str_detect(tweets$text[50384], "sTf")
str_detect(tweets$text[50384], "stF")
str_detect(tweets$text[50384], "STF")
str_detect(tweets$text[50384], "stf")

#alguns tweets nao possuem a palavra "stf" no campo tweets$text
#porem possuem "stf" no campo tweets$screen_name e por isso foram coletadas
tweets$text[246835]
tweets$screen_name[246835]


getTweetsSemSTF <- function() {
  ##### Varre todos os registros de tweets coletados
  ##### Recupera os indices no data.frame tweets 
  ##### que nao possuem a palavra "stf" em tweets$text
  i <- 1
  qtd <- 0
  tamanho <- length(tweets$text)
  indicesTweetSemStf <- c()
  
  while(i <= tamanho) {
    if((str_count(tweets$text[i], "sTF") < 1) && (str_count(tweets$text[i], "StF") < 1) && (str_count(tweets$text[i], "STf") < 1) && (str_count(tweets$text[i], "Stf") < 1) && (str_count(tweets$text[i], "sTf") < 1) && (str_count(tweets$text[i], "stF") < 1) && (str_count(tweets$text[i], "STF") < 1) && (str_count(tweets$text[i], "stf") < 1)) {
      qtd <- qtd + 1
      indicesTweetSemStf <- c(indicesTweetSemStf, i)
      print(paste0("Indice: ", i))
    }
    
    i <- i+1
  }
  
  rm(qtd)
  rm(i)
  rm(tamanho)
  
  return(indicesTweetSemStf)
}


getTweetComScreenName <- function(indicesTweetSemStf) {
  ##### a partir de uma lista de indices do data.frame tweets
  ##### verifica se esses tweets que nao possuem a palavra"stf" 
  ##### no tweets$text possuem "STF_oficial" em tweets$text
  i <- 1
  qtdComScreenName <- 0
  indicesTweetComScreenName <- c()
  tamanho <- length(indicesTweetSemStf)
  
  while(i <= tamanho) {
    aux <- indicesTweetSemStf[i]
    if(tweets$screen_name[aux] == "STF_oficial") {
      qtdComScreenName <- qtdComScreenName + 1
      indicesTweetComScreenName <- c(indicesTweetComScreenName, aux)
    }
    
    i <- i+1
  }
  
  print(paste0(qtdComScreenName," tweets com screen_name igual a STF."))
  
  rm(qtdComScreenName)
  rm(i)
  rm(aux)
  rm(tamanho)
  rm(indicesTweetSemStf)
  
  return(indicesTweetComScreenName)
}


getTweetSemScreenName <- function(indicesTweetSemStf) {
  ##### a partir de uma lista de indices do data.frame tweets
  ##### verifica se esses tweets que nao possuem a palavra"stf" 
  ##### no tweets$text possuem "STF_oficial" em tweets$text
  i <- 1
  qtdSemScreenName <- 0
  indicesTweetSemScreenName <- c()
  tamanho <- length(indicesTweetSemStf)
  
  while(i <= tamanho) {
    aux <- indicesTweetSemStf[i]
    if(tweets$screen_name[aux] != "STF_oficial") {
      qtdSemScreenName <- qtdSemScreenName + 1
      indicesTweetSemScreenName <- c(indicesTweetSemScreenName, aux)
    }
    
    i <- i+1
  }
  
  print(paste0(qtdSemScreenName," tweets com screen_name diferente de STF."))
  
  rm(qtdSemScreenName)
  rm(i)
  rm(aux)
  rm(tamanho)
  rm(indicesTweetSemStf)
  
  return(indicesTweetSemScreenName)
}

indicesTweetSemStf <- getTweetsSemSTF()
#145 tweets nao possuem a palavra "stf" no campo tweets$text
indicesTweetSemStf
length(indicesTweetSemStf)


indicesTweetComScreenName <- getTweetComScreenName(indicesTweetSemStf)
#136 tweets com screen_name igual a "STF_oficial"
#mas nao possuem a palavra "stf" no campo tweets$text e por isso fora coletados
indicesTweetComScreenName
length(indicesTweetComScreenName)
tweets$text[246837]
tweets$screen_name[246837]


indicesTweetSemScreenName <- getTweetSemScreenName(indicesTweetSemStf)
#9 Tweets que NAO possuem screen_name igual a STF_oficial 
#mas mesmo assim foram coletados
indicesTweetSemScreenName
length(indicesTweetSemScreenName)
tweets$text[16549]
tweets$text[29241]
tweets$text[40295]
tweets$text[50384]
tweets$text[74198]
tweets$text[80257]
tweets$text[193912]
tweets$text[195561]
tweets$text[222923]


###### A partir dos 136 tweets com screen_name igual a "STF_oficial"
###### remover todos registros com screen_name = STF_oficial 
###### pois nao refletem opnioes de usuarios sobre o stf 
###### e sim postagens do proprio stf
#pegamos o user_id do usuario STF_oficial
userId <- tweets$user_id[indicesTweetComScreenName[1]]
#sao 205 tweets do usuario STF_oficial
length(filter(tweets, user_id == userId)$user_id)

#fazemos a remocao
tweets_selecionados <- filter(tweets, user_id != userId)
length(tweets$user_id) - length(filter(tweets, user_id == userId)$user_id) #255619


###### Dos 9 Tweets que NAO possuem screen_name igual a STF_oficial 
###### apenas o de indice 50384 tem conteudo de texto sobre o STF
###### e nao tem screen_name = STF_oficial os outros 8 so contem o texto
###### "account is temporarily unavailable because it violates the Twitter Media Policy."
###### indicando que os usu�rio excluiu seu tweet ou descompartilhou-o 
###### ou o pr�prio twitter o bloqueou devido suas politicas
ids <- c(tweets$user_id[16549],
        tweets$user_id[29241],
        tweets$user_id[40295],
        tweets$user_id[74198],
        tweets$user_id[80257],
        tweets$user_id[193912],
        tweets$user_id[195561],
        tweets$user_id[222923])
#fazemos a remocao
tweets_selecionados <- filter(tweets_selecionados, !(user_id %in% ids))

#nao ha mais tweets do usuario STF_oficial
length(filter(tweets_selecionados, tweets_selecionados$screen_name == "STF_oficial")$user_id)

#tweets do usuario STF_oficial
totalTweetsStfOficial <- length(filter(tweets, user_id == userId)$user_id)
porcentagemTweetsStfOficial <- (totalTweetsStfOficial/255824)*100

#tweets removidos
totalTweetsRemovidosManual <- length(ids) + totalTweetsStfOficial
porcentagemTweetsRemovidosManual <- (totalTweetsRemovidosManual/255824)*100

#tweets bloqueados removidos
totalTweetsBloqueados <- length(ids)
porcentagemTweetsBloqueados <- (totalTweetsBloqueados/255824)*100

#tweets de usuarios diferentes de STF_oficial
totalTweetsOutrosUsuarios <- length(tweets_selecionados$user_id)
porcentagemTweetsOutrosUsuarios <- (totalTweetsOutrosUsuarios/255824)*100


rm(userId)
rm(ids)
rm(indicesTweetSemScreenName)
rm(indicesTweetComScreenName)
rm(indicesTweetSemStf)
rm(getTweetsSemSTF)
rm(getTweetComScreenName)
rm(getTweetSemScreenName)

#rm(totalTweetsRemovidosManual)
#rm(totalTweetsOutrosUsuarios)
#rm(totalTweetsStfOficial)
#rm(totalTweetsBloqueados)
#rm(porcentagemTweetsOutrosUsuarios)
#rm(porcentagemTweetsStfOficial)
#rm(porcentagemTweetsRemovidosManual)
#rm(porcentagemTweetsBloqueados)
