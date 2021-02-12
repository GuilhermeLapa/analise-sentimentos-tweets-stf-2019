

#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("tidytext")
#install.packages("quanteda")

library("lubridate")
library("ggplot2")
library("tidytext")
library("quanteda")


########################### Analise de sentimentos Pacote Syuzhet ###########################
#install.packages("syuzhet")
library("syuzhet")
termos <- as.character(tweets_selecionados$text)
sentimentosMatriz <- get_nrc_sentiment(termos)

##Todos os sentimentos
barplot(colSums(sentimentosMatriz), las=2, col=c("red", "lightgreen", "orange", "lightblue", "pink", "grey", "yellow", "darkgreen", "darkblue", "black"), 
        main = "Análise de sentimentos de Tweets sobre STF")
##Apenas negativos e positivos
barplot(colSums(sentimentosMatriz[,9:10]), las=2, col=c("red", "lightblue"),  
        main = "Sentimentos Positivos/Negativos nos Tweets sobre STF")
###########################
rm(termos)
rm(sentimentosMatriz)




########################### Análise de sentimentos Lexicon ###########################
#Utilizados Lexico da Linguateca ReLi e Lexico CHEN/SKIENA-2014 - KAGGLE
#foi necessario ajustar os arquivos da Linguateca ReLi para o formato utf-8, 
#pois estavam perdendo os caracteres com acentuacao e remover observacoes e linhas em branco

########################### Importando as palavras da Linguateca ReLi ###########################
adjetivos_negativos <- read.csv("ADJ_Negativos.txt", header = F, sep = "\t", strip.white = F, stringsAsFactors = F, encoding="UTF-8")
expres_negativas <- read.csv("MWE_Negativos.txt", header = F, sep = "\t", strip.white = F, stringsAsFactors = F, encoding="UTF-8")
verbos_negativos <- read.csv("Verbos_Negativos.txt", header = F, sep = "\t", strip.white = F, stringsAsFactors = F, encoding="UTF-8")
substantivos_negativos <- read.csv("Subst_Negativos.txt", header = F, sep = "\t", strip.white = F, stringsAsFactors = F, encoding="UTF-8")
adjetivos_positivos <- read.csv("ADJ_Positivos.txt", header = F, sep = "\t", strip.white = F, stringsAsFactors = F, encoding="UTF-8")
expres_positivas <- read.csv("MWE_Positivos.txt", header = F, sep = "\t", strip.white = F, stringsAsFactors = F, encoding="UTF-8")
verbos_positivos <- read.csv("Verbos_Positivos.txt", header = F, sep = "\t", strip.white = F, stringsAsFactors = F, encoding="UTF-8")
substantivos_positivos <- read.csv("Subst_Positivos.txt", header = F, sep = "\t", strip.white = F, stringsAsFactors = F, encoding="UTF-8")

########################### Importando as palavras do Lexico CHEN/SKIENA-2014 - KAGGLE ###########################
kaggle_palavras_pos <- read.csv("positive_words_pt.txt", header = F, sep = "\t", strip.white = F, stringsAsFactors = F, encoding="UTF-8")
kaggle_palavras_neg <- read.csv("negative_words_pt.txt", header = F, sep = "\t", strip.white = F, stringsAsFactors = F, encoding="UTF-8")

########################### Montando data frame de polaridade ###########################
DATA_FRAME_Polaridades <- adjetivos_negativos %>% 
  mutate(palavra = V1, polaridade = -1, tipo='adjetivo', sentimento='negativo') %>%
  select(palavra, polaridade, tipo, sentimento) %>%
  arrange(palavra)

qtd <-  length(expres_negativas$V1)
DATA_FRAME_Polaridades <- bind_rows(DATA_FRAME_Polaridades, list(palavra = expres_negativas$V1, polaridade=rep(-1,qtd), tipo=rep('expressao', qtd), sentimento=rep('negativo', qtd)))
qtd <-  length(verbos_negativos$V1)
DATA_FRAME_Polaridades <- bind_rows(DATA_FRAME_Polaridades, list(palavra=verbos_negativos$V1, polaridade=rep(-1,qtd), tipo=rep('verbo',qtd), sentimento=rep('negativo',qtd)))
qtd <-  length(substantivos_negativos$V1)
DATA_FRAME_Polaridades <- bind_rows(DATA_FRAME_Polaridades, list(palavra=substantivos_negativos$V1, polaridade=rep(-1,qtd), tipo=rep('substantivo',qtd), sentimento=rep('negativo',qtd)))
qtd <-  length(kaggle_palavras_neg$V1)
DATA_FRAME_Polaridades <- bind_rows(DATA_FRAME_Polaridades, list(palavra=kaggle_palavras_neg$V1, polaridade=rep(-1,qtd), tipo=rep('noclass',qtd), sentimento=rep('negativo',qtd)))
qtd <-  length(adjetivos_positivos$V1)
DATA_FRAME_Polaridades <- bind_rows(DATA_FRAME_Polaridades, list(palavra=adjetivos_positivos$V1, polaridade=rep(1,qtd), tipo=rep('adjetivo',qtd), sentimento=rep('positivo',qtd)))
qtd <-  length(expres_positivas$V1)
DATA_FRAME_Polaridades <- bind_rows(DATA_FRAME_Polaridades, list(palavra=expres_positivas$V1, polaridade=rep(1,qtd), tipo=rep('expressao',qtd), sentimento=rep('positivo',qtd)))
qtd <-  length(verbos_positivos$V1)
DATA_FRAME_Polaridades <- bind_rows(DATA_FRAME_Polaridades, list(palavra=verbos_positivos$V1, polaridade=rep(1,qtd), tipo=rep('verbo',qtd), sentimento=rep('positivo',qtd)))
qtd <-  length(substantivos_positivos$V1)
DATA_FRAME_Polaridades <- bind_rows(DATA_FRAME_Polaridades, list(palavra=substantivos_positivos$V1, polaridade=rep(1,qtd), tipo=rep('substantivo',qtd), sentimento=rep('positivo',qtd)))
qtd <-  length(kaggle_palavras_pos$V1)
DATA_FRAME_Polaridades <- bind_rows(DATA_FRAME_Polaridades, list(palavra=kaggle_palavras_pos$V1, polaridade=rep(1,qtd), tipo=rep('noclass',qtd), sentimento=rep('positivo',qtd)))

rm(adjetivos_negativos)
rm(expres_negativas)
rm(verbos_negativos)
rm(substantivos_negativos)
rm(adjetivos_positivos)
rm(expres_positivas)
rm(verbos_positivos)
rm(substantivos_positivos)
rm(kaggle_palavras_pos)
rm(kaggle_palavras_neg)

########################### Remover termos duplicados do data frame de polaridade ###########################
DATA_FRAME_Polaridades <- DATA_FRAME_Polaridades[!duplicated(DATA_FRAME_Polaridades$palavra),]
DATA_FRAME_Polaridades %>% count()

save.image("~/dataframe PARTE4.RData")


positivas <- DATA_FRAME_Polaridades %>% filter(sentimento == 'positivo') %>% select(palavra)
negativas <- DATA_FRAME_Polaridades %>% filter(sentimento == 'negativo') %>% select(palavra)
dicionario_polaridades <- dictionary(list(positivas=as.character(positivas$palavra), negativas=as.character(negativas$palavra)))

rm(positivas)
rm(negativas)

########################### CONSTRUIR UMA MATRIZ DFM ESPARSSA A PARTIR DO DICIONÁRIO ###########################
dfmPorSentimento <- dfm(corpus(corpus), dictionary=dicionario_polaridades)

pontuacaoPorGrupo <- tidy(dfmPorSentimento %>% 
                            dfm_group(groups='whois'))
pontuacaoPorGrupo

###################################### CLASSIFICAR CADA TWEET COMO POSITIVO OU NEGATIVO ######################################
DF_analisePorTweet <- data.frame(tweets_selecionados)
i <- 1
tamanho <- length(DF_analisePorTweet$text)
while(i <= tamanho) {
  dfm_analisePorTweet <- dfm(DF_analisePorTweet$text[i], dictionary=dicionario_polaridades)
  dfm_positivas <- dfm_select(dfm_analisePorTweet, pattern = "positivas")
  dfm_negativas <- dfm_select(dfm_analisePorTweet, pattern = "negativas")
  if(as.logical(dfm_positivas >= dfm_negativas)) {
    classe <- "positivo"
  } else {
    classe <- "negativo"
  }
  DF_analisePorTweet$classificacao[i] <- classe
  i <- i+1
}

DF_analisePorTweet$data <- as.Date(DF_analisePorTweet$created_at)

write.csv(DF_analisePorTweet, file="dataframe PARTE5.csv", row.names=TRUE)
save.image("~/dataframe PARTE5.RData")

rm(i)
rm(tamanho)
rm(classe)
rm(dfm_positivas)
rm(dfm_negativas)
rm(dfm_analisePorTweet)
