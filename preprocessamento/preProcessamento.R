

#carregar
library("tm")
library("wordcloud")
library("RColorBrewer")
library("stringr")
library("dplyr")
library("SnowballC")
library("jsonlite")

preProcessamentoTweets <- function(corpus) {
	#Transformar todos caracteres em minúsculos
	corpus <- tm_map(corpus, tolower)
	#Remover pontuação
	corpus <- tm_map(corpus, removePunctuation)
	#Remover todos os números
	corpus <- tm_map(corpus, removeNumbers);
	#Remover espaçoos em branco extras
	corpus <- tm_map(corpus, stripWhitespace)
	#Remover as palavras ruído, considera apenas as stopwords predefindas no R
	corpus <- tm_map(corpus, removeWords, stopwords('portuguese'))
	#Remover as URLs
	removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
	corpus <- tm_map(corpus, removeURL)
	#remove qualquer coisa que não sejam letras em português e espaço.
	removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
	corpus <- tm_map(corpus, content_transformer(removeNumPunct))
	
	rm(removeURL)
	rm(removeNumPunct)
	
	return(corpus)
}

getFrequenciaPalavras <- function(dtmMatriz) {
	#Criamos um dataframe com a frequência de cada palavra a partir da dtmMatriz
	frequenciaPalavras <- sort(rowSums(dtmMatriz),decreasing=TRUE)
	dataFrameFrequencia <- data.frame(palavra = names(frequenciaPalavras),frequencia=frequenciaPalavras)
	
	return(dataFrameFrequencia)
}

criarMatrizTermoDocumento <- function(corpus) {
	#Cria a matriz termo-documento
	matrizTermoDocumento <- TermDocumentMatrix(corpus)
	matrizTermoDocumento <- removeSparseTerms(matrizTermoDocumento, 0.99)
	dtmMatriz <- as.matrix(matrizTermoDocumento)
	
	return(dtmMatriz)
}

#Separamos as variáveis com maior relevância
tweets_selecionados <- select(tweets_selecionados, 
							user_id,
							status_id,
							created_at,
							screen_name,
							text,
							source,
							display_text_width,
							reply_to_status_id,
							reply_to_user_id,
							reply_to_screen_name,
							is_quote,
							is_retweet,
							favorite_count,
							retweet_count, 
							location)


write.csv(tweets_selecionados, file="tweets selecionados PARTE2.csv", row.names=TRUE)
save.image("~/dataframe PARTE2.RData")


#Colapsando todos os tweets em um vetor de uma posicao.
vetorTweets <- paste(tweets_selecionados$text, collapse = " ")

#Criamos o Corpus
vetorFonteTweets <- VectorSource(vetorTweets)
corpus <- Corpus(vetorFonteTweets)

#Executamos o pré-processamento
corpus <- preProcessamentoTweets(corpus)

#Cria a matriz termo-documento
dtmMatriz <- criarMatrizTermoDocumento(corpus)
#Obtemos a frequência de cada palavra
dataFrameFrequencia <- getFrequenciaPalavras(dtmMatriz)


write.csv(dtmMatriz, file="dtmMatriz PARTE3.csv", row.names=TRUE)
save.image("~/dataframe PARTE3.RData")
rm(dtmMatriz)


#Cria a primeira nuvem de palavras
wordcloud(corpus, min.freq = 1, max.words=Inf, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

########################### Remoção manual de palavras ###########################
novas_stopwords_pt <- c(stopwords("pt"), 
                        "aaa",
                        "aaaa",
                        "aaaaa",
                        "aaaaaa",
                        "aaaaaaa",
                        "aaaaaaaa",
                        "aaaaaaaaaa",
                        "aaaaaaaaaaa",
                        "aaaaaaaaaaaaaaaa",
                        "aaaaaaaaaaaaaaaaaaaa",
                        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                        "aaaaaaaaaaaaaah",
                        "aaaaaaaah",
                        "aaaaaah",
                        "aaaaaahhhhhj",
                        "aaaahh",
                        "aaaahhh",
                        "aaaahhhh",
                        "aaaaahhhhh",
                        "aaaaaheu",
                        "aaaaah",
                        "aaah",
                        "aaahh",
                        "aaahhhh",
                        "aaahq",
                        "pra",
                        "t�",
                        "desses",
                        "dessas",
                        "dessa",
                        "desse",
                        "sempre",
                        "pois",
                        "assim",
                        "l�",
                        "a�",
                        "porque",
                        "vcs",
                        "outro",
                        "sai",
                        "etc",
                        "vamo",
                        "vamos",
                        "vai",
                        "ser",
                        "todo",
                        "agora",
                        "dia",
                        "via",
                        "ter",
                        "fazer",
                        "quer",
                        "cara",
                        "faz",
                        "ver",
                        "deve",
                        "favor",
                        "hora",
                        "gente",
                        "dar",
                        "ent�o",
                        "acho",
                        "fez",
                        "d�",
                        "outra",
                        "t�o",
                        "ap�s",
                        "kkkkk",
                        "aqui",
                        "diz",
                        "v�o",
                        "onde",
                        "aqui",
                        "vez",
                        "todos",
                        "sendo",
                        "querem",
                        "ainda",
                        "pro",
                        "deveria")

#Remove as novas stopwords selecionadas manualmente do corpus
corpus <- tm_map(corpus, removeWords, novas_stopwords_pt)
#Executa o pré-processamento novamente sem as novas stopwords
corpus <- preProcessamentoTweets(corpus)

#Cria a matriz termo-documento
dtmMatriz <- criarMatrizTermoDocumento(corpus)
#Obtemos a frequência de cada palavra
dataFrameFrequencia <- getFrequenciaPalavras(dtmMatriz)
frequenciaPalavras <- sort(rowSums(dtmMatriz),decreasing=TRUE)


write.csv(dtmMatriz, file="dtmMatriz PARTE4.csv", row.names=TRUE)
save.image("~/dataframe PARTE4.RData")


#Desconsideramos para a wordcloud as palavras descriminativas (OUTLIERS), aquelas que ocorrem com muita frequência ou raramente
View(frequenciaPalavras)
filter(dataFrameFrequencia, frequenciaPalavras == max(dataFrameFrequencia$frequencia))
count(filter(dataFrameFrequencia, frequenciaPalavras == 1))

#Removemos as palavras com maior frequência (os outliers)
outliers <- c('stf','stfoficial')
corpus <- tm_map(corpus, removeWords, outliers)

#REFAZEMOS a dtmMatriz e a dataFrameFrequencia após a retirada dos OUTLIERS
dtmMatriz <- criarMatrizTermoDocumento(corpus)
dataFrameFrequencia <- getFrequenciaPalavras(dtmMatriz)

#Geramos uma nova nuvem de palavras ; min.freq = 2 desconsideramos as com frequencia == 1
wordcloud(corpus, min.freq = 2, max.words=Inf, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

#STEMMING
#O corpus após o stemming deve ser guardado em nova variável para não afetar os gráficos
stemmed_corpus <- tm_map(corpus, stemDocument, language = "portuguese")
stemmed_dtm <- TermDocumentMatrix(stemmed_corpus)
stemmed_dtm_mt <- as.matrix(stemmed_dtm)
stemmed_dtf_frequencia <- sort(rowSums(stemmed_dtm_mt),decreasing=TRUE)
stemmed_DTF <- data.frame(palavra = names(stemmed_dtf_frequencia),frequencia=stemmed_dtf_frequencia)

rm(vetorTweets)
rm(vetorFonteTweets)
rm(dtmMatriz)
rm(outliers)
rm(stemmed_dtm)
rm(stemmed_dtm_mt)
rm(stemmed_frequencia)
rm(preProcessamentoTweets)
