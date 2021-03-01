

#install.packages("ggplot2")
library("ggplot2")
library("dplyr")
library("quanteda")
library("tm")
library("dplyr")



###################################### GRAFICOS ######################################
##### Grafico por polaridade ######################################
pontuacaoPorGrupo %>%
  ggplot(aes(term, count)) +
  geom_bar(stat = 'identity') +
  geom_smooth() +
  xlab("Termos") +
  ylab("Quantidade") +
  ggtitle("Frequ�ncia por polaridade") +
  aes(fill= term,color = term) + scale_color_manual(values = c("red", "green"))

#####Graficos Tweets ano 2019#####
select(DF_analisePorTweet, classificacao, data) %>% 
  ggplot() +
  xlab("Data") +
  ylab("Quantidade de Tweets") +
  ggtitle("Tweets sobre STF ano 2019") +
  geom_bar(aes(x = data), color = "black", fill = "light blue")

#####Graficos Tweets positivos#####
df_dfm_positivas <- filter(DF_analisePorTweet, classificacao=="positivo")
df_dfm_positivas <- select(df_dfm_positivas, classificacao, data)

filter(df_dfm_positivas, data>=as.Date('2019-01-01'), data<=as.Date('2019-01-31')) %>% 
  ggplot() +
  xlab("Data") +
  ylab("Quantidade de Tweets") +
  ggtitle("Tweets Positivos Janeiro 2019") +
  geom_bar(aes(x = data), color = "black", fill = "light blue")


#####Graficos Tweets negativos#####
df_dfm_negativas <- filter(DF_analisePorTweet, classificacao=="negativo")
df_dfm_negativas <- select(df_dfm_negativas, classificacao, data)

filter(df_dfm_negativas, data>=as.Date('2019-01-01'), data<=as.Date('2019-01-31')) %>% 
  ggplot() +
  xlab("Data") +
  ylab("Quantidade de Tweets") +
  ggtitle("Tweets Negativos Janeiro 2019") +
  geom_bar(aes(x = data), color = "black", fill = "red")


rm(df_dfm_positivas)
rm(df_dfm_negativas)
############################################################################




#####################################Graficos com ggplot2
barras <- DTF_frequencia$frequencia[2:100]
porcentagem <- round(barras/sum(barras)*100)

##########GRAFICO BARRAS
legenda= DTF_frequencia$palavra[1:10]
ggplot(DTF_frequencia[1:10,], aes(x = palavra, y = frequencia, fill = legenda)) + 
        geom_bar(stat = "identity") + 
        theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(size = 12, color = "blue", hjust = 0.5)) + 
        xlab("Palavras") + ylab("Frequ�ncia") + 
        ggtitle("10 Palavras Mais Frequentes")
##########
legenda= DTF_frequencia$palavra[2:11]
ggplot(DTF_frequencia[2:11,], aes(x = palavra, y = frequencia, fill = legenda)) + 
        geom_bar(stat = "identity") + 
        theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(size = 12, color = "blue", hjust = 0.5)) + 
        xlab("Palavras") + ylab("Frequ�ncia") + 
        ggtitle("10 Palavras Mais Frequentes Exceto STF")

#########GRAFICO BOXPLOT
legenda= DTF_frequencia$palavra[1:10]
DTF_frequencia[1:10,] %>% 
        ggplot() + 
        geom_boxplot(aes(x = palavra, y = frequencia, fill = legenda)) + 
        theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1)) + 
        xlab("Palavras") + ylab("Frequência") + 
        ggtitle("10 Palavras Mais Frequentes")

legenda= DTF_frequencia$palavra[2:11]
DTF_frequencia[2:11,] %>% 
        ggplot() + 
        geom_boxplot(aes(x = palavra, y = frequencia, fill = legenda)) + 
        theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1)) + 
        xlab("Palavras") + ylab("Frequência") + 
        ggtitle("10 Palavras Mais Frequentes Exceto STF")

#########GRAFICO SCATTERPLOT
filter(DTF_frequencia, frequencia>=5000) %>% 
        ggplot(aes(x= palavra, y= frequencia)) +
        geom_point() + 
        theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1)) + 
        xlab("Palavras") + ylab("Frequência") + 
        ggtitle("Palavras com frequência maior que 5000")

filter(filter(DTF_frequencia, frequencia>=5000), palavra!="stf", palavra!="stfoficial") %>% 
        ggplot(aes(x= palavra, y= frequencia)) +
        geom_point() + 
        theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1)) + 
        xlab("Palavras") + ylab("Frequência") + 
        ggtitle("Palavras com frequência maior que 5000 exceto STF e STFOFICIAL")

filter(filter(DTF_frequencia, frequencia>=4000), palavra!="stf", palavra!="stfoficial") %>% 
        ggplot(aes(x= palavra, y= frequencia)) +
        geom_point() + #geom_smooth(method = "lm") + 
        theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1)) + 
        xlab("Palavras") + ylab("Frequência") + 
        ggtitle("Palavras com frequência maior que 4000 exceto STF e STFOFICIAL")

#########################
rm(barras)
rm(porcentagem)
rm(legenda)
