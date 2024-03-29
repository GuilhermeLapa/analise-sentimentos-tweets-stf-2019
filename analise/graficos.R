

#install.packages("ggplot2")
library("ggplot2")
library("dplyr")
library("quanteda")
library("tm")
library("dplyr")


DF_analisePorTweet$mes <- as.integer(str_sub(DF_analisePorTweet$data,6,7))


###################################### GRAFICOS ######################################
#####Graficos Tweets ano 2019#####
select(DF_analisePorTweet, classificacao, mes) %>% 
  ggplot() +
  scale_y_continuous(limits = c(0,70000), breaks = seq(0,70000,5000)) + 
  scale_x_continuous(limits = c(0,13), breaks = seq(1:12)) + 
  xlab("M�s") +
  ylab("Quantidade de Tweets") +
  ggtitle("Tweets sobre STF ano 2019") +
  geom_bar(aes(x = mes), color = "black", fill = "light green")

#####Graficos Tweets ano 2019 com rotulos#####
ggplot(select(DF_analisePorTweet, classificacao, mes), aes(x=mes)) +
  scale_x_continuous(limits = c(0,13), breaks = seq(1:12)) +
  scale_y_continuous(limits = c(0,70000), breaks = seq(0,70000,5000)) + 
  geom_bar(color = "black", fill= "light green")+
  geom_text(aes(label=..count..), stat='count', position=position_fill(vjust=0.5), vjust=-0.2) + 
  xlab("M�s") +
  ylab("Quantidade de Tweets") +
  ggtitle("Tweets sobre STF ano 2019")


##### Grafico por polaridade ######################################
pontuacaoPorGrupo

pontuacaoPorGrupo %>%
  ggplot(aes(term, count)) +
  geom_bar(stat = 'identity', fill=c("lightblue", "tomato")) +
  scale_y_continuous(limits = c(0,290000), breaks = seq(0,286000,10000)) + 
  labs(x="Termos", 
       y="Quantidade", 
       title="Frequ�ncia por polaridade")

##### Grafico por polaridade no ano 2019 ######################################
ggplot(DF_analisePorTweet,aes(x=factor(mes),fill=factor(classificacao)))+
  scale_y_continuous(limits = c(0,60000), breaks = seq(0,60000,5000)) + 
  geom_bar(position="dodge")+
  geom_text(aes(label=..count..), stat='count', position=position_dodge(0.9), vjust=-0.2, show.legend = FALSE, size= 3) + 
  xlab("M�s") +
  ylab("Quantidade de Tweets") +
  ggtitle("Quantidade de Tweets Por Polaridade no ano 2019")


#####Graficos Tweets positivos#####
df_dfm_positivas <- filter(DF_analisePorTweet, classificacao=="positivo")
df_dfm_positivas <- select(df_dfm_positivas, classificacao, data)

filter(df_dfm_positivas, data>=as.Date('2019-12-01'), data<=as.Date('2019-12-31')) %>% 
  ggplot() +
  xlab("Data") +
  ylab("Quantidade de Tweets") +
  ggtitle("Tweets Positivos Dezembro 2019") +
  geom_bar(aes(x = data), color = "black", fill = "light blue")


#####Graficos Tweets negativos#####
df_dfm_negativas <- filter(DF_analisePorTweet, classificacao=="negativo")
df_dfm_negativas <- select(df_dfm_negativas, classificacao, data)

filter(df_dfm_negativas, data>=as.Date('2019-12-01'), data<=as.Date('2019-12-31')) %>% 
  ggplot() +
  xlab("Data") +
  ylab("Quantidade de Tweets") +
  ggtitle("Tweets Negativos Dezembro 2019") +
  geom_bar(aes(x = data), color = "black", fill = "tomato")


rm(df_dfm_positivas)
rm(df_dfm_negativas)
#################################################################################




#####################################Graficos com ggplot2
barras <- dataFrameFrequencia$frequencia[2:100]
porcentagem <- round(barras/sum(barras)*100)

##########GRAFICO BARRAS
legenda= dataFrameFrequencia$palavra[1:10]
ggplot(dataFrameFrequencia[1:10,], aes(x = palavra, y = frequencia, fill = legenda)) + 
        geom_bar(stat = "identity") + 
        scale_y_continuous(limits = c(0,25000), breaks = seq(0,25000,2500)) + 
        theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(size = 12, color = "blue", hjust = 0.5)) + 
        xlab("Palavras") + ylab("Frequ�ncia") + 
        ggtitle("10 Palavras Mais Frequentes")

#########GRAFICO BOXPLOT
legenda= dataFrameFrequencia$palavra[1:10]
dataFrameFrequencia[1:10,] %>% 
        ggplot() + 
        geom_boxplot(aes(x = palavra, y = frequencia, fill = legenda)) + 
        scale_y_continuous(limits = c(12000,25000), breaks = seq(12000,25000,1000)) + 
        theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1)) + 
        xlab("Palavras") + ylab("Frequ�ncia") + 
        ggtitle("10 Palavras Mais Frequentes")

#########GRAFICO SCATTERPLOT
filter(dataFrameFrequencia, frequencia>=5000) %>% 
        ggplot(aes(x= palavra, y= frequencia)) +
        geom_point() + 
        scale_y_continuous(limits = c(5000,25000), breaks = seq(5000,25000,1000)) + 
        theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1)) + 
        xlab("Palavras") + ylab("Frequ�ncia") + 
        ggtitle("Palavras com frequ�ncia maior que 5000")

#########################
rm(barras)
rm(porcentagem)
rm(legenda)
