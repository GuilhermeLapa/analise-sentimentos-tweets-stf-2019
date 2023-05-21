![badgeR](https://img.shields.io/badge/R-276DC3?style=flat&logo=r&logoColor=white)
![badgeRstudio](https://img.shields.io/badge/RStudio-75AADB?style=flat&logo=RStudio&logoColor=white)

# Análise de Sentimentos dos Tweets Relacionados ao Superior Tribunal Federal no Ano de 2019

Um projeto desenvolvido em R para o trabalho de conclusão de curso do Bacharelado em Sistemas de Infomação da Universidade Federal Rural de Pernambuco. O trabalho completo pode ser baixado pelo link: [Análise de Sentimentos dos Tweets Relacionados ao Superior Tribunal Federal no Ano de 2019.pdf](https://github.com/GuilhermeLapa/analise-sentimentos-tweets-stf-2019/files/11311640/Analise.de.Sentimentos.dos.Tweets.Relacionados.ao.Superior.Tribunal.Federal.no.Ano.de.2019.pdf).

## Detalhes

Os algoritmos foram desenvolvidos com o objetivo de executar as seguintes etapas do processo de análise de sentimentos do trabalho:

- Coleta de dados.
- Pré-processamento.
- Classificação.
- Análise e geração de gráficos.

## Objetivo

O objetivo deste trabalho é identificar os sentimentos da população brasileira sobre o Superior Tribunal Federal do Brasil através dos conteúdos de tweets entre Janeiro e Dezembro de 2019.

A técnica utilizada para a análise de sentimentos (mineração de emoção) foi a Classificação através de análise léxica.

Os resultados mostram opiniões bastante polarizadas, mas que predominam, de forma geral, opiniões negativas em relação ao STF (estimativa em 51,7%).

## Coleta de Dados

O processo de coleta foi repetido para cada dia de 2019, começando em 01/01/2019 e sendo incrementado em 1 dia até 31/12/2019. Foram utilizados os pacotes [Strinr](https://stringr.tidyverse.org/), [Rvest](https://rvest.tidyverse.org/) do [Tidyverse](https://www.tidyverse.org/) e ainda o pacote [Rtweet](https://docs.ropensci.org/rtweet/articles/rtweet.html) para acessar a API do Twitter. A quantidade de tweets obtidos foi de 255824 registros. A seguir os passos da etapa de coleta.

![Processo da Etapa de Coleta](https://user-images.githubusercontent.com/14200057/234019949-e63fea8b-ce63-491f-acdc-59bfc6876d46.png)

## Pré-processamento

Nessa etapa foram utilizados os pacotes [Dplyr](https://dplyr.tidyverse.org/) e [TM](https://cran.r-project.org/web/packages/tm/) para a manipulação de dados e gerenciar a manipulação de documentos respectivamente. Foram executadas as tarefas a seguir.

- Transformação de todos os caracteres em minúsculos.
- Remoção de pontuação e números.
- Remoção de URLs, pois não possuem valor semântico.
- Remoção de palavras ruído (stopwords).
- Realização do Stemming.

## Classificação

Para realizar a classificação foi criado um léxico baseado nos dados do [Léxico do ReLi](https://www.linguateca.pt/Repositorio/ReLi/) e o léxico para português criado por [(CHEN; SKIENA, 2014)](https://aclanthology.org/P14-2063/) disponibilizado na rede [Kaggle](https://www.kaggle.com/datasets/rtatman/sentiment-lexicons-for-81-languages).

## Análise

Para realizar a análise foram criadas nuvens de palavras utilizando-se o pacote [Wordcloud](https://cran.r-project.org/web/packages/wordcloud/index.html) já os gráficos foram gerados com o auxílio do [ggplot2](https://ggplot2.tidyverse.org/).

## Resultados

Gráfico de Dispersão sem os outliers:

![Gráfico de Dispersão sem os outliers](https://user-images.githubusercontent.com/14200057/234031345-d3fd91a9-ab91-4872-9c53-d7dd9151867d.png)


Nuvem de Palavras sem os outliers:

![Nuvem de Palavras sem os outliers](https://user-images.githubusercontent.com/14200057/234031600-733f4b84-ed71-424d-a0e1-5c1374b3df5e.png)


Gráfico de Barras com as 10 Palavras mais Frequentes sem os outliers:

![Gráfico de Barras com as 10 Palavras mais Frequentes sem os outliers](https://user-images.githubusercontent.com/14200057/234031678-c9c8eb6f-9414-4c9f-9142-b5f9cb6c8341.png)


Quantidade de tweets por mês no ano de 2019:

![Quantidade de tweets por mês no ano de 2019](https://user-images.githubusercontent.com/14200057/234031757-c904b7d4-804a-45bb-ac4c-42c9f18dedd9.png)


Quantidade de tweets positivos e negativos por mês em 2019:

![Quantidade de tweets positivos e negativos por mês em 2019](https://user-images.githubusercontent.com/14200057/234031820-c8edffc0-80b8-4ecf-af99-38bb02e6f221.png)

