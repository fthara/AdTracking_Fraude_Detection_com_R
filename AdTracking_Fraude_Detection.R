# Setando a pasta de trabalho
getwd()
setwd("/Users/fernando/Desktop/AdTracking_Fraude_Detection_com_R")

library(data.table)
# lendo o arquivo de treino
df <- fread("https://media.githubusercontent.com/media/fthara/AdTracking_Fraude_Detection_com_R/master/train_sample.csv")
# Visualização do Data Frame
View(df)
# Resumo do Data Frame
str(df)

# Criando uma função para transformar variáveis em fatores
convert_factor <- function(df, variaveis){
  for(variavel in variaveis){
    df[[variavel]] = as.factor(df[[variavel]])
  }
  return(df)
}

# A variável ip é apenas um nome do, portanto não será utilizada em meu modelo
# preditivo, mas será transformada em fator para manipularmos posteriormente.
# A variável target "is_attributed" também será tranformada para fator.
# Criando o vetor de variaveis
variaveis <- c('ip', 'is_attributed')
# Chamando a função
df <- convert_factor(df, variaveis)

# Verificando se o df tem algum valor na.
sum(is.na(df) == TRUE)

# Vamos separar o "click_time" entre duas variáveis data e hora, mantendo a variável
# e depois alterar o tipo da variável para data e hora.
library(dplyr)
library(tidyr)
df$click_time2 <- df$click_time
df <- df %>%
  separate(click_time2, c("click_date", "click_hour"), " ")

# Convertendo click_date para o tipo Data
df$click_date <- as.Date(df$click_date)

# Convertendo click_hour para o tipo Hora
#install.packages("hms")
library(hms)
df$click_hour <- as_hms(df$click_hour)

# Convertendo click_time para o tipo POSIXct
df$click_time <- as.POSIXct(df$click_time)

# Conferindo novamente os dados.
str(df)

# Agora podemos fazer a exploração dos dados.
# Analizando a variável target
table(df$is_attributed)
prop.table(table(df$is_attributed))

library(ggplot2)

ggplot(df, aes(x=is_attributed, ..count..)) +
  geom_bar() +
  ggtitle("Número de Downloads Feitos ou Não") +
  xlab("Downloads Feitos ou Não") +
  ylab("Quantidade")

# A diferença entre o número de downloads feitos e não feitos é muito grande, vamos 
# utilizar o método ROSE para fazer o balanceamento dos dados.

# Como a variável attributed_time não existe nos outros data sets e também é uma "resposta"
# do algoritmo vou excluí-la desse data set também.
df$attributed_time <- NULL

# Explorando a variável ip
# A variável ip é apenas o nome da maquina, por isso, dificilmente conseguimos extrair 
# algo disso na modelagem, mas podemos ver a quantidade de vezes que esses ips foram coletados.
df$ip <- as.character(df$ip)
# Criando a variável count_ip
df$count_ip <- as.numeric(ave(df$ip, df$ip, FUN = length))
df$ip <- as.factor(df$ip)

# Agora podemos fazer um gráfico para ver como os ips com números grandes de cliques se
# comportam.
df %>%
  filter(count_ip > 100) %>%
  ggplot(aes(ip, ..count..)) +
  geom_bar(aes(fill = is_attributed), position = "dodge") +
  ggtitle("Quantidade de Cliques de Ips Maiores que 100 \n Separados por Download ou Não") +
  xlab("Ips com mais de 100 cliques") +
  ylab("Quantidade") +
  theme(axis.text.x = element_text(angle = 60),
        plot.title = element_text(hjust = 0.5))

# Podemos ver que existem pouquíssimos downloads realizados por quem realiza muitos cliques.

# Filtrando por downloads feitos e fazendo o gráfico para ver o número de cliques de um ip
# que fez download.
df %>%
  filter(is_attributed == 1) %>%
  ggplot(aes(x=count_ip, y=as.numeric(ip))) +
  geom_point() +
  ggtitle("Números de Cliques de um Ip que fez Download") +
  xlab("Ip") +
  ylab("Quantidade") +
  theme(plot.title = element_text(hjust = 0.5))
  
# Vamos filtrar para ver quantos cliques acima de 100 que fizeram o download.
nrow(df %>%
       filter(is_attributed == 1) %>%
       filter(count_ip > 100))

# Temos apenas 7 downloads feito quando o número de cliques é maior que 100. Esses números
# são considerados por mim outliers, portanto irei excluí-los do meu data frame.
df <- df[!(df$is_attributed==1 & df$count_ip>100)]

nrow(df[(df$is_attributed == 1 & df$count_ip == 1)])
# Em números podemos ver que quase mais da metade de downloads feitos foram por quem clicou apenas
# 1 vez.

# Analizando a variável app.
df %>%
  filter(is_attributed == 1) %>%
  ggplot(aes(x=as.factor(app), ..count..)) +
    geom_bar() +
    ggtitle("Quantidade de Downloads Feitos por App") +
    xlab("Tipo de App") +
    ylab("Quantidade") +
    theme(axis.text.x = element_text(angle = 60),
          plot.title = element_text(hjust = 0.5))

# Como podemos ver, a maioria dos downloads foram feitos pelo app do tipo 19 e também tem
# outros apps que se destacam no número de downloads.
df %>%
  filter(app < 40) %>%
  ggplot(aes(as.factor(app), ..count..)) +
    geom_bar() +
    ggtitle("Quantidade de Apps de Id`s menores que 40") +
    xlab("Tipo de App") +
    ylab("Quantidade") +
    theme(axis.text.x = element_text(angle = 60),
          plot.title = element_text(hjust = 0.5))

# Analizando esses dois gráficos podemos ver que os apps do tipo 19 e 35 não são muitos 
# considerados a outros, mas têm um valor bem relevante de downloads.

nrow(df[df$app==19])
nrow(df[df$app==35])
nrow(df[df$app==35 & df$is_attributed ==1])

# Vendo em números podemos ver até que os apps do tipo 35 têm mais downloads do que apenas
# cliques.

# Analizando a variável device
df %>%
  filter(is_attributed == 1) %>%
  ggplot(aes(x=as.factor(device), ..count..)) +
    geom_bar() +
    ggtitle("Quantidade de Downloads Feitos por Device") +
    xlab("Tipo de Device") +
    ylab("Quantidade") +
    theme(axis.text.x = element_text(angle = 60),
          plot.title = element_text(hjust = 0.5))

df %>%
  filter(device < 40) %>%
  ggplot(aes(as.factor(device))) +
    geom_bar(stat="count") +
    ggtitle("Quantidade de Devices de Id`s menores que 40") +
    xlab("Tipo de Device") +
    ylab("Quantidade") +
    theme(axis.text.x = element_text(angle = 60),
          plot.title = element_text(hjust = 0.5))

# Como podemos ver a maioria dos downloads foram feitos pelos devices 0 e 1, porém a quantidade
# de dispositivos com devices do tipo 1 é bem maior do que os do tipo 0

# Número de devices do tipo 1
nrow(df[df$device==1])
# Número de downloads do device do tipo 1
nrow(df[df$device==1 & df$is_attributed ==1])
# Proporção de downloads feitos de device 1
nrow(df[df$device==1 & df$is_attributed ==1]) / 
  (nrow(df[df$device==1]) + nrow(df[df$device==1 & df$is_attributed ==1]))

# Número de devices do tipo 0
nrow(df[df$device==0])
# Número de downloads do device do tipo 0
nrow(df[df$device==0 & df$is_attributed ==1])
# Proporção de downloads feitos de device 0
nrow(df[df$device==0 & df$is_attributed ==1]) /
  (nrow(df[df$device==0]) + nrow(df[df$device==0 & df$is_attributed ==1]))

# Número de devices do tipo 16
nrow(df[df$device==16])
# Número de downloads do device do tipo 16
nrow(df[df$device==16 & df$is_attributed ==1])
# Proporção de downloads feitos de device 16
nrow(df[df$device==16 & df$is_attributed ==1]) /
  (nrow(df[df$device==16]) + nrow(df[df$device==16 & df$is_attributed ==1]))

# Número de devices do tipo 97
nrow(df[df$device==97])
# Número de downloads do device do tipo 97
nrow(df[df$device==97 & df$is_attributed ==1])
# Proporção de downloads feitos de device 97
nrow(df[df$device==97 & df$is_attributed ==1]) /
  (nrow(df[df$device==97]) + nrow(df[df$device==97 & df$is_attributed ==1]))  

# Analizando os devices do tipo 16 e 97 percebemos que eles têm uma proporção
# bem maior de downloads do que os devices 1 e 0.

# Analizando a variável OS
df %>%
  filter(is_attributed == 1) %>%
  ggplot(aes(x=as.factor(os), ..count..)) +
    geom_bar() +
    ggtitle("Quantidade de Downloads Feitos por Os") +
    xlab("Tipo de Os") +
    ylab("Quantidade") +
    theme(axis.text.x = element_text(angle = 60),
          plot.title = element_text(hjust = 0.5))

df %>%
  filter(os < 40) %>%
  ggplot(aes(as.factor(os), ..count..)) +
    geom_bar() +
    ggtitle("Quantidade de Os`s de Id`s menores que 40") +
    xlab("Tipo de Os") +
    ylab("Quantidade") +
    theme(axis.text.x = element_text(angle = 60),
          plot.title = element_text(hjust = 0.5))

# Podemos ver que temos muitos os`s do tipo 19, 13, 0 e 24 que fizeram download do app,
# porém os do tipo 0 e 24 têm uma quantidade bem menor do que os do tipo 19 e 13.

# Analizando a variável channel
df %>%
  filter(is_attributed == 1) %>%
  ggplot(aes(x=as.factor(channel), ..count..)) +
    geom_bar() +
    ggtitle("Quantidade de Downloads Feitos por Channel") +
    xlab("Tipo de Channel") +
    ylab("Quantidade") +
    theme(axis.text.x = element_text(angle = 60),
          plot.title = element_text(hjust = 0.5))

df %>%
  filter(channel <= 100) %>%
  ggplot(aes(as.factor(channel), ..count..)) +
    geom_bar() +
    ggtitle("Quantidade de Channels de Id`s menores que 100") +
    xlab("Tipo de Channel") +
    ylab("Quantidade") +
    theme(axis.text.x = element_text(angle = 60),
          plot.title = element_text(hjust = 0.5))

df %>%
  filter(channel > 100 & channel <= 150) %>%
  ggplot(aes(as.factor(channel), ..count..)) +
    geom_bar() +
    ggtitle("Quantidade de Channels de Id`s menores \nque 100 e maiores que 150") +
    xlab("Tipo de Channel") +
    ylab("Quantidade") +
    theme(axis.text.x = element_text(angle = 60),
          plot.title = element_text(hjust = 0.5))
  

df %>%
  filter(channel > 150 & channel <= 250) %>%
  ggplot(aes(as.factor(channel), ..count..)) +
    geom_bar() +
    ggtitle("Quantidade de Channels de Id`s menores \nque 150 e maiores que 250") +
    xlab("Tipo de Channel") +
    ylab("Quantidade") +
    theme(axis.text.x = element_text(angle = 60),
          plot.title = element_text(hjust = 0.5))

df %>%
  filter(channel > 250 & channel <= 350) %>%
  ggplot(aes(as.factor(channel), ..count..)) +
    geom_bar() +
    ggtitle("Quantidade de Channels de Id`s menores \nque 250 e maiores que 350") +
    xlab("Tipo de Channel") +
    ylab("Quantidade") +
    theme(axis.text.x = element_text(angle = 60),
          plot.title = element_text(hjust = 0.5))

df %>%
  filter(channel > 350 & channel <= 450) %>%
  ggplot(aes(as.factor(channel), ..count..)) +
    geom_bar() +
    ggtitle("Quantidade de Channels de Id`s menores \nque 350 e maiores que 450") +
    xlab("Tipo de Channel") +
    ylab("Quantidade") +
    theme(axis.text.x = element_text(angle = 60),
          plot.title = element_text(hjust = 0.5))

df %>%
  filter(channel > 450 & channel <= 550) %>%
  ggplot(aes(as.factor(channel), ..count..)) +
  geom_bar() +
  ggtitle("Quantidade de Channels de Id`s menores \nque 450 e maiores que 550") +
  xlab("Tipo de Channel") +
  ylab("Quantidade") +
  theme(axis.text.x = element_text(angle = 60),
        plot.title = element_text(hjust = 0.5))

# Podemos ver que alguns channels são bem acessados porém exitem poucos downloads por eles,
# por outro lado existem alguns como o do tipo 274 que não é muito acessado mas existem 
# muitos downloads feitos por ele.

# Analizando a variável click date
table(df$click_date)
df %>%
  filter(is_attributed == 1) %>%
  count(click_date) %>%
  ggplot(aes(x = click_date, n)) + 
  geom_point() +
  geom_line()

# A data não me parece influenciar muito no download do app, mas estou curioso com relação ao
# horário, qual é a faixa de horário onde mais temos downloads feitos?
# Função para converter horas em segundos
convert_hour <- function(hour){
  h <- 60*60*hour
  return(h)
}
# Criando a variável range_hour que separa o dia em 6 partes (4 em 4 horas).
df$range_hour <-findInterval(df$click_hour,c(convert_hour(0), convert_hour(4),
                                             convert_hour(8), convert_hour(12),
                                             convert_hour(16), convert_hour(20),
                                             convert_hour(24)))
df$range_hour = as.factor(df$range_hour)
levels(df$range_hour)<-c("0 as 4", "4 as 8", "8 as 12", "12 as 16", "16 as 20", "16 as 24")

df %>%
  filter(is_attributed == 1) %>%
  ggplot(aes(x = range_hour, ..count..)) + 
  geom_bar() +
  ggtitle("Quantidade de Downloads por Faixa de Horário") +
  xlab("Faixas dos Horários") +
  ylab("Quantidade") +
  theme(plot.title = element_text(hjust = 0.5))

# Podemos ver que a maior parte dos downloads são feitos de madrugada, entre meia noite e 
# 04 da manhã e ao longo dos outros períodos esse valor vai diminuindo.

# Para o nosso primeiro modelo preditivo vou considerar as colunas app, device, os, channel
# count_ip e range_hour. Assim temos um baseline para melhorar nas próximas previsões.
# Fazendo o split do data set para treinar o modelo.
require(caret)
set.seed(123)
trainIndex <- createDataPartition(df$is_attributed, p = 0.7,list = FALSE,  times = 1)
train <- df[trainIndex,]
test <- df[-trainIndex,]

# Criando dos modelos preditivos
formula_v1 <- as.formula('is_attributed ~ app + device + os + channel + count_ip + range_hour')
# Treinando o modelo com o algoritmo de regressão logística
model_glm_v1 <- glm(formula = formula_v1, data = train, family = "binomial")
# Verificando alguns resultados do modelo treinado
summary(model_glm_v1)
# Realizando a predição com o modelo treinado
pred_glm_v1 <- predict(model_glm_v1, test, type="response")
# Arredondando para 0 ou 1
pred_glm_v1 <- round(pred_glm_v1)
#Confusion Matrix da predição.
confusionMatrix(table(data = pred_glm_v1, reference = test$is_attributed),
                positive = '1')
# Curva roc para o model_glm_v1
#install.packages("plotROC")
library(ROCR)
library(pROC)
library(ROSE)
auc(test$is_attributed,  pred_glm_v1)
roc.curve(test$is_attributed, pred_glm_v1, plotit = T, col = "red")
# Nosso modelo teve uma ótima acurácia de 99%, mas a pela métrica AUC o desempenho
# não foi tão bom assim. Vamo tester outros algoritmos e depois fazer o balanceamento
# da variável is_target.

# Criando o modelo com o algoritmo Árvore de Decissão
library(C50)
# Treinando o modelo
modelo_tree_v1 = C5.0(formula_v1, data = train) 
# Previsões nos dados de teste
pred_tree_v1 = predict(modelo_tree_v1, test, type='class')
# Confusion Matrix
confusionMatrix(test$is_attributed, pred_tree_v1, positive = '1')
pred_tree_v1 <- as.numeric(pred_tree_v1)
auc(test$is_attributed, as.numeric(pred_tree_v1))
roc.curve(test$is_attributed, pred_tree_v1, plotit = T, col = "blue", add.roc = T)

# Tanto nesse modelo como no anterior podemos ver que o alogritmo aprendeu muito
# sobre o evento de não se fazer download, porém também erra muito ao tentar descobrir
# se a pessoa fez o downaload. O fato da acuracia ser alta não significa que
# o modelo esteja prevendo direito, apenas que ele acerta muito porque mais de 99%
# dos casos são de downloads não realizados. Por isso o balanceamento é importante.

# Antes vou submeter o modelo de regressão linear no kaggle para ter uma base para melhoria
# do algoritmo.
# Como o arquivo de teste é muito grande, estava ocupando muita memória em meu computador,
# então decidi particionar o teste em 4 partes que foram gravados em arquivos csv, e agora
# vou ler eles em separado.

# função para transformar os dados do arquivo teste
transform_test <- function(test){
  test$click_time2 <- test$click_time
  test <- test %>%
    separate(click_time2, c("click_date", "click_hour"), " ")
  test$click_hour <- as_hms(test$click_hour)
  test$ip <- as.character(test$ip)
  test$count_ip <- as.numeric(ave(test$ip, test$ip, FUN = length))
  test$range_hour <-findInterval(test$click_hour, c(convert_hour(0), convert_hour(4),
                                                    convert_hour(8), convert_hour(12),
                                                    convert_hour(16), convert_hour(20),
                                                    convert_hour(24)))
  test$range_hour = as.factor(test$range_hour)
  levels(test$range_hour)<-c("0 as 4", "4 as 8", "8 as 12", "12 as 16", "16 as 20", "16 as 24")
  return(test)
}

# Lendo o arquivo test_p1 e fazendo a previsão com o agloritmo glm
test_kaggle_p1 <- fread("https://media.githubusercontent.com/media/fthara/AdTracking_Fraude_Detection_com_R/master/test_p1.csv")
# transformando o arquivo para a predição
test_kaggle_p1 <- transform_test(test_kaggle_p1)
# realizando a predição
pred_glm_v1_p1 <- predict(model_glm_v1, test_kaggle_p1, type="response")
# arredondando os valores preditos
pred_glm_v1_p1 <- round(pred_glm_v1_p1)
# criando um data frame com resposta e predição
prediction1_p1 <- data.frame('click_id' = test_kaggle_p1$click_id,
                             'is_attributed' = pred_glm_v1_p1)
# Visualizando o data frame
head(prediction1_p1)
# Excluindo o arquivo de teste
rm(test_kaggle_p1)

# Lendo o arquivo test_p2 e fazendo a previsão com o agloritmo glm
test_kaggle_p2 <- fread("https://media.githubusercontent.com/media/fthara/AdTracking_Fraude_Detection_com_R/master/test_p2.csv")
# transformando o arquivo para a predição
test_kaggle_p2 <- transform_test(test_kaggle_p2)
# realizando a predição
head(test_kaggle_p2)
# arredondando os valores preditos
pred_glm_v1_p2 <- predict(model_glm_v1, test_kaggle_p2, type="response")
# criando um data frame com resposta e predição
pred_glm_v1_p2 <- round(pred_glm_v1_p2)
# criando um data frame com resposta e predição
prediction1_p2 <- data.frame('click_id' = test_kaggle_p2$click_id,
                             'is_attributed' = pred_glm_v1_p2)
# Visualizando o data frame
head(prediction1_p2)
# Excluindo o arquivo de teste
rm(test_kaggle_p2)

# Lendo o arquivo test_p3 e fazendo a previsão com o agloritmo glm
test_kaggle_p3 <- fread("https://media.githubusercontent.com/media/fthara/AdTracking_Fraude_Detection_com_R/master/test_p3.csv")
# transformando o arquivo para a predição
test_kaggle_p3 <- transform_test(test_kaggle_p3)
# realizando a predição
head(test_kaggle_p3)
# arredondando os valores preditos
pred_glm_v1_p3 <- predict(model_glm_v1, test_kaggle_p3, type="response")
# criando um data frame com resposta e predição
pred_glm_v1_p3 <- round(pred_glm_v1_p3)
# criando um data frame com resposta e predição
prediction1_p3 <- data.frame('click_id' = test_kaggle_p3$click_id,
                             'is_attributed' = pred_glm_v1_p3)
# Visualizando o data frame
head(prediction1_p3)
# Excluindo o arquivo de teste
rm(test_kaggle_p3)

# Lendo o arquivo test_p4 e fazendo a previsão com o agloritmo glm
test_kaggle_p4 <- fread("https://media.githubusercontent.com/media/fthara/AdTracking_Fraude_Detection_com_R/master/test_p4.csv")
# transformando o arquivo para a predição
test_kaggle_p4 <- transform_test(test_kaggle_p4)
# realizando a predição
head(test_kaggle_p4)
# arredondando os valores preditos
pred_glm_v1_p4 <- predict(model_glm_v1, test_kaggle_p4, type="response")
# criando um data frame com resposta e predição
pred_glm_v1_p4 <- round(pred_glm_v1_p4)
# criando um data frame com resposta e predição
prediction1_p4 <- data.frame('click_id' = test_kaggle_p4$click_id,
                            'is_attributed' = pred_glm_v1_p4)
# Visualizando o data frame
head(prediction1_p4)
# Excluindo o arquivo de teste
rm(test_kaggle_p4)

# Juntando as 4 previsões
prediction1 <- rbind(prediction1_p1, prediction1_p2,
                       prediction1_p3, prediction1_p4)

# Escrevendo o arquivo em formato csv.
#fwrite(prediction1, "prediction1.csv", row.names = F, sep = ",")

# Removendo alguns dados para limpar a memória do R
rm(prediction1_p1, prediction1_p2, prediction1_p3, prediction1_p4)
rm(pred_glm_v1_p1, pred_glm_v1_p2, pred_glm_v1_p3, pred_glm_v1_p4)
rm(pred_glm_v1, pred_tree_v1)
rm(prediction1)
rm(model_glm_v1, modelo_tree_v1)

# Essa predição deu como resultado 0.5 na curva AUC, o que é um resultado muito
# ruim. Vou fazer o balanceamento para ver como fica o resultado.

# Segunda tentativa de previsão do desafio.
# Balanceando os dados
# Feature selection nos dados de treino e teste
train <- train %>% select(app, device, os, channel, count_ip, 
                          range_hour, is_attributed)
test <- test %>% select(app, device, os, channel, count_ip, 
                        range_hour, is_attributed)
# ROSE nos dados de treino
rose_train <- ROSE(is_attributed ~ ., data = train, seed = 1)$data
prop.table(table(rose_train$is_attributed))

# ROSE nos dados de teste
rose_test <- ROSE(is_attributed ~ ., data = test, seed = 1)$data
prop.table(table(rose_test$is_attributed))

# Agora vamos criar o modelo preditivo.

# Treinando o modelo com o algoritmo de regressão logística
model_glm_v2 <- glm(is_attributed ~ ., data = rose_train, family = "binomial")
# Verificando alguns resultados do modelo treinado
summary(model_glm_v2)
# Realizando a predição com o modelo treinado
pred_glm_v2 <- predict(model_glm_v2, rose_test, type="response")
# Arredondando os valores
pred_glm_v2 <- round(pred_glm_v2)
#Confusion Matrix da predição.
confusionMatrix(table(data = pred_glm_v2, reference = rose_test$is_attributed),
                positive = '1')
# Curva roc para o model_glm_v2
roc.curve(rose_test$is_attributed, pred_glm_v2, plotit = T, 
          col = "darkred", add.roc = T)
# A curva AUC teve uma boa melhora em relação ao algoritmo anterior, mas mesmo
# assim, continua fraca. Vamos tentar com outros algoritmos e depois fazer mais 
# alterações no data set original.

# Criando o modelo com o algoritmo Árvore de Decissão
# Treinando o modelo
modelo_tree_v2 = C5.0(is_attributed ~ ., data = rose_train) 
# Previsões nos dados de teste
pred_tree_v2 = predict(modelo_tree_v2, rose_test, type='class')
# Confusion Matrix
confusionMatrix(rose_test$is_attributed, pred_tree_v2, positive = '1')
pred_tree_v2 <- as.numeric(pred_tree_v2)
# Para o modelo de árvore de decisão a acurácia não caiu tanto, mas agora vamos
# ver como a curva AUC se comporta.
# Curva roc para o modelo_tree_v2
roc.curve(rose_test$is_attributed, pred_tree_v2, plotit = T,
          col = "blue", add.roc = T)
# Nossa curva AUC deu como resultado em 0.896, o que é excelente. Agora vamos
# tentar com outros algoritmos para ver se conseguimos bater esse número.

# Criando o modelo com o algoritmo SVM (Suport Vector Machine)
library(e1071)
# treinando o modelo
modelo_svm_v1 <- svm(is_attributed ~ ., data = rose_train, 
                     type = 'C-classification', kernel = 'radial') 
# Previsões nos dados de teste
pred_svm_v1 = predict(modelo_svm_v1, rose_test)
# Confusion Matrix
confusionMatrix(rose_test$is_attributed, pred_svm_v1, positive = '1')
# Curva roc para o modelo_svm_v1
roc.curve(rose_test$is_attributed, pred_svm_v1, plotit = T,
          col = "purple", add.roc = T)
# O desempenho tanto para árvore de decisão como para o SVM foi o mesmo, porém a árvore de
# decisão teve uma acurácia melhor e é um modelo mais rápido para treinar.

# Criando o modelo com o algoritmo Random Forest
library(rpart)
# treinando o modelo
modelo_rf_v1 = rpart(is_attributed ~ ., data = rose_train, 
                     control = rpart.control(cp = .0005)) 
# Previsões nos dados de teste
pred_rf_v1 = predict(modelo_rf_v1, rose_test, type='class')
# Confusion Matrix
confusionMatrix(rose_test$is_attributed, pred_rf_v1, positive = '1')
# Curva roc para o modelo_rf_v1
roc.curve(rose_test$is_attributed, pred_rf_v1, plotit = T,
          col = "yellow", add.roc = T)

# Criando o modelo com outro algoritmo Random Forest
library(randomForest)
# Treinando o modelo
modelo_rf_v2 = randomForest(formula = is_attributed ~ ., data = rose_train,
                            importance = TRUE)
# Previsões nos dados de teste
pred_rf_v2 = predict(modelo_rf_v2, rose_test, type='class')
# Confusion Matrix
confusionMatrix(rose_test$is_attributed, pred_rf_v2, positive = '1')
# Curva roc para o modelo_rf_v2
roc.curve(rose_test$is_attributed, pred_rf_v2, plotit = T,
          col = "grey", add.roc = T)

# Criando o modelo com o algoritmo Naive Bayes
modelo_nb_v1 = naiveBayes(is_attributed ~ ., data=rose_train)
# Previsões nos dados de teste
pred_nb_v1 <- predict(modelo_nb_v1, newdata=rose_test)
# Confusion Matrix
confusionMatrix(rose_test$is_attributed, pred_nb_v1, positive = '1')
# Curva roc para o modelo_nb_v1
roc.curve(rose_test$is_attributed, pred_nb_v1, plotit = T,
          col = "brown", add.roc = T)

# O melhor desempenho que tivemos foi do modelo_rf_v2, com random forest, vamos agora
# simular esse modelo nos dados de teste do kaggle.

# Lendo o arquivo test_p1 e fazendo a previsão com o agloritmo random forest
test_kaggle_p1 <- fread("https://media.githubusercontent.com/media/fthara/AdTracking_Fraude_Detection_com_R/master/test_p1.csv")
# transformando o arquivo para a predição
test_kaggle_p1 <- transform_test(test_kaggle_p1)
# realizando a predição
pred_rf_v2_p1 <- predict(modelo_rf_v2, test_kaggle_p1, type="response")
# criando um data frame com resposta e predição
prediction2_p1 <- data.frame('click_id' = test_kaggle_p1$click_id,
                             'is_attributed' = pred_rf_v2_p1)
# Visualizando o data frame
head(prediction2_p1)
# Excluindo o arquivo de teste
rm(test_kaggle_p1)

# Lendo o arquivo test_p2 e fazendo a previsão com o agloritmo random forest
test_kaggle_p2 <- fread("https://media.githubusercontent.com/media/fthara/AdTracking_Fraude_Detection_com_R/master/test_p2.csv")
# transformando o arquivo para a predição
test_kaggle_p2 <- transform_test(test_kaggle_p2)
# realizando a predição
pred_rf_v2_p2 <- predict(modelo_rf_v2, test_kaggle_p2, type="response")
# criando um data frame com resposta e predição
prediction2_p2 <- data.frame('click_id' = test_kaggle_p2$click_id,
                             'is_attributed' = pred_rf_v2_p2)
# Visualizando o data frame
head(prediction2_p2)
# Excluindo o arquivo de teste
rm(test_kaggle_p2)

# Lendo o arquivo test_p3 e fazendo a previsão com o agloritmo random forest
test_kaggle_p3 <- fread("https://media.githubusercontent.com/media/fthara/AdTracking_Fraude_Detection_com_R/master/test_p3.csv")
# transformando o arquivo para a predição
test_kaggle_p3 <- transform_test(test_kaggle_p3)
# realizando a predição
pred_rf_v2_p3 <- predict(modelo_rf_v2, test_kaggle_p3, type="response")
# criando um data frame com resposta e predição
prediction2_p3 <- data.frame('click_id' = test_kaggle_p3$click_id,
                             'is_attributed' = pred_rf_v2_p3)
# Visualizando o data frame
head(prediction2_p3)
# Excluindo o arquivo de teste
rm(test_kaggle_p3)

# Lendo o arquivo test_p4 e fazendo a previsão com o agloritmo random forest
test_kaggle_p4 <- fread("https://media.githubusercontent.com/media/fthara/AdTracking_Fraude_Detection_com_R/master/test_p4.csv")
# transformando o arquivo para a predição
test_kaggle_p4 <- transform_test(test_kaggle_p4)
# realizando a predição
pred_rf_v2_p4 <- predict(modelo_rf_v2, test_kaggle_p4, type="response")
# criando um data frame com resposta e predição
prediction2_p4 <- data.frame('click_id' = test_kaggle_p4$click_id,
                             'is_attributed' = pred_rf_v2_p4)
# Visualizando o data frame
head(prediction2_p4)
# Excluindo o arquivo de teste
rm(test_kaggle_p4)

# Juntando as 4 predições
prediction2 <- rbind(prediction2_p1, prediction2_p2,
                     prediction2_p3, prediction2_p4)

# Escrevendo o data set em um arquivo csv.
#fwrite(prediction2, "prediction2.csv", row.names = F, sep = ",")

# Excluindo alguns arquivos para limpara a memória do R.
rm(prediction2_p1, prediction2_p2, prediction2_p3, prediction2_p4, prediction2)
rm(pred_rf_v2_p1, pred_rf_v2_p2, pred_rf_v2_p3, pred_rf_v2_p4)
rm(model_glm_v2, modelo_tree_v2, modelo_svm_v1, modelo_rf_v1, modelo_rf_v2, modelo_nb_v1)
rm(pred_glm_v2, pred_tree_v2, pred_svm_v1, pred_rf_v1, pred_rf_v2, pred_nb_v1)
rm(train, teste, rose_train, rose_test)

# Para essa predição o score alcançado foi de 0,689, mas ainda não estou contente com o
# resultado, vamos tentar fazer mais algumas alterações no data set para ver se o algorítimo
# melhora

# Terceira tentativa de submissão no kaggle
# Nesta terceira tentativa vamos ralizar a predição com os valores de count_app, count_device,
# count_os, count_channel, count_ip e range_hour. Pois como as variáveis app, device, os e channel
# são apenas ids (um código de identificação deles) isso não quer dizer muita coisa. Então pensei
# em ver como a quantidade de valor dessas variáveis influencia no modelo. Eu poderia
# transformá-los em fatores, mas descobri que no arquivo de teste existem mais "tipo" dessas
# variáveis, então fica um pouco ruim para transformá-las.

# Função para criar as novas variáveis.
num_vars <- function(df, col){
  df[[col]] <- as.character(df[[col]])
  count_col <- as.numeric(ave(df[[col]], df[[col]], FUN = length))
  return(count_col)
}

# Criando as novas variáveis
df$count_app <- num_vars(df, 'app')
df$count_device <- num_vars(df, 'device')
df$count_os <- num_vars(df, 'os')
df$count_channel <- num_vars(df, 'channel')

# Criando as variáveis train e test
train <- df[trainIndex,]
test <- df[-trainIndex,]

# Balanceando os dados
# Feature selection nos dados de treino e teste
train <- train %>% select(count_app, count_device, count_os, count_channel,
                          count_ip, range_hour, is_attributed)
test <- test %>% select(count_app, count_device, count_os, count_channel,
                        count_ip, range_hour, is_attributed)
# ROSE nos dados de treino
rose_train <- ROSE(is_attributed ~ ., data = train, seed = 1)$data
prop.table(table(rose_train$is_attributed))
# ROSE nos dados de teste
rose_test <- ROSE(is_attributed ~ ., data = test, seed = 1)$data
prop.table(table(rose_test$is_attributed))


# Treinando o modelo com o algoritmo de regressão logística
model_glm_v3 <- glm(is_attributed ~ ., data = rose_train, family = "binomial")
# Verificando alguns resultados do modelo treinado
summary(model_glm_v3)
# Realizando a predição com o modelo treinado
pred_glm_v3 <- predict(model_glm_v3, rose_test, type="response")
# Arredondando para 0 ou 1
pred_glm_v3 <- round(pred_glm_v3)
#Confusion Matrix da predição.
confusionMatrix(table(data = pred_glm_v3, reference = rose_test$is_attributed),
                positive = '1')
# Curva roc para o model_glm_v3
roc.curve(rose_test$is_attributed, pred_glm_v3, plotit = T, 
          col = "darkred")

# Criando o modelo com o algoritmo Árvore de Decissão
modelo_tree_v3 = C5.0(is_attributed ~ ., data = rose_train) 
# Previsões nos dados de teste
pred_tree_v3 = predict(modelo_tree_v3, rose_test, type='class')
# Confusion Matrix
confusionMatrix(rose_test$is_attributed, pred_tree_v3, positive = '1')
# Previsão nos dados de teste
pred_tree_v3 <- as.numeric(pred_tree_v3)
# Curva roc para o modelo_tree_v3
roc.curve(rose_test$is_attributed, pred_tree_v3, plotit = T,
          col = "blue", add.roc = T)

# Criando o modelo com o algoritmo SVM (Suport Vector Machine)
modelo_svm_v2 <- svm(is_attributed ~ ., data = rose_train, 
                     type = 'C-classification', kernel = 'radial') 
# Previsão nos dados de teste
pred_svm_v2 = predict(modelo_svm_v2, rose_test)
# Confusion Matrix
confusionMatrix(rose_test$is_attributed, pred_svm_v2, positive = '1')
# Curva roc para o modelo_svm_v2
roc.curve(rose_test$is_attributed, pred_svm_v2, plotit = T,
          col = "purple", add.roc = T)

# Criando o modelo com o algoritmo Random Forest
modelo_rf_v3 = rpart(is_attributed ~ ., data = rose_train, 
                     control = rpart.control(cp = .0005)) 
# Previsões nos dados de teste
pred_rf_v3 = predict(modelo_rf_v3, rose_test, type='class')
# Confusion Matrix
confusionMatrix(rose_test$is_attributed, pred_rf_v3, positive = '1')
# Curva roc para o modelo_rf_v3
roc.curve(rose_test$is_attributed, pred_rf_v3, plotit = T,
          col = "yellow", add.roc = T)

# Criando o modelo com outro algoritmo Random Forest
modelo_rf_v4 = randomForest(formula = is_attributed ~ ., data = rose_train,
                            importance = TRUE)
# Previsões nos dados de teste
pred_rf_v4 = predict(modelo_rf_v4, rose_test, type='class')
# Confusion Matrix
confusionMatrix(rose_test$is_attributed, pred_rf_v4, positive = '1')
# Curva roc para o modelo_rf_v4
roc.curve(rose_test$is_attributed, pred_rf_v4, plotit = T,
          col = "grey", add.roc = T)

# Esses valores estão ótimos, mas vamos normalizar os dados para ver se melhora a
# previsão.

# Copiando o data frame original
norm.df <- df

# Função para normalizar os dados
normdf <- function(df, col){
  df[[col]] <- scale(df[[col]])
  return(df)
}

# Normalizando os dados numéricos
norm.df <- normdf(norm.df, 'count_app')
norm.df <- normdf(norm.df, 'count_device')
norm.df <- normdf(norm.df, 'count_os')
norm.df <- normdf(norm.df, 'count_channel')
norm.df <- normdf(norm.df, 'count_ip')

# Criando as variáveis train e test
norm.train <- norm.df[trainIndex,]
norm.test <- norm.df[-trainIndex,]

# Feature selection nos dados de treino e teste
norm.train <- norm.train %>% select(count_app, count_device, count_os, 
                                    count_channel, count_ip, range_hour,
                                    is_attributed)
norm.test <- norm.test %>% select(count_app, count_device, count_os,
                                  count_channel, count_ip, range_hour,
                                  is_attributed)

# ROSE nos dados de treino
norm.rose_train <- ROSE(is_attributed ~ ., data = norm.train, seed = 1)$data
prop.table(table(norm.rose_train$is_attributed))
# ROSE nos dados de teste
norm.rose_test <- ROSE(is_attributed ~ ., data = norm.test, seed = 1)$data
prop.table(table(norm.rose_test$is_attributed))

# Treinando o modelo com o algoritmo de regressão logística
model_glm_v4 <- glm(is_attributed ~ ., data = norm.rose_train, family = "binomial")
# Verificando alguns resultados do modelo treinado
summary(model_glm_v4)
# Realizando a predição com o modelo treinado
pred_glm_v4 <- predict(model_glm_v4, norm.rose_test, type="response")
# Arredondando para 0 ou 1
pred_glm_v4 <- round(pred_glm_v4)
#Confusion Matrix da predição.
confusionMatrix(table(data = pred_glm_v4, reference = norm.rose_test$is_attributed),
                positive = '1')
# Curva roc para o model_glm_v4
roc.curve(norm.rose_test$is_attributed, pred_glm_v4, plotit = T, 
          col = "darkred")

# Criando o modelo com o algoritmo Árvore de Decissão
modelo_tree_v4 = C5.0(is_attributed ~ ., data = norm.rose_train) 
# Previsão nos dados de teste
pred_tree_v4 = predict(modelo_tree_v4, norm.rose_test, type='class')
# Confusion Matrix
confusionMatrix(norm.rose_test$is_attributed, pred_tree_v4, positive = '1')
# Convertendo para numérico
pred_tree_v4 <- as.numeric(pred_tree_v4)
# Curva roc para o modelo_tree_v4
roc.curve(norm.rose_test$is_attributed, pred_tree_v4, plotit = T,
          col = "blue", add.roc = T)

# Criando o modelo com o algoritmo SVM (Suport Vector Machine)
modelo_svm_v3 <- svm(is_attributed ~ ., data = norm.rose_train, 
                     type = 'C-classification', kernel = 'radial') 
# Previsão nos dados de teste
pred_svm_v3 = predict(modelo_svm_v3, norm.rose_test)
# Confusion Matrix
confusionMatrix(norm.rose_test$is_attributed, pred_svm_v3, positive = '1')
# Curva roc para o modelo_svm_v3
roc.curve(norm.rose_test$is_attributed, pred_svm_v3, plotit = T,
          col = "purple", add.roc = T)

# Criando o modelo com o algoritmo Random Forest
modelo_rf_v5 = rpart(is_attributed ~ ., data = norm.rose_train, 
                     control = rpart.control(cp = .0005)) 
# Previsão nos dados de teste
pred_rf_v5 = predict(modelo_rf_v5, norm.rose_test, type='class')
# Confusion Matrix
confusionMatrix(norm.rose_test$is_attributed, pred_rf_v5, positive = '1')
# Curva roc para o modelo_rf_v5
roc.curve(norm.rose_test$is_attributed, pred_rf_v5, plotit = T,
          col = "yellow", add.roc = T)

# Criando o modelo com outro algoritmo Random Forest
modelo_rf_v6 = randomForest(formula = is_attributed ~ ., data = norm.rose_train,
                            importance = TRUE)
# Previsão nos dados de teste
pred_rf_v6 = predict(modelo_rf_v6, norm.rose_test, type='class')
# Confusion Matrix
confusionMatrix(norm.rose_test$is_attributed, pred_rf_v6, positive = '1')
# Curva roc para o modelo_v1
roc.curve(norm.rose_test$is_attributed, pred_rf_v6, plotit = T,
          col = "darkgreen", add.roc = T)


# Como os resultados foram piores do que sem a normalização, vou utilizar o modelo_rf_v4
# (sem a normalização) na terceira previsão.
# Limpando alguns dados que não serão mais usados
rm(model_glm_v3, modelo_tree_v3, modelo_svm_v2, modelo_rf_v3, model_glm_v4,
   modelo_tree_v4, modelo_svm_v3, modelo_rf_v5, modelo_rf_v6)
rm(pred_glm_v3, pred_tree_v3, pred_svm_v2, pred_rf_v3, pred_glm_v4, pred_tree_v4,
   pred_svm_v3, pred_rf_v5, pred_rf_v6)
rm(norm.df, norm.train, norm.test, norm.rose_train, norm.rose_test)

# Antes vamos criar uma função que conta a quantidade de valores em cada variável que modificamos
# com count no data set de treino para criar com os mesmos valores no data set de teste. Para os valores que 
# não existem no data set de treino, os valores de count vão ficar iguais a 1, pois são os valores que predominam
# nessas variáveis counts.
# Função para que retorna um data set valores e quantidade de cada valor
count_vars <- function(df, var){
  filtra_var <- df %>%
    group_by_(var) %>%
    summarise(count = n())
  return(filtra_var)
}

# Criando o novo data set.
dfapp <- count_vars(df, 'app')
dfdevice <- count_vars(df, 'device')
dfos <- count_vars(df, 'os')
dfchannel <- count_vars(df, 'channel')

# Função para calcular a moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Verificando quais quantidades mais aparecem em cada variável.
getmode(dfapp$count)
getmode(dfdevice$count)
getmode(dfos$count)
getmode(dfchannel$count)
#Como podemos comprovar o valor 1 é o valor mais frequente nas variáveis counts

# Lendo o arquivo parte 1
test_kaggle_p1 <- fread("https://media.githubusercontent.com/media/fthara/AdTracking_Fraude_Detection_com_R/master/test_p1.csv")
test_kaggle_p1 <- transform_test(test_kaggle_p1)
# Criando a coluna count_app
test_kaggle_p1$count_app <- dfapp$count[match(test_kaggle_p1$app, dfapp$app)]
# Renumerando os valores NA`s para 1
test_kaggle_p1[(is.na(test_kaggle_p1$count_app) == TRUE), 'count_app'] = 1
# Criando a coluna count_device
test_kaggle_p1$count_device <- dfdevice$count[match(test_kaggle_p1$device, dfdevice$device)]
# Renumerando os valores NA`s para 1
test_kaggle_p1[(is.na(test_kaggle_p1$count_device) == TRUE), 'count_device'] = 1
# Criando a coluna count_os
test_kaggle_p1$count_os <- dfos$count[match(test_kaggle_p1$os,dfos$os)]
# Renumerando os valores NA`s para 1
test_kaggle_p1[(is.na(test_kaggle_p1$count_os) == TRUE), 'count_os'] = 1
# Criando a coluna count_app
test_kaggle_p1$count_channel <- dfchannel$count[match(test_kaggle_p1$channel,dfchannel$channel)]
# Renumerando os valores NA`s para 1
test_kaggle_p1[(is.na(test_kaggle_p1$count_channel) == TRUE), 'count_channel'] = 1
# Criando um data set com o real e o previsto
test_kaggle_p1 <- test_kaggle_p1[, c('click_id', 'count_app', 'count_device', 'count_os',
                                     'count_channel', 'count_ip', 'range_hour')]
# Realizando a previsão
pred_rf_v3_p1 <- predict(modelo_rf_v4, test_kaggle_p1, type="response")
# Criando um data set com o real e o previsto
prediction3_p1 <- data.frame('click_id' = test_kaggle_p1$click_id,
                             'is_attributed' = pred_rf_v3_p1)
# Visualizando os primeiros dados
head(prediction3_p1)
# Excluindo o data set de teste
rm(test_kaggle_p1)


# Lendo o arquivo parte 2
test_kaggle_p2 <- fread("https://media.githubusercontent.com/media/fthara/AdTracking_Fraude_Detection_com_R/master/test_p2.csv")
test_kaggle_p2 <- transform_test(test_kaggle_p2)
# Criando a coluna count_app
test_kaggle_p2$count_app <- dfapp$count[match(test_kaggle_p2$app, dfapp$app)]
# Renumerando os valores NA`s para 1
test_kaggle_p2[(is.na(test_kaggle_p2$count_app) == TRUE), 'count_app'] = 1
# Criando a coluna count_device
test_kaggle_p2$count_device <- dfdevice$count[match(test_kaggle_p2$device, dfdevice$device)]
# Renumerando os valores NA`s para 1
test_kaggle_p2[(is.na(test_kaggle_p2$count_device) == TRUE), 'count_device'] = 1
# Criando a coluna count_os
test_kaggle_p2$count_os <- dfos$count[match(test_kaggle_p2$os, dfos$os)]
# Renumerando os valores NA`s para 1
test_kaggle_p2[(is.na(test_kaggle_p2$count_os) == TRUE), 'count_os'] = 1
# Criando a coluna count_app
test_kaggle_p2$count_channel <- dfchannel$count[match(test_kaggle_p2$channel, dfchannel$channel)]
# Renumerando os valores NA`s para 1
test_kaggle_p2[(is.na(test_kaggle_p2$count_channel) == TRUE), 'count_channel'] = 1
test_kaggle_p2 <- test_kaggle_p2[, c('click_id', 'count_app', 'count_device', 'count_os',
                                     'count_channel', 'count_ip', 'range_hour')]
# Realizando a previsão
pred_rf_v3_p2 <- predict(modelo_rf_v4, test_kaggle_p2, type="response")
# Criando um data set com o real e o previsto
prediction3_p2 <- data.frame('click_id' = test_kaggle_p2$click_id,
                             'is_attributed' = pred_rf_v3_p2)
# Visualizando os primeiros dados
head(prediction3_p2)
# Excluindo o data set de teste
rm(test_kaggle_p2)


# Lendo o arquivo parte 3
test_kaggle_p3 <- fread("https://media.githubusercontent.com/media/fthara/AdTracking_Fraude_Detection_com_R/master/test_p3.csv")
test_kaggle_p3 <- transform_test(test_kaggle_p3)
# Criando a coluna count_app
test_kaggle_p3$count_app <- dfapp$count[match(test_kaggle_p3$app, dfapp$app)]
# Renumerando os valores NA`s para 1
test_kaggle_p3[(is.na(test_kaggle_p3$count_app) == TRUE), 'count_app'] = 1
# Criando a coluna count_device
test_kaggle_p3$count_device <- dfdevice$count[match(test_kaggle_p3$device, dfdevice$device)]
# Renumerando os valores NA`s para 1
test_kaggle_p3[(is.na(test_kaggle_p3$count_device) == TRUE), 'count_device'] = 1
# Criando a coluna count_os
test_kaggle_p3$count_os <- dfos$count[match(test_kaggle_p3$os, dfos$os)]
# Renumerando os valores NA`s para 1
test_kaggle_p3[(is.na(test_kaggle_p3$count_os) == TRUE), 'count_os'] = 1
# Criando a coluna count_app
test_kaggle_p3$count_channel <- dfchannel$count[match(test_kaggle_p3$channel, dfchannel$channel)]
# Renumerando os valores NA`s para 1
test_kaggle_p3[(is.na(test_kaggle_p3$count_channel) == TRUE), 'count_channel'] = 1
test_kaggle_p3 <- test_kaggle_p3[, c('click_id', 'count_app', 'count_device', 'count_os',
                                     'count_channel', 'count_ip', 'range_hour')]
# Realizando a previsão
pred_rf_v3_p3 <- predict(modelo_rf_v4, test_kaggle_p3, type="response")
# Criando um data set com o real e o previsto
prediction3_p3 <- data.frame('click_id' = test_kaggle_p3$click_id,
                             'is_attributed' = pred_rf_v3_p3)
# Visualizando os primeiros dados
head(prediction3_p3)
# Excluindo o data set de teste
rm(test_kaggle_p3)


# Lendo o arquivo parte 4
test_kaggle_p4 <- fread("https://media.githubusercontent.com/media/fthara/AdTracking_Fraude_Detection_com_R/master/test_p4.csv")
test_kaggle_p4 <- transform_test(test_kaggle_p4)
# Criando a coluna count_app
test_kaggle_p4$count_app <- dfapp$count[match(test_kaggle_p4$app, dfapp$app)]
# Renumerando os valores NA`s para 1
test_kaggle_p4[(is.na(test_kaggle_p4$count_app) == TRUE), 'count_app'] = 1
# Criando a coluna count_device
test_kaggle_p4$count_device <- dfdevice$count[match(test_kaggle_p4$device, dfdevice$device)]
# Renumerando os valores NA`s para 1
test_kaggle_p4[(is.na(test_kaggle_p4$count_device) == TRUE), 'count_device'] = 1
# Criando a coluna count_os
test_kaggle_p4$count_os <- dfos$count[match(test_kaggle_p4$os, dfos$os)]
# Renumerando os valores NA`s para 1
test_kaggle_p4[(is.na(test_kaggle_p4$count_os) == TRUE), 'count_os'] = 1
# Criando a coluna count_app
test_kaggle_p4$count_channel <- dfchannel$count[match(test_kaggle_p4$channel, dfchannel$channel)]
# Renumerando os valores NA`s para 1
test_kaggle_p4[(is.na(test_kaggle_p4$count_channel) == TRUE), 'count_channel'] = 1
test_kaggle_p4 <- test_kaggle_p4[, c('click_id', 'count_app', 'count_device', 'count_os',
                                     'count_channel', 'count_ip', 'range_hour')]
# Realizando a previsão
pred_rf_v3_p4 <- predict(modelo_rf_v4, test_kaggle_p4, type="response")
# Criando um data set com o real e o previsto
prediction3_p4 <- data.frame('click_id' = test_kaggle_p4$click_id,
                             'is_attributed' = pred_rf_v3_p4)
# Visualizando os primeiros dados
head(prediction3_p4)
# Excluindo o data set de teste
rm(test_kaggle_p4)

# Juntando as 4 predições
prediction3 <- rbind(prediction3_p1, prediction3_p2, prediction3_p3, prediction3_p4)
# Escrevendo o data set em um arquivo csv.
#fwrite(prediction3, "prediction3.csv", row.names = F, sep = ",")

# Excluindo algumas variáveis que não serão mais utilizadas
rm(prediction3_p1, prediction3_p2, prediction3_p3, prediction3_p4)
rm(pred_rf_v3_p1, pred_rf_v3_p2, pred_rf_v3_p3, pred_rf_v3_p4)
rm(modelo_rf_v4, pred_rf_v4)
rm(prediction3)

# O score dessa previsão ficou em 0.72 no kaggle, uma pequena melhora em relação ao algoritmo
# anterior.

# Agora vamos para a ultima tentativa de melhoria do modelo. Vamos adicionar as variáveis
# de Id`s no nosso modelo preditivo. Pois assim conseguimos separar aquelas que têm a mesma
# quantidade, mas que tem mais downloads. Só que essas variáveis continuarão numéricas para evitar
# erros durante a modelagem e previsão dos testes, já que nos testes existem valores diferentes
# do sample do data set.

# Criando as variáveis train e test
train <- df[trainIndex,]
test <- df[-trainIndex,]

# Balanceando os dados
# Feature selection nos dados de treino e teste
train <- train %>% select(app, device, os, channel, count_app, count_device, count_os,
                          count_channel, count_ip, range_hour, is_attributed)
test <- test %>% select(app, device, os, channel, count_app, count_device, count_os,
                        count_channel, count_ip, range_hour, is_attributed)
# ROSE nos dados de treino
rose_train <- ROSE(is_attributed ~ ., data = train, seed = 1)$data
prop.table(table(rose_train$is_attributed))
# ROSE nos dados de teste
rose_test <- ROSE(is_attributed ~ ., data = test, seed = 1)$data
prop.table(table(rose_test$is_attributed))

# Vamos treinar o modelo apenas com o algoritmo random forest, pois foi o que teve os
# melhores resultados até o momento.
# Criando o modelo com outro algoritmo Random Forest
modelo_rf_v7 = randomForest(formula = is_attributed ~ ., data = rose_train,
                            importance = TRUE)
# Previsão nos dados de teste
pred_rf_v7 = predict(modelo_rf_v7, rose_test, type='class')
# Confusion Matrix
confusionMatrix(rose_test$is_attributed, pred_rf_v7, positive = '1')
# Curva roc para o modelo_v1
roc.curve(rose_test$is_attributed, pred_rf_v7, plotit = T,
          col = "darkblue", add.roc = T)

# Lendo o arquivo parte 1
test_kaggle_p1 <- fread("https://media.githubusercontent.com/media/fthara/AdTracking_Fraude_Detection_com_R/master/test_p1.csv")
test_kaggle_p1 <- transform_test(test_kaggle_p1)
# Criando a coluna count_app
test_kaggle_p1$count_app <- dfapp$count[match(test_kaggle_p1$app, dfapp$app)]
# Renumerando os valores NA`s para 1
test_kaggle_p1[(is.na(test_kaggle_p1$count_app) == TRUE), 'count_app'] = 1
# Criando a coluna count_device
test_kaggle_p1$count_device <- dfdevice$count[match(test_kaggle_p1$device, dfdevice$device)]
# Renumerando os valores NA`s para 1
test_kaggle_p1[(is.na(test_kaggle_p1$count_device) == TRUE), 'count_device'] = 1
# Criando a coluna count_os
test_kaggle_p1$count_os <- dfos$count[match(test_kaggle_p1$os,dfos$os)]
# Renumerando os valores NA`s para 1
test_kaggle_p1[(is.na(test_kaggle_p1$count_os) == TRUE), 'count_os'] = 1
# Criando a coluna count_app
test_kaggle_p1$count_channel <- dfchannel$count[match(test_kaggle_p1$channel,dfchannel$channel)]
# Renumerando os valores NA`s para 1
test_kaggle_p1[(is.na(test_kaggle_p1$count_channel) == TRUE), 'count_channel'] = 1
# Criando um data set com o real e o previsto
test_kaggle_p1 <- test_kaggle_p1[, c('app', 'device', 'os', 'channel', 'click_id', 'count_app', 
                                     'count_device', 'count_os', 'count_channel', 'count_ip', 'range_hour')]
# Realizando a previsão
pred_rf_v4_p1 <- predict(modelo_rf_v7, test_kaggle_p1, type="response")
# Criando um data set com o real e o previsto
prediction4_p1 <- data.frame('click_id' = test_kaggle_p1$click_id,
                             'is_attributed' = pred_rf_v4_p1)
# Visualizando os primeiros dados
head(prediction4_p1)
# Excluindo o data set de teste
rm(test_kaggle_p1)


# Lendo o arquivo parte 2
test_kaggle_p2 <- fread("https://media.githubusercontent.com/media/fthara/AdTracking_Fraude_Detection_com_R/master/test_p2.csv")
test_kaggle_p2 <- transform_test(test_kaggle_p2)
# Criando a coluna count_app
test_kaggle_p2$count_app <- dfapp$count[match(test_kaggle_p2$app, dfapp$app)]
# Renumerando os valores NA`s para 1
test_kaggle_p2[(is.na(test_kaggle_p2$count_app) == TRUE), 'count_app'] = 1
# Criando a coluna count_device
test_kaggle_p2$count_device <- dfdevice$count[match(test_kaggle_p2$device, dfdevice$device)]
# Renumerando os valores NA`s para 1
test_kaggle_p2[(is.na(test_kaggle_p2$count_device) == TRUE), 'count_device'] = 1
# Criando a coluna count_os
test_kaggle_p2$count_os <- dfos$count[match(test_kaggle_p2$os, dfos$os)]
# Renumerando os valores NA`s para 1
test_kaggle_p2[(is.na(test_kaggle_p2$count_os) == TRUE), 'count_os'] = 1
# Criando a coluna count_app
test_kaggle_p2$count_channel <- dfchannel$count[match(test_kaggle_p2$channel, dfchannel$channel)]
# Renumerando os valores NA`s para 1
test_kaggle_p2[(is.na(test_kaggle_p2$count_channel) == TRUE), 'count_channel'] = 1
test_kaggle_p2 <- test_kaggle_p2[, c('app', 'device', 'os', 'channel', 'click_id', 'count_app', 
                                     'count_device', 'count_os', 'count_channel', 'count_ip', 'range_hour')]
# Realizando a previsão
pred_rf_v4_p2 <- predict(modelo_rf_v7, test_kaggle_p2, type="response")
# Criando um data set com o real e o previsto
prediction4_p2 <- data.frame('click_id' = test_kaggle_p2$click_id,
                             'is_attributed' = pred_rf_v4_p2)
# Visualizando os primeiros dados
head(prediction4_p2)
# Excluindo o data set de teste
rm(test_kaggle_p2)


# Lendo o arquivo parte 3
test_kaggle_p3 <- fread("https://media.githubusercontent.com/media/fthara/AdTracking_Fraude_Detection_com_R/master/test_p3.csv")
test_kaggle_p3 <- transform_test(test_kaggle_p3)
# Criando a coluna count_app
test_kaggle_p3$count_app <- dfapp$count[match(test_kaggle_p3$app, dfapp$app)]
# Renumerando os valores NA`s para 1
test_kaggle_p3[(is.na(test_kaggle_p3$count_app) == TRUE), 'count_app'] = 1
# Criando a coluna count_device
test_kaggle_p3$count_device <- dfdevice$count[match(test_kaggle_p3$device, dfdevice$device)]
# Renumerando os valores NA`s para 1
test_kaggle_p3[(is.na(test_kaggle_p3$count_device) == TRUE), 'count_device'] = 1
# Criando a coluna count_os
test_kaggle_p3$count_os <- dfos$count[match(test_kaggle_p3$os, dfos$os)]
# Renumerando os valores NA`s para 1
test_kaggle_p3[(is.na(test_kaggle_p3$count_os) == TRUE), 'count_os'] = 1
# Criando a coluna count_app
test_kaggle_p3$count_channel <- dfchannel$count[match(test_kaggle_p3$channel, dfchannel$channel)]
# Renumerando os valores NA`s para 1
test_kaggle_p3[(is.na(test_kaggle_p3$count_channel) == TRUE), 'count_channel'] = 1
test_kaggle_p3 <- test_kaggle_p3[, c('app', 'device', 'os', 'channel', 'click_id', 'count_app', 
                                     'count_device', 'count_os', 'count_channel', 'count_ip', 'range_hour')]
# Realizando a previsão
pred_rf_v4_p3 <- predict(modelo_rf_v7, test_kaggle_p3, type="response")
# Criando um data set com o real e o previsto
prediction4_p3 <- data.frame('click_id' = test_kaggle_p3$click_id,
                             'is_attributed' = pred_rf_v4_p3)
# Visualizando os primeiros dados
head(prediction4_p3)
# Excluindo o data set de teste
rm(test_kaggle_p3)


# Lendo o arquivo parte 4
test_kaggle_p4 <- fread("https://media.githubusercontent.com/media/fthara/AdTracking_Fraude_Detection_com_R/master/test_p4.csv")
test_kaggle_p4 <- transform_test(test_kaggle_p4)
# Criando a coluna count_app
test_kaggle_p4$count_app <- dfapp$count[match(test_kaggle_p4$app, dfapp$app)]
# Renumerando os valores NA`s para 1
test_kaggle_p4[(is.na(test_kaggle_p4$count_app) == TRUE), 'count_app'] = 1
# Criando a coluna count_device
test_kaggle_p4$count_device <- dfdevice$count[match(test_kaggle_p4$device, dfdevice$device)]
# Renumerando os valores NA`s para 1
test_kaggle_p4[(is.na(test_kaggle_p4$count_device) == TRUE), 'count_device'] = 1
# Criando a coluna count_os
test_kaggle_p4$count_os <- dfos$count[match(test_kaggle_p4$os, dfos$os)]
# Renumerando os valores NA`s para 1
test_kaggle_p4[(is.na(test_kaggle_p4$count_os) == TRUE), 'count_os'] = 1
# Criando a coluna count_app
test_kaggle_p4$count_channel <- dfchannel$count[match(test_kaggle_p4$channel, dfchannel$channel)]
# Renumerando os valores NA`s para 1
test_kaggle_p4[(is.na(test_kaggle_p4$count_channel) == TRUE), 'count_channel'] = 1
test_kaggle_p4 <- test_kaggle_p4[, c('app', 'device', 'os', 'channel', 'click_id', 'count_app', 
                                     'count_device', 'count_os', 'count_channel', 'count_ip', 'range_hour')]
# Realizando a previsão
pred_rf_v4_p4 <- predict(modelo_rf_v7, test_kaggle_p4, type="response")
# Criando um data set com o real e o previsto
prediction4_p4 <- data.frame('click_id' = test_kaggle_p4$click_id,
                             'is_attributed' = pred_rf_v4_p4)
# Visualizando os primeiros dados
head(prediction4_p4)
# Excluindo o data set de teste
rm(test_kaggle_p4)

# Juntando as 4 predições
prediction4 <- rbind(prediction4_p1, prediction4_p2, prediction4_p3, prediction4_p4)
# Escrevendo o data set em um arquivo csv.
#fwrite(prediction4, "prediction4.csv", row.names = F, sep = ",")


# Infelizmente esse modelo não trouxe resultados muitos relevantes para o kaggle, mas
# podemos ver a melhora pela métrica de AUC do modelo. Talvez treinando um pouco mais ou
# usando o data set original, que fica muito pesado em meu computador, o modelo fique melhor.
