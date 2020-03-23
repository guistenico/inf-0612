setwd("/home/diogo/MDC/inf-0612/Trabalho")

library(ggplot2)

consecutive <- function(vector, k = 1) {
  n <- length(vector)
  result <- logical(n)
  for (i in (1+k):n){
    if (all(vector[(i-k):(i-1)] == vector[i])){result[i] <- TRUE}
  }
  for (i in 1:(n-k)){
    if (all(vector[(i+1):(i+k)] == vector[i])){result[i] <- TRUE}
  }
  return(result)
}

# Carrega dados do arquivo .csv
columns <- c("id", "horario", "temp", "vento", "umid", "sensa")
cepagri <- read.csv("cepagri.csv", header = TRUE, sep = ",", col.names = columns, stringsAsFactors = FALSE)

# Remove coluna "id" desnecessária
cepagri$id <- NULL

# Transforma tipo da coluna de horário para data
cepagri$horario <- as.Date(as.character(cepagri$horario), format = '%d/%m/%Y-%H:%M')

# Mostra os tipos das colunas
sapply(cepagri, class)

# Seleciona somente os dados no perído de interesse
cepagri <- cepagri[cepagri$horario >= "2015-01-01",]
head(cepagri)
cepagri <- cepagri[cepagri$horario <= "2019-12-31",]
tail(cepagri)

# Criação das colunas de ano e mês
cepagri$ano <- as.numeric(format(cepagri$horario,'%Y'))
cepagri$mes <- as.numeric(format(cepagri$horario,'%m'))
cepagri$mes <- factor(month.abb[cepagri$mes], levels = month.abb, ordered = TRUE)
cepagri$dia <- as.numeric(format(cepagri$horario,'%d'))

# Substitui valores na coluna temp iguais a ' [ERRO]' por NA
sum(cepagri$temp == ' [ERRO]')
cepagri$temp <- as.character(cepagri$temp)
cepagri$temp <- as.numeric(cepagri$temp)
sum(is.na(cepagri$temp))

# Remove linhas com valores NA por enquanto
cepagri <- cepagri[rowSums(is.na(cepagri))==0,]

# Remove valores de 99.9 na coluna de sensaçã térmica
cepagri[cepagri$sensa == 99.9, "sensa"] <- NA

# Remove linhas com valores NA por enquanto
cepagri <- cepagri[rowSums(is.na(cepagri))==0,]

# Sumário das colunas de dados
summary(cepagri[,2:5])

# Remoção de dados repetidos por 1h
filtro <- consecutive(cepagri$temp, 6)
cepagri <- cepagri[!filtro,]

# Análise 1 - El Niño e La Niña
# Os anos de 2017, 2018 foram significativamente menos úmidos comparados
# aos anos anteriores de 2015 e 2016. Esta queda na umidade é justificada
# pela ocorrência do evento La Niña, que torna os invernos no Sul e Sudeste
# do Brasil mais áridos e verões com temperaturas mais amenas. Em 2015 e
# 2016 ocorreu o fenômeno El Niño, que aumentas as temperaturas de forma
# geral no sudeste.

verao <- cepagri
verao <- verao[verao$mes == 'Jan' | verao$mes == 'Feb' | verao$mes == 'Mar',]
verao$ano <- as.character(verao$ano)

inverno <- cepagri
inverno <- inverno[inverno$mes == 'Jul' | inverno$mes == 'Aug' | inverno$mes == 'Sep',]
inverno$ano <- as.character(inverno$ano)

# plot da temperatura no verão
ggplot(verao, aes(x = ano,
                  y = temp,
                  group = ano,
                  fill = ano)) +
  geom_violin() + 
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Temperatura no verão ")

# plot da umidade no inverno
ggplot(inverno, aes(x = ano,
                    y = umid,
                    group = ano,
                    fill = ano)) +
  geom_violin() + 
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Umidade no inverno ")


# tabela de temperatura e umidade média por mês
temp_mes <- NULL
for (ano in unique(cepagri$ano)){
  aux <- cepagri[cepagri$ano==ano,]
  temp_mes <- rbind(temp_mes, tapply(aux$temp, aux$mes, mean))
}
rownames(temp_mes) <- unique(cepagri$ano); temp_mes

umid_mes <- NULL
for (ano in unique(cepagri$ano)){
  aux <- cepagri[cepagri$ano==ano,]
  umid_mes <- rbind(umid_mes, tapply(aux$umid, aux$mes, mean))
}
rownames(umid_mes) <- unique(cepagri$ano); umid_mes

# Análise 2 - Sensação térmica e vento
# Esta análise busca relacionar a sensação térmica com a velocidade do vento.
# É de se esperar que a sensação seja mais baixa quando o vento atinge velocidades
# elevadas. Quando observamos a densidade de sensação para ventos fracos, médios e fortes,
# observamos que as menores sensações são acompanhadas de ventos fortes ou médios.

# criando faixas de velocidade do vento
cepagri$vento_cat <- NULL
cepagri$vento_cat <- ifelse(cepagri$vento <= 25, 'Vento Fraco', 
                     ifelse(cepagri$vento > 25 & cepagri$vento <= 50, 'Vento Médio', 'Vento Forte'))

# plot da umidade no inverno
cepagri_2016 <- cepagri[cepagri$ano==2016,]
ggplot(cepagri_2016, aes(x = sensa,
                         colour = vento_cat,
                         fill = vento_cat)) +
  geom_density(alpha = 0.25) +
  labs(title = "Velocidade do vento e sensação térmica ")




