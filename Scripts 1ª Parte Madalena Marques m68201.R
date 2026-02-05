#Delineamento Experimental
#Packages necessários para trabalho 
install.packages("skimr") #instalar package 
install.packages("psych") #instalar package
#Biblioteca necessárias para o trabalho 
library(skimr)
library(psych)
library(ggplot2)
library(dplyr)

#Ler a base dados
dadost <- tobacco_data #exportar os ficheiros excel através do import dataset e ler o mesmo 
head(dadost) # para verificar se está tudo ok
str(dadost) #para verificar se está tudo ok 
skim(dadost) # estatistica descritiva 

#Confirmar os missing value 
names(dadost) #nome das colunas 
colSums(is.na(dadost[, c("gender", "age.gr", "cigs.per.day", "disease")])) #contar o número de dados por coluna 
#Converter os missing values das variaveis 
dadost$gender <- ifelse(is.na(dadost$gender), "Desconhecido", dadost$gender)
dadost$age<-ifelse(is.na(dadost$age), 0, dadost$age)
dadost$BMI<-ifelse(is.na(dadost$BMI),0,dadost$BMI)
dadost$disease <- ifelse(is.na(dadost$disease), "Desconhecido", dadost$disease)
dadost$cigs.per.day<-ifelse(is.na(dadost$cigs.per.day),0,dadost$cigs.per.day)
dadost$age.gr<-ifelse(is.na(dadost$age.gr),0,dadost$age.gr)
describe(dadost) # estatistica descritiva 

#Dados quantitativos
#BMI (indice massa corporal)
dadost$BMI_class <- cut(dadost$BMI,
                       breaks = c(0, 18.5, 24.9, 29.9, 34.9, Inf),
                       labels = c("Baixo Peso", "Normal", "Sobrepeso", "Obesidade I", "Obesidade II+"),
                       right = FALSE) #agrupar em classes BMI 

tabela_bmi <- dadost %>%
  group_by(BMI_class) %>%
  summarise(
    Frequência = n()
  ) %>%
  mutate(
    Percentagem = round(100 * Frequência / sum(Frequência), 1)
  ) #tabela de frequência para variavel BMI
tabela_bmi
#Histograma do BMI
ggplot(dadost, aes(x = BMI)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribuição do IMC", x = "IMC", y = "Frequência")
#Idade
dadost$age_class <- cut(dadost$age,
                       breaks = seq(from = min(dadost$age, na.rm = TRUE),
                                    to = max(100, na.rm = TRUE),
                                    by = 20),
                       right = FALSE) #agrupar em classes 

tabela_idade <- dadost %>%
  group_by(age_class) %>%
  summarise(
    Frequência = n()
  ) %>%
  mutate(
    Percentagem = round(100 * Frequência / sum(Frequência), 1)
  )
tabela_idade
#Histrograma de Idade 
ggplot(tabela_idade, aes(x = age_class, y = Frequência, fill = age_class)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Distribuição de Idade em Classes",
       x = "Faixa Etária", y = "Frequência") +
  theme(legend.position = "none")
#Cigs.per.day
dadost$cigs.per.day_class <- cut(dadost$cigs.per.day,
                                 breaks = seq(from = min(dadost$cigs.per.day, na.rm = TRUE),
                                                                  to = max(50, na.rm = TRUE),
                                                                  by = 10),
                                 right = FALSE) # agrupar por classes 
tabela_cigs <- dadost %>%
  group_by(cigs.per.day_class) %>%
  summarise(
    Frequência = n()
  ) %>%
  mutate(
    Percentagem = round(100 * Frequência / sum(Frequência), 1)
  )#tabela de frequência 
tabela_cigs
#Gráfico de Barras para variavel cigarros
ggplot(tabela_cigs, aes(x = cigs.per.day_class, y = Frequência, fill = cigs.per.day_class)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Distribuição do Consumo de Cigarros por Dia",
       x = "Cigarros por Dia (em classes)", y = "Frequência") +
  theme(legend.position = "none")
#Dados qualititativos ordinais
#Idade em 4 categorias 
table(dadost$age.gr, useNA = "no") #use=NA foi usado para exlcuir os NA da variavel 

#Histograma da variavel da idade em 4 categorias 
dadost %>%
  filter(!is.na(age.gr)) %>%
  count(age.gr) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ggplot(aes(x = age.gr, y = percent, fill = age.gr)) +
  geom_col() +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            vjust = -0.5) +
  theme_minimal() +
  labs(title = "Distribuição por Grupo Etário (com percentagens)",
       x = "Faixa Etária", y = "Percentagem (%)") +
  theme(legend.position = "none") #foi usado para meter as percentagens na idade em 4 categorias

#Relação entre IMC e a pessoas que fumam 
cor(dadost$BMI, dadost$cigs.per.day, use = "complete.obs") #foi usado para calcular a correlação entre as duas variaveis 
#Gráfico de dispersão das duas variáveis 
ggplot(dadost, aes(x = cigs.per.day, y = BMI)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  labs(title = "Relação entre Cigarros por Dia e IMC",
       x = "Cigarros por Dia", y = "IMC")
#Testar Normalidade entre estas 2 variaveis
shapiro.test(dadost$cigs.per.day)
shapiro.test(dadost$BMI)
#Pessoas que tem a doença 
#Gráfico de Barras para saber quantas pessoas tem doenças associadas 
ggplot(dadost, aes(x = diseased, fill = diseased)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribuição de Doença", x = "Doente", y = "Número de Pessoas")
#Relação entre Idade com doença
table(dadost$diseased) #para saber o saber as doenças associadas 
dadost$diseased <- factor(dadost$diseased, levels = c("No", "Yes")) #converter a variavel a doença
dadost %>%
  group_by(diseased) %>%
  summarise(
    Média_Idade = mean(age, na.rm = TRUE),
    Mediana_Idade = median(age, na.rm = TRUE),
    Desvio_Padrão = sd(age, na.rm = TRUE),
    n = n()
  ) #tabela de frequência entre as duas variaveis
#Gráfico de dispersão entre as duas variaveis 
ggplot(dadost, aes(x = age, fill = diseased)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  labs(title = "Distribuição da Idade por Estado de Saúde",
       x = "Idade (anos)", y = "Densidade") +
  scale_fill_manual(values = c("No" = "lightblue", "Yes" = "salmon"))
# Relação entre a Idade com IMC
str(dadost[, c("age", "BMI")]) #para confirmar se as variaveis são numéricas 
cor(dadost$age, dadost$BMI, use = "complete.obs") #foi usado para calcular a correlação entre as duas variaveis 
#Gráfico de dispersão das duas variáveis 
ggplot(dadost, aes(x = age, y = BMI)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(title = "Relação entre Idade e IMC",
       x = "Idade (anos)", y = "IMC")
#Tabelas de Contigência entre genero e número de fumadores 
table(dadost$gender, dadost$smoker)
#Tabelas de Contigência entre  idade com quatro categorias e doenças 
table(dadost$age.gr, dadost$disease)
#Tabelas de Contigência entre género e doenças 
table(dadost$gender,dadost$disease)













