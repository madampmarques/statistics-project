#Delineamento Experimental 
#Analise de Variancias 
#Bibliotecas
install.packages("gplots")
library(car)
library(gplots)
#Ler a base dados
dados<-penguins
#Omitir os NA
dados_limpos <- na.omit(dados)
#ANOVA de um factor testar a diferenca de médias de massa corporal por espécie
resultado_aov <- aov(body_mass_g ~ species, data = dados_limpos)
summary(resultado_aov)
#GraudeLiberdade
df_res_1way <- df.residual(resultado_aov)
print(df_res_1way)
#Análise do Teste de Tukey HSD
TukeyHSD(resultado_aov)
#Análise de residuos 
par(mfrow=c(2,2))
plot(resultado_aov)
layout(1)
#teste para pressuposto da normalidade 
shapiro.test(residuals(resultado_aov))
#teste dos pressupostos da análise de variância
bartlett.test(body_mass_g ~ species, data = dados_limpos)
# ANOVA de Dois Fatores com Interação 
resultado_aov_2way <- aov(body_mass_g ~ species * sex, data = dados_limpos)
#Visualização da Tabela ANOVA
summary(resultado_aov_2way)
#Análise de resíduos
par(mfrow=c(2,2))
plot(resultado_aov_2way)
layout(1)
#teste de pressuposto para normalidade
shapiro.test(residuals(resultado_aov_2way))
#teste para homogeidade das variâncias 
leveneTest(body_mass_g ~ species * sex, data = dados_limpos)
#Análise do Teste de Tukey HSD
TukeyHSD(resultado_aov_2way)
#ICs de 95% (padrão)
confint(resultado_aov_2way, level = 0.95)
#Regressão Linear com 1 fator 
modelo_regressao_fator <- lm(body_mass_g ~ species, data = dados_limpos)
summary(modelo_regressao_fator)
#Intervalos de confiança a 95% 
confint(modelo_regressao_fator,level=0.95)
#Análise de Residuos
par(mfrow=c(2,2))
plot(modelo_regressao_fator)
#Normalidade de Variâncias
shapiro.test(residuals(modelo_regressao_fator))
#Homogeneidade das variâncias 
leveneTest(body_mass_g ~ species,data=dados_limpos)
#Regressão linear de 2 fatores 
# Variável Dependente: body_mass_g (massa corporal)
# Fatores: species * sex (inclui os efeitos principais e a interação)
modelo_regressao_2f <- lm(body_mass_g ~ species * sex, data = dados_limpos)
summary(modelo_regressao_2f)
#Análise de Residuos
par(mfrow=c(2,2))
plot(modelo_regressao_2f)
layout(1)
#Normalidade de Variâncias
shapiro.test(residuals(modelo_regressao_2f))
#Homogeneidade das variâncias 
leveneTest(body_mass_g ~ species*sex, data = dados_limpos, center = median)
#Intervalos de Confianca a 95%
confint(modelo_regressao_2f,level=0.95)
