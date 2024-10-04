#instalar pacotes
install.packages("readxl")
install.packages("janitor")
install.packages("ggthemes")
install.packages("caret")
install.packages("ROCR")
install.packages("pROC")
install.packages("tidymodels")
install.packages("tidyverse")

# carregar pacotes
library(MASS)
library(readxl)
library(janitor)
library(ggthemes)
library(caret)
library(ROCR)
library(pROC)
library(tidymodels)
library(tidyverse)

#conjunto de dados
dados <- read_xlsx(path = "dados/dados_cdrl.xlsx", sheet = "besouro")

glimpse(dados)

dados <- dados |>
  mutate(f = m - y)

glimpse(dados)

# ajuste - regressão logística
fit <- glm(cbind(y, f) ~ dose,
           family = binomial(link = "logit"),
           data = dados)

names(fit)

exp(coef(fit))

probs <- predict(fit, type = "response")
fity <- (dados$m)*probs

dados_sum <- dados |>
  mutate(prob_est = probs, y_est = fity)

ggplot(dados, aes(x = dose, y = y/m)) +
  geom_point() +
  stat_smooth(method = "glm", color = "pink", se = FALSE,
              method.args = list(family = binomial)) +
  labs(x = "Dose", y = "Proporção de mortos") +
  theme_bw()

# conjunto de dados - besouro binario
dados <- read_xlsx(path = "dados/dados_cdrl.xlsx",
                   sheet = "besouro_bin")
glimpse(dados)

fit <- glm(y ~ dose,
           family = binomial(link = "logit"),
           data = dados)
summary(fit)

ggplot(dados, aes(x = dose, y = y)) +
  geom_point() +
  stat_smooth(method = "glm", color = "pink", se = FALSE,
              method.args = list(family = binomial)) +
  labs(x = "Dose", y = "Desfecho") +
  theme_bw()


# acuracia
probs <- predict(fit, type = "response")
pred_classe <- ifelse(probs < 0.5, 0, 1)
mean(pred_classe == dados$y)

# matriz de confusão - caret
dados <- dados |>
  mutate(y_cat = factor(ifelse(y == 0, "nao", "sim")))

cats <- factor(ifelse(probs < 0.5, "nao", "sim"))

confusionMatrix(
  data = relevel(cats, ref = "sim"),
  reference = relevel(dados$y_cat, ref = "sim"))

#curva ROC - pacote pROC
pROC::auc(dados$y, pred_classe)
predictions <- predict()




