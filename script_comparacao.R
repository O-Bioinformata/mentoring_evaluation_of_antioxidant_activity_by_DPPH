# Instalar pacotes necessários
install.packages("tidyverse")
install.packages("car")

# Carregar pacotes
library(tidyverse)
library(car)

# Dados fornecidos
data <- data.frame(
  Concentration = rep(c(10, 25, 50, 100, 125, 250), 3),
  Substance = rep(c("Luteolina", "Fisostigmina", "Quercetina"), each = 6),
  Sequestration = c(80.05, 81.61, 93.17, 93.17, 94.00, 93.49,
                    46.89, 52.44, 56.41, 60.43, 61.87, 67.55,
                    62.52, 73.93, 90.10, 91.27, 92.23, 92.58),
  SD = c(0.113, 0.325, 0.665, 0.308, 1.457, 0.141,
         2.643, 0.579, 0.084, 1.260, 0.147, 0.749,
         1.741, 0.955, 0.953, 0.997, 0.338, 0.034)
)

# Análise de variância (ANOVA)
anova_model <- aov(Sequestration ~ Substance * Concentration, data = data)
summary(anova_model)

# Teste post-hoc de Tukey
tukey <- TukeyHSD(anova_model)
tukey

# Gráfico de barras com erro padrão
data %>%
  group_by(Substance, Concentration) %>%
  summarize(mean_sequestration = mean(Sequestration), .groups = 'drop') %>%
  ggplot(aes(x = as.factor(Concentration), y = mean_sequestration, fill = Substance)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_sequestration - SD, ymax = mean_sequestration + SD),
                position = position_dodge(0.9), width = 0.25) +
  labs(x = "Concentração (µg/mL)", y = "Percentual de Sequestro (%)") +
  theme_minimal()

# Boxplots para visualizar a distribuição dos dados
ggplot(data, aes(x = as.factor(Concentration), y = Sequestration, fill = Substance)) +
  geom_boxplot() +
  labs(x = "Concentração (µg/mL)", y = "Percentual de Sequestro (%)") +
  theme_minimal()

# Gráficos de linha para mostrar a tendência
ggplot(data, aes(x = as.factor(Concentration), y = Sequestration, color = Substance, group = Substance)) +
  geom_line() +
  geom_point() +
  labs(x = "Concentração (µg/mL)", y = "Percentual de Sequestro (%)") +
  theme_minimal()
