# Delineamento Inteiramente ao Acaso


# leitura e transformação dos dados
plasma <- read.csv("Plasma.csv", sep = ",")
plasma$Potencia <- factor(plasma$Potencia)

# modelo ANOVA
mod <- aov(Taxa_Corrosao ~ Potencia, data = plasma)
summary(mod)

# análise dos resíduos
qqnorm(rstandard(mod))
qqline(rstandard(mod))

plot(fitted(mod), rstandard(mod))


# teste de tukey
library(agricolae)
tukey <- HSD.test(y = mod, trt = 'Potencia')
tukey
