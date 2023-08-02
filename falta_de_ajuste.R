# TESTE DE FALTA DE AJUSTE


# packages ----------------------------------------------------------------

library(ggplot2)

# import dataset ----------------------------------------------------------

corrosao <- data.frame(potencia = c(rep(160,5),rep(180,5),rep(200,5),rep(220,5)),
                       taxa = c(575,542,530,539,570,565,593,590,579,610,
                                600,651,610,637,629,725,700,715,685,710))
corrosao$potencia_factor <- factor(corrosao$potencia)


milho <- read.csv("milho.csv")
milho$doses_fator = factor(milho$Doses)


# teste de falta de ajuste ------------------------------------------------


# modelos
# regression model
mod_reduced = lm(taxa ~ potencia, data = corrosao)
anova(object = mod_reduced)
# anova model
mod_full = aov(taxa ~ potencia_factor, data = corrosao)
summary(mod_full)

# o teste de falta de ajuste 
anova(mod_reduced, mod_full)

# análise dos resíduos 
qqnorm(rstandard(mod_reduced))
qqline(rstandard(mod_reduced))

plot(fitted(mod_reduced), rstandard(mod_reduced), main = "Resíduos vs Predito",
     xlab = "Predito", ylab = "Resíduo", pch = 16)



# polinômios ortogonais ---------------------------------------------------

# dispersão dos dados
ggplot(data = corrosao, mapping = aes(x = potencia, y = taxa)) +
  geom_point() + xlab("Potencia") + ylab("Taxa de Corrosão")

# contrastes de polinomios
contrasts(corrosao$potencia_factor) = contr.poly(n = 4)

# anova - polinômios ortogonais
mod_anova <- aov(taxa ~ potencia_factor, data = corrosao)
summary(mod_anova, split = list("potencia_factor" = list("L"=1, "Q"=2, "C"=3)))

# ajuste do modelo de regressão
mod_reg = lm(taxa ~ I(potencia) + I(potencia^2), data = corrosao)

ggplot(data = corrosao, mapping = aes(x = potencia, y = taxa)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~I(x)+I(x^2))

# análise dos resíduos
qqnorm(rstandard(mod_reg))
qqline(rstandard(mod_reg))

plot(x = fitted(mod_reg), y = rstandard(mod_reg), pch=16)


# delineamento casualizado em blocos ---------------------------------------

# dispersão dos dados
ggplot(data = milho, mapping = aes(x = Doses, y = Prod)) +
  geom_point()

# contrastes polinômios ortogonais
contrasts(milho$doses_fator) = contr.poly(n = 5)
contrasts(milho$doses_fator)

# modelo e ajuste
mod_anova = aov(Prod ~ Blocos + doses_fator, data = milho)
summary(mod_anova, split = list("doses_fator" = list("L"=1,"Q"=2,"C"=3,"^4"=4)))

ggplot(data = milho, mapping = aes(x = Doses, y = Prod)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y~poly(x,3))
