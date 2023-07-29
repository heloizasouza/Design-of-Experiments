# ANOVA one way 

rm(list = ls())

######## Completely randomized design #####

# dataset imput
cars <- data.frame(type = factor(c(rep('subcompact',10),rep('compact',10), 
                              rep('midsize',10), rep('full size',10))),
                     obs = c(3,5,3,7,6,5,3,2,1,6,1,3,4,7,5,6,3,2,1,7,
                             4,1,3,5,7,1,2,4,2,7,3,5,7,5,10,3,4,7,2,7)
                     )

# anova assumpsions:
anova <- aov(obs~type, data = cars)

  # residuals normality
qqnorm(residuals(anova))
qqline(residuals(anova), col = 'blue')
shapiro.test(residuals(anova))

  # homogeneity of variance
plot(fitted(anova), residuals(anova))
abline(h=0, col='blue')
bartlett.test(obs~type, cars)

# anova analysis
summary(anova)


##### causality blocks ######

# data imput
resp <- data.frame(algor = factor(c(rep(1,6), rep(2,6), rep(3,6), rep(4,6),
                                    rep(5,6), rep(6,6))),
                   bloc = factor(c(rep(1:6,6))),
                   proj = c(1244,21,82,2221,905,839,281,129,396,1306,336,910,
                            220,84,458,543,300,794,225,83,425,552,291,826,
                            19,11,-34,121,15,103,-20,35,-53,170,104,199))

# anova assumpsions:
anova2 <- aov(proj~algor+bloc, data = resp)
  # residuals normality
qqnorm(residuals(anova2))
qqline(residuals(anova2), col='blue')
#shapiro.test(residuals(anova2))

  # homogeneity of variance
plot(fitted(anova2), residuals(anova2))
abline(h=0, col='blue')
#bartlett.test(proj~algor, data=resp)

# anova analysis
summary(anova2)
tukey <- HSD.test(anova2, 'algor')
tukey$groups


##### latin square #####

# data imput
dados <- data.frame(ordem = factor(c(rep(1,4), rep(2,4), rep(3,4), rep(4,4))),
                    oper = factor(c(rep(1:4,4))), tipo = factor(c('C','D','A',
                    'B','B','C','D','A','A','B','C','D','D','A','B','C')),
                    x = c(10,14,7,8,7,18,11,8,5,10,11,9,10,10,12,14))

# anova analysis
anova3 <- aov(x~ordem+oper+tipo, data = dados)
summary(anova3)
