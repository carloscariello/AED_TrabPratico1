
library(readr)
trabalho <- read_delim("trabalho_dados.csv", 
                             ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                         grouping_mark = "."), trim_ws = TRUE)
View(trabalho)


Indep <- trabalho$B1
Dep <- trabalho$MediaFinal
plot(Indep,Dep)
Regressao <- lm(Dep~Indep)
summary(Regressao)

library(broom) 

estimatedSum <- tidy(Regressao)
estimatedSum$estimate[2]
estimatedSum$estimate[1]


Reta<-function(x)estimatedSum$estimate[2]*x+estimatedSum$estimate[1]
plot.function(Reta,from=min(Indep),to=max(Indep))
points(Indep,Dep)


#24.2106
Predicao<-Reta(19)
Predicao

#18.30114
Predicao<-Reta(13)
Predicao

#8.451943
Predicao<-Reta(3)
Predicao