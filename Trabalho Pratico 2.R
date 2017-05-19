#library(readxl)
#trabalho <- read_excel("trabalho_dados.xls")

library(readr)
trabalho <- read_delim("trabalho_dados.csv", 
                       ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                   grouping_mark = "."), trim_ws = TRUE)
View(trabalho)


nf <- trabalho$NotaFinal

plot(nf)

hist(nf,main  = 'Aproveitamento dos Alunos', ylab  = 'Frequencia',xlab  = 'Media Final')

i <- 1
j <- 1
x_bar <- 0
R <- 0
limit <- length(nf)/3

for(z in 1:limit){
  aux <- c(nf[i], nf[i+1], nf[i+2])
  x_bar[j] <- mean(aux)
  R[j] <- max(aux) - min(aux)
  j <- j+1
  i <- i+3
}

x_bar <- round(x_bar)
LSC <- mean(x_bar)+1.023*mean(R)
LIC <- mean(x_bar)-1.023*mean(R)
LC <- mean(x_bar)

#Arredondando valores
LSC <- ceil(LSC)
LIC <- floor(LIC)
LC <- round(LC)

#Criando Matriz de LSC
mLSC <- matrix(LSC, nrow =length(x_bar), ncol=1)
mLSC

#Criando Matriz de LIC
mLIC <- matrix(LIC, nrow =length(x_bar), ncol=1)
mLIC

#Criando Matriz de LC
mLC <- matrix(LC, nrow =length(x_bar), ncol=1)
mLC

#Combinado Matrizes X_BAR, LSC, LIC e LC
matriz = cbind(x_bar,mLSC,mLIC,mLC,R)
colnames(matriz) <- c("x_bar","LSC","LIC","LC","R")
matriz

write.csv(matriz, "graf.csv", row.names = FALSE)


#Extra plotar grafico de controle no R

library(qicharts)
qic(x_bar, chart = 'i', 
    main  = 'Aproveitamento dos Alunos', 
    ylab  = 'Media',
    xlab  = 'Amostra')