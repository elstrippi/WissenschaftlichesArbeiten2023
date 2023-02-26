#Aufgabe3_b:

#b)
ktgo_var <- factor(Daten$Studienfach)

functiona <- function(x){
  Durchschnitt <- mean(x)
  Varianz <- var(x)
  Standardabweichung <- sqrt(Varianz)
  Median <- median(x)
  int_qabs <- IQR(x)
  Ergebnisse <- data.frame(Durchschnitt, Varianz, Standardabweichung, 
                           Median)
  print(Ergebnisse)
}

functiona(table(ktgo_var))
