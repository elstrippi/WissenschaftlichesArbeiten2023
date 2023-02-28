#Augabe3_c:
categorical_relationship <- function(var1, var2) {
  # Erstellen der Kreuztabelle
  tab <- table(var1, var2)
  # Durchführung des Chi-Quadrat-Tests
  chi_sq_test <- chisq.test(tab)
  # Rückgabe der Ergebnisse des Chi-Quadrat-Tests
  return(chi_sq_test)
}

