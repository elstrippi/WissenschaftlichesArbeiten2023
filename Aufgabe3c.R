categorical_relationship <- function(var1, var2) {
  # Erstellen der Kreuztabelle
  tab <- table(var1, var2)
  # Berechnung des Kontingenzkoeffizienten
  cont_coeff <- sqrt(chisq.test(tab)$statistic / (chisq.test(tab)$statistic + sum(tab) + sum(tab, margin = 2)))
  # Rückgabe des Kontingenzkoeffizienten
  return(cont_coeff)
}
