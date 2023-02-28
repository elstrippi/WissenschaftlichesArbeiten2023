bivariate_stats <- function(met_var, mlk) {
  
  # Konvertiere die dichotome Variable in einen Faktor
  
  mlkc<- ifelse(mlk == TRUE, "Ja", "Nein")
  
  # Berechne die Häufigkeiten und Prozentsätze der dichotomen Variable
  dich_freq <- table(mlkc)
  dich_pct <- round(prop.table(dich_freq) * 100, 2)
  
  # Berechne die deskriptiven Statistiken der metrischen Variable nach der dichotomen Variable
  summary_by_dich <- aggregate(met_var ~ mlkc, FUN = summary)
  
  # Extrahiere die gewünschten Statistiken und formatiere sie
  mean_vec <- round(summary_by_dich$met_var[, "Mean"], 2)
  median_vec <- ifelse("50%" %in% colnames(summary_by_dich$met_var),
                       round(summary_by_dich$met_var[, "50%"], 2),
                       round(summary_by_dich$met_var[, "Mean"], 2))
  min_vec <- round(summary_by_dich$met_var[, "Min."], 2)
  max_vec <- round(summary_by_dich$met_var[, "Max."], 2)
  
  # Kombiniere die Ergebnisse zu einem Datenrahmen
  result <- data.frame(
                       MatheLk  = table(mlkc),
                       mean = mean_vec,
                       median = median_vec,
                       min = min_vec,
                       max = max_vec)
  
  # Gib die Ergebnisse aus
  print(result)
}


