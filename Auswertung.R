library(moments)
library(ggplot2)
library(gridExtra)
Daten <- read.csv("C:\\Users\\DabLordMaxDab\\Downloads\\WissenschaftlichesArbeiten2023-main\\WissenschaftlichesArbeiten2023-main\\githubDaten.csv", header = TRUE)
deskStaterstellinator<- function(vector, print = FALSE){
  Durchschnitt <- mean(vector)
  Varianz <- var(vector)
  Standardabweichung <- sqrt(Varianz)
  Median <- median(vector)
  Minimum <- min(vector)
  Maximum <- max(vector)
  Schiefe <- skewness(vector)
  Wölbung <- kurtosis(vector)
  if(print){
    cat("      Deskriptive Maße")
    cat("\n")
    cat("-----------------------------")
    cat("\n")
    cat("Durchschnitt       ")
    cat(Durchschnitt)
    cat("\n")
    cat("Varianz            ")
    cat(Varianz)
    cat("\n")
    cat("Standardabweichung ")
    cat(Standardabweichung)
    cat("\n")
    cat("Median             ")
    cat(Median)
    cat("\n")
    cat("Minimum            ")
    cat(Minimum)
    cat("\n")
    cat("Maximum            ")
    cat(Maximum)
    cat("\n")
    cat("Schiefe            ")
    cat(Schiefe)
    cat("\n")
    cat("Wölbung            ")
    cat(Wölbung)
  }
  
  
  ausgabe <- list("Durchschnitt" = Durchschnitt, 
                  "Varianz" = Varianz, 
                  "Standardabweichung" = Standardabweichung, 
                  "Median" = Median, 
                  "Minimum" = Minimum, 
                  "Maximum" = Maximum,
                  "Schiefe" = Schiefe, 
                  "Wölbung" = Wölbung)
  
  invisible(ausgabe)
}
SatKat <- function(x){
  tab <- table(x)
  AnzmAusp <- length(table(x)) # Anzahl Merkmalsauspraegungen
  rel.Hauefikeit <- prop.table(tab) # relative Hauefigkeiten
  Modus <- max(rel.Hauefikeit) # Modus
  Entropie <- sum(rel.Hauefikeit*log2(1/rel.Hauefikeit))
  normierteEntr. <- Entropie/(log2(AnzmAusp)) # normierte Entropie
  #Ergebnisse <- data.frame(AnzmAusp, rel.Hauefikeit, Modus, Entropie, 
  #normierteEntr.)
  Ergebnisse <- list("AnzmAusp"=AnzmAusp, "rel.Hauefikeit"=rel.Hauefikeit,
                     "Modus"=Modus, "Entropie"=Entropie, 
                     "normierteEntr."=normierteEntr.)
  
  print(Ergebnisse)
}
DesBiStat <- function(x,y){
  korrelation <- cor(x,y)
  kovarianz <- cov(x,y)
  Ergebnisse <- data.frame(korrelation, kovarianz)
  print(Ergebnisse)
}
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
kategorisierer3000 = function(var){
  ord_var = factor(var, ordered = TRUE)
  quant = quantile(ord_var, type = 1)
  #Hier drin werden die Umbenennungen für die Faktoren gespeichert
  new_level = rep(0, length(levels(ord_var)))
  print(quant)
  #Alle Level werden der Reihe nach mit den Quantilen verglichen
  for(i in 1:length(levels(ord_var))){
    if(as.numeric(levels(ord_var) [i]) < quant[2]){
      new_level[i] = "niedrig"
    }
    else if(as.numeric(levels(ord_var) [i]) > quant[4]){
      new_level[i] = "hoch"
    }
    else{
      new_level[i] = "mittel"
    }
  }
  #Weise dem Faktor die neuen Level zu
  levels(ord_var) = new_level
  return(ord_var)
}
multi_katplotter = function(x){
  if(ncol(x) == 3){
    rowname1 = colnames(x)[1]
    rowname2 = colnames(x)[2]
    rowname3 = colnames(x)[3]
    rowname1 = sym(rowname1)
    rowname2 = sym(rowname2)
    rowname3 = sym(rowname3)
    p1 = ggplot(x, aes(x = !!rowname1)) + geom_bar()
    p2 = ggplot(x, aes(x = !!rowname2)) + geom_bar()
    p3 = ggplot(x, aes(x = !!rowname3)) + geom_bar()
    grid.arrange(p1, p2, p3, nrow = 3)
  }
  if(ncol(x) == 4){
    rowname1 = rownames(x)[1]
    rowname2 = rownames(x)[2]
    rowname3 = rownames(x)[3]
    rowname4 = rownames(x)[4]
    sym(rowname1)
    sym(rowname2)
    sym(rowname3)
    sym(rowname4)
    p1 = ggplot(x, aes(x = !!rowname1)) + geom_bar()
    p2 = ggplot(x, aes(x = !!rowname2)) + geom_bar()
    p3 = ggplot(x, aes(x = !!rowname3)) + geom_bar()
    p4 = ggplot(x, aes(x = !!rowname4)) + geom_bar()
    grid.arrange(p1, p2, p3, p4, nrow = 4)
  }
}
D=Daten$MatheLK
D
d2= replace(D,D == "True",TRUE)
mlk =d2
bivariate_stats(Daten$Alter,d2)
k=deskStaterstellinator(Daten$MatheLK)
k
o=data.frame(Daten$InteresseAnMathe,Daten$InteresseAnProgrammieren)
SatKat(d2)
bivariate_stats(Daten$InteresseAnMathe,Daten$MatheLK)
DesBiStat(Daten$Studienfach,Daten$MatheLK)
multi_katplotter(data.frame(Daten$Studienfach ,d2,Daten$InteresseAnProgrammieren))
