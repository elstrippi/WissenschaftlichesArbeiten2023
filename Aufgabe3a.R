#a)
library(moments)
#Daten <- read.csv("githubDaten.csv", header = TRUE)
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

#functiona(Daten$Alter, print = TRUE)
#d <- functiona(Daten$Alter)
#d
