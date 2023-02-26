#Augabe3_c:

functiona <- function(x,y){
  korrelation <- cor(x,y)
  kovarianz <- cov(x,y)
  Ergebnisse <- data.frame(korrelation, kovarianz)
  print(Ergebnisse)
}

functiona(as.numeric(ktgo_var), as.numeric(Daten$InteresseAnMathe))