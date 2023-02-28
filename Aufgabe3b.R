#Aufgabe3_b:

#b)
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
#x <- factor(Daten$Studienfach)
#functiona(factor(Daten$Studienfach))
