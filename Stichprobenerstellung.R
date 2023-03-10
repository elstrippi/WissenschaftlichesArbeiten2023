#Erstellung einer Stichprobe
set.seed(420)
Alter <- rnorm(100, 25, 2)
Studienfach <- sample(c(rep("Statistik", 3), rep("Data Science", 3), rep("Informatik", 2), "Mathe"), 100, replace = TRUE)

#Interesse am Programmieren fuer unterschiedliche Studiengaenge
#Fuer jeden Studiengang werden eigene Zahlen gezogen 
ProgMathe <- sample(1:7, 5, replace = TRUE)
ProgInfo <- sample(5:7, 22, replace = TRUE)
ProgData <- sample(3:7, 38, replace = TRUE)
ProgStat <- sample(3:7, 35, replace = TRUE)

InteresseAnProgrammieren <- 1:100
InteresseAnProgrammieren[Studienfach=="Statistik"] <- ProgStat
InteresseAnProgrammieren[Studienfach=="Data Science"] <- ProgData
InteresseAnProgrammieren[Studienfach=="Informatik"] <- ProgInfo
InteresseAnProgrammieren[Studienfach=="Mathe"] <- ProgMathe


#Interesse an Mathe fuer unterschiedliche Studiengaenge
#Fuer jeden Studiengang werden eigene Zahlen gezogen 
MatheMathe <- sample(5:7, 5, replace = TRUE)
MatheInfo <- sample(1:7, 22, replace = TRUE)
MatheData <- sample(3:7, 38, replace = TRUE)
MatheStat <- sample(3:7, 35, replace = TRUE)

InteresseAnMathe <- 1:100
InteresseAnMathe[Studienfach=="Statistik"] <- MatheStat
InteresseAnMathe[Studienfach=="Data Science"] <- MatheData
InteresseAnMathe[Studienfach=="Informatik"] <- MatheInfo
InteresseAnMathe[Studienfach=="Mathe"] <- MatheMathe


#War jemand im Mathe LK?
##Fuer jeden Studiengang werden eigene Werte gezogen 
MatheLK <- 1:100
MatheLK[Studienfach=="Mathe"] <- "TRUE"
MatheLK[Studienfach=="Data Science"] <- sample(c(TRUE, TRUE, TRUE, FALSE), 38, replace = TRUE)
MatheLK[Studienfach=="Statistik"]<- sample(c(TRUE, TRUE, TRUE, FALSE), 35, replace = TRUE)
MatheLK[Studienfach=="Informatik"]<- sample(c(TRUE, FALSE), 22, replace = TRUE)


#.csv erstellen 
Daten <- data.frame(Alter, Studienfach, InteresseAnMathe, InteresseAnProgrammieren, MatheLK)
write.csv(Daten, "githubDaten.csv")

read.csv("githubDaten.csv", sep = ",")

#a)
#b)
str(Daten)
ktgo_var <- factor(Daten$Studienfach)

#b)
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


#c)

functiona <- function(x,y){
  korrelation <- cor(x,y)
  kovarianz <- cov(x,y)
  Ergebnisse <- data.frame(korrelation, kovarianz)
  print(Ergebnisse)
}

functiona(as.numeric(ktgo_var), as.numeric(Daten$InteresseAnMathe))
functiona(as.numeric(Daten$InteresseAnProgrammieren), as.numeric(Daten$InteresseAnMathe))
functiona(as.numeric(ktgo_var), as.numeric(Daten$InteresseAnProgrammieren))
