set.seed(420)
Alter <- rnorm(100, 25, 4)
Studienfach <- sample(c(rep("Statistik", 3), rep("Data Science", 3), rep("Informatik", 2), "Mathe"), 100, replace = TRUE)


ProgMathe <- sample(1:7, 5, replace = TRUE)
ProgInfo <- sample(5:7, 22, replace = TRUE)
ProgData <- sample(3:7, 38, replace = TRUE)
ProgStat <- sample(3:7, 35, replace = TRUE)

InteresseAnProgrammieren <- 1;100
InteresseAnProgrammieren[Studienfach=="Statisik"] <- ProgStat
InteresseAnProgrammieren[Studienfach=="Data Science"] <- ProgData
InteresseAnProgrammieren[Studienfach=="Informatik"] <- ProgInfo
InteresseAnProgrammieren[Studienfach=="Mathe"] <- ProgMathe


MatheMathe <- sample(5:7, 5, replace = TRUE)
MatheInfo <- sample(1:7, 22, replace = TRUE)
MatheData <- sample(3:7, 38, replace = TRUE)
MatheStat <- sample(3:7, 35, replace = TRUE)

InteresseAnMathe <- 1;100
InteresseAnMathe[Studienfach=="Statisik"] <- MatheStat
InteresseAnMathe[Studienfach=="Data Science"] <- MatheData
InteresseAnMathe[Studienfach=="Informatik"] <- MatheInfo
InteresseAnMathe[Studienfach=="Mathe"] <- MatheMathe

MatheLK <- 1:100
MatheLK[Studienfach=="Mathe"] <- "True"
MatheLK[Studienfach=="Data Science"] <- sample(c(TRUE, TRUE, TRUE, FALSE), 38, replace = TRUE)
MatheLK[Studienfach=="Statistik"]<- sample(c(TRUE, TRUE, TRUE, FALSE), 35, replace = TRUE)
MatheLK[Studienfach=="Informatik"]<- sample(c(TRUE, FALSE), 22, replace = TRUE)



write.csv(Daten, "githubDaten.csv")
