set.seed(420)
Alter <- rnorm(100, 25, 4)
Studienfach <- sample(c(rep("Statistik", 3), rep("Data Science", 3), rep("Informatik", 2), "Mathe"), 100, replace = TRUE)
InteresseAnMathe <- sample(1:7, 100, replace = TRUE)
InteresseAnProgrammieren <- sample(1:7, 100, replace = TRUE)
MatheLK <- sample(c(TRUE, FALSE), 100, replace=TRUE)

Daten <- data.frame(Alter, Studienfach, InteresseAnMathe, InteresseAnProgrammieren, MatheLK)

Daten$InteresseAnMathe[which(Daten$Studienfach=="Mathe")] <- Daten$InteresseAnMathe[which(Daten$Studienfach=="Mathe")] + 2
Daten$InteresseAnMathe[which(Daten$InteresseAnMathe > 7)] <- 7

Daten$InteresseAnProgrammieren[which(Daten$Studienfach=="Informatik")] <- Daten$InteresseAnProgrammieren[which(Daten$Studienfach=="Informatik")] + 2
Daten$InteresseAnProgrammieren[which(Daten$InteresseAnMathe > 7)] <- 7

mean(Daten$InteresseAnMathe[which(Daten$Studienfach=="Mathe")])
mean(Daten$InteresseAnMathe[which(Daten$Studienfach=="Informatik")])
mean(Daten$InteresseAnMathe[which(Daten$Studienfach=="Data Science")])
mean(Daten$InteresseAnMathe[which(Daten$Studienfach=="Statistik")])


mean(Daten$InteresseAnProgrammieren[which(Daten$Studienfach=="Mathe")])
mean(Daten$InteresseAnProgrammieren[which(Daten$Studienfach=="Informatik")])
mean(Daten$InteresseAnProgrammieren[which(Daten$Studienfach=="Data Science")])
mean(Daten$InteresseAnProgrammieren[which(Daten$Studienfach=="Statistik")])


write.csv(Daten, "githubDaten.csv")
