#install.packages("moments", "ggplot2","gridExtra")
library(moments)
library(ggplot2)
library(gridExtra)
source("Aufgabe3a.R")
source("Aufgabe3b.R")
source("Aufgabe3c.R")
source("Aufgabe3d.R")
source("Aufgabe3e.R")
source("Aufgabe3f.R")
Daten <- read.csv("githubDaten.csv", header = TRUE)


d2= replace(D,D == "True",TRUE)

bivariate_stats(Daten$InteresseAnMathe,d2)
#MatheLk.mlkc MatheLk.Freq  mean median min max
#1           Ja           63 25.05  25.05   1  99
#2         Nein           37 20.70  25.05   1 100
bivariate_stats(Daten$InteresseAnProgrammieren,d2)
#MatheLk.mlkc MatheLk.Freq mean median min max
#1           Ja           63 25.3   25.3   1  99
#2         Nein           37 21.3   25.3   1 100
deskStaterstellinator(Daten$Alter, print = TRUE)
#Deskriptive Maße
#-----------------------------
#  Durchschnitt       24.69245
#Varianz            3.787929
#Standardabweichung 1.94626
#Median             25.02333
#Minimum            19.10242
#Maximum            28.84016
#Schiefe            -0.4124345
#Wölbung            3.01862
deskStaterstellinator(Daten$InteresseAnMathe, print = TRUE)
#Deskriptive Maße
#----------------------------
#  Durchschnitt       23.44
#Varianz            911.4812
#Standardabweichung 30.19075
#Median             7
#Minimum            1
#Maximum            100
#Schiefe            1.287875
#Wölbung            3.129802

deskStaterstellinator(Daten$InteresseAnProgrammieren, print = TRUE)
#Deskriptive Maße
#-----------------------------
##  Durchschnitt       23.82
#Varianz            896.6743
#Standardabweichung 29.94452
#Median             6.5
###Minimum            1
#Maximum            100
#Schiefe            1.297431
#Wölbung            3.15324
multi_katplotter(data.frame(Daten$Studienfach,Daten$InteresseAnMathe,Daten$MatheLK))
categorical_relationship(subset(Daten, Studienfach == "Mathe")$Studienfach ,subset(Daten, Studienfach == "Mathe")$MatheLK)
#Error in chisq.test(tab) : 'x' muss mindestens 2 Elemente
#Also haben alle 
categorical_relationship(subset(Daten, Studienfach == "Statistik")$Studienfach ,subset(Daten, Studienfach == "Statistik")$MatheLK)
#X-squared 
#0.2506963 
categorical_relationship(subset(Daten, Studienfach == "Data Science")$Studienfach ,subset(Daten, Studienfach == "Data Science")$MatheLK)
#X-squared 
#0.2490488
categorical_relationship(subset(Daten, Studienfach == "Informatik")$Studienfach ,subset(Daten, Studienfach == "Informatik")$MatheLK)
#X-squared 
#0.1853406
