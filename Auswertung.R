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


deskStaterstellinator(Daten$Alter,print = TRUE)
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
SatKat(Daten$Studienfach)
#$AnzmAusp
#[1] 4

#$rel.Hauefikeit
#x
#Data Science   Informatik        Mathe    Statistik 
#0.38         0.22         0.05         0.35 

#$Modus
#[1] 0.38

#$Entropie
#[1] 1.757223

#$normierteEntr.
#[1] 0.8786117
bivariate_stats(Daten$InteresseAnMathe,Daten$MatheLK)
#MatheLk.mlkc MatheLk.Freq  mean median min max
#1           Ja           63 25.05  25.05   1  99
#2         Nein           37 20.70  25.05   1 100                
bivariate_stats(Daten$InteresseAnProgrammieren,Daten$MatheLK)
#MatheLk.mlkc MatheLk.Freq mean median min max
#1           Ja           63 25.3   25.3   1  99
#2         Nein           37 21.3   25.3   1 100
kategorisierer3000(Daten$InteresseAnMathe)
#0%  25%  50%  75% 100% 
#1    4    7   36  100 
#41 Levels: 1 < 2 < 3 < 4 < 5 < 6 < 7 < 10 < 17 < 24 < 25 < 26 < 28 < ... < 100
#[1] niedrig mittel  mittel  niedrig mittel  mittel  mittel  mittel  mittel 
#[10] mittel  mittel  mittel  mittel  mittel  mittel  mittel  mittel  mittel 
#[19] mittel  mittel  niedrig mittel  niedrig mittel  mittel  mittel  niedrig
#[28] mittel  mittel  mittel  niedrig mittel  mittel  mittel  niedrig mittel 
#[37] mittel  mittel  mittel  mittel  hoch    mittel  hoch    niedrig hoch   
#[46] niedrig hoch    mittel  mittel  mittel  mittel  mittel  mittel  niedrig
#[55] niedrig hoch    mittel  mittel  mittel  hoch    hoch    hoch    mittel 
#[64] niedrig hoch    mittel  mittel  hoch    hoch    hoch    hoch    hoch   
#[73] mittel  hoch    hoch    mittel  niedrig mittel  mittel  hoch    niedrig
#[82] mittel  hoch    mittel  mittel  mittel  mittel  mittel  hoch    hoch   
#[91] niedrig hoch    hoch    hoch    mittel  niedrig mittel  mittel  hoch   
#[100] hoch   
#Levels: niedrig < mittel < hoch
kategorisierer3000(Daten$InteresseAnProgrammieren)
#0%  25%  50%  75% 100% 
#1    5    6   36  100 
#41 Levels: 1 < 2 < 3 < 4 < 5 < 6 < 7 < 10 < 17 < 24 < 25 < 26 < 28 < ... < 100
#[1] niedrig mittel  mittel  mittel  mittel  mittel  mittel  niedrig niedrig
#[10] mittel  mittel  mittel  mittel  niedrig mittel  mittel  mittel  mittel 
#[19] mittel  mittel  mittel  niedrig mittel  mittel  mittel  mittel  niedrig
##[28] mittel  mittel  niedrig mittel  mittel  mittel  mittel  niedrig mittel 
#[37] mittel  mittel  mittel  mittel  hoch    niedrig hoch    mittel  hoch   
#[46] mittel  hoch    mittel  mittel  niedrig mittel  mittel  mittel  mittel 
#[55] mittel  hoch    mittel  niedrig mittel  hoch    hoch    hoch    mittel 
#[64] niedrig hoch    mittel  mittel  hoch    hoch    hoch    hoch    hoch   
#[73] niedrig hoch    hoch    mittel  mittel  niedrig mittel  hoch    mittel 
#[82] mittel  hoch    mittel  niedrig mittel  mittel  mittel  hoch    hoch   
#[91] mittel  hoch    hoch    hoch    niedrig mittel  niedrig niedrig hoch   
#[100] hoch   
#Levels: niedrig < mittel < hoch
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

#categorical_relationship(subset(Daten, Studienfach == "Mathe")$Studienfach ,subset(Daten, Studienfach == "Mathe")$MatheLK)
#Error in chisq.test(tab) : 'x' muss mindestens 2 Elemente
subset(Daten, Studienfach == "Mathe")$MatheLK
#"True" "True" "True" "True" "True"
categorical_relationship(subset(Daten, Studienfach == "Statistik")$Studienfach ,subset(Daten, Studienfach == "Statistik")$MatheLK)
#X-squared 
#0.2506963 
categorical_relationship(subset(Daten, Studienfach == "Data Science")$Studienfach ,subset(Daten, Studienfach == "Data Science")$MatheLK)
#X-squared 
#0.2490488
categorical_relationship(subset(Daten, Studienfach == "Informatik")$Studienfach ,subset(Daten, Studienfach == "Informatik")$MatheLK)
#X-squared 
#0.1853406
