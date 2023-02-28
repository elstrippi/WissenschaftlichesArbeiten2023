#Erwartet als Eingabe eine mindestens ordinal skalierte Variable, und gibt einen
#Faktor aus, bei dem die untersten 25% der vorherigen Daten zu "niedrig", die 
#obersten 25% zu "hoch" und der Rest zu "mittel" umbenannt wurden
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
