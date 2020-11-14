library(data.table)
setDT(test3_matta)
donne=test3_matta[!is.na(Oxford)] #Suppression des val a NA
View(donne) #Vizu des data 

#???Oxford / Matta

cor.test(formula=~Oxford+Matta,data=donne,method = "spearman",alternative = "greater")

#Matta/matta 3 mois : variabilité intra-observateur

cor.test(formula=~Matta+Matta3mois,data=donne,method = "spearman",alternative = "greater")
