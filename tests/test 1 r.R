#pré-requis: importer le fichier Excel (.xslx) dans R, sous le nom de test1.xlsx

#on charge les données 
library(data.table)
setDT(test1)

#on vire les lignes auxquelles manque le Oxford OU l'autotest
mydata=test1[!is.na(Oxford)&!is.na(Autotest)] 

#permet d'afficher les données visuellement (1 fois)
# View(mydata)

#calcul de corrélation entre Auto Test et Oxford
# METHODO
    #oxford: qualitative ordinale (car c'est un score)
    #Auto Test: qualitative ordinale (idem)

# alternative: "greater" si les 2 variables vont dans le meme sens, "lesser" sinon

#Spearman
cor.test(x=mydata$`Auto-test`,y=mydata$Oxford,method = "spearman",alternative = "greater")
#Pearson
cor.test(x=mydata$`Auto-test`,y=mydata$Oxford,method = "pearson",alternative = "greater")
#Kendall
cor.test(x=mydata$`Auto-test`,y=mydata$Oxford,method = "kendall",alternative = "greater")

cor.test(formula=~Oxford+Autotest,data=mydata,method = "spearman",alternative = "greater")
