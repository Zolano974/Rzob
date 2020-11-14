#pré-requis: importer le fichier Excel (.xslx) dans R, sous le nom de test2.xlsx

#on charge les données 
library(data.table)
install.packages("ggplot2")
install.packages("plotly")

setDT(test1)
donne=test1[!is.na(Oxford)] #Suppression des val a NA
#View(donne) #Vizu des data 

#oxford/age 
# cf methode stat --> on a choisi la methode Spearman
cor.test(x=donne$Oxford,y=donne$age,method = "spearman",alternative = "two.sided")

wilcox.test(donne$Fractcode,donne$Oxford)


#oxford/fracture

# METHODO
    #Oxford: qualitative ordinale
    #Fracture : Qualitative nominale >2 groupes
    # on lit dans le table --> il faut utiliser le q de cochran

# qqchose comme -->
cochran.qtest(Oxford ~ Fracture | Nom, data=donnee);

#friedman : KO c'était une erreur de méthodo (on avait compté "typefracture" comme ordinale)
# friedman.test(Oxford ~ Fracture | Nom,data=donne)

friedman.test(Fracture ~ Oxford | Nom,data=donne) #--> WTF ?


#Boxplot: vizu intéressante mais attention: ça permet d'observer, pas de démontrer !

#boxplot par défaut de merde
boxplot(donne[Fractcode==1]$Oxford)

# Box plot pour comparer les type de fracture au scores (bon boxplot)
# library(plotly)
# fig <- plot_ly(y = ~donne[Fracture=="paroi postérieure"]$Oxford, type = "box")
# fig <- fig %>% add_trace(y = ~donne[Fracture=="transversale"]$Oxford)
# fig <- fig %>% add_trace(y = ~donne[Fracture=="bi colonne"]$Oxford)

# fig
summary(donne[Fracture=="transversale"]$Oxford)


#oxford/luxation
# METHODO
    #Oxford: Qualitative ordinale
    #Luxation: Qualitative nominale 2 groupe (= binaire)
    #on lit dans la table --> il faut utiliser le test de wilcox
wilcox.test(Oxford ~ Luxation, data=donne)

#boxplot par defaut de merde
boxplot(donne[Fractcode==1]$Oxford)