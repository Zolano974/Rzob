library(data.table)
setDT(test_4_complics)
donne=test_4_complics[!is.na(Oxford)] #Suppression des val a NA
View(donne) #Vizu des data 


#METHODO : 
wilcox.test(Oxford ~ Déficitneurologique, data=donne)

wilcox.test(Oxford ~ PTH, data=donne)

wilcox.test(Oxford ~ Infection, data=donne)
wilcox.test(Oxford ~ total, data=donne)
