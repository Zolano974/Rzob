# TODO

# Arthrose : 
#   

#
#   -> automate graphs generation
#
#   -> AUTOMATE TABLE EXPORTS
#
#   -> regrouper (1,2) et (3,4) 


  # -> correlation MATTA, type de fracture
  # -> correlation Resultats Radios (arthrose), type de fracture
  # -> correlation chaque score, type de fracture


# 
# Comparaison perte de sang entre 2 ou 4 main :
#   
#   
#   HYPOTHESE A : a 4 mains la durée est plus courte
# 
# 
# HYPOTHESE B : a 4 mains la perte de sang est moindre
# 
# On va vouloir utiliser un Student (T-test), mais on a des préconditions :
#   
#   0) on calcule la moyenne de chaque groupe (hypothèse A ou B)
# 
# 1) test égalité des variances (variable A ou B) : fisher test
# 
# dans notre cas : 0.22 ==> c'est good on a homoscédaticité (les variances ne diffèrent pas significativement (ce serait le cas si <0.05)'
# 
# ==> on va donc pouvoir appliquer notre T-test
# 
# student.test( serie1, serie1, mode=oneTail, homeosedastic=True) #onetail : tu présuppose une supérioté d'un des 2 groupes; twotails: tu penses qu'ils sont différents mais n'oriente pas



