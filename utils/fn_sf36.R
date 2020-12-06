calculate_sf36_means <- function(dataset){
  
  mean_sf36_fonctionphysique = round(mean(dataset$fonctionphysique), 2)
  mean_sf36_limitationphysique = round(mean(dataset$limitationphysique), 2)
  mean_sf36_limitationemotionnelle = round(mean(dataset$limitationemotionnelle), 2)
  mean_sf36_energiefatigue = round(mean(dataset$energiefatigue), 2)
  mean_sf36_bienetre = round(mean(dataset$bienetre), 2)
  mean_sf36_fonctionsociale = round(mean(dataset$fonctionsociale), 2)
  mean_sf36_douleur = round(mean(dataset$douleur), 2)
  mean_sf36_santegenerale = round(mean(dataset$santegenerale), 2)
  mean_sf36_modificationsantee = round(mean(dataset$modificationsante), 2)
  
  sf36_means <- hash()
                .set(sf36_means,
                  fonctionphysique=mean_sf36_fonctionphysique,
                  limitationphysique=mean_sf36_limitationphysique,
                  limitationemotionnelle=mean_sf36_limitationemotionnelle,
                  energiefatigue=mean_sf36_energiefatigue,
                  bienetre=mean_sf36_bienetre,
                  fonctionsociale=mean_sf36_fonctionsociale,
                  douleur=mean_sf36_douleur,
                  santegenerale=mean_sf36_santegenerale,
                  modificationsante=mean_sf36_modificationsantee
                )
    return(sf36_means)
}