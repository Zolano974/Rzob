#Functions related to Fracture : 
# Fracture repartition by age, traumatisme

source("utils/theming_plots.R")
#
# Generate stacked histogram to display the repartition of fracture types according to trauma
#
generate_plot_fracture_by_traumatisme <- function(dataset) {
  
  #defining order for "Traumatisme"  (/!\ if orientation=x order needs to be reversed)
  dataset$Traumatisme <- factor(
    dataset$Traumatisme, 
    levels=c(
      "AVP",
      "Chute", 
      "Sport",
      "Agricole" 
    )
  )

  plot = dataset %>%
    # ggplot(      aes(y=Traumatisme, fill=Fracture, order=Fracture)) +
    ggplot(      aes(x=Traumatisme, fill=factor(
          Fracture, 
          #here we define the order of the stacked values
          levels=c(
            "T",
            "colonne antérieure",
            "transversale",
            "paroi postérieure", 
            "transversale et paroi postérieure",
            "bi colonne"
          )
        )
      )
    ) +
    geom_bar(
      position="fill",
      stat='count', 
      orientation="x", 
      alpha=0.9,
      colour="white"
    ) +
    scale_y_continuous(labels=scales::percent) +
    labs (
      title= "Répartition des fractures en fonction du type de trauma",
      y="",
      x="Type de traumatisme",
      fill="Fracture"
    ) +
    scale_fill_brewer(palette="RdBu") +
    default_theming(
      y_title_angle = 90,
      title_size = 16,
      legend_title_size = 14
    )
  
  return(plot)

}

#
# Generate stacked histogram to display the repartition of fracture types according to age
#

generate_plot_fracture_by_age <- function(dataset){
  
  plot = dataset %>%
    ggplot(aes(x=Age, fill=factor(
          Fracture, 
          #here we define the order of the stacked values
          levels=c(
            "T",
            "colonne antérieure",
            "transversale",
            "paroi postérieure", 
            "transversale et paroi postérieure",
            "bi colonne"
          )
        )
      )
    ) +
    geom_histogram(
      position="fill",
      binwidth=15,
      orientation="x",
      colour="white"
    ) +
    scale_y_continuous(labels=scales::percent) +
    labs (
      title= "Répartition des fractures en fonction de l'âge",
      x="Classe d'âge",
      y="",
      fill="Fracture"
    ) +
    scale_fill_brewer(palette="RdBu") +
    default_theming(
      y_title_angle = 90,
      title_size = 16,
      legend_title_size = 14
    )
  
  return(plot)
}

generate_histogram_fracture_expchir <- function(dataset){
  
  #order by chirurgien custom
  dataset$Chirurgien <- factor(
    dataset$Chirurgien, 
    levels=c(
      "Junior", 
      "Senior",
      "Expérimenté"
    )
  )
  

  plot = dataset %>%
    ggplot(aes(x=Chirurgien, fill=factor(
      Fracture, 
      #here we define the order of the stacked values
      levels=c(
        "T",
        "colonne antérieure",
        "transversale",
        "paroi postérieure", 
        "transversale et paroi postérieure",
        "bi colonne"
      )
    )
    )) +
    geom_bar(
      stat='count',
      position="fill",
      orientation="x",
      colour="white"
    ) +
    scale_y_continuous(labels=scales::percent) +
    labs (
      title= "Type de Fracture en fonction de l'expérience du chirurgien",
      fill= "Fracture",
      x="Expérience du chirurgien",
      y=""
    ) +
    scale_fill_brewer(palette="RdBu") +
    default_theming(
      x_text_angle=45,
      x_text_vjust = 0.5,
      y_text_size=10,
      y_title_vjust = 0.5,
      title_size = 16
    )
  
  return(plot)
}



my_sandbox <- function(){

  mydata3 = ReccueilR[!is.na(Chirurgien)&!is.na(Fracture)]
  # View(mydata3)

  count_fractures_by_exp_chir("bi colonne","Expérimenté",  mydata3 )

  # length(unique(mydata3$Fracture)

  # count_fractures_by_exp_chir("Junior", "T", mydata3 )


  #liste des distinct values CHirurhien, qui se répètent autant qu'il y a de factures
  Chirurgien = c(rep(unique(mydata3$Chirurgien), each=length(unique(mydata3$Fracture))))
  Fracture

  Fracture = c(rep(unique(mydata3$Fracture), times=length(unique(mydata3$Chirurgien))))
  listTypeFracture

  # ZobTypeFracture = c(rep(c("T", "Colonne antérieure"), times = 3)
  Frequency <- c(
    #Junior
    percentage_fractures_by_expchir("Junior", "T", mydata3 ),
    percentage_fractures_by_expchir("Junior", "colonne antérieure", mydata3 ),
    percentage_fractures_by_expchir("Junior", "transversale", mydata3 ),
    percentage_fractures_by_expchir("Junior", "paroi postérieure", mydata3 ),
    percentage_fractures_by_expchir("Junior", "transversale et paroi postérieure", mydata3 ),
    percentage_fractures_by_expchir("Junior", "bi colonne", mydata3 ),
    #Senior
    percentage_fractures_by_expchir("Senior", "T", mydata3 ),
    percentage_fractures_by_expchir("Senior", "colonne antérieure", mydata3 ),
    percentage_fractures_by_expchir("Senior", "transversale", mydata3 ),
    percentage_fractures_by_expchir("Senior", "paroi postérieure", mydata3 ),
    percentage_fractures_by_expchir("Senior", "transversale et paroi postérieure", mydata3 ),
    percentage_fractures_by_expchir("Senior", "bi colonne", mydata3 ),
    #Expérimenté
    percentage_fractures_by_expchir("Expérimenté", "T", mydata3 ),
    percentage_fractures_by_expchir("Expérimenté", "colonne antérieure", mydata3 ),
    percentage_fractures_by_expchir("Expérimenté", "transversale", mydata3 ),
    percentage_fractures_by_expchir("Expérimenté", "paroi postérieure", mydata3 ),
    percentage_fractures_by_expchir("Expérimenté", "transversale et paroi postérieure", mydata3 ),
    percentage_fractures_by_expchir("Expérimenté", "bi colonne", mydata3 )
  )

  zobdata = data.frame(Chirurgien, Fracture, Frequency)


  # qplot(aes(x=Chirurgien, y=Frequency, fill=Frequency), data = zobdata, geom = "bar", fill = Category,     theme_set(theme_bw()))
  zobdata$Chirurgien <- factor(
    zobdata$Chirurgien, 
    levels=c(
      "Junior", 
      "Senior",
      "Expérimenté"
    )
  )

  zobdata %>%
    ggplot(aes(x=Chirurgien, y=factor(Frequency), fill=factor(
      Fracture,
      #here we define the order of the stacked values
      levels=c(
        "T",
        "colonne antérieure",
        "transversale",
        "paroi postérieure",
        "transversale et paroi postérieure",
        "bi colonne"
      )
    ))) +
    # scale_y_discrete(labels=scales::percent) +
    geom_bar(
      # position="fill",
      stat='identity',
      orientation="x",
      colour="white"
    ) +
    labs (
      title= "Type de Fracture en fonction de l'expérience du chirurgien",
      fill= "Fracture",
      x="Expérience du chirurgien",
      y=""
    ) +
    scale_fill_manual(values = c("#99ccff", "#ff6666", "#DCDCDD", "#FF6600", "#e4b671", "#ecec56")) +
    geom_text(aes(label = Frequency), size = 2, hjust = 0.5, position = position_stack(vjust = 0.5))
  
}
