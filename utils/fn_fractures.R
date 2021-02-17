#Functions related to Fracture : 
# Fracture repartition by age, traumatisme

source("utils/theming_plots.R")


factor_fracture_x_levels <- function(){
  return(c(
      "PP", 
      "CA",
      "TR",
      "T",
      "TR-PP",
      "BC"
    )
  )
}

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
    ggplot(
      aes(
        x=Traumatisme,
        y = (..count..)/sum(..count..),
        fill=factor(
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
      position="stack",
      orientation="x", 
      alpha=0.8,
      colour="white",
      width=0.7
    ) +
    labs (
      title= "",
      y="",
      x="Type de traumatisme",
      fill="Fracture"
    ) +
    stat_count(
      geom = "text",
      position="stack",
      aes(label = ..count..),
      size=2.5,
      vjust = 1.8
    ) +      
    scale_fill_brewer(palette=palette_fractures()) +
    default_theming(
      y_title_angle = 90,
      y_text_size = 0,
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
    ggplot(
      aes(
        x=Age, 
        fill=factor(
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
      position="stack",
      binwidth=10,
      orientation="x",
      colour="white",
      alpha=0.8
    ) +
    scale_x_continuous(breaks=c(20,30,40,50,60,70),labels=c("10-20","20-30","30-40","40-50","50-60","60-70")) +
    labs (
      title= "",
      x="Classe d'âge",
      y="",
      fill="Fracture"
    ) +
    # stat_count(
    #   geom = "text",
    #   position="stack",
    #   aes(label = ..count..),
    #   size=3,
    #   vjust = 1
    # ) +    
    scale_fill_brewer(palette=palette_fractures()) +
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
    ggplot(
      aes(
        x=Chirurgien, 
        y = (..count..)/sum(..count..),
        fill=factor(
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
      position="stack",
      orientation="x",
      colour="white",
      width=0.7,
      alpha=0.8
    ) +
    # scale_y_continuous(labels=scales::percent) +
    labs (
      title= "",
      fill= "Fracture",
      x="Expérience du chirurgien",
      y=""
    ) +
    stat_count(
      geom = "text",
      position="stack",
      # aes(label = paste(round((..count..)/sum(..count..)*100), "%")),
      aes(label = ..count..),
      size=2.5,
      vjust = 2
    ) +        
    scale_fill_brewer(palette=palette_fractures()) +
    default_theming(
      x_text_angle=45,
      x_text_vjust = 0.5,
      y_text_size=0,
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
      colour="white",
      width=0.4
    ) +
    labs (
      title= "",
      fill= "Fracture",
      x="Expérience du chirurgien",
      y=""
    ) +
    scale_fill_manual(values = c("#99ccff", "#ff6666", "#DCDCDD", "#FF6600", "#e4b671", "#ecec56")) +
    geom_text(aes(label = Frequency), size = 2, hjust = 0.5, position = position_stack(vjust = 0.5))
  
}
