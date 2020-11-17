#theming of plots
source("utils/theming_colors.R")

default_theming <- function( 
    legend_position = "right",
    legend_title_size= 18,
    x_text_angle=0,
    x_text_size=12, 
    x_text_vjust=0,
    x_title_angle=0, 
    x_title_size=15, 
    x_title_vjust=0,
    y_text_angle=0,
    y_text_size=12,
    y_text_vjust=0,
    y_title_angle=0, 
    y_title_size=15,
    y_title_vjust=0
  ){
  theme = theme(
  
    #background
    panel.background= generate_background_panel(),
    panel.border= generate_panel_border(),
    panel.grid = element_line(),
    
    #legend
    legend.title = element_text(
      size=legend_title_size,
      colour = "black"
    ),
    legend.position = legend_position,
    
    #X
    #title
    axis.title.x =  element_text(
      size=x_title_size, 
      angle=x_title_angle,
      colour= color_dark_purple(),
      vjust=x_title_vjust
    ),
    #text
    axis.text.x =  element_text(
      size=x_text_size, 
      angle = x_text_angle,
      colour = color_dark_grey(),
      vjust=x_text_vjust
    ),
    
    #Y
    #title
    axis.title.y = element_text(
      size=y_title_size, 
      angle=y_title_angle,
      colour= color_dark_purple(),
      vjust=y_title_vjust
    ),
    #test
    axis.text.y = element_text(
      size=y_text_size,
      angle = y_text_angle,
      colour = color_dark_grey(),
      vjust=y_text_vjust
    ),
  )
  return(theme)
}

generate_background_panel <- function(){
  return(
    element_rect(
      fill="white",
      colour="black",
      size = 0.5,
      linetype = "solid"
    )
  )
}

generate_panel_border <- function(){
  return(
    element_rect(
      fill = NA,
      colour="black",
      size = 0.5,
      linetype = "solid"
    )
  )
}