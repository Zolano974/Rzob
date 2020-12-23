#theming colors

color_dark_grey <- function(){
  return("#1c1f24")
}

color_dark_purple <- function(){
  return("#343434")
}

color_generic_stats <- function(){
  return("#3648bf")
}

palette_matta <- function(){
  return("BuPu")
}

palette_fractures <- function(){
  return("RdBu")
}

palette_radios <- function(){
  return("Spectral")
}

color_boxplot_scores <- function(scorename){
  if(scorename == "Oxford"){
     return("#008f24")
  }
  else if(scorename == "Womac"){
    return("#00158f");
  }
  else if(scorename == "HarrisHS"){
    return("#8f0000");
  }
  else if(scorename == "PMA"){
    return("#cf7113");
  }
  
  return("#333333");
  
}

fill_boxplot_scores <- function(scorename){
  if(scorename == "Oxford"){
     return("#bdffcd")
  }
  else if(scorename == "Womac"){
    return("#a3b1ff");
  }
  else if(scorename == "HarrisHS"){
    return("#ffabab");
  }
  else if(scorename == "PMA"){
    return("#ffddba");
  }
  
  return("#DDDDDD");
  
}