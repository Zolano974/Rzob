
count_fractures_by_exp_chir <- function( expchir, fracture, dataset){
  
  t_fracture <- quote(fracture)
  t_expchir <- quote(expchir)
  
  return(nrow(dataset[Chirurgien == eval(t_expchir) & Fracture == eval(t_fracture)]))
}

percentage_fractures_by_expchir <- function (expchir, fracture, dataset){
  
  # return(count_fractures_by_exp_chir(expchir, fracture, dataset))
  
  total = nrow(dataset)
  
  occurrences = count_fractures_by_exp_chir(expchir, fracture, dataset)
  
  if(occurrences == 0){
    return(0)
  }
  
  return(round((total / occurrences), 2) )

}