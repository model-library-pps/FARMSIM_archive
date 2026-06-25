save_fromField <- function(Store, fields, seasons) {

  # Please adjust this value to the number of variables you wish to store in the output matrix
  nr_of_variables <- 15
  #Output <-matrix(data=NA, nrow=Param$sys$simulationTime + 1, ncol=nr_of_variables)
  Output <- array (data = NA, dim = c(seasons + 1, nr_of_variables, fields))

  for (field in seq (1,fields,1)) {
    # Add variables you wish to save as output here:
    Output[,1,field]  <- Store[[field]]$crop$grainYield
    Output[,2,field]  <- Store[[field]]$crop$stoverYield
    Output[,3,field]  <- Store[[field]]$soil$C$totalSoilC  
    Output[,4,field]  <- Store[[field]]$crop$waterLimitedYield
  }

  return(Output)
}


