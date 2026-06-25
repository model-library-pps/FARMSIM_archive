rm(list = ls())
require(FIELD.QUEFTS, lib.loc = dirname(dirname(normalizePath("run_field.r", winslash = "/"))))
root.field <- dirname(dirname(normalizePath("run_field.r", winslash = "/")))

# Set the number of fields on the farm
numberOfFields <- 5
#--Initializing the parameters and state variables
tmp             <- init_param(numberOfFields)
Fields          <- tmp[[1]]
CropParameters  <- tmp[[2]]
numberOfSeasons <- tmp[[3]]
Aux             <- tmp[[4]]
rm(tmp)

#------------------------------------------
cat('Running simulation for', numberOfSeasons, 'seasons, with', numberOfFields, 'fields\n')
#------------------------------------------

Store <- init_store_variables(numberOfFields, numberOfSeasons)
  

for (Season in seq(1, numberOfSeasons, 1)) {

  for (Field in seq(1, numberOfFields, 1)) {
  
    if (Season == 1) { 
      #--Store initial values of variables
      Store <- store_variables(Store, Fields[[Field]]$param, Fields[[Field]]$crop, Fields[[Field]]$fert, 
                               Fields[[Field]]$soil, 1, Field)
    }
    
    #--Select the parameters for the right crop from the rotation scheme
    tmp <- init_crop(Fields[[Field]]$param, Fields[[Field]]$crop, CropParameters, Season, Aux$capture)
    Fields[[Field]]$param <- tmp[[1]]
    Fields[[Field]]$crop  <- tmp[[2]]
    rm(tmp)

    #--Update the organic fertiliser C, N and P pools
    Fields[[Field]]$fert <- update_fertilisers(Fields[[Field]]$fert, Season)

    #--Running the soil C module
    tmp   <- soil_C(Fields[[Field]]$param, Fields[[Field]]$fert, Fields[[Field]]$crop, Fields[[Field]]$soil,
                    Season)
    Fields[[Field]]$param <- tmp[[1]]
    Fields[[Field]]$fert  <- tmp[[2]]
    Fields[[Field]]$crop  <- tmp[[3]]
    Fields[[Field]]$soil  <- tmp[[4]]
    rm(tmp)

    #--Running QUEFTS
    tmp   <- soil_supply (Fields[[Field]]$param, Fields[[Field]]$soil, Season)
    Fields[[Field]]$param <- tmp[[1]]
    Fields[[Field]]$soil  <- tmp[[2]]
    rm(tmp)

    #--Calculate nutrients available for plant uptake
    #Nitrogen
    tmp   <- available_N (Fields[[Field]]$soil, Fields[[Field]]$fert, Fields[[Field]]$crop, Season)
    Fields[[Field]]$fert  <- tmp[[1]]
    Fields[[Field]]$crop  <- tmp[[2]]
    Fields[[Field]]$soil  <- tmp[[3]]
    rm(tmp)

    #Phosporus
    tmp   <- available_P (Fields[[Field]]$soil, Fields[[Field]]$fert, Fields[[Field]]$crop, Season)
    Fields[[Field]]$fert  <- tmp[[1]]
    Fields[[Field]]$crop  <- tmp[[2]]
    Fields[[Field]]$soil  <- tmp[[3]]
    rm(tmp)

    #Potassium
    tmp   <- available_K (Fields[[Field]]$soil, Fields[[Field]]$fert, Fields[[Field]]$crop, Season)
    Fields[[Field]]$fert  <- tmp[[1]]
    Fields[[Field]]$crop  <- tmp[[2]]
    Fields[[Field]]$soil  <- tmp[[3]]
    rm(tmp)

    #--Cropsim
    Fields[[Field]]$crop   <- cropsim (Fields[[Field]]$param, Fields[[Field]]$soil, Aux$meteo, 
                                       Fields[[Field]]$crop, Aux$capture, Season)
                                              
    #--Integrate state variables
    tmp  <- integrate_states (Fields[[Field]]$soil, Fields[[Field]]$crop, Fields[[Field]]$fert, 
                              Fields[[Field]]$param, Season)
    Fields[[Field]]$soil <- tmp[[1]]
    Fields[[Field]]$crop <- tmp[[2]]
    Fields[[Field]]$fert <- tmp[[3]]
    rm(tmp)

    #--Mass balance
    Fields[[Field]]$balance <- balance_check (Fields[[Field]]$balance, Fields[[Field]]$fert, Fields[[Field]]$crop, 
                                              Fields[[Field]]$param, Fields[[Field]]$soil, Season, Field)
    #--Store values of variables
    Store <- store_variables (Store, Fields[[Field]]$param, Fields[[Field]]$crop, Fields[[Field]]$fert, 
                              Fields[[Field]]$soil, Season + 1, Field)  

  }
}

source('select_output.r')
Output <- select_output(Store, numberOfFields, numberOfSeasons)
