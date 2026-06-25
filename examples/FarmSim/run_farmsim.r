#--Clear the memory
rm(list = ls())
t1 <- proc.time()

#--Load the required packages
require(FIELD.QUEFTS, lib.loc = dirname(normalizePath("run_farmsim.r", winslash = "/")))
require(LivSim, lib.loc = dirname(normalizePath("run_farmsim.r", winslash = "/")))
source('heapsim.r')
source('save_fromLivSim.r')
source('save_fromField.r')

root.field <- dirname(normalizePath("run_farmsim.r", winslash = "/"))
root.livsim <- dirname(normalizePath("run_farmsim.r", winslash = "/"))

#------------------------------------------Initialise FIELD----------------------------------------
#--Set the number of fields on the farm
numberOfFields <- 2   # -
areaPerField   <- c(1.4, 0.8)   # ha field-1
if (length(areaPerField) != numberOfFields) {
  stop("Please make sure that the area of each field has been specified.")
}
#--Initializing the parameters and state variables
tmp             <- init_param (numberOfFields)
Fields          <- tmp[[1]]
CropParameters  <- tmp[[2]]
numberOfSeasons <- tmp[[3]]
Aux             <- tmp[[4]]
rm(tmp)

initialFields <- Fields
#--------------------------------------------------------------------------------------------------

#-----------------------------------------Initialise LivSim----------------------------------------
#--Reading input for LivSim
LivSim <- list()
tmp                       <- input_livsim ()
LivSim$cows 			        <- tmp[[1]]
LivSim$breed	            <- tmp[[2]]
LivSim$param			        <- tmp[[3]]
LivSim$livsimmgmt         <- tmp[[4]]
LivSim$foragequality      <- tmp[[5]]
LivSim$concentratequality <- tmp[[6]]
rm(tmp)

#--Herd management variables
# Variables that should not affect herd management should be set to: Inf
Herd <- list()
Herd$target_herd           <- 4   # target size of the herd
Herd$max_lact              <- 2   # maximum number of lactations before replacement
Herd$max_open              <- 3   # maximum period open before replacement
Herd$replacement_age_bulls <- 2   # age at which bulls are sold
Herd$max_bulls             <- 5   # maximum number of bulls to keep in the herd
Herd$soldAnimals 		       <- vector(mode = 'list')

LivSim$initial_cows <- LivSim$cows
# Here the input of the amount of feed needs to be adjusted. In the current installment more feed is available
# when the herd size increases.
LivSim$feedinput    <- as.matrix(read.csv(paste(root.livsim, "/LivSim/data/FeedInput.csv", sep=""), header = FALSE))

#--Setting auxiliary variables
LivSim$param$sys$ylen 		 	    <- 365
LivSim$param$sys$timestep 	 	  <- LivSim$param$sys$tfactor / LivSim$param$sys$ylen
LivSim$param$sys$simulationstep <- 12
LivSim$param$sys$monthstep      <- LivSim$param$sys$ylen / 12 / LivSim$param$sys$ylen
LivSim$param$sys$nbcows 		    <- ncol (LivSim$initial_cows)
LivSim$param$milk_allowance 	  <- 0.60   # Fraction of the produced milk that is given to the calf. Only applicable to breeds mere and azawak (=BreedID is 4 or 5)
LivSim$param$max_conc        	  <- 0.40   # Maximum contribution of concentrate to diet of cows as a fraction of the roughage dry matter intake
LivSim$param$calving_rate   	  <- 0.95   # Calving rate (per year)
simulationTime                  <- LivSim$param$sys$simulationtime
numberOfTimes                   <- 3

#----------------------------------------Initialise HeapSim----------------------------------------
numberOfMonths <- simulationTime * LivSim$param$sys$simulationstep
Heap <- list(amountDM = vector('numeric', length = numberOfMonths + 1),
             amountC  = vector('numeric', length = numberOfMonths + 1),
             amountN  = vector('numeric', length = numberOfMonths + 1),
             amountP  = vector('numeric', length = numberOfMonths + 1),
             amountK  = vector('numeric', length = numberOfMonths + 1),
             manureApplied = 0)
    
# The fun begins...
# First we create and set a number of auxiliary variables to help control the run and guide the communication between
# FIELD and LivSim 
lengthSeason <- 12 # length of the growing season used for FIELD and therefore the number of months that LivSim should
                   # be run before running FIELD
manureDM.from.livsim <- matrix(data = 0, ncol = numberOfFields, nrow = numberOfSeasons)
drySeason       <- c(1,2,3,4,5,6,7,8,9,10,11,12)             # Define which months form the dry season
lengthDrySeason <- length(drySeason)    # Length of the dry season (months)
applicationFactor <- c(1, 0) # This vector contains numbers indicating which proportion of the heaped material should be applied to each field

# Some checks to determine whether the defined allocation of manure makes any sense.
if (length(applicationFactor) != numberOfFields) {
  stop('Please make sure that the manure application for all fields has been defined.')
}  

if (sum(applicationFactor) > 1) {
  stop('More material seems to be applied to the fields than is available; please check the allocation factor for all fields.')
}

# Check whether the same length of the run has been specified for FIELD and LivSim.
if (simulationTime != numberOfSeasons) {
  stop("Please make sure that the number of seasons specified in FIELD and LivSim match.")
}

# Display some of the most important settings to the user, so they can check whether the model has
# been setup properly.
cat(
"
############################################################################ \n
Simulation started at", format(Sys.time(), "%X"), "on", format(Sys.time(), "%a %d %b %Y"),"\n
Summary of model settings
----------------------------------------------------------------------------
Number of seasons:", numberOfSeasons, "
Number of fields :", numberOfFields, "
Area per field   :", areaPerField, "\n
############################################################################
"
)

for(counter in seq(1, numberOfTimes, 1)) {
  
  cat("Current run is number", counter,".\n")
  
  # The code below should be within a for-loop if we include sensitivity analysis.
  #--Reading initial number of cows
  nbcows        <- LivSim$param$sys$nbcows
  SizeHerd      <- nbcows
  countDead     <- 0
  countReplaced <- 0
  LivSim$cows   <- LivSim$initial_cows
  Fields        <- initialFields
  #--------------------------------------------------------------------------------------------------

  #--Initialise the output matrices for FIELD (1) and LivSim (2)
  Store      <- init_store_variables(numberOfFields, numberOfSeasons) # (1)
  outputCows <- output1(LivSim$cows, LivSim$param)                    # (2)

               
  for (year in seq(1, simulationTime, 1)) {	
    
    Season <- year
    
    #----------------------- Application of manure from the heap to the fields in FIELD ---------------------------------
    
    heapDM <- Heap$amountDM[(Season - 1) * lengthSeason + 1] 
    heapC  <- Heap$amountC[(Season - 1) * lengthSeason + 1] 
    heapN  <- Heap$amountN[(Season - 1) * lengthSeason + 1] 
    heapP  <- Heap$amountP[(Season - 1) * lengthSeason + 1] 
    heapK  <- Heap$amountK[(Season - 1) * lengthSeason + 1]
    
    
    if (heapDM == 0) {
      heapCContent <- 0
      heapNContent <- 0
      heapPContent <- 0
    } else {
      heapCContent <- heapC / heapDM
      heapNContent <- heapN / heapDM
      heapPContent <- heapP / heapDM
    }
    
    # Recalculate C, N and P-content based on amounts of C, N and P in heaped material and manure -> currently for manure 
    # only changes in amount of C over time are calculated, should we add N and P??? 
    

    # The amount of manure applied to the field is determined by the application factor, manure application is corrected for the area of the fields.
    for (i in seq(1, numberOfFields, 1)) { 
      if (applicationFactor[i] >= 0) {
        manureDM.from.livsim[Season, i] <- (heapDM * applicationFactor[i]) / areaPerField[i]
      } else {
        stop('You seem to have allocated a negative amount of manure to this field.')
      }
    }
    
    # The amount of manure applied to the field is subtracted from the amount on the heap
    Heap$manureApplied <- sum(manureDM.from.livsim[Season, ])
    
    #################################################### FIELD ##########################################################
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
      Fields[[Field]]$fert$manure$C <- Fields[[Field]]$fert$manure$C + manureDM.from.livsim[Season, Field] * heapCContent
            
      # Amount of C in manure
      # CContent of manure
      # NContent of manure
      # PContent of manure
      
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
      
      #--Store values of variables
     Store <- store_variables(Store, Fields[[Field]]$param, Fields[[Field]]$crop, Fields[[Field]]$fert, 
                              Fields[[Field]]$soil, Season + 1, Field)
    }
    
    # Before we go to the next timestep we need to take some of the output from FIELD and use it
    # as input for LivSim. The total amount of crop residues, corrected for the fraction of the residues
    # that is left in the field, should be used over the coming months to sustain the livestock until 
    # the next harvest. 
    
    # Crop residues are taken from the field(s) and fed to the cattle, the amount of feed is corrected for 
    # the area of the fields
    feed.from.field <- vector(mode = 'numeric', length = numberOfFields)
    for (i in seq(1, numberOfFields, 1)) {
      feed.from.field[i] <- Fields[[i]]$crop$stoverYield * (1 - Fields[[i]]$param$residuesLeftInField[Season]) * areaPerField[i]
    }
    totalFeed     <- sum(feed.from.field)
      
    # Feed quality: make sure that the quality parameters of one of the forages (feed id's 1-3) is set properly and
    # that the feed from crop residues is added to the right feed id.
    
    #################################################### LivSim #########################################################
    for (simtime in seq(1, lengthSeason, 1)) { # months
      timenr <- simtime + (year - 1) * LivSim$param$sys$simulationstep		
      
      number_of_cows_beforenewcalves <- ncol(LivSim$cows)

      for (cownr in 1:number_of_cows_beforenewcalves) {							
        Cow <- LivSim$cows[,cownr]
              
        if (Cow$char$death == 0 && Cow$char$replaced == 0) {
          Diet            <- define_diet (Cow, LivSim$feedinput, LivSim$foragequality, LivSim$concentratequality, simtime)								
          # During the dry season feed from FIELD is added to the diet of the animals
          if(simtime %in% drySeason) {
            # The total amount of feed available is divided equally between the months of the dry season and the animals in the herd
            feedPerAnimal   <- (totalFeed / lengthDrySeason) / SizeHerd
            totalFeed       <- totalFeed - feedPerAnimal
            Diet$foragequantity[1] <- Diet$foragequantity[1] + feedPerAnimal # <- forage id 1 is used for the maize stover coming from field
          }
          # Run LivSim
          tmp          <- livsim (Cow, LivSim$cows, LivSim$param, LivSim$livsimmgmt, LivSim$breed, Diet, nbcows)
          Cow_new      <- tmp[[1]]
          New_cow      <- tmp[[2]]
          nbcows       <- tmp[[3]]
          Feed         <- tmp[[4]]
          LivSim$Param <- tmp[[5]]
          rm(tmp)				
        } else if (Cow$char$death==1) {				
          Cow_new      <- define_Cow_new ()				
          Cow_new      <- cow_dead (Cow, Cow_new)
          countDead    <- countDead + 1
          SizeHerd     <- SizeHerd - 1				
        } else if (Cow$char$replaced == 1) {				
          Cow_new        <- define_Cow_new ()				
          Cow_new        <- cow_replaced (Cow, Cow_new)
          countReplaced  <- countReplaced + 1
          SizeHerd       <- SizeHerd - 1
        } else {
          Cow_new <- Cow
        }

        LivSim$cows[,cownr] <- Cow_new

       # Update the status of the herd			
        tmp <- update_herd (Cow, LivSim$cows, New_cow, SizeHerd, Herd)
        LivSim$cows      <- tmp[[1]]
        Herd             <- tmp[[2]]
        New_cow          <- tmp[[3]]
        SizeHerd         <- tmp[[4]]
        rm(tmp)

        
        outputCows <- output(cownr, timenr, LivSim$cows, outputCows)			
      }

      nbcows <-ncol(LivSim$cows)

      if (nbcows > number_of_cows_beforenewcalves) {
        for (cownr in (number_of_cows_beforenewcalves + 1):nbcows) {
          outputCows <- output_new (cownr, timenr, LivSim$cows, outputCows)
        }
      }
      
      # After LivSim has been run for all animals in the herd, the status of the heap is updated.      
      Heap <- heapsim (Heap, outputCows, timenr, Season, lengthSeason)  
    }
    
  }  

  # Storing a selection from the output and saving it in a temporary file  
  fromLivSim <- save_fromLivSim (outputCows, fromLivSim)
  fullname <- paste("tempLivSim", counter, sep="")
  save(fromLivSim, file=fullname)	
  
  fromField <- save_fromField(Store, numberOfFields, numberOfSeasons)
  fullname <- paste("tempFIELD", counter, sep="")
  save(fromField, file=fullname)
}

# The data in the temporary files are combined into a list and
# saved in a single output file
save_LivSim <- vector('list')
for (i in 1:numberOfTimes) {
  name <- paste("tempLivSim", i, sep="")
  load(name)
  save_LivSim[[i]] <- fromLivSim
}
save(save_LivSim, file="save_LivSim")

save_FIELD <- vector('list')
for (i in 1:numberOfTimes) {
  name <- paste("tempFIELD", i, sep="")
  load(name)
  save_FIELD[[i]] <- fromField
}
save(save_FIELD, file="save_FIELD")

# Deleting the generated temporary files
for (i in 1:numberOfTimes) {
  name <- paste("tempLivSim", i, sep = "")
  file.remove(name)
  name <- paste("tempFIELD", i, sep = "")
  file.remove(name)
}

run.time <- (proc.time() - t1)[3]
cat("Run executed in", run.time, "seconds\n")
