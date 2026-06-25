rm(list=ls())
# start timer
t1 <- proc.time()

root.livsim <- dirname(dirname(normalizePath("run_livsim_sensitivity.r",winslash="/")))
require(LivSim, lib.loc = root.livsim)

LivSim <- list()
#-------------------Reading input for LivSim-------------------
tmp <- input_livsim()
LivSim$cows 			         <- tmp[[1]]
LivSim$breed			         <- tmp[[2]]
LivSim$param			         <- tmp[[3]]
LivSim$livsimMgmt		       <- tmp[[4]]
LivSim$forageQuality	     <- tmp[[5]]
LivSim$concentrateQuality  <- tmp[[6]]
rm(tmp)

Initial_Cows <- LivSim$cows

# Feed input
LivSim$feedInput <- read.csv("data/FeedInput.csv", header=FALSE)

#-----------------------Herd management variables--------------------------
# Variables that should not affect herd management should be set to: Inf
Herd <- list()
Herd$target_herd           <- 4   # target size of the herd
Herd$max_lact              <- 2   # maximum number of lactations before replacement
Herd$max_open              <- 3   # maximum period open before replacement
Herd$replacement_age_bulls <- 2   # age at which bulls are sold
Herd$max_bulls             <- 5   # maximum number of bulls to keep in the herd
Herd$soldAnimals 		       <- vector(mode = 'list')

#--------------------------------Setting auxiliary variables-------------------------
LivSim$param$sys$ylen 		 	    <- 365
LivSim$param$sys$timestep 	 	  <- LivSim$param$sys$tfactor / LivSim$param$sys$ylen
LivSim$param$sys$simulationstep <- 12
LivSim$param$sys$monthstep      <- LivSim$param$sys$ylen / 12 / LivSim$param$sys$ylen
LivSim$param$sys$nbcows 		    <- ncol(Initial_Cows)
LivSim$param$milk_allowance 	  <- 0.60   # Fraction of the produced milk that is given to the calf. Only applicable to breeds mere and azawak (=BreedID is 4 or 5)
LivSim$param$max_conc        	  <- 0.40   # Maximum contribution of concentrate to diet of cows as a fraction of the roughage dry matter intake
LivSim$param$calving_rate   	  <- 0.95   # Calving rate (per year)
counter              	          <- 1      # Internal variable      
numberoftimes        	          <- 30     # Number of iterations for the sensitivity run
#------------------------------Sourcing necessary function---------------------------

for (counter in seq(1, numberoftimes, 1)) {
  
  cat("Current run is number", counter, "\n")

  if (exists("outputCows")) {
    rm(outputCows)
  }

  #Reading initial number of cows
  nbcows         <- LivSim$param$sys$nbcows
  SizeHerd       <- nbcows
  count_dead     <- 0
  count_replaced <- 0
  LivSim$cows    <- Initial_Cows

  # Initialise the output matrix
  outputCows <- output1(LivSim$cows, LivSim$param)

  for (year in 1:LivSim$param$sys$simulationtime) {	
  
    for (simtime in 1:12) { # months
      timenr <- simtime + (year - 1) * LivSim$param$sys$simulationstep		
      number_of_cows_beforenewcalves <- ncol(LivSim$cows)

      for (cownr in 1:number_of_cows_beforenewcalves) {							
        Cow <- LivSim$cows[,cownr]
      
	    if (Cow$char$death == 0 && Cow$char$replaced == 0) {
          Diet         <- define_diet (Cow, LivSim$feedInput, LivSim$forageQuality, LivSim$concentrateQuality, simtime)								
          tmp          <- livsim (Cow, LivSim$cows, LivSim$param, LivSim$livsimMgmt, LivSim$breed, Diet, nbcows)
          Cow_new      <- tmp[[1]]
          New_cow      <- tmp[[2]]
          nbcows       <- tmp[[3]]
          Feed         <- tmp[[4]]
          LivSim$param <- tmp[[5]]
          rm(tmp)				
        } else if (Cow$char$death==1) {				
          Cow_new      <- define_Cow_new ()				
          Cow_new      <- cow_dead (Cow, Cow_new)
          count_dead   <- count_dead + 1
          SizeHerd     <- SizeHerd - 1				
        } else if (Cow$char$replaced == 1) {				
          Cow_new        <- define_Cow_new ()				
          Cow_new        <- cow_replaced (Cow, Cow_new)
          count_replaced <- count_replaced + 1
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
    }
  }
  # Storing a selection from the output and saving it in a temporary file  
  saved_output <- save_fromLivSim (outputCows, saved_output)
  fullname <- paste("temp", counter, sep="")
  save(saved_output, file=fullname)    
}

# The data in the temporary files are combined into a list and
# saved in a single output file
save_settings <- vector('list')
for (i in 1:numberoftimes) {
  name <- paste("temp", i, sep="")
  load(name)
  save_settings[[i]] <- saved_output
}
save(save_settings, file="save_output")

# Deleting the generated temporary files
for (i in 1:numberoftimes) {
  name <- paste("temp",i,sep="")
  file.remove(name)
}

run.time <- (proc.time() - t1)[3]
cat("Run executed in", run.time, "seconds\n")
