heapsim <- function(heap, outputcows, timestep, season, lengthseason) {

##################################### Input HeapSim #####################################
#--settings of the model
# non-decomposable fraction of the manure on the heap
inertDM <- 0.3      # (-)
# inertC  <- 0.3
# inertN  <- 0.3
# inertP  <- 0.3
# inertK  <- 0.3
# relative decomposition rate of the manure on the heap
rdrDM <- 0.3        # month-1
# rdrC  <- 0.3
# rdrN  <- 0.3
# rdrP  <- 0.3
# rdrK  <- 0.3
# non-decomposable fraction of the organic residues
inertDM.OR <- 0.3   # (-)
inertC.OR  <- 0.3   # (-)
inertN.OR  <- 0.3   # (-)
inertP.OR  <- 0.3   # (-)
inertK.OR  <- 0.3   # (-)
# C-content of the animal manure is estimated from data (see dataset_maize.xls)
manureCContent <- 0.27    # kg C kg DM-1

# Data on 'refusals'?

# Data on faeces and urine are taken from LivSim
totalFaecalDM <- sum(outputcows['faecdm', , timestep + 1])    # kg DM
totalFaecalC  <- totalFaecalDM * manureCContent               # kg C
totalFaecalN  <- sum(outputcows['faecn', , timestep + 1])     # kg N
totalFaecalP  <- sum(outputcows['faecp', , timestep + 1])     # kg P
totalFaecalK  <- sum(outputcows['faeck', , timestep + 1])     # kg K

totalUrinaryN <- sum(outputcows['urinaryn', , timestep + 1])  # kg N
totalUrinaryP <- sum(outputcows['urinaryp', , timestep + 1])  # kg P
totalUrinaryK <- sum(outputcows['urinaryk', , timestep + 1])  # kg K

inputTotalDM <- totalFaecalDM                                 # kg DM
inputTotalC  <- totalFaecalC                                  # kg C
inputTotalN  <- totalFaecalN + totalUrinaryN                  # kg N
inputTotalP  <- totalFaecalP + totalUrinaryP                  # kg P
inputTotalK  <- totalFaecalK + totalUrinaryK                  # kg K


################################################# HeapSim ##########################################################################
# Currently the decomposition of DM is calculated directly, the decomposition rates of the other nutrients is based on the DM 
# decomposition and the ratio of the other nutrients to the DM content of the manure.

if (timestep == (season - 1) * lengthseason + 1) {
  heap$amountDM[timestep + 1] <- heap$amountDM[timestep] - heap$manureApplied
  heap$amountC[timestep + 1] <- heap$amountC[timestep] - heap$manureApplied * (heap$amountC[timestep] / heap$amountDM[timestep])
  heap$amountN[timestep + 1] <- heap$amountN[timestep] - heap$manureApplied * (heap$amountN[timestep] / heap$amountDM[timestep])
  heap$amountP[timestep + 1] <- heap$amountP[timestep] - heap$manureApplied * (heap$amountP[timestep] / heap$amountDM[timestep])
  heap$amountK[timestep + 1] <- heap$amountK[timestep] - heap$manureApplied * (heap$amountK[timestep] / heap$amountDM[timestep])  
}

heapDecompositionDM <- rdrDM * (1 - inertDM) * heap$amountDM[timestep]        # kg DM month-1
heapDecompositionC  <- heapDecompositionDM / (totalFaecalDM / totalFaecalC)   # kg C month-1
heapDecompositionN  <- heapDecompositionDM / (totalFaecalDM / totalFaecalN)   # kg N month-1
heapDecompositionP  <- heapDecompositionDM / (totalFaecalDM / totalFaecalP)   # kg P month-1
heapDecompositionK  <- heapDecompositionDM / (totalFaecalDM / totalFaecalK)   # kg K month-1

# heapDecompositionC  <- rdrC * (1 - inertC) * heap$amountC[timestep]
# heapDecompositionN  <- rdrN * (1 - inertN) * heap$amountN[timestep]
# heapDecompositionP  <- rdrP * (1 - inertP) * heap$amountP[timestep]
# heapDecompositionK  <- rdrK * (1 - inertK) * heap$amountK[timestep]

heapNetRateOfChangeDM <- -heapDecompositionDM + inputTotalDM
heapNetRateOfChangeC  <- -heapDecompositionC + inputTotalC
heapNetRateOfChangeN  <- -heapDecompositionN + inputTotalN
heapNetRateOfChangeP  <- -heapDecompositionP + inputTotalP
heapNetRateOfChangeK  <- -heapDecompositionK + inputTotalK

if (is.nan(heapNetRateOfChangeC)) {heapNetRateOfChangeC <- 0} 
if (is.nan(heapNetRateOfChangeN)) {heapNetRateOfChangeN <- 0} 
if (is.nan(heapNetRateOfChangeP)) {heapNetRateOfChangeP <- 0} 
if (is.nan(heapNetRateOfChangeK)) {heapNetRateOfChangeK <- 0} 

heap$amountDM[timestep + 1] <- heap$amountDM[timestep] + heapNetRateOfChangeDM  # kg DM
heap$amountC[timestep + 1] <- heap$amountC[timestep] + heapNetRateOfChangeC     # kg C
heap$amountN[timestep + 1] <- heap$amountN[timestep] + heapNetRateOfChangeN     # kg N
heap$amountP[timestep + 1] <- heap$amountP[timestep] + heapNetRateOfChangeP     # kg P
heap$amountK[timestep + 1] <- heap$amountK[timestep] + heapNetRateOfChangeK     # kg K

return(heap)
}
