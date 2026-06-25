load(save_LivSim)
timesteps <- dim(save_LivSim[[1]])[3]
size <- (timesteps - 1) / 12   # we want output per year, we should, therefore, divide by twelve

bw                <- matrix(data = NA, nrow = size, ncol = length(save_LivSim))   # dim = nr. of timesteps * nr. of reruns
faecdm            <- matrix(data = NA, nrow = size, ncol = length(save_LivSim))
milk              <- matrix(data = NA, nrow = size, ncol = length(save_LivSim))
deaths            <- matrix(data = NA, nrow = size, ncol = length(save_LivSim))
calves            <- matrix(data = NA, nrow = size, ncol = length(save_LivSim))
dmi               <- matrix(data = NA, nrow = size, ncol = length(save_LivSim))
cropResidueIntake <- matrix(data = NA, nrow = size, ncol = length(save_LivSim))
totalNExcreted    <- matrix(data = NA, nrow = size, ncol = length(save_LivSim))
nrDead            <- vector(mode = 'numeric', length = length(save_LivSim))
nrCalves          <- vector(mode = 'numeric', length = length(save_LivSim))

aux <- seq(1, timesteps, 12)

for (i in seq(1, length(save_LivSim), 1)) {

  for (j in seq(1, size, 1)) {
    # bw
    temp <- save_LivSim[[i]]['bw', ,(aux[j]:aux[j + 1])]
    temp <- temp[temp > 0]
    bw[j, i] <- mean(temp[!is.na(temp)])
    # faecdm
    temp <- save_LivSim[[i]]['faecdm', ,(aux[j]:aux[j + 1])]
    faecdm[j, i] <- sum(temp[!is.na(temp)])
    # milk
    temp <- save_LivSim[[i]]['milk_production', ,(aux[j]:aux[j + 1])]
    milk[j, i] <- sum(temp[!is.na(temp)])
    # dry matter intake
    temp <- save_LivSim[[i]]['inputdm', ,(aux[j]:aux[j + 1])]
    dmi[j, i] <- sum(temp[!is.na(temp)])
    # crop residue intake
    temp <- save_LivSim[[i]]['forint', ,(aux[j]:aux[j + 1])]
    cropResidueIntake[j, i] <- sum(temp[!is.na(temp)])
    # total N excreted
    temp1 <- save_LivSim[[i]]['faecn', ,(aux[j]:aux[j + 1])]
    temp2 <- save_LivSim[[i]]['urinaryn', ,(aux[j]:aux[j + 1])]
    totalNExcreted[j, i] <- sum(temp1[!is.na(temp1)]) + sum(temp2[!is.na(temp2)])
  }
  
  nrDead[i]   <- sum(save_LivSim[[i]]['death', , timesteps] > 0)
  nrCalves[i] <- sum(save_LivSim[[i]]['nbcalv', , timesteps])
}

mean_bw                <- rowMeans(bw)
mean_faecdm            <- rowMeans(faecdm)
mean_milk              <- rowMeans(milk)
mean_dmi               <- rowMeans(dmi)
mean_cropResidueIntake <- rowMeans(cropResidueIntake)
mean_totalNExcreted    <- rowMeans(totalNExcreted)
mean_nrDead            <- mean(nrDead)
mean_nrCalves          <- mean(nrCalves)

sd_bw                <- vector(mode = 'numeric', length = size)
sd_faecdm            <- vector(mode = 'numeric', length = size)
sd_milk              <- vector(mode = 'numeric', length = size)
sd_dmi               <- vector(mode = 'numeric', length = size)
sd_cropResidueIntake <- vector(mode = 'numeric', length = size)
sd_totalNExcreted    <- vector(mode = 'numeric', length = size)
sd_nrDead            <- sd(nrDead)
sd_nrCalves          <- sd(nrCalves)

for (i in seq(1, , 1)) {
  sd_bw[i]                <- sd(bw[i, ])
  sd_faecdm[i]            <- sd(faecdm[i, ])
  sd_milk[i]              <- sd(milk[i, ])
  sd_dmi[i]               <- sd(dmi[i, ])
  sd_cropResidueIntake[i] <- sd(cropResidueIntake[i, ])
  sd_totalNExcreted[i]    <- sd(totalNExcreted[i, ])
}

write.csv(mean_bw, file = 'mean_bw.csv')
write.csv(mean_faecdm, file = 'mean_faecdm.csv')
write.csv(mean_milk, file = 'mean_milk.csv')
write.csv(mean_dmi, file = 'mean_dmi.csv')   
write.csv(mean_cropResidueIntake, file = 'mean_cropResidueIntake.csv')
write.csv(mean_totalNExcreted, file = 'mean_totalNExcreted.csv')   
write.csv(mean_nrDead, file = 'mean_nrDead.csv')          
write.csv(mean_nrCalves, file = 'mean_nrCalves.csv')

write.csv(sd_bw, file = 'sd_bw.csv')
write.csv(sd_faecdm, file = 'sd_faecdm.csv')
write.csv(sd_milk, file = 'sd_milk.csv')
write.csv(sd_dmi, file = 'sd_dmi.csv')   
write.csv(sd_cropResidueIntake, file = 'sd_cropResidueIntake.csv')
write.csv(sd_totalNExcreted, file = 'sd_totalNExcreted.csv')   
write.csv(sd_nrDead, file = 'sd_nrDead.csv')          
write.csv(sd_nrCalves, file = 'sd_nrCalves.csv')
