load(save_FIELD)
size <- dim(save_FIELD[[1]])

grainYield_1  <- matrix(data = NA, nrow = size[1], ncol = length(save_FIELD))
stoverYield_1 <- matrix(data = NA, nrow = size[1], ncol = length(save_FIELD))
totalSoilC_1  <- matrix(data = NA, nrow = size[1], ncol = length(save_FIELD))
grainYield_2  <- matrix(data = NA, nrow = size[1], ncol = length(save_FIELD))
stoverYield_2 <- matrix(data = NA, nrow = size[1], ncol = length(save_FIELD))
totalSoilC_2  <- matrix(data = NA, nrow = size[1], ncol = length(save_FIELD))

for (i in seq(1, length(save_FIELD), 1)) {

  grainYield_1[, i]  <- save_FIELD[[i]][, 1, 1]
  grainYield_2[, i]  <- save_FIELD[[i]][, 1, 2]
  stoverYield_1[, i] <- save_FIELD[[i]][, 2, 1]
  stoverYield_2[, i] <- save_FIELD[[i]][, 2, 2]
  totalSoilC_1[, i]  <- save_FIELD[[i]][, 3, 1]
  totalSoilC_2[, i]  <- save_FIELD[[i]][, 3, 2]

}

mean_grainYield_1  <- rowMeans(grainYield_1)
mean_grainYield_2  <- rowMeans(grainYield_2)
mean_stoverYield_1 <- rowMeans(stoverYield_1)
mean_stoverYield_2 <- rowMeans(stoverYield_2)
mean_totalSoilC_1  <- rowMeans(totalSoilC_1)
mean_totalSoilC_2  <- rowMeans(totalSoilC_2)

sd_grainYield_1  <- vector(mode = 'numeric', length = size[1])
sd_grainYield_2  <- vector(mode = 'numeric', length = size[1])
sd_stoverYield_1 <- vector(mode = 'numeric', length = size[1])
sd_stoverYield_2 <- vector(mode = 'numeric', length = size[1])
sd_totalSoilC_1  <- vector(mode = 'numeric', length = size[1])
sd_totalSoilC_2  <- vector(mode = 'numeric', length = size[1])

for (i in seq(1, size[1], 1)) {
  sd_grainYield_1[i]  <- sd(grainYield_1[i, ])
  sd_grainYield_2[i]  <- sd(grainYield_2[i, ])
  sd_stoverYield_1[i] <- sd(stoverYield_1[i, ])
  sd_stoverYield_2[i] <- sd(stoverYield_2[i, ])
  sd_totalSoilC_1[i]  <- sd(totalSoilC_1[i, ])
  sd_totalSoilC_2[i]  <- sd(totalSoilC_2[i, ])
}

write.csv(mean_grainYield_1, file = 'mean_grainYield_1.csv')
write.csv(mean_grainYield_2, file = 'mean_grainYield_2.csv')
write.csv(mean_stoverYield_1, file = 'mean_stoverYield_1.csv')
write.csv(mean_stoverYield_2, file = 'mean_stoverYield_2.csv')
write.csv(mean_totalSoilC_1, file = 'mean_totalSoilC_1.csv')
write.csv(mean_totalSoilC_2, file = 'mean_totalSoilC_2.csv')

write.csv(sd_grainYield_1, file = 'sd_grainYield_1.csv')
write.csv(sd_grainYield_2, file = 'sd_grainYield_2.csv')
write.csv(sd_stoverYield_1, file = 'sd_stoverYield_1.csv')
write.csv(sd_stoverYield_2, file = 'sd_stoverYield_2.csv')
write.csv(sd_totalSoilC_1, file = 'sd_totalSoilC_1.csv')
write.csv(sd_totalSoilC_2, file = 'sd_totalSoilC_2.csv')