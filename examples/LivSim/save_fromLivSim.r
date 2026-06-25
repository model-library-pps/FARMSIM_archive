save_fromLivSim <-function(outputCows, saved_output) {

  if (exists("saved_output")) {
    rm(saved_output)
  }

  varnames <-c("id", "damid", "sex", "age", "bw", "death", "inputdm", "milkdm", "concentratedm", "milk_production", "faecdm", "nbcalv", "agefirstcalv", "agelastcalv", "forint", "faecn", "urinaryn")

  saved_output <- array(0, c(length(varnames), length(outputCows[1,,1]), length(outputCows[1,1,])))
  rownames(saved_output) <- varnames

  saved_output[1, , ]  <- outputCows[2, , ]       # 1.  id
  saved_output[2, , ]  <- outputCows[3, , ]       # 2.  damid
  saved_output[3, , ]  <- outputCows[4, , ]       # 3.  sex
  saved_output[4, , ]  <- outputCows[5, , ]       # 4.  age
  saved_output[5, , ]  <- outputCows[7, , ]       # 5.  bw
  saved_output[6, , ]  <- outputCows[12, , ]      # 6.  death	
  saved_output[7, , ]  <- outputCows[35, , ]      # 7.  input$dm
  saved_output[8, , ]  <- outputCows[39, , ]      # 8.  input$milkdm
  saved_output[9, , ]  <- outputCows[40, , ]      # 9.  input$concentratedm
  saved_output[10, , ] <- outputCows[45, , ]      # 10. milk production
  saved_output[11, , ] <- outputCows[53, , ]      # 11. faecal dry matter
  saved_output[12, , ] <- outputCows[54, , ]      # 12. nb. of calves
  saved_output[13, , ] <- outputCows[60, , ]      # 13. age first calving
  saved_output[14, , ] <- outputCows[61, , ]      # 14. age last calving
  saved_output[15, , ] <- outputCows[64, , ]      # 15. forage intake
  saved_output[16, , ] <- outputCows[47, , ]      # 16. faecal N
  saved_output[17, , ] <- outputCows[50, , ]      # 17. urinary N
    
  return(saved_output)
}
