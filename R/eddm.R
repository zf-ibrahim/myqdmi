##' @export

source.model <- function(sourceInput, receptorInput, windInput, sourceActivity, vg = 5, dustGenerated = 0.731952, AnemometerHeight = 16){

  sourceInput <- sourceInput
  receptorInput <- receptorInput
  windInput <- windInput
  sourceActivity <- sourceActivity

  AH <- AnemometerHeight

  #sheet k
  ((-1*exp(-0.12*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.14)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "A", windClass = "class1")))

    #sheet k
    *-0.12 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.14)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "A", windClass = "class1"))

    #sheet L
    *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.14/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

    *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "A", windClass = "class1", windDirection = "S"),
                        ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "A", windClass = "class1", windDirection = "N"),
                               ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "A", windClass = "class1", windDirection = "E"),
                                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "A", windClass = "class1", windDirection = "W"),
                                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "A", windClass = "class1", windDirection = "SW"),
                                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "A", windClass = "class1", windDirection = "SE"),
                                                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "A", windClass = "class1", windDirection = "NE"),
                                                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "A", windClass = "class1", windDirection = "NW"),
                                                                         "error"))))))))))
   + (-1*exp(-0.12*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.14)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "A", windClass = "class2")))

      #sheet k
      *-0.12 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.14)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "A", windClass = "class2"))

      #sheet L
      *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.14/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

      *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "A", windClass = "class2", windDirection = "S"),
                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "A", windClass = "class2", windDirection = "N"),
                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "A", windClass = "class2", windDirection = "E"),
                                        ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "A", windClass = "class2", windDirection = "W"),
                                               ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "A", windClass = "class2", windDirection = "SW"),
                                                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "A", windClass = "class2", windDirection = "SE"),
                                                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "A", windClass = "class2", windDirection = "NE"),
                                                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "A", windClass = "class2", windDirection = "NW"),
                                                                           "error"))))))))))
   +(-1*exp(-0.12*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.14)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "A", windClass = "class3")))

     #sheet k
     *-0.12 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.14)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "A", windClass = "class3"))

     #sheet L
     *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.14/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

     *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "A", windClass = "class3", windDirection = "S"),
                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "A", windClass = "class3", windDirection = "N"),
                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "A", windClass = "class3", windDirection = "E"),
                                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "A", windClass = "class3", windDirection = "W"),
                                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "A", windClass = "class3", windDirection = "SW"),
                                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "A", windClass = "class3", windDirection = "SE"),
                                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "A", windClass = "class3", windDirection = "NE"),
                                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "A", windClass = "class3", windDirection = "NW"),
                                                                          "error"))))))))))
   + (-1*exp(-0.12*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.14)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "A", windClass = "class4")))

      #sheet k
      *-0.12 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.14)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "A", windClass = "class4"))

      #sheet L
      *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.14/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

      *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "A", windClass = "class4", windDirection = "S"),
                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "A", windClass = "class4", windDirection = "N"),
                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "A", windClass = "class4", windDirection = "E"),
                                        ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "A", windClass = "class4", windDirection = "W"),
                                               ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "A", windClass = "class4", windDirection = "SW"),
                                                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "A", windClass = "class4", windDirection = "SE"),
                                                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "A", windClass = "class4", windDirection = "NE"),
                                                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "A", windClass = "class4", windDirection = "NW"),
                                                                           "error"))))))))))
   ###################### B
   + (-1*exp(-0.135*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.15)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "B", windClass = "class1")))

      #sheet k
      *-0.135 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.15)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "B", windClass = "class1"))

      #sheet L
      *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.15/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

      *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "B", windClass = "class1", windDirection = "S"),
                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "B", windClass = "class1", windDirection = "N"),
                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "B", windClass = "class1", windDirection = "E"),
                                        ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "B", windClass = "class1", windDirection = "W"),
                                               ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "B", windClass = "class1", windDirection = "SW"),
                                                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "B", windClass = "class1", windDirection = "SE"),
                                                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "B", windClass = "class1", windDirection = "NE"),
                                                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "B", windClass = "class1", windDirection = "NW"),
                                                                           "error"))))))))))
   + (-1*exp(-0.135*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.15)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "B", windClass = "class2")))

      #sheet k
      *-0.135 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.15)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "B", windClass = "class2"))

      #sheet L
      *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.15/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

      *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "B", windClass = "class2", windDirection = "S"),
                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "B", windClass = "class2", windDirection = "N"),
                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "B", windClass = "class2", windDirection = "E"),
                                        ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "B", windClass = "class2", windDirection = "W"),
                                               ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "B", windClass = "class2", windDirection = "SW"),
                                                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "B", windClass = "class2", windDirection = "SE"),
                                                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "B", windClass = "class2", windDirection = "NE"),
                                                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "B", windClass = "class2", windDirection = "NW"),
                                                                           "error"))))))))))
   +(-1*exp(-0.135*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.15)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "B", windClass = "class3")))

     #sheet k
     *-0.135 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.15)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "B", windClass = "class3"))

     #sheet L
     *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.15/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

     *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "B", windClass = "class3", windDirection = "S"),
                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "B", windClass = "class3", windDirection = "N"),
                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "B", windClass = "class3", windDirection = "E"),
                                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "B", windClass = "class3", windDirection = "W"),
                                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "B", windClass = "class3", windDirection = "SW"),
                                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "B", windClass = "class3", windDirection = "SE"),
                                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "B", windClass = "class3", windDirection = "NE"),
                                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "B", windClass = "class3", windDirection = "NW"),
                                                                          "error"))))))))))
   + (-1*exp(-0.135*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.15)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "B", windClass = "class4")))

      #sheet k
      *-0.135 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.15)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "B", windClass = "class4"))

      #sheet L
      *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.15/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

      *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "B", windClass = "class4", windDirection = "S"),
                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "B", windClass = "class4", windDirection = "N"),
                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "B", windClass = "class4", windDirection = "E"),
                                        ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "B", windClass = "class4", windDirection = "W"),
                                               ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "B", windClass = "class4", windDirection = "SW"),
                                                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "B", windClass = "class4", windDirection = "SE"),
                                                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "B", windClass = "class4", windDirection = "NE"),
                                                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "B", windClass = "class4", windDirection = "NW"),
                                                                           "error"))))))))))
   #############C
   +(-1*exp(-0.183*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.18)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "C", windClass = "class1")))

     #sheet k
     *-0.183 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.18)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "C", windClass = "class1"))

     #sheet L
     *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.18/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

     *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "C", windClass = "class1", windDirection = "S"),
                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "C", windClass = "class1", windDirection = "N"),
                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "C", windClass = "class1", windDirection = "E"),
                                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "C", windClass = "class1", windDirection = "W"),
                                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "C", windClass = "class1", windDirection = "SW"),
                                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "C", windClass = "class1", windDirection = "SE"),
                                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "C", windClass = "class1", windDirection = "NE"),
                                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "C", windClass = "class1", windDirection = "NW"),
                                                                          "error"))))))))))
   + (-1*exp(-0.183*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.18)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "C", windClass = "class2")))

      #sheet k
      *-0.183 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.18)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "C", windClass = "class2"))

      #sheet L
      *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.18/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

      *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "C", windClass = "class2", windDirection = "S"),
                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "C", windClass = "class2", windDirection = "N"),
                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "C", windClass = "class2", windDirection = "E"),
                                        ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "C", windClass = "class2", windDirection = "W"),
                                               ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "C", windClass = "class2", windDirection = "SW"),
                                                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "C", windClass = "class2", windDirection = "SE"),
                                                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "C", windClass = "class2", windDirection = "NE"),
                                                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "C", windClass = "class2", windDirection = "NW"),
                                                                           "error"))))))))))
   +(-1*exp(-0.183*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.18)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "C", windClass = "class3")))

     #sheet k
     *-0.183 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.18)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "C", windClass = "class3"))

     #sheet L
     *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.18/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

     *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "C", windClass = "class3", windDirection = "S"),
                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "C", windClass = "class3", windDirection = "N"),
                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "C", windClass = "class3", windDirection = "E"),
                                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "C", windClass = "class3", windDirection = "W"),
                                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "C", windClass = "class3", windDirection = "SW"),
                                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "C", windClass = "class3", windDirection = "SE"),
                                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "C", windClass = "class3", windDirection = "NE"),
                                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "C", windClass = "class3", windDirection = "NW"),
                                                                          "error"))))))))))
   + (-1*exp(-0.183*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.18)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "C", windClass = "class4")))

      #sheet k
      *-0.183 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.18)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "C", windClass = "class4"))

      #sheet L
      *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.18/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

      *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "C", windClass = "class4", windDirection = "S"),
                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "C", windClass = "class4", windDirection = "N"),
                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "C", windClass = "class4", windDirection = "E"),
                                        ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "C", windClass = "class4", windDirection = "W"),
                                               ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "C", windClass = "class4", windDirection = "SW"),
                                                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "C", windClass = "class4", windDirection = "SE"),
                                                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "C", windClass = "class4", windDirection = "NE"),
                                                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "C", windClass = "class4", windDirection = "NW"),
                                                                           "error"))))))))))
   ########## D
   +(-1*exp(-0.115*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "D", windClass = "class1")))

     #sheet k
     *-0.115 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "D", windClass = "class1"))

     #sheet L
     *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.3/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

     *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "D", windClass = "class1", windDirection = "S"),
                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "D", windClass = "class1", windDirection = "N"),
                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "D", windClass = "class1", windDirection = "E"),
                                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "D", windClass = "class1", windDirection = "W"),
                                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "D", windClass = "class1", windDirection = "SW"),
                                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "D", windClass = "class1", windDirection = "SE"),
                                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "D", windClass = "class1", windDirection = "NE"),
                                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "D", windClass = "class1", windDirection = "NW"),
                                                                          "error"))))))))))
   + (-1*exp(-0.115*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "D", windClass = "class2")))

      #sheet k
      *-0.115 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "D", windClass = "class2"))

      #sheet L
      *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.3/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

      *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "D", windClass = "class2", windDirection = "S"),
                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "D", windClass = "class2", windDirection = "N"),
                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "D", windClass = "class2", windDirection = "E"),
                                        ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "D", windClass = "class2", windDirection = "W"),
                                               ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "D", windClass = "class2", windDirection = "SW"),
                                                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "D", windClass = "class2", windDirection = "SE"),
                                                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "D", windClass = "class2", windDirection = "NE"),
                                                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "D", windClass = "class2", windDirection = "NW"),
                                                                           "error"))))))))))
   +(-1*exp(-0.115*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "D", windClass = "class3")))

     #sheet k
     *-0.115 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "D", windClass = "class3"))

     #sheet L
     *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.3/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

     *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "D", windClass = "class3", windDirection = "S"),
                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "D", windClass = "class3", windDirection = "N"),
                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "D", windClass = "class3", windDirection = "E"),
                                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "D", windClass = "class3", windDirection = "W"),
                                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "D", windClass = "class3", windDirection = "SW"),
                                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "D", windClass = "class3", windDirection = "SE"),
                                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "D", windClass = "class3", windDirection = "NE"),
                                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "D", windClass = "class3", windDirection = "NW"),
                                                                          "error"))))))))))
   + (-1*exp(-0.115*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "D", windClass = "class4")))

      #sheet k
      *-0.115 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "D", windClass = "class4"))

      #sheet L
      *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.3/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

      *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "D", windClass = "class4", windDirection = "S"),
                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "D", windClass = "class4", windDirection = "N"),
                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "D", windClass = "class4", windDirection = "E"),
                                        ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "D", windClass = "class4", windDirection = "W"),
                                               ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "D", windClass = "class4", windDirection = "SW"),
                                                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "D", windClass = "class4", windDirection = "SE"),
                                                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "D", windClass = "class4", windDirection = "NE"),
                                                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "D", windClass = "class4", windDirection = "NW"),
                                                                           "error"))))))))))
   ###########E
   +(-1*exp(-0.16*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "E", windClass = "class1")))

     #sheet k
     *-0.16 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "E", windClass = "class1"))

     #sheet L
     *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.3/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

     *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "E", windClass = "class1", windDirection = "S"),
                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "E", windClass = "class1", windDirection = "N"),
                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "E", windClass = "class1", windDirection = "E"),
                                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "E", windClass = "class1", windDirection = "W"),
                                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "E", windClass = "class1", windDirection = "SW"),
                                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "E", windClass = "class1", windDirection = "SE"),
                                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "E", windClass = "class1", windDirection = "NE"),
                                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "E", windClass = "class1", windDirection = "NW"),
                                                                          "error"))))))))))
   + (-1*exp(-0.16*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "E", windClass = "class2")))

      #sheet k
      *-0.16 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "E", windClass = "class2"))

      #sheet L
      *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.3/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

      *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "E", windClass = "class2", windDirection = "S"),
                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "E", windClass = "class2", windDirection = "N"),
                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "E", windClass = "class2", windDirection = "E"),
                                        ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "E", windClass = "class2", windDirection = "W"),
                                               ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "E", windClass = "class2", windDirection = "SW"),
                                                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "E", windClass = "class2", windDirection = "SE"),
                                                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "E", windClass = "class2", windDirection = "NE"),
                                                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "E", windClass = "class2", windDirection = "NW"),
                                                                           "error"))))))))))
   +(-1*exp(-0.16*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "E", windClass = "class3")))

     #sheet k
     *-0.16 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "E", windClass = "class3"))

     #sheet L
     *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.3/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

     *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "E", windClass = "class3", windDirection = "S"),
                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "E", windClass = "class3", windDirection = "N"),
                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "E", windClass = "class3", windDirection = "E"),
                                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "E", windClass = "class3", windDirection = "W"),
                                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "E", windClass = "class3", windDirection = "SW"),
                                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "E", windClass = "class3", windDirection = "SE"),
                                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "E", windClass = "class3", windDirection = "NE"),
                                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "E", windClass = "class3", windDirection = "NW"),
                                                                          "error"))))))))))
   + (-1*exp(-0.16*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "E", windClass = "class4")))

      #sheet k
      *-0.16 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "E", windClass = "class4"))

      #sheet L
      *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.3/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

      *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "E", windClass = "class4", windDirection = "S"),
                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "E", windClass = "class4", windDirection = "N"),
                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "E", windClass = "class4", windDirection = "E"),
                                        ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "E", windClass = "class4", windDirection = "W"),
                                               ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "E", windClass = "class4", windDirection = "SW"),
                                                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "E", windClass = "class4", windDirection = "SE"),
                                                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "E", windClass = "class4", windDirection = "NE"),
                                                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "E", windClass = "class4", windDirection = "NW"),
                                                                           "error"))))))))))
   ###########F
   +(-1*exp(-0.114*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.4)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "F", windClass = "class1")))

     #sheet k
     *-0.114 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.4)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "F", windClass = "class1"))

     #sheet L
     *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.4/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

     *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "F", windClass = "class1", windDirection = "S"),
                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "F", windClass = "class1", windDirection = "N"),
                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "F", windClass = "class1", windDirection = "E"),
                                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "F", windClass = "class1", windDirection = "W"),
                                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "F", windClass = "class1", windDirection = "SW"),
                                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "F", windClass = "class1", windDirection = "SE"),
                                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "F", windClass = "class1", windDirection = "NE"),
                                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "F", windClass = "class1", windDirection = "NW"),
                                                                          "error"))))))))))
   + (-1*exp(-0.114*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.4)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "F", windClass = "class2")))

      #sheet k
      *-0.114 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.4)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "F", windClass = "class2"))

      #sheet L
      *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.4/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

      *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "F", windClass = "class2", windDirection = "S"),
                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "F", windClass = "class2", windDirection = "N"),
                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "F", windClass = "class2", windDirection = "E"),
                                        ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "F", windClass = "class2", windDirection = "W"),
                                               ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "F", windClass = "class2", windDirection = "SW"),
                                                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "F", windClass = "class2", windDirection = "SE"),
                                                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "F", windClass = "class2", windDirection = "NE"),
                                                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "F", windClass = "class2", windDirection = "NW"),
                                                                           "error"))))))))))
   +(-1*exp(-0.114*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.4)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "F", windClass = "class3")))

     #sheet k
     *-0.114 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.4)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "F", windClass = "class3"))

     #sheet L
     *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.4/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

     *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "F", windClass = "class3", windDirection = "S"),
                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "F", windClass = "class3", windDirection = "N"),
                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "F", windClass = "class3", windDirection = "E"),
                                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "F", windClass = "class3", windDirection = "W"),
                                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "F", windClass = "class3", windDirection = "SW"),
                                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "F", windClass = "class3", windDirection = "SE"),
                                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "F", windClass = "class3", windDirection = "NE"),
                                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "F", windClass = "class3", windDirection = "NW"),
                                                                          "error"))))))))))
   + (-1*exp(-0.114*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.4)/us(sourceInput, windInput,AnemometerHeight = AH, stability = "F", windClass = "class4")))

      #sheet k
      *-0.114 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.4)/ us(sourceInput, windInput,AnemometerHeight = AH, stability = "F", windClass = "class4"))

      #sheet L
      *(dustGenerated/(calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)) * (0.4/distance(sourceInput, receptorInput, sourceActivity = sourceActivity)) * ((8 * 1000000)/(24*3.14159*distance(sourceInput, receptorInput, sourceActivity = sourceActivity)))

      *as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "N", windFreq(windInput, stability = "F", windClass = "class4", windDirection = "S"),
                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "s", windFreq(windInput, stability = "F", windClass = "class4", windDirection = "N"),
                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "w", windFreq(windInput, stability = "F", windClass = "class4", windDirection = "E"),
                                        ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "E", windFreq(windInput, stability = "F", windClass = "class4", windDirection = "W"),
                                               ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NE", windFreq(windInput, stability = "F", windClass = "class4", windDirection = "SW"),
                                                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "NW", windFreq(windInput, stability = "F", windClass = "class4", windDirection = "SE"),
                                                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SW", windFreq(windInput, stability = "F", windClass = "class4", windDirection = "NE"),
                                                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceActivity) == "SE", windFreq(windInput, stability = "F", windClass = "class4", windDirection = "NW"),
                                                                           "error")))))))))))


}
