##' Source model for dust deposited rate estimation
##' 
##'
##' \code{source.model} Estimation of Dust Deposition Model (EDDM) established to estimate dust deposited rate at receptor
##' 
##' \code{source.model} require the source and receptor location in Easting and Northing format.
##' The \code{sourceActivity} for this model is the type of material processing activity in the quarry or
##' if the \code{sourceInput} is the quarries location, the \code{sourceActivity} is the name of the quarries.
##' 
##' @param sourceInput A data frame containing \code{sourceActivity} is a name for the pointsource, \code{type} (the type of material processing),
##' \code{x} for Easting, \code{y} for Northing and \code{z} for elevation height of the location.
##' 
##' @param receptorInput A data frame containing \code{receptor} the name/label for receptor point,
##' \code{x} for Easting, \code{y} for Northing and \code{z} for elevation height of the location.
##' 
##' @param windInput A data frame containing \code{ws} for windspeed (m/s) and \code{wd} for wind direction
##' 
##' @param sourceActivity The pointsource name such as \sQuote{primaryCrusher} or \sQuote{point A} which is
##' according to the \code{souceInput} \code{sourceActivity}.
##' 
##' @param dustGenerated A numeric value of estimation dust generation at a point source based on the site activity. To estimate \code{dustGenerated}
##' at a point source either use function of \code{blasting}, \code{drilling}, \code{pavedRoad}, \code{pavedRoad}, \code{unpavedRoad},
##' \code{primaryCrusher}, \code{secondaryCrusher}, \code{tertiaryCrusher}, \code{screening}, or \code{stockpile}.
##' 
##' @param vg A constant numeric value for settling velocity. The default value for source model is 5 m/s.
##' 
##' @param AnemometerHeight Height of anemometer installation from sea level.
##' 
##' @export
##' 
##' @return The results will be the estimation of dust deposited rate at all the receptors from a pointsource.
##' The unit measurement for dust deposited rate is ug/m2/month.
##' 
##' @author Zul Fadhli & Dr Izhar Abadi
##' 
##' @examples 
##' 
##' #demo
##' source.model(sourceInput, receptorInput, windInput, sourceActivity = "primaryCrusher", vg = 5, dustGenerated = 0.731952, AnemometerHeight = 16)

source.model <- function(sourceInput, receptorInput, windInput, sourceActivity, vg = 5, dustGenerated = 0.731952, AnemometerHeight = 16){

  sourceInput <- sourceInput
  receptorInput <- receptorInput
  windInput <- windInput
  sourceActivity <- sourceActivity

  AH <- AnemometerHeight

  #sheet k
  ((-1*exp(-0.12*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.14)/us(sourceInput, windInput, sourceActivity = sourceActivity, AnemometerHeight = AH, stability = "A", windClass = "class1")))

    #sheet k
    *-0.12 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.14)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "A", windClass = "class1"))

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
   + (-1*exp(-0.12*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.14)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "A", windClass = "class2")))

      #sheet k
      *-0.12 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.14)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "A", windClass = "class2"))

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
   +(-1*exp(-0.12*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.14)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "A", windClass = "class3")))

     #sheet k
     *-0.12 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.14)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "A", windClass = "class3"))

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
   + (-1*exp(-0.12*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.14)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "A", windClass = "class4")))

      #sheet k
      *-0.12 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.14)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "A", windClass = "class4"))

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
   + (-1*exp(-0.135*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.15)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "B", windClass = "class1")))

      #sheet k
      *-0.135 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.15)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "B", windClass = "class1"))

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
   + (-1*exp(-0.135*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.15)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "B", windClass = "class2")))

      #sheet k
      *-0.135 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.15)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "B", windClass = "class2"))

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
   +(-1*exp(-0.135*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.15)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "B", windClass = "class3")))

     #sheet k
     *-0.135 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.15)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "B", windClass = "class3"))

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
   + (-1*exp(-0.135*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.15)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "B", windClass = "class4")))

      #sheet k
      *-0.135 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.15)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "B", windClass = "class4"))

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
   +(-1*exp(-0.183*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.18)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "C", windClass = "class1")))

     #sheet k
     *-0.183 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.18)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "C", windClass = "class1"))

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
   + (-1*exp(-0.183*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.18)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "C", windClass = "class2")))

      #sheet k
      *-0.183 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.18)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "C", windClass = "class2"))

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
   +(-1*exp(-0.183*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.18)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "C", windClass = "class3")))

     #sheet k
     *-0.183 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.18)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "C", windClass = "class3"))

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
   + (-1*exp(-0.183*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.18)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "C", windClass = "class4")))

      #sheet k
      *-0.183 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.18)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "C", windClass = "class4"))

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
   +(-1*exp(-0.115*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "D", windClass = "class1")))

     #sheet k
     *-0.115 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "D", windClass = "class1"))

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
   + (-1*exp(-0.115*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "D", windClass = "class2")))

      #sheet k
      *-0.115 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "D", windClass = "class2"))

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
   +(-1*exp(-0.115*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "D", windClass = "class3")))

     #sheet k
     *-0.115 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "D", windClass = "class3"))

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
   + (-1*exp(-0.115*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "D", windClass = "class4")))

      #sheet k
      *-0.115 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "D", windClass = "class4"))

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
   +(-1*exp(-0.16*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "E", windClass = "class1")))

     #sheet k
     *-0.16 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "E", windClass = "class1"))

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
   + (-1*exp(-0.16*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "E", windClass = "class2")))

      #sheet k
      *-0.16 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "E", windClass = "class2"))

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
   +(-1*exp(-0.16*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "E", windClass = "class3")))

     #sheet k
     *-0.16 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "E", windClass = "class3"))

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
   + (-1*exp(-0.16*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "E", windClass = "class4")))

      #sheet k
      *-0.16 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.3)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "E", windClass = "class4"))

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
   +(-1*exp(-0.114*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.4)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "F", windClass = "class1")))

     #sheet k
     *-0.114 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.4)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "F", windClass = "class1"))

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
   + (-1*exp(-0.114*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.4)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "F", windClass = "class2")))

      #sheet k
      *-0.114 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.4)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "F", windClass = "class2"))

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
   +(-1*exp(-0.114*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.4)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "F", windClass = "class3")))

     #sheet k
     *-0.114 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.4)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "F", windClass = "class3"))

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
   + (-1*exp(-0.114*(vg)*((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.4)/us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "F", windClass = "class4")))

      #sheet k
      *-0.114 * (vg) * ((distance(sourceInput, receptorInput, sourceActivity = sourceActivity)^0.4)/ us(sourceInput, windInput, sourceActivity = sourceActivity,AnemometerHeight = AH, stability = "F", windClass = "class4"))

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
