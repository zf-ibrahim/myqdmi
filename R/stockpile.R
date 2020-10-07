##' Stockpile dust generation
##' 
##' A function to estimate dust generation from stockpile in quarry.
##' 
##' @param quarryInput data frame containing column names: quarryProduction - quarry production tonne per hour,
##' blastingFreq - blasting frequency per month, areaBlasted - area blasted (km. sq), blasthole - average of depth of blast hole (m), drillhole - drilling hole per month,
##' moisture - percentage of moisture content, silt - percentage of silt content, unpavedDist - unpaved road distant (KM), pavedDist - unpaved road distance (KM), 
##' unpavedVehicle - number of vehicle count travel using unpaved road per month, pavedVehicle - number of vehicle count travel using unpaved road per month, 
##' nPrimary - number of primary crusher, nSecondary - number of secondary crusher, nTertiary - number of tertiary crusher, nScreen - number of screen.
##' @param particleSize particle size of dust deposited rate. The default value is "tsp" for total suspended solid 
##' and other available particle size is "pm10" for particulate matter <10 micron. 
##' @param QuarryProduction Numeric value for quarry production tonne per hour.
##' @param moistureContent Percentage of soil moisture content, use the default to read moisture value inside quarryInput dataframe
##' 
##' @export
##' 
##' @author Zul Fadhli & Dr. Izhar Abadi
##' 
##' @example
##' #demo
##' stockpile <- function(quarryInput, particleSize = "tsp")

stockpile <- function(quarryInput, particleSize = "tsp", QuarryProduction = quarryInput$quarryProduction,
                            mositure = quarryInput$moisture)

{
  quarryInput <- quarryInput

  if(particleSize == "tsp") {
    ks = 0.74
  }else if (particleSize == "pm10"){
    ks = 0.35
  }

  avgws <- mean(windInput$ws)

  (ks*0.0016*((avgws/2.2)^1.3)/((mositure/2)^1.4))*QuarryProduction*8*26*12/1000


}
