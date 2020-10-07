##' Dust Emission from Screening activity
##' 
##' A function to estimate dust generation from Screening activity.
##'
##' @param quarryInput data frame containing column names: quarryProduction - quarry production tonne per hour,
##' blastingFreq - blasting frequency per month, areaBlasted - area blasted (km. sq), blasthole - average of depth of blast hole (m), drillhole - drilling hole per month,
##' moisture - percentage of moisture content, silt - percentage of silt content, unpavedDist - unpaved road distant (KM), pavedDist - unpaved road distance (KM), 
##' unpavedVehicle - number of vehicle count travel using unpaved road per month, pavedVehicle - number of vehicle count travel using unpaved road per month, 
##' nPrimary - number of primary crusher, nSecondary - number of secondary crusher, nTertiary - number of tertiary crusher, nScreen - number of screen.
##' @param particleSize particle size of dust deposited rate. The default value is "tsp" for total suspended solid 
##' and other available particle size is "pm10" for particulate matter <10 micron. 
##' @param QuarryProduction Numeric value for quarry production tonne per hour. Use default to read the value in quarryInput dataframe.
##' @param totalScreen Numeric value for the active Screening unit in the quarry location. Use default to read the value in quarryInput dataframe.
##' 
##' @export
##' 
##' @author Zul Fadhli & Dr. Izhar Abadi
##' 
##' @example 
##' #demo
##' screening(quarryInput, particleSize = "tsp", QuarryProduction = quarryInput$quarryProduction,
##'                           totalScreen = quarryInput$nScreen)
##' 
screening <- function(quarryInput, particleSize = "tsp", QuarryProduction = quarryInput$quarryProduction,
                      totalScreen = quarryInput$nScreen)

{
  quarryInput <- quarryInput

  if(particleSize == "tsp") {
    efc = 0.008
  }else if (particleSize == "pm10"){
    efc = 0.0043
  }



  efc*QuarryProduction*8*26*12/totalScreen/1000


}
