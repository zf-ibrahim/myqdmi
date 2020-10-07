##' Dust Emission from Secondary crusher
##' 
##' A function to estimate dust generation from Secondary crusher activity.
##'
##' @param quarryInput data frame containing column names: quarryProduction - quarry production tonne per hour,
##' blastingFreq - blasting frequency per month, areaBlasted - area blasted (km. sq), blasthole - average of depth of blast hole (m), drillhole - drilling hole per month,
##' moisture - percentage of moisture content, silt - percentage of silt content, unpavedDist - unpaved road distant (KM), pavedDist - unpaved road distance (KM), 
##' unpavedVehicle - number of vehicle count travel using unpaved road per month, pavedVehicle - number of vehicle count travel using unpaved road per month, 
##' nPrimary - number of primary crusher, nSecondary - number of secondary crusher, nTertiary - number of tertiary crusher, nScreen - number of screen.
##' @param particleSize particle size of dust deposited rate. The default value is "tsp" for total suspended solid 
##' and other available particle size is "pm10" for particulate matter <10 micron. 
##' @param QuarryProduction Numeric value for quarry production tonne per hour. Use default to read the value in quarryInput dataframe.
##' @param crusherCount Numeric value for the active Secondary crusher unit in the quarry location. Use default to read the value in quarryInput dataframe.
##' 
##' @export
##' 
##' @author Zul Fadhli & Dr. Izhar Abadi
##' 
##' @examples
##' #demo 
##' secondaryCrusher(quarryInput, particleSize = "tsp", QuarryProduction = quarryInput$quarryProduction, crusherCount = quarryInput$nSecondary)






secondaryCrusher <- function(quarryInput, particleSize = "tsp", QuarryProduction = quarryInput$quarryProduction,
                           crusherCount = quarryInput$nSecondary)

{
  
  #Secondary crusher TSP & PM10 (Year)
  #Emission Factor TSP for Secondary crusher is 0.03
  #Emission Factor PM10 for Secondary crusher is 0.012
  
  quarryInput <- quarryInput

  if(particleSize == "tsp") {
    particleSize = 0.03
  }else if (particleSize == "pm10"){
    particleSize = 0.012
  }


  (particleSize*(QuarryProduction*8*26)/crusherCount)*12/1000

}








