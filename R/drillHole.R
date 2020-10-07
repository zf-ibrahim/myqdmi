#' Drilling
#' 
#' A function to estimate dust generation from Drilling activity.
#'
#' @param quarryInput data frame containing column names: quarryProduction - quarry production tonne per hour,
#' blastingFreq - blasting frequency per month, areaBlasted - area blasted (km. sq), blasthole - average of depth of blast hole (m), drillhole - drilling hole per month,
#' moisture - percentage of moisture content, silt - percentage of silt content, unpavedDist - unpaved road distant (KM), pavedDist - paved road distanct (KM), 
#' unpavedVehicle - number of vehicle count travel using unpaved road per month, pavedVehicle - number of vehicle count travel using paved road per month, 
#' nPrimary - number of primary crusher, nSecondary - number of secondary crusher, nTertiary - number of tertiary crusher, nScreen - number of screen.
#' 
#' @param particleSize particle size of dust deposited rate. The default value is "tsp" for total suspended solid 
#' and other available particle size is "pm10" for particulate matter <10 micron. 
#' 
#' @param drillHole Total count drilling hole per month
#'
#' @return Dust generation from drilling activity per month
#' @export
#' 
#' @author Zul Fadhli & Dr Izhar Abadi
#'
#' @examples
#' #demo
#' drillingquarryInput, particleSize = "tsp", drillHole = quarryInput$drillhole)

drilling <- function(quarryInput, particleSize = "tsp",
                   drillHole = quarryInput$drillhole)

{
  quarryInput <- quarryInput

  if(particleSize == "tsp") {
    efd = 0.59
  }else if (particleSize == "pm10"){
    efd = 0.31
  }



  (efd*drillHole)/1000*12

}
