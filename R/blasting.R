#' Blasting
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
#' @param areaBlasted a numeric value for area blasted (km. sq), or use the default to read areaBlasted value inside quarryInput dataframe.
#' 
#' @param blastFreq  a numeric value for blasting frequency per month, or use the default to read blastFreq value inside quarryInput dataframe.
#' 
#' @param blastHole.depth a numeric value for average of depth of blast hole (m), or use the default to read blasthole value inside quarryInput dataframe.
#' 
#' @param moistureContent a numeric value for percentage of moisture content, or use the default to read moisture value inside quarryInput dataframe.
#'
#' @return A numeric value of estimation dust generation/\code{dustGenerated} for blasting activity.
#' @export
#'
#' @examples
#' #demo
#' blasting(quarryInput, particleSize = "tsp")

blasting <- function(quarryInput, particleSize = "tsp", areaBlasted = quarryInput$areaBlasted,
                   blastFreq = quarryInput$blastingFreq, blastHole.depth = quarryInput$blasthole, moistureContent = quarryInput$moisture)

{
  quarryInput <- quarryInput

  if(particleSize == "tsp") {
    coeb = 1
  }else if (particleSize == "pm10"){
    coeb = 0.52
  }





  ((344*((areaBlasted^0.8)/((moistureContent^1.9)*(blastHole.depth^1.8))))*coeb)*blastFreq*12/1000


}
