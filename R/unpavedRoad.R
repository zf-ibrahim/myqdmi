##' @export
# unpaved road

unpavedRoad <- function(quarryInput, particleSize = "tsp",
                   vehicleTravelled = quarryInput$unpavedVehicle, roadDistance = quarryInput$unpavedDist,
                   siltContent = quarryInput$silt, moistureContent = quarryInput$moisture, vehicleWeight = 10)

{
  quarryInput <- quarryInput

  if(particleSize == "tsp") {
    kpave = 2.82
  }else if (particleSize == "pm10"){
    kpave = 0.73
  }

  if(particleSize == "tsp") {
    a = 0.8
  }else if (particleSize == "pm10"){
    a = 0.8
  }

  if(particleSize == "tsp") {
    b = 0.5
  }else if (particleSize == "pm10"){
    b = 0.4
  }

  if(particleSize == "tsp") {
    c = 0.4
  }else if (particleSize == "pm10"){
    c = 0.3
  }

  #w is average of vehicle weight travelled on this road

  vehicleWeight <- 10

  ef <- kpave*(siltContent/12)^a*(vehicleWeight/3)^b/(moistureContent/0.2)^c

  ef*vehicleTravelled*12*(roadDistance/roadDistance)/1000/1000
}
