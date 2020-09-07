# paved road

# unpaved road

pavedRoad <- function(quarryInput, particleSize = "tsp",
                        vehicleTravelled = quarryInput$pavedVehicle, roadDistance = quarryInput$pavedDist,
                        sp = 5, vehicleWeight = 10)

{
  quarryInput <- quarryInput

  if(particleSize == "tsp") {
    kpave = 24
  }else if (particleSize == "pm10"){
    kpave = 4.6
  }

  sp <- 5
  vehicleWeight <- 10

  kpave*(sp/2)^0.65*(vehicleWeight/3)^1.5*vehicleTravelled*roadDistance/roadDistance/1000*12/1000/1000


}
