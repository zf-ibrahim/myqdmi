##' @export
#calculate all the results

sourceModel <- function(quarryInput = quarryInput,
                        sourceInput = sourceInput,
                        receptorInput = receptorInput,
                        windInput = windInput,
                        particleSize = "tsp"
                        ){

  size <- particleSize

quarryInput <- quarryInput
sourceInput <- sourceInput
receptorInput <- receptorInput
windInput <- windInput



a <- primaryCrusher(quarryInput, particleSize = size)

b <- secondaryCrusher(quarryInput, particleSize = size)

c <- tertiaryCrusher(quarryInput, particleSize = size)

d <- stockpile(quarryInput, particleSize = size)

e <- screening(quarryInput, particleSize = size, QuarryProduction = quarryInput$quarryProduction,
               totalScreen = quarryInput$nScreen)

f <- unpavedRoad(quarryInput, particleSize = size)

g <- pavedRoad(quarryInput, particleSize = size)

h <- blasting(quarryInput, particleSize = size, areaBlasted = quarryInput$areaBlasted,
              blastFreq = quarryInput$blastingFreq, blastHole.depth = quarryInput$blasthole, moistureContent = quarryInput$moisture)


ii <- drilling(quarryInput, particleSize = size,
               drillHole = quarryInput$drillhole)


results <- rep(NA,nrow(receptorInput))

for(i in 1:nrow(sourceInput)){

  results[i] <- list(


    source.model(sourceInput, receptorInput, sourceActivity =

                  sourceInput[sourceInput$sourceActivity == sourceInput[i, "sourceActivity"],"sourceActivity"]


                , windInput, vg = 5, AnemometerHeight = 16,
                dustGenerated = if(sourceInput[i, "type"]== "primary crusher"){
                  a
                }else if(sourceInput[i, "type"]== "secondary crusher"){
                  b
                }else if(sourceInput[i, "type"]== "tertiary crusher"){
                  c
                }else if(sourceInput[i, "type"]== "stockpile"){
                  d
                }else if(sourceInput[i, "type"]== "screening"){
                  e
                }else if(sourceInput[i, "type"]== "unpaved road"){
                  f
                }else if(sourceInput[i, "type"]== "paved road"){
                  g
                }else if(sourceInput[i, "type"]== "blasting"){
                  h
                }else if(sourceInput[i, "type"]== "drilling"){
                  ii
                }


    )


  )
}


myresults <- data.frame(matrix(unlist(results), nrow=NROW(results), ncol = nrow(receptorInput), byrow=T),stringsAsFactors=F)

receptorName <- receptorInput$receptor

myresults <- rbind(receptorName, myresults)

sourceSite <- c("sites" ,sourceInput$sourceActivity)


myresults <- cbind(sourceSite, myresults)

names(myresults) <- as.matrix(myresults[1, ])
myresults <- myresults[-1, ]
myresults[] <- lapply(myresults, function(x) type.convert(as.character(x)))

myresults



}
