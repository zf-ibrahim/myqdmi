# Granite Receptor Model

#2.98 - 0.0123(AWS) - 0.00073(DSR) + 0.0006((ABA + TDH)/2) + 0.0089((NWB+WB)/2)


receptorModel.granite <- function(sourceInput, receptorInput, windInput, sourceActivity = "primaryCrusher"){


sourceInput <- sourceInput
receptorInput <- receptorInput
windInput <- windInput
sourceName <- sourceActivity



nwb<-(as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "A", windClass = "class1", windDirection = "S"),
                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "A", windClass = "class1", windDirection = "N"),
                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "A", windClass = "class1", windDirection = "E"),
                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "A", windClass = "class1", windDirection = "W"),
                                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "A", windClass = "class1", windDirection = "SW"),
                                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "A", windClass = "class1", windDirection = "SE"),
                                                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "A", windClass = "class1", windDirection = "NE"),
                                                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "A", windClass = "class1", windDirection = "NW"),
                                                                     "error")))))))))

+as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "A", windClass = "class2", windDirection = "S"),
                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "A", windClass = "class2", windDirection = "N"),
                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "A", windClass = "class2", windDirection = "E"),
                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "A", windClass = "class2", windDirection = "W"),
                                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "A", windClass = "class2", windDirection = "SW"),
                                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "A", windClass = "class2", windDirection = "SE"),
                                                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "A", windClass = "class2", windDirection = "NE"),
                                                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "A", windClass = "class2", windDirection = "NW"),
                                                                     "error")))))))))

  +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "A", windClass = "class3", windDirection = "S"),
                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "A", windClass = "class3", windDirection = "N"),
                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "A", windClass = "class3", windDirection = "E"),
                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "A", windClass = "class3", windDirection = "W"),
                                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "A", windClass = "class3", windDirection = "SW"),
                                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "A", windClass = "class3", windDirection = "SE"),
                                                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "A", windClass = "class3", windDirection = "NE"),
                                                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "A", windClass = "class3", windDirection = "NW"),
                                                                       "error")))))))))

   +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "A", windClass = "class4", windDirection = "S"),
                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "A", windClass = "class4", windDirection = "N"),
                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "A", windClass = "class4", windDirection = "E"),
                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "A", windClass = "class4", windDirection = "W"),
                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "A", windClass = "class4", windDirection = "SW"),
                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "A", windClass = "class4", windDirection = "SE"),
                                                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "A", windClass = "class4", windDirection = "NE"),
                                                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "A", windClass = "class4", windDirection = "NW"),
                                                                        "error")))))))))

   +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "B", windClass = "class1", windDirection = "S"),
                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "B", windClass = "class1", windDirection = "N"),
                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "B", windClass = "class1", windDirection = "E"),
                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "B", windClass = "class1", windDirection = "W"),
                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "B", windClass = "class1", windDirection = "SW"),
                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "B", windClass = "class1", windDirection = "SE"),
                                                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "B", windClass = "class1", windDirection = "NE"),
                                                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "B", windClass = "class1", windDirection = "NW"),
                                                                        "error")))))))))

   +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "B", windClass = "class2", windDirection = "S"),
                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "B", windClass = "class2", windDirection = "N"),
                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "B", windClass = "class2", windDirection = "E"),
                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "B", windClass = "class2", windDirection = "W"),
                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "B", windClass = "class2", windDirection = "SW"),
                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "B", windClass = "class2", windDirection = "SE"),
                                                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "B", windClass = "class2", windDirection = "NE"),
                                                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "B", windClass = "class2", windDirection = "NW"),
                                                                        "error")))))))))

  +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "B", windClass = "class3", windDirection = "S"),
                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "B", windClass = "class3", windDirection = "N"),
                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "B", windClass = "class3", windDirection = "E"),
                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "B", windClass = "class3", windDirection = "W"),
                                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "B", windClass = "class3", windDirection = "SW"),
                                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "B", windClass = "class3", windDirection = "SE"),
                                                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "B", windClass = "class3", windDirection = "NE"),
                                                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "B", windClass = "class3", windDirection = "NW"),
                                                                       "error")))))))))

   +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "B", windClass = "class4", windDirection = "S"),
                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "B", windClass = "class4", windDirection = "N"),
                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "B", windClass = "class4", windDirection = "E"),
                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "B", windClass = "class4", windDirection = "W"),
                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "B", windClass = "class4", windDirection = "SW"),
                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "B", windClass = "class4", windDirection = "SE"),
                                                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "B", windClass = "class4", windDirection = "NE"),
                                                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "B", windClass = "class4", windDirection = "NW"),
                                                                        "error")))))))))

  +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "C", windClass = "class1", windDirection = "S"),
                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "C", windClass = "class1", windDirection = "N"),
                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "C", windClass = "class1", windDirection = "E"),
                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "C", windClass = "class1", windDirection = "W"),
                                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "C", windClass = "class1", windDirection = "SW"),
                                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "C", windClass = "class1", windDirection = "SE"),
                                                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "C", windClass = "class1", windDirection = "NE"),
                                                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "C", windClass = "class1", windDirection = "NW"),
                                                                       "error")))))))))

   +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "C", windClass = "class2", windDirection = "S"),
                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "C", windClass = "class2", windDirection = "N"),
                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "C", windClass = "class2", windDirection = "E"),
                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "C", windClass = "class2", windDirection = "W"),
                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "C", windClass = "class2", windDirection = "SW"),
                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "C", windClass = "class2", windDirection = "SE"),
                                                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "C", windClass = "class2", windDirection = "NE"),
                                                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "C", windClass = "class2", windDirection = "NW"),
                                                                        "error")))))))))

  +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "C", windClass = "class3", windDirection = "S"),
                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "C", windClass = "class3", windDirection = "N"),
                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "C", windClass = "class3", windDirection = "E"),
                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "C", windClass = "class3", windDirection = "W"),
                                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "C", windClass = "class3", windDirection = "SW"),
                                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "C", windClass = "class3", windDirection = "SE"),
                                                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "C", windClass = "class3", windDirection = "NE"),
                                                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "C", windClass = "class3", windDirection = "NW"),
                                                                       "error")))))))))

   +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "C", windClass = "class4", windDirection = "S"),
                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "C", windClass = "class4", windDirection = "N"),
                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "C", windClass = "class4", windDirection = "E"),
                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "C", windClass = "class4", windDirection = "W"),
                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "C", windClass = "class4", windDirection = "SW"),
                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "C", windClass = "class4", windDirection = "SE"),
                                                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "C", windClass = "class4", windDirection = "NE"),
                                                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "C", windClass = "class4", windDirection = "NW"),
                                                                        "error")))))))))

  +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "D", windClass = "class1", windDirection = "S"),
                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "D", windClass = "class1", windDirection = "N"),
                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "D", windClass = "class1", windDirection = "E"),
                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "D", windClass = "class1", windDirection = "W"),
                                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "D", windClass = "class1", windDirection = "SW"),
                                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "D", windClass = "class1", windDirection = "SE"),
                                                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "D", windClass = "class1", windDirection = "NE"),
                                                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "D", windClass = "class1", windDirection = "NW"),
                                                                       "error")))))))))

   +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "D", windClass = "class2", windDirection = "S"),
                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "D", windClass = "class2", windDirection = "N"),
                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "D", windClass = "class2", windDirection = "E"),
                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "D", windClass = "class2", windDirection = "W"),
                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "D", windClass = "class2", windDirection = "SW"),
                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "D", windClass = "class2", windDirection = "SE"),
                                                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "D", windClass = "class2", windDirection = "NE"),
                                                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "D", windClass = "class2", windDirection = "NW"),
                                                                        "error")))))))))

  +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "D", windClass = "class3", windDirection = "S"),
                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "D", windClass = "class3", windDirection = "N"),
                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "D", windClass = "class3", windDirection = "E"),
                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "D", windClass = "class3", windDirection = "W"),
                                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "D", windClass = "class3", windDirection = "SW"),
                                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "D", windClass = "class3", windDirection = "SE"),
                                                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "D", windClass = "class3", windDirection = "NE"),
                                                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "D", windClass = "class3", windDirection = "NW"),
                                                                       "error")))))))))

   +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "D", windClass = "class4", windDirection = "S"),
                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "D", windClass = "class4", windDirection = "N"),
                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "D", windClass = "class4", windDirection = "E"),
                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "D", windClass = "class4", windDirection = "W"),
                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "D", windClass = "class4", windDirection = "SW"),
                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "D", windClass = "class4", windDirection = "SE"),
                                                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "D", windClass = "class4", windDirection = "NE"),
                                                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "D", windClass = "class4", windDirection = "NW"),
                                                                        "error")))))))))

  +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "E", windClass = "class1", windDirection = "S"),
                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "E", windClass = "class1", windDirection = "N"),
                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "E", windClass = "class1", windDirection = "E"),
                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "E", windClass = "class1", windDirection = "W"),
                                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "E", windClass = "class1", windDirection = "SW"),
                                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "E", windClass = "class1", windDirection = "SE"),
                                                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "E", windClass = "class1", windDirection = "NE"),
                                                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "E", windClass = "class1", windDirection = "NW"),
                                                                       "error")))))))))

   +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "E", windClass = "class2", windDirection = "S"),
                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "E", windClass = "class2", windDirection = "N"),
                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "E", windClass = "class2", windDirection = "E"),
                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "E", windClass = "class2", windDirection = "W"),
                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "E", windClass = "class2", windDirection = "SW"),
                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "E", windClass = "class2", windDirection = "SE"),
                                                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "E", windClass = "class2", windDirection = "NE"),
                                                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "E", windClass = "class2", windDirection = "NW"),
                                                                        "error")))))))))

  +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "E", windClass = "class3", windDirection = "S"),
                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "E", windClass = "class3", windDirection = "N"),
                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "E", windClass = "class3", windDirection = "E"),
                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "E", windClass = "class3", windDirection = "W"),
                                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "E", windClass = "class3", windDirection = "SW"),
                                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "E", windClass = "class3", windDirection = "SE"),
                                                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "E", windClass = "class3", windDirection = "NE"),
                                                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "E", windClass = "class3", windDirection = "NW"),
                                                                       "error")))))))))

   +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "E", windClass = "class4", windDirection = "S"),
                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "E", windClass = "class4", windDirection = "N"),
                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "E", windClass = "class4", windDirection = "E"),
                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "E", windClass = "class4", windDirection = "W"),
                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "E", windClass = "class4", windDirection = "SW"),
                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "E", windClass = "class4", windDirection = "SE"),
                                                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "E", windClass = "class4", windDirection = "NE"),
                                                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "E", windClass = "class4", windDirection = "NW"),
                                                                        "error")))))))))

  +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "F", windClass = "class1", windDirection = "S"),
                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "F", windClass = "class1", windDirection = "N"),
                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "F", windClass = "class1", windDirection = "E"),
                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "F", windClass = "class1", windDirection = "W"),
                                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "F", windClass = "class1", windDirection = "SW"),
                                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "F", windClass = "class1", windDirection = "SE"),
                                                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "F", windClass = "class1", windDirection = "NE"),
                                                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "F", windClass = "class1", windDirection = "NW"),
                                                                       "error")))))))))

   +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "F", windClass = "class2", windDirection = "S"),
                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "F", windClass = "class2", windDirection = "N"),
                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "F", windClass = "class2", windDirection = "E"),
                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "F", windClass = "class2", windDirection = "W"),
                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "F", windClass = "class2", windDirection = "SW"),
                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "F", windClass = "class2", windDirection = "SE"),
                                                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "F", windClass = "class2", windDirection = "NE"),
                                                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "F", windClass = "class2", windDirection = "NW"),
                                                                        "error")))))))))

  +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "F", windClass = "class3", windDirection = "S"),
                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "F", windClass = "class3", windDirection = "N"),
                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "F", windClass = "class3", windDirection = "E"),
                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "F", windClass = "class3", windDirection = "W"),
                                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "F", windClass = "class3", windDirection = "SW"),
                                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "F", windClass = "class3", windDirection = "SE"),
                                                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "F", windClass = "class3", windDirection = "NE"),
                                                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "F", windClass = "class3", windDirection = "NW"),
                                                                       "error")))))))))

   +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "F", windClass = "class4", windDirection = "S"),
                       ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "F", windClass = "class4", windDirection = "N"),
                              ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "F", windClass = "class4", windDirection = "E"),
                                     ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "F", windClass = "class4", windDirection = "W"),
                                            ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "F", windClass = "class4", windDirection = "SW"),
                                                   ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "F", windClass = "class4", windDirection = "SE"),
                                                          ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "F", windClass = "class4", windDirection = "NE"),
                                                                 ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "F", windClass = "class4", windDirection = "NW"),
                                                                        "error")))))))))
  +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "A", windClass = "calm", windDirection = "S"),
                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "A", windClass = "calm", windDirection = "N"),
                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "A", windClass = "calm", windDirection = "E"),
                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "A", windClass = "calm", windDirection = "W"),
                                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "A", windClass = "calm", windDirection = "SW"),
                                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "A", windClass = "calm", windDirection = "SE"),
                                                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "A", windClass = "calm", windDirection = "NE"),
                                                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "A", windClass = "calm", windDirection = "NW"),
                                                                       "error")))))))))
  +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "B", windClass = "calm", windDirection = "S"),
                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "B", windClass = "calm", windDirection = "N"),
                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "B", windClass = "calm", windDirection = "E"),
                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "B", windClass = "calm", windDirection = "W"),
                                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "B", windClass = "calm", windDirection = "SW"),
                                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "B", windClass = "calm", windDirection = "SE"),
                                                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "B", windClass = "calm", windDirection = "NE"),
                                                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "B", windClass = "calm", windDirection = "NW"),
                                                                       "error")))))))))
  +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "C", windClass = "calm", windDirection = "S"),
                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "C", windClass = "calm", windDirection = "N"),
                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "C", windClass = "calm", windDirection = "E"),
                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "C", windClass = "calm", windDirection = "W"),
                                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "C", windClass = "calm", windDirection = "SW"),
                                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "C", windClass = "calm", windDirection = "SE"),
                                                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "C", windClass = "calm", windDirection = "NE"),
                                                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "C", windClass = "calm", windDirection = "NW"),
                                                                       "error")))))))))
  +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "D", windClass = "calm", windDirection = "S"),
                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "D", windClass = "calm", windDirection = "N"),
                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "D", windClass = "calm", windDirection = "E"),
                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "D", windClass = "calm", windDirection = "W"),
                                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "D", windClass = "calm", windDirection = "SW"),
                                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "D", windClass = "calm", windDirection = "SE"),
                                                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "D", windClass = "calm", windDirection = "NE"),
                                                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "D", windClass = "calm", windDirection = "NW"),
                                                                       "error")))))))))
  +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "E", windClass = "calm", windDirection = "S"),
                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "E", windClass = "calm", windDirection = "N"),
                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "E", windClass = "calm", windDirection = "E"),
                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "E", windClass = "calm", windDirection = "W"),
                                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "E", windClass = "calm", windDirection = "SW"),
                                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "E", windClass = "calm", windDirection = "SE"),
                                                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "E", windClass = "calm", windDirection = "NE"),
                                                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "E", windClass = "calm", windDirection = "NW"),
                                                                       "error")))))))))
  +as.numeric(ifelse( bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "N", windFreq(windInput, stability = "F", windClass = "calm", windDirection = "S"),
                      ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "s", windFreq(windInput, stability = "F", windClass = "calm", windDirection = "N"),
                             ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "w", windFreq(windInput, stability = "F", windClass = "calm", windDirection = "E"),
                                    ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "E", windFreq(windInput, stability = "F", windClass = "calm", windDirection = "W"),
                                           ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NE", windFreq(windInput, stability = "F", windClass = "calm", windDirection = "SW"),
                                                  ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "NW", windFreq(windInput, stability = "F", windClass = "calm", windDirection = "SE"),
                                                         ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SW", windFreq(windInput, stability = "F", windClass = "calm", windDirection = "NE"),
                                                                ifelse(bearing(sourceInput, receptorInput, sourceActivity = sourceName) == "SE", windFreq(windInput, stability = "F", windClass = "calm", windDirection = "NW"),
                                                                       "error"))))))))))


totalwind <- (calmFreq(windInput, calm = F)+calmFreq(windInput, calm = T)+1)

pwb <- nwb/totalwind*100

aws <- mean(windInput$ws)

tdh <- quarryInput$drillhole

aba <- quarryInput$areaBlasted

dsr <- distance(sourceInput, receptorInput, sourceActivity = sourceName)

2.98 - (0.0123*(aws)) - (0.00073*(dsr)) + (0.0006*((aba + tdh)/2)) + (0.0089*((nwb+pwb)/2))
}
