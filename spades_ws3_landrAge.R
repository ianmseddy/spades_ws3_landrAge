defineModule(sim, list(
  name = "spades_ws3_landrAge",
  description = "a simple module for linking LandR forest structure with spades_ws3",
  keywords = "",
  authors = structure(list(list(given = c("Ian"), family = "Eddy", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.0.9004", spades_ws3_landrAge = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "spades_ws3_landrAge.Rmd")),
  reqdPkgs = list('raster'),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant")),
    defineParameter('basenames', 'character', NA, NA, NA,
                    'vector of MU basenames to load, beginning with tsa, e.g. "tsa40"'),
    defineParameter('base.year', 'numeric', 2015, NA, NA, "base year of forest inventory data"),
    defineParameter("tifPath", "character", 'tif', NA, NA,
                    "the name of the directory where harvest tifs are stored (currently in inputs)")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = 'landscape', objectClass = 'RasterStack',
                 desc = 'a raster stack consisting of FMU, THLB, AU, Block ID, and stand age', sourceURL = NA),
    expectsInput(objectName = 'rstCurrentBurn', objectClass = 'RasterLayer',
                 desc = 'a binary raster representing annual burn')
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = 'rstCurrentHarvest', objectClass = 'RasterLayer',
                  desc = 'a raster representing annual harvest areas')
  )
))

## event types
#   - type `init` is required for initialization

doEvent.spades_ws3_landrAge = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "spades_ws3_landrAge", 'adjustBurnedPixels', eventPriority = 9)
      sim <- scheduleEvent(sim, time(sim), "spades_ws3_landrAge", "outputHarvestRst", eventPriority = 5.5)
    },

    adjustBurnedPixels = {

      if (!is.null(sim$rstCurrentBurn)){
        if (compareRaster(target = sim$landscape$age, current = sim$rstCurrentBurn)) {
        #adjust age of burned pixels - this module assumes annual burns
        sim$landscape$age[sim$rstCurrentBurn == 1] <- 0
        } else {
          warning("rstCurrentBurn properties do not align with sim$landscape$age")
        }
      } else {
        message(paste0('no rstCurrentBurn detected in year '), time(sim))
      }
      sim <- scheduleEvent(sim, time(sim) + 1, "spades_ws3_landrAge", "adjustBurnedPixels")

    },

    outputHarvestRst = {
      harvestYear <- P(sim)$base.year + time(sim) - start(sim)
      #e.g. 2015 + 2018 - 2011, if start(sim) != base.year
      sim$rstCurrentHarvest <- buildHarvest(harvestYear,
                                            basenames = P(sim)$basenames,
                                            tifPath = P(sim)$tifPath,
                                            inputPath = inputPath(sim))
      #harvest raster is binary
      sim$rstCurrentHarvest[sim$rstCurrentHarvest != 1] <- 0
      sim$rstCurrentHarvest@data@attributes$Year <- time(sim)

    },

    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot(sim$object)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
buildHarvest <- function(harvestYear, basenames, tifPath, inputPath) {

  filePaths <- file.path(inputPath, tifPath, basenames, paste0("projected_harvest_", harvestYear, ".tif"))
  outputRaster <- lapply(filePaths, raster)

  if (length(outputRaster) > 1){
    names(outputRaster)[1:2] <- c("x", "y") #needed for mosaic
    outputRaster$FUN <- 'mean'
    outputRaster$na.rm <- TRUE
    outputRaster <- do.call(mosaic, outputRaster)
    outputRaster[is.nan(outputRaster)] <- NA # replace NaN values with NA
  } else {
    outputRaster <- outputRaster[[1]]
  }

  return(outputRaster)
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
