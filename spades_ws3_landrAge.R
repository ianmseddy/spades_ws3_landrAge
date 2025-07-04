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
  loadOrder = list(after = c("spades_ws3_dataInit")),
  reqdPkgs = list('terra', 'data.table'),
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
    expectsInput(objectName = 'landscape', objectClass = 'SpatRaster',
                 desc = 'a raster stack consisting of FMU, THLB, AU, Block ID, and stand age', sourceURL = NA),
    expectsInput(objectName = "rasterToMatch", objectClass = "SpatRaster", desc = "foo"),
    expectsInput(objectName = 'rstCurrentBurn', objectClass = 'SpatRaster',
                 desc = 'a binary raster representing annual burn'),
    expectsInput(objectName = 'pixelGroupMap', objectClass = 'SpatRaster',
                 desc = 'map of pixelGroups in LandR simulations'),
    expectsInput(objectName = "studyArea", objectClass = "SpatVector", desc = "foo"),
    expectsInput(objectName = 'cohortData', objectClass = 'data.table',
                 desc = "Columns: B, pixelGroup, speciesCode, Indicating several features about ages and current vegetation of stand")
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = 'rstCurrentHarvest', objectClass = 'SpatRaster',
                  desc = 'a raster representing annual harvest areas'),
    createsOutput(objectName = 'harvestStats', objectClass = 'data.frame',
                  desc = 'data.frame witih simple harvest reporting over landscape'),
    createsOutput(objectName = 'harvestedCohorts', objectClass = 'data.table',
                  desc = 'contains species, age, and biomass of harvested cohorts'),
    createsOutput(objectName = 'harvestPixelHistory', objectClass = 'data.table',
                  desc = 'table with pixel id and year of harvested pixels')
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
        if (compareRaster(sim$landscape$age, sim$rstCurrentBurn)) {
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
      rstCurrentHarvest <- buildHarvest(harvestYear,
                                        basenames = P(sim)$basenames,
                                        tifPath = P(sim)$tifPath,
                                        inputPath = inputPath(sim))

      ws3count <- sum(rstCurrentHarvest[] == 1, na.rm = TRUE)

      rstCurrentHarvest[is.na(rstCurrentHarvest)] <- 0
      rstCurrentHarvest[is.na(sim$pixelGroupMap)] <- NA
      sim$rstCurrentHarvest <- rstCurrentHarvest
      landrCount <- sum(sim$rstCurrentHarvest[] == 1, na.rm = TRUE)

      currentHarvestStats <- data.frame('ws3_harvestArea_pixels' = ws3count,
                                        'LandR_harvestArea_pixels' = landrCount,
                                        'year' = time(sim))

      harvestOutputs<- makeHarvestedCohorts(pixelGroupMap = sim$pixelGroupMap,
                                            rstCurrentHarvest = sim$rstCurrentHarvest,
                                            cohortData = sim$cohortData,
                                            currentTime = time(sim))

      sim$harvestedCohorts <- harvestOutputs$harvestedCohorts
      sim$harvestPixelHistory <- rbind(sim$harvestPixelHistory, harvestOutputs$harvestPixelHistory)
      sim$harvestStats <- rbind(sim$harvestStats, currentHarvestStats)

      sim <- scheduleEvent(sim, time(sim) + 1, 'spades_ws3_landrAge', 'outputHarvestRst', eventPriority = 5.5)
    },

    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  sim$harvestStats <- data.frame('ws3_harvestArea_pixels' = numeric(0), 'LandR_harvestArea_pixels' = numeric(0),
                                 'year' = numeric(0))
  sim$harvestPixelHistory <- data.table( 'pixelIndex' = numeric(0), 'year' = numeric(0))
  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  sim <- saveFiles(sim)
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {

  Plot(sim$rstCurrentHarvest)
  return(invisible(sim))
}

### template for your event1
buildHarvest <- function(harvestYear, basenames, tifPath, inputPath) {

  filePaths <- file.path(inputPath, tifPath, basenames, paste0("projected_harvest_", harvestYear, ".tif"))
  outputRaster <- lapply(filePaths, FUN = rast)

  if (length(outputRaster) > 1){


    outputRaster <- rast(outputRaster)
    outputRaster[is.nan(outputRaster)] <- NA # replace NaN values with NA
  } else {
    outputRaster <- outputRaster[[1]]
  }

  return(outputRaster)
}

### template for your event2
makeHarvestedCohorts <- function(pixelGroupMap, rstCurrentHarvest, cohortData, currentTime) {

  #this object is necessary in the event harvest occurs on a pixelGroup 0.
  #this is possible if the pixelGroup is at longevity or gets burned.
  #For this reason, we retain the cohort info here.
  browser()
  cdLong <- data.table(pixelGroup = as.vector(pixelGroupMap),
                       pixelIndex = 1:ncell(pixelGroupMap),
                       harvest = as.vector(rstCurrentHarvest)) |>
    na.omit()
  cdLong <- cdLong[harvest == 1,]

  #must be cartesian because multiple cohorts, multiple pixels per PG
  harvestedPixels <- cohortData[cdLong, on = c('pixelGroup'), allow.cartesian = TRUE]

  harvestPixelHistory <- data.frame('pixelIndex' = cdLong$pixelIndex, 'year' = rep(currentTime, length = length(cdLong$pixelIndex)))
  return(list('harvestedCohorts' = harvestedPixels, 'harvestPixelHistory' = harvestPixelHistory))
}

.inputObjects <- function(sim) {

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  if (!suppliedElsewhere("studyArea", sim)) {
    studyArea <- sim$landscape[[1]]
    studyArea <- rast(ext(studyArea))
  }

  if (!suppliedElsewhere("rasterToMatch", sim)) {
    sim$rasterToMatch <- terra::rast(sim$landscape[[1]])
    #get the spatial attributes
    sim$rasterToMatch[] <- 1
    sim$rasterToMatch <- mask(sim$rasterToMatch, sim$studyArea)
  }

  if (!suppliedElsewhere("pixelGroupMap", sim)) {
    sim$pixelGroupMap <- rast(sim$landscape[[1]])
    sim$pixelGroupMap[] <- 1:ncell(sim$landscape)
  }

  if (!suppliedElsewhere("cohortData", sim)) {
    sim$cohortData <- data.table(
      #lucky numbers
      "ecoregionGroup" = as.factor("ecofoo_13"),
      "speciesCode" = "foo6",
      "pixelGroup" = unique(as.vector(sim$pixelGroupMap), na.rm = TRUE),
      "B" = 7777,
      "age" = 42)
  }
  browser()

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
