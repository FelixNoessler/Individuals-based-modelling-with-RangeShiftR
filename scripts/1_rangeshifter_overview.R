#' ---
#' title: "Getting started with RangeShiftR"
#'
#' In this overview, we quickly go through the main functionality of the *RangeShiftR* R-package in order to get a first overview of the basic structure. It is also meant for setting up the appropriate folder structures on your machine.
#' 
#' Please note that the materials provided here are specifically designed for teaching and introduce specific exemplary case studies tailored towards our Master module *Quantitative conservation biogeography*. More elaborate tutorials for exploring the full functionality of RangeShiftR can be found here: https://rangeshifter.github.io/RangeshiftR-tutorials/
#' 
#' # Create a new project in RStudio
#' We recommend using RStudio throughout the course. If you have not already done so, please visit the RStudio website (http://rstudio.org/) and download and install RStudio for your operating system.
#'
#' **RStudio project**
#' 
#' We recommend setting up an RStudio project for the entire course and within the RStudio project separate R scripts for each practical. To do so:
#' 
#' - Open RStudio
#' - Go to the tab "File" and select "New Project"
#' - Select either "New Directory" if you haven't set up a course folder yet, or select "Existing Directory" to point towards an existing folder. This directory will be the working folder for the course. Navigate to the path of the project folder on your machine (e.g. ’~Documents/Courses/QCB)
#' - Name your project     
#' - Use your file explorer on your machine, find your project folder, and create five sub-folders: data, scripts, models, results, and figures
#' - In RStudio, create a new empty R script by going to the tab "File", select "New File"  and then "R script"
#' - In the new R script, type `# Session 1: Getting started with RangeShiftR` and save the file in your folder "scripts" within your project folder, e.g. as "1_GettingStarted.R"
#' 
#' # `RangeShiftR` basics
#' 
#' Within the standard workflow of *RangeShiftR*, a simulation is defined by:
#' 
#' 1) a so-called parameter master object that contains the simulation modules, which represent the model structure, as well as the (numeric) values of all necessary simulation parameters, and 
#' 2) the path to the working directory on the disc where the simulation inputs and outputs are stored.
#' 
#' The standard workflow of *RangeShiftR* is to load input maps from ASCII raster files and to write all simulation output into text files. Therefore, the specified working directory needs to have a certain folder structure: It should contain 3 sub-folders named 'Inputs', 'Outputs' and 'Output_Maps'.
#' 
#' ## Running a first working example
#' 
#' Load the package by typing:
## -----------------------------------------------------------------------------------------------------------------------------

library(RangeShiftR)

#' 
#' Create a parameter master object with all the default settings and store it:
## -----------------------------------------------------------------------------------------------------------------------------

s <- RSsim()

#' 
#' Use your file explorer on your machine, navigate to the "models" folder within your project, and create a sub-folder for the current practical, e.g. "GettingStarted". Next, we go back to your RStudio project and store the path in a variable. This can either be the relative path from your R working directory or the absolute path.
## -----------------------------------------------------------------------------------------------------------------------------

dirpath = "models/GettingStarted/"

#' 
#' Create the RS folder structure, if it doesn't yet exist:
dir.create(paste0(dirpath,"Inputs"), showWarnings = TRUE)
dir.create(paste0(dirpath,"Outputs"), showWarnings = TRUE)
dir.create(paste0(dirpath,"Output_Maps"), showWarnings = TRUE)

#' 
#' With this, we are already set to run our first simulation by typing:

RunRS(s,dirpath)

#' 
#' You should find the generated output - the simulation results as well as some log files - in the 'Outputs' folder.
#' 
#' # Simulation modules
#' 
#' To look at the parameter master in more detail, simply type: 
## -----------------------------------------------------------------------------------------------------------------------------

s


#' 
#' It contains of a number of parameter modules that each define different aspects of the RangeShifter simulation. Specifically, there are:
#' 
#'  - Simulation
#'  - Landscape
#'  - Demography
#'  - Dispersal
#'  - Genetics
#'  - Initialisation
#'  #'
#'
#' In the following, we go through some of the most relevant aspects of each module.
#' 
#' ## Simulation
#' 
#' This module is used to set general simulation parameters (e.g. simulation ID, number of replicates, and number of years to simulate) and to control output types (plus some more specific settings). For this overview, we will stick to the defaults:
## -----------------------------------------------------------------------------------------------------------------------------

sim <- Simulation(Simulation = 2,
                  Years = 50,
                  Replicates = 2,
                  OutIntPop = 50)

#' 
#' For detailed information on this module (or any other), please see the documentation.
## ---- eval=FALSE--------------------------------------------------------------------------------------------------------------

?Simulation

#' 
#' 
#' ## Landscape
#' 
#' *RangeShiftR* can either import a map from an ASCII raster file in the 'Inputs' folder or generate a random map to use in the simulation.
#' 
#' For each option, there is a corresponding function to create a Landscape parameter object

land <- ImportedLandscape()
land <- ArtificialLandscape()

#' 
#' Imported landscapes can provide either (binary or continuous) habitat suitability or land type codes. Furthermore, they can be either patch- or cell-based. We cover both types of landscapes in the remaining tutorials.
#' 
#' Artificially generated landscapes can only contain (binary or continuous) habitat suitability and are always cell-based.
#' 
#' For our example, we define an artificial landscape:
## -----------------------------------------------------------------------------------------------------------------------------

land <- ArtificialLandscape(Resolution = 10,  # in meters
                            K_or_DensDep = 1500,  # ~ 15 inds/cell
                            propSuit = 0.2,
                            dimX = 129, dimY = 257, 
                            fractal = T, hurst = 0.3,
                            continuous = F)


#' 
#' ## Demography
#' 
#' The Demography module contains all the local population dynamics of your simulated species. Generally there are two types:
#'  
#'  - Unstructured model / non-overlapping generations
#'  - Stage-structured model / overlapping generations
#'  
#' For the first case, create a simple `Demography()` module (the maximum growth rate `Rmax`  is the only required parameter)
## -----------------------------------------------------------------------------------------------------------------------------

demo <- Demography(Rmax = 2.2, ReproductionType = 1, PropMales = 0.45)

#' 
#' The option `ReproductionType` determines the way that different sexes are considered:
#' 
#'  0 = asexual / only female model    
#'  1 = simple sexual model    
#'  2 = sexual model with explicit mating system    
#' 
#' In order to make a stage-structured model, we have to additionally create a stage-structure sub-module within the Demography module. Here, we have already defined a Demography object and can use '+' to add the StageStructure sub-module.
## -----------------------------------------------------------------------------------------------------------------------------

stg <- StageStructure(Stages = 3,
                      TransMatrix = matrix(c(0,1,0,5.7,.5,.4,3.4,0,.9),nrow = 3),
                      FecDensDep = T,
                      SurvDensDep = T)
demo <- demo + stg

#' 
#' Alternatively, we define the sub-module within the Demography module:
## -----------------------------------------------------------------------------------------------------------------------------

demo <- Demography(StageStruct = stg, ReproductionType = 1, PropMales = 0.45)

#' 
#' *RangeShiftR* provides a number of useful functions to explore the model set-up. For example, we can plot the rates from the transition matrix:
## -----------------------------------------------------------------------------------------------------------------------------

plotProbs(stg)

#' 
#' 
#' ## Dispersal
#' 
#' The dispersal process is modelled wih three sub-processes (see the schematic figure above): `Emigration()`, `Transfer()` and `Settlement()`.
## -----------------------------------------------------------------------------------------------------------------------------

disp <-  Dispersal(Emigration = Emigration(EmigProb = 0.2),
                   Transfer   = DispersalKernel(Distances = 50),
                   Settlement = Settlement() )

#' 
#' We can use the function `plotProbs()` to plot various functional relationships, for example a dispersal kernel with a stage-dependent mean transfer distance:
## -----------------------------------------------------------------------------------------------------------------------------

plotProbs(DispersalKernel(Distances = matrix(c(0,1,2,70,50,30),nrow = 3), StageDep = T))
plotProbs(DispersalKernel(Distances = 50))

#' 
#' This is using mostly default options. For example, we can change the settlement condition so that a female individual, that arrives in an unsuitable cell, will wait for another time step and disperse again, while the males will die if arriving in an unsuitable cell:
## -----------------------------------------------------------------------------------------------------------------------------

disp <-  disp + Settlement(SexDep = T,
                           Settle = matrix(c(0,1,1,0), nrow = 2))

#' 
#' ## Initialisation
#' 
#' In order to control the initial distribution of individuals in the landscape at year *0*, we set initialisation rules. We choose to initialise *3* individuals per habitat cell. Additionally, since we define a stage-structured model, we have to specify the initial proportion of stages:
## -----------------------------------------------------------------------------------------------------------------------------

init <- Initialise(FreeType = 0,
                   NrCells = 2250,
                   InitDens = 2, 
                   IndsHaCell = 3, 
                   PropStages = c(0,0.7,0.3))
init

#' 
#' 
#' ## Parameter master
#' 
#' After all settings have been made in their respective modules, we are ready to combine them to a parameter master object, which is needed to run the simulation.
#' 
## -----------------------------------------------------------------------------------------------------------------------------

s <- RSsim(simul = sim, land = land, demog = demo, dispersal = disp, init = init)

# Alternative notation:
# s <- RSsim() + land + demo + disp + sim + init + gene

#' 
#' We can check the parameter master (or any single module) for potential parameter conflicts:
## -----------------------------------------------------------------------------------------------------------------------------

validateRSparams(s)

#' 
#' 
#' ## Run the simulation
#' 
#' Once the parameter master has been defined, we can run the simulations in the specified RS directory.

RunRS(s, dirpath)

#' 
#' 
#' # Plot results
#' 
#' All results are stored in the Outputs folder. *RangeShiftR* provides some in-built functions to access and plot these results. 
#' Here, we plot the abundance and occupancy time series:

range_df <- readRange(s, dirpath)

# ...with replicates:
par(mfrow=c(1,2))
plotAbundance(range_df)
plotOccupancy(range_df)

# ...with standard deviation:
par(mfrow=c(1,2))
plotAbundance(range_df, sd=T, replicates = F)
plotOccupancy(range_df, sd=T, replicates = F)

#' 
#' Although *RangeShiftR* provides a number of functions for easy post-processing and plotting of results, it may also be desirable to further process the results yourself. As a simple example for such a workflow, let's plot the spatial distribution of abundance. For doing so, we load the data from the population output files and process it using the `raster` package:
# read population output file into a dataframe

pop_df <- readPop(s, dirpath)

# Not all years have the same number of populated and thus listed cells. For stacking, we set a common extent with the values used in the landscape module:

ext <- c(0,1290,0,2570)
res <- 10

# Make stack of different raster layers for each year and for only one repetition (Rep==0):

pop_wide_rep0 <- reshape(subset(pop_df,Rep==0)[,c('Year','x','y','NInd')], timevar='Year', v.names=c('NInd'), idvar=c('x','y'), direction='wide')

# use raster package to make a raster from the data frame

library(raster)
stack_years_rep0 <- rasterFromXYZ(pop_wide_rep0)
names(stack_years_rep0) <- c('Year.0', 'Year.50')
spplot(stack_years_rep0, zlim = c(0,10))

