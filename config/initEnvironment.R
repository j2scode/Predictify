## ---- init_environment
#==============================================================================#
#                              initEnvironment                                 #
#==============================================================================#
#'  initEnvironment
#' 
#' This function initializes the environment with the configuration files,
#' the helper functions and the functions from the core analysis.  It also boosts 
#' the java memory allocations
#' 
#' @author John James
#' @export
initEnvironment <- function() {

  # Load project configuration
  message("Initializing Environment")
  message("...sourcing configuration files")
  source('./config/schema.R')
  library(utils)

  # Allocating Memory
  message("...boosting memory allocation")
  options(java.parameters = '-Xmx12g')
  
  # Sourcing helper functions
  message("...sourcing helper functions")
  functions <- list.files(directories$libDir, full.names = T)
  lapply(functions, source, echo = FALSE, verbose = FALSE)
  
  # Load packages
  message("...loading R packages")
  lapply(dependencies$rPackages, loadPackage)
  
  # Sourcing analysis scripts libraries
  message("...sourcing core analysis functions")
  core <- list.files(directories$sourceDir, full.names = T)
  lapply(core, source, echo = FALSE, verbose = FALSE)
  
  message("Environment Loaded\n")
}
initEnvironment()
## ---- end