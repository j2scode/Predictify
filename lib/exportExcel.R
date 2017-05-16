## ---- export_excel
#==============================================================================#
#                              exportExcel                                     #
#==============================================================================#
#'  exportExcel
#' 
#' This function takes as its parameters, a data frame and a path to an 
#' excel file, then write the data frame to the excel file.
#' 
#' @param df <- the data frame to be exported
#' @param path <- the path to the file
#' @author John James
#' @export
exportExcel <- function(df, path) {
  write.xlsx2(df, path)
} 
# library(xlsx)
# path <- file.path(directories$analysisDir, 'spectra.xlsx')
# df <- spectra
# exportExcel(df, path)