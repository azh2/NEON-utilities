##############################################################################################
#' @title Join data files in a zipped NEON data package by table type

#' @author
#' Christine Laney \email{claney@battelleecology.org}
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Given a zipped data file, do a full join of all data files, grouped by table type.
#' This should result in a small number of large files.

#' @param filepath The location of the zip file
#' @param savepath The location to save the output files to
#' @param folder T or F: does the filepath point to a parent, unzipped folder, or a zip file? If F, assumes the filepath points to a zip file. Defaults to F.
#' @param saveUnzippedFiles T or F: should the unzipped monthly data folders be retained?
#' @param dpID Data product ID of product to stack. Not needed; defaults to NA, included for back compatibility
#' @return All files are unzipped and one file for each table type is created and written. If savepath="envt" is specified, output is a named list of tables; otherwise, function output is null and files are saved to the location specified.

#' @examples
#' \dontrun{
#' # To unzip and merge files downloaded from the NEON Data Portal
#' stackByTable("~/NEON_par.zip")
#' 
#' # To unzip and merge files downloaded using zipsByProduct()
#' stackByTable("~/filesToStack00024", folder=T)
#' }

#' @export

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2017-07-02 (Christine Laney): Original creation
#   2017-09-28 (Claire Lunch): Add error messages
#   2018-04-03 (Christine Laney):
#     * Add error/warning messages for AOP, eddy covariance, and hemispheric
#       digital photo data products (and if the latter, don't allow user to remove the unzipped files).
#     * Allow user to specify the filepath to save to
#   2018-05-08 (Christine Laney):
#     * Remove extranous parameters dpID and package (obtain from data package)

##############################################################################################

stackByTable <- function(filepath, savepath=NA, folder=FALSE, saveUnzippedFiles=FALSE, dpID=NA){

  #### Check whether data should be stacked ####
  if(folder==FALSE){
    files <- listFilesInZip(filepath)
    files <- files$Name[grep(files$Name, pattern = "NEON.D[[:digit:]]{2}.[[:alpha:]]{4}.")]
    if(length(files) == 0){
      stop("Data files are not present in specified filepath.")
    }
  }

  if(folder==TRUE){
    files <- list.files(filepath, pattern = "NEON.D[[:digit:]]{2}.[[:alpha:]]{4}.")
    if(length(files) == 0){
      stop("Data files are not present in specified filepath.")
    }
  }

  dpID <- substr(files[1], 15, 27)
  package <- substr(files[1], nchar(files[1])-25, nchar(files[1])-21)
  if(package == "anded"){package <- "expanded"}

  # error message if dpID isn't formatted as expected
  if(regexpr("DP[1-4]{1}.[0-9]{5}.001",dpID)!=1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.001, where the first placeholder must be between 1 and 4.", sep=" "))
  }

  if(substr(dpID, 5, 5) == "3"){
    stop("This is an AOP data product, files cannot be stacked. Use byFileAOP() or byTileAOP() if you would like to download data.")
  }

  if(dpID == "DP4.00200.001"){
    stop("This eddy covariance data product is in HDF5 format and cannot be stacked.")
  }

  if(dpID == "DP1.10017.001" && package != 'basic'){
    saveUnzippedFiles = TRUE
    writeLines("Note: Digital hemispheric photos (in NEF format) cannot be stacked; only the CSV metadata files will be stacked.")
  }

  #### If all checks pass, unzip and stack files ####

  envt <- 0
  
  if(folder==FALSE) {
    if(is.na(savepath)){savepath <- substr(filepath, 1, nchar(filepath)-4)}
    if(savepath=="envt") {
      savepath <- file.path(tempdir(), paste("store", format(Sys.time(), "%Y%m%d%H%M%S"), sep=""))
      envt <- 1
    }
    orig <- list.files(savepath)
    if(length(grep(files, pattern = ".zip")) > 0){
      unzipZipfile(zippath = filepath, outpath = savepath, level = "all")
    }
  }

  if(folder==TRUE) {
    if(is.na(savepath)){savepath <- filepath}
    if(savepath=="envt") {
      savepath <- file.path(tempdir(), paste("store", format(Sys.time(), "%Y%m%d%H%M%S"), sep=""))
      envt <- 1
    }
    orig <- list.files(savepath)
    if(length(grep(files, pattern = ".zip")) > 0){
      unzipZipfile(zippath = filepath, outpath = savepath, level = "in")
    } else {
      if(length(grep(files, pattern = ".csv"))>0 & filepath!=savepath) {
        if(!dir.exists(savepath)){dir.create(savepath)}
        for(i in files) {
          file.copy(paste(filepath, i, sep="/"), savepath)
        }
      }
    }
  }

  stackDataFiles(savepath)
  
  if(saveUnzippedFiles == FALSE){cleanUp(savepath, orig)}
  
  if(envt==1) {
    ls <- list.files(paste(savepath, "stackedFiles", sep="/"))
    fls <- list(length(ls))
    ind <- 0
    for(i in unlist(ls)) {
      ind <- ind+1
      if(substring(i, nchar(i)-3, nchar(i))!=".csv") {
        next
      } else {
        fls[[ind]] <- utils::read.delim(paste(savepath, "stackedFiles", i, sep="/"), sep=",")
        names(fls)[ind] <- substring(i, 1, nchar(i)-4)
      }
    }
    return(fls)
    unlink(paste(savepath, "stackedFiles", sep="/"))
  }

}

