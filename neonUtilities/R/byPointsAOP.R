##############################################################################################
#' @title Serially download a set of AOP files for a given set of woody vegetatation structure points.

#' @author
#' Ben Weinstein \email{ben.weinstein@weecology.org}

#' @description
#' Query the API for AOP data by site, year, and product, and download all files found, preserving original
#' folder structure. Downloads serially to avoid overload; may take a very long time.
#'
#' @param dpID The identifier of the NEON data product to pull, in the form DPL.PRNUM.REV, e.g. DP1.10023.001
#' @param site The four-letter code of a single NEON site, e.g. 'CLBJ'.
#' @param year The four-digit year to search for data. Defaults to 2017.
#' @param check.size T or F, should the user be told the total file size before downloading? Defaults to T. When working in batch mode, or other non-interactive workflow, use check.size=F.
#' @param savepath The file path to download to. Defaults to NA, in which case the working directory is used.
#' @param allSites Boolean. Download data for all sites and years.
#' @return A folder in the working directory, containing all files meeting query criteria.
#' @importFrom magrittr "%>%"
#' 
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

##############################################################################################

byPointsAOP <- function(dpID, site="SJER", year="2017", check.size=TRUE, savepath=NA,allSites=F) {
  
  # error message if dpID isn't formatted as expected
  if(regexpr("DP[1-4]{1}.[0-9]{5}.001",dpID)!=1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.001", sep=" "))
  }
  
  # query the products endpoint for the product requested
  productUrl <- paste0("http://data.neonscience.org/api/v0/products/", dpID)
  req <- httr::GET(productUrl)
  avail <- jsonlite::fromJSON(httr::content(req, as="text"), simplifyDataFrame=TRUE, flatten=TRUE)
  
  # error message if product not found
  if(!is.null(avail$error$status)) {
    stop(paste("No data found for product", dpID, sep=" "))
  }
  
  # error message if data are not from AOP
  if(avail$data$productScienceTeamAbbr!="AOP") {
    stop(paste(dpID, "is not a remote sensing product. Use zipsByProduct()"))
  }
  
  # get the urls for months with data available, and subset to site
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  
  #Get a specific site, or just select a year
  if(!allSites==T){
    month.urls <- month.urls[grep(paste(site, year, sep="/"), month.urls)]
  }
  
  # error message if nothing is available
  if(length(month.urls)==0) {
    return("There are no data at the selected site and year.")
  }
  
  # get and stash the file names, S3 URLs, file size, and download status (default = 0) in a data frame
  
  file.urls.current <- getFileUrls(month.urls)
  selected_tiles<-screenurls(site,file.urls.current,dpID,savepath)
  
  if(nrow(selected_tiles)==0){
    print(paste(site,"no tiles to download"))
    return(NULL)
  }
  
  filter.size <- sum(as.numeric(as.character(selected_tiles$size)), na.rm=T)
  filter.size.read <- humanReadable(filter.size, units = "auto", standard = "SI")
  
  downld.size <- sum(as.numeric(as.character(file.urls.current$size)), na.rm=T)
  downld.size.read <- humanReadable(downld.size, units = "auto", standard = "SI")
  
  print(paste("Downloading" ,filter.size.read, "of",downld.size.read,"catalog"))
  
  # ask user if they want to proceed
  # can disable this with check.size=F
  if(check.size==TRUE) {
    resp <- readline(paste("Continuing will download", nrow(selected_tiles), "files totaling approximately",
                           filter.size.read, ". Do you want to proceed y/n: ", sep=" "))
    if(!(resp %in% c("y","Y"))) {
      stop("Download halted.")
    }
  }
  
  # create folder in working directory to put files in
  if(is.na(savepath)) {
    filepath <- paste(getwd(), "/", dpID, sep="")
  } else {
    filepath <- paste(savepath, "/", dpID, sep="")
  }
  if(dir.exists(filepath) == F) dir.create(filepath, showWarnings=T)
  
  # copy zip files into folder
  j <- 1
  messages <- list()
  while(j <= nrow(selected_tiles)) {
    print(j)
    path1 <- strsplit(selected_tiles$URL[j], "\\?")[[1]][1]
    pathparts <- strsplit(path1, "\\/")
    path2 <- paste(pathparts[[1]][4:(length(pathparts[[1]])-1)], collapse="/")
    newpath <- paste0(filepath, "/", path2)
    
    if(dir.exists(newpath) == F) dir.create(newpath, recursive = T)
    t <- try(downloader::download(selected_tiles$URL[j],
                                    paste(newpath, selected_tiles$name[j], sep="/"),
                                  mode="wb"), silent = T)
    
    if(class(t) == "try-error"){
      writeLines("File could not be downloaded. URLs may have expired. Getting new URLs.")
      file.urls.new <- getFileUrls(month.urls)
      selected_tiles<-screenurls(site,file.urls.new,dpID,savepath)
      writeLines("Continuing downloads.")}
    if(class(t) != "try-error"){
      messages[j] <- paste(selected_tiles$name[j], "downloaded to", newpath, sep=" ")
      j = j + 1
    }
  }
  writeLines(paste("Successfully downloaded ", length(messages), " files."))
  writeLines(paste0(messages, collapse = "\n"))
}

screenurls<-function(siteID,file.urls.current,dpID=dpID,savepath=savepath){
  ##Select plots for a given site
  site_plots<-plots %>% filter(siteID==site) %>% select(siteID,plotID,easting,northing)
  
  #Find geographic index of each plot
  site_plots<-site_plots %>% mutate(tile=paste(trunc(site_plots$easting/1000)*1000,trunc(site_plots$northing/1000)*1000,sep="_"))
  
  #Unique tiles
  tiles<-unique(site_plots$tile)
  
  selected_tiles<-list()
  for(x in 1:length(tiles)){
    selected_tiles[[x]]<-file.urls.current[stringr::str_detect(file.urls.current$name,tiles[x]),]
  }
  selected_tiles<-bind_rows(selected_tiles)
  
  if(dpID=="DP1.30010.001"){
    #look for versions, only keep the highest number, group_by site
    selected_tiles<-selected_tiles %>% mutate(version=as.numeric(stringr::str_match(selected_tiles$URL,"/V(\\w+)/")[,2])) %>% 
      group_by(plotID) %>% filter(version==max(version))
  }
  
  if(dpID=="DP1.30003.001"){
    #Only classified laz
    selected_tiles<-selected_tiles %>% filter(stringr::str_detect(name,"_classified_"))
  }
  
  #Check which tiles have already been downloaded
  filepath <- paste(savepath, "/", dpID, sep="")
  path1 <- strsplit(selected_tiles$URL[1], "\\?")[[1]][1]
  pathparts <- strsplit(path1, "\\/")
  path2 <- paste(pathparts[[1]][4:(length(pathparts[[1]])-1)], collapse="/")
  newpath <- paste0(filepath, "/", path2)
  downloaded<-list.files(newpath)
  
  #remove downloaded tiles, stop if nothing left to download
  selected_tiles<-selected_tiles[!selected_tiles$name %in% downloaded,]
  return(selected_tiles)
}

getFileUrls <- function(m.urls){
  url.messages <- character()
  file.urls <- c(NA, NA, NA,NA)
  for(i in 1:length(m.urls)) {
    tmp <- httr::GET(m.urls[i])
    tmp.files <- jsonlite::fromJSON(httr::content(tmp, as="text"),
                                    simplifyDataFrame=T, flatten=T)
    
    # check for no files
    if(length(tmp.files$data$files)==0) {
      url.messages <- c(url.messages, paste("No files found for site", tmp.files$data$siteCode,
                                            "and year", tmp.files$data$month, sep=" "))
      next
    }
    
    file.urls <- rbind(file.urls, cbind(tmp.files$data$files$name,
                                        tmp.files$data$files$url,
                                        tmp.files$data$files$size,
                                        tmp.files$data$siteCode))
    
    # get size info
    file.urls <- data.frame(file.urls, row.names=NULL)
    colnames(file.urls) <- c("name", "URL", "size","plotID")
    file.urls$URL <- as.character(file.urls$URL)
    file.urls$name <- as.character(file.urls$name)
    
    if(length(url.messages) > 0){writeLines(url.messages)}
    file.urls <- file.urls[-1, ]
    return(file.urls)
  }
}