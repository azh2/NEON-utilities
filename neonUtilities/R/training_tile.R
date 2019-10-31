##############################################################################################
#' @title Download training tiles that don't overlap with tower plots for RGB, LiDAR and Hyperspec

#' @author
#' Ben Weinstein \email{ben.weinstein@weecology.org}
#'
#' @param dpID The identifier of the NEON data product to pull, in the form DPL.PRNUM.REV, e.g. DP1.10023.001
#' @param site The four-letter code of a single NEON site, e.g. 'CLBJ'.
#' @param year The four-digit year to search for data. Defaults to 2017.
#' @param check.size T or F, should the user be told the total file size before downloading? Defaults to T. When working in batch mode, or other non-interactive workflow, use check.size=F.
#' @param savepath The file path to download to. Defaults to NA, in which case the working directory is used.
#' @param n Number of training tiles to download.
#' @return A folder in the working directory, containing all files meeting query criteria.
#' @importFrom magrittr "%>%"
#'
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

##############################################################################################

training_tile <- function(site="SCBI", year="2019", check.size=TRUE, savepath=NA,n=2) {

  dpIDs = c("DP3.30010.001","DP1.30003.001","DP3.30006.001")

  files_to_download<-list()

  for(dpID in dpIDs){
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
  month.urls <- month.urls[grep(paste(site, year, sep="/"), month.urls)]

  # error message if nothing is available
  if(length(month.urls)==0) {
    return("There are no data at the selected site and year.")
  }

  # get and stash the file names, S3 URLs, file size, and download status (default = 0) in a data frame
  files_to_download[[dpID]] <- getFileUrls(month.urls)
  }

  file.urls.current<-dplyr::bind_rows(files_to_download)

  selected_tiles<-screenurls(site,file.urls.current,savepath)

  #select two tiles
  if(nrow(selected_tiles)==0){
    print(paste(site,"no tiles to download"))
    return(NULL)
  }

  filter.size <- sum(as.numeric(as.character(selected_tiles$size)), na.rm=T)
  filter.size.read <- gdata::humanReadable(filter.size, units = "auto", standard = "SI")

  downld.size <- sum(as.numeric(as.character(file.urls.current$size)), na.rm=T)
  downld.size.read <- gdata::humanReadable(downld.size, units = "auto", standard = "SI")

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
    filepath <- paste(getwd(), "/", "training_tile", sep="")
  } else {
    filepath <- paste(savepath, "/", "training_tile", sep="")
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

screenurls<-function(site,file.urls.current,savepath=savepath){

  ##Select plots for a given site
  data(package="neonUtilities","plots")
  print(head(plots))
  site_plots<-plots %>% dplyr::filter(siteID==site) %>% dplyr::select(siteID,plotID,easting,northing)

  if(nrow(site_plots) == 0){
    stop("No plots overlap for Site")
  }

  #Find geographic index of each plot
  site_plots<-site_plots %>% dplyr::mutate(tile=paste(trunc(site_plots$easting/1000)*1000,trunc(site_plots$northing/1000)*1000,sep="_"))

  #Unique tiles
  tiles<-unique(site_plots$tile)

  rejected_tiles<-list()
  for(x in 1:length(tiles)){
    rejected_tiles[[x]]<-file.urls.current[stringr::str_detect(file.urls.current$name,tiles[x]),]
  }
  rejected_tiles<-dplyr::bind_rows(rejected_tiles)

  #remove tiles from list
  file.urls.current<-file.urls.current %>% dplyr::filter(!name %in% rejected_tiles$name)

  #just download n tiles
  #select desired number of rgb tiles, get geoindex, and get corresponding laz and hyperspec
  selected_tiles<-file.urls.current %>% filter(stringr::str_detect(name,".tif")) %>% dplyr::sample_n(n)
  selected_index<-stringr::str_match(selected_tiles$name,pattern="(\\d+_\\d+)_image")[,2]
  file.urls.current %>% filter(stringr::str_detect(name,paste(selected_index, sep="", collapse = "|")))  %>% filter(stringr::str_detect(name,".laz|.h5|.tif"))
  return(file.urls.current)
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
