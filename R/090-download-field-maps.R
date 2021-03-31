require(RCurl)
library(XML)
library(magrittr)

##we are going to do this a bit sloppy and set a working directory
setwd("C:/Users/allan/OneDrive/New_Graph/Current/2019-023_Bulkley_fish_passage/field_maps/moe_2018")

##increase your timeout limit to allow download of bigger files
options(timeout=180)


url = "https://hillcrestgeo.ca/outgoing/forMasse/bulkley/"

filenames <- getURL(url,verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)
filenames <- getHTMLLinks(filenames, xpQuery = "//a/@href[contains(., '.pdf')]") #https://stackoverflow.com/questions/32213591/list-files-on-http-ftp-server-in-r  

# ##or more precisely
# getHTMLLinks(
#   filenames,
#   xpQuery = "//a/@href['.pdf'=substring(., string-length(.) - 3)]"
# )

for (filename in filenames) {
  download.file(paste(url, filename, sep = ""), paste(getwd(), "/", filename,
                                                      sep = ""), mode = "wb")
}


##now get the new maps
setwd("C:/Users/allan/OneDrive/New_Graph/Current/2019-023_Bulkley_fish_passage/field_maps/assessment/planning_maps")
url = "https://www.hillcrestgeo.ca/outgoing/fishpassage/projects/bulkley/assessment/planning_maps/"

filenames <- getURL(url,verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)
filenames <- getHTMLLinks(filenames, xpQuery = "//a/@href[contains(., '.pdf')]") #https://stackoverflow.com/questions/32213591/list-files-on-http-ftp-server-in-r  

##this could prb just be an lapply but not going to look at it now. just recycle from last project and go to bed.
for (filename in filenames) {
  download.file(paste(url, filename, sep = ""), paste(getwd(), "/", filename,
                                                      sep = ""), mode = "wb")
}

##now get the new maps morixw
setwd("C:/Users/allan/OneDrive/New_Graph/Current/2019-023_Bulkley_fish_passage/field_maps/assessment/planning_maps")
url = "https://hillcrestgeo.ca/outgoing/fishpassage/projects/morice/mapping/"

filenames <- getURL(url,verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)
filenames <- getHTMLLinks(filenames, xpQuery = "//a/@href[contains(., '.pdf')]") #https://stackoverflow.com/questions/32213591/list-files-on-http-ftp-server-in-r  

##this could prb just be an lapply but not going to look at it now. just recycle from last project and go to bed.
for (filename in filenames) {
  download.file(paste(url, filename, sep = ""), paste(getwd(), "/", filename,
                                                      sep = ""), mode = "wb")
}

##archive the maps used for Bulk planning 
path = "C:/Users/allan/OneDrive/New_Graph/Current/2019-023_Bulkley_fish_passage/field_maps/2020_planning/BULK"
url = "https://www.hillcrestgeo.ca/outgoing/fishpassage/projects/bulkley/archive/2020-09-01/"

filenames <- getURL(url,verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE) %>% 
  getHTMLLinks(xpQuery = "//a/@href[contains(., '.pdf')]") #https://stackoverflow.com/questions/32213591/list-files-on-http-ftp-server-in-r  

##this could prb just be an lapply but not going to look at it now. just recycle from last project and go to bed.
for (filename in filenames) {
  download.file(paste(url, filename, sep = ""), paste(path, "/", filename,
                                                      sep = ""), mode = "wb")
}

##archive the maps used for MORR planning 
path = "C:/Users/allan/OneDrive/New_Graph/Current/2019-023_Bulkley_fish_passage/field_maps/2020_planning/MORR"
url = "https://www.hillcrestgeo.ca/outgoing/fishpassage/projects/morice/"

filenames <- getURL(url,verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE) %>% 
  getHTMLLinks(xpQuery = "//a/@href[contains(., '.pdf')]") #https://stackoverflow.com/questions/32213591/list-files-on-http-ftp-server-in-r  

##this could prb just be an lapply but not going to look at it now. just recycle from last project and go to bed.
for (filename in filenames) {
  download.file(paste(url, filename, sep = ""), paste(path, "/", filename,
                                                      sep = ""), mode = "wb")
}
