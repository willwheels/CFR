library(xml2)
library(purrr)
library(httr)
library(stringr)

## reads one volume within a given year, removes the namespace, and saves to a file

get_one_volume <- function(volume_link, read_this_year, file_path){
  
  CFR_volume <- read_xml(volume_link) %>%
    xml_ns_strip()
  
  filename <- paste0("title40_",str_extract(volume_link, "vol\\d+"),"_", read_this_year, ".xml")
  
  write_xml(CFR_volume, file = file.path(file_path, filename))
  
  Sys.sleep(10)
  
  print(paste(read_this_year, volume_link))
  
}


## reads all volumes for a given year, includes creating necessary save directories

read_one_year <- function(read_this_year){
  
  get_year_url <- paste0("http://www.govinfo.gov/bulkdata/CFR/", read_this_year, "/title-40")
  
  file_path <- file.path("data", "read_xml_from_web", read_this_year)
  
  if (!dir.exists(file_path))
    dir.create(file_path, recursive = TRUE)
  
  links <-  GET(get_year_url) %>%
    httr::content("parsed") %>%
    xml_find_all(".//link") %>%
    xml_text(.) 
  
  links <- subset(links, endsWith(links, ".xml"))
  
  Sys.sleep(10)
  
  walk(links, get_one_volume, read_this_year = read_this_year, file_path = file_path )
  
  
}



# Start the clock!
ptm <- proc.time()

years <- as.character(seq(1997, 2019))

walk(years, read_one_year)

# Stop the clock
proc.time() - ptm



## 7907 seconds on EDAP large x2
## 10734.036 seconds on 4x (with new download inc. 2019)
## 8171 6/27 but error
## 7853 on EDAP 6/28

# Error in open.connection(x, "rb") : 
#   Failed to connect to www.govinfo.gov port 80: Timed out
# In addition: Warning message:
#   In for (i in seq_len(n)) { :
#       closing unused connection 3 (http://www.govinfo.gov/bulkdata/CFR/2002/title-40/CFR-2002-title40-vol9.xml)


# Start the clock!
ptm <- proc.time()

read_one_year("2019")

# Stop the clock
proc.time() - ptm

# 451 second for 2019