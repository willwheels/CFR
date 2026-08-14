library(xml2)
library(purrr)

source("read and process bottom up CFR functions.R")

years <- as.character(seq(1998, 2018))

output_dir <- file.path(".", "data", "processed_xml_bottom_up")

if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
  
read_and_process_one_year <- function(year){
  
  # Start the clock!
  ptm <- proc.time()
  
  infile_path <- file.path(".", "data", "read_xml_from_web", year)
  
  files <- list.files(infile_path, full.names = TRUE)
  
  year_xml <- map_dfr(files, process_file) %>%
    mutate(CFR_year = year)
  
  outfile <- file.path(output_dir, paste0("processed_xml_", year, ".Rds"))
  
  # Stop the clock
  ptm2 <- proc.time() - ptm
  
  print(year)
  print(ptm2)
  
  save(year_xml, file = outfile)
  

  
}

#plan(multiprocess)

# Start the clock!
ptm <- proc.time()

walk(years, read_and_process_one_year)


# Stop the clock
proc.time() - ptm

## 1997 took 2.3 hours before trying parallel



