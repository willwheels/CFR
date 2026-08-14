library(xml2)
library(tidyverse)
library(furrr)




process_paragraph1 <- function(paragraph){
  
  dummy_df <- data.frame(chapter_title = NA_character_, subchapter_title = NA_character_, part_number = NA_character_,
                         part_title = NA_character_, subpart_title = NA_character_, section_number = NA_character_,
                         section_subject = NA_character_, paragraph_text = NA_character_, CFR_year = NA_character_,
                         stringsAsFactors = FALSE)
  
  ancestors <- xml_find_all(paragraph, ".//ancestor::*")

  for (node in ancestors) {
    
    node_name <- xml_name(node)

    if (node_name == "CHAPTER") {
      dummy_df <- dummy_df %>%
        mutate(chapter_title = xml_text(xml_find_first(node, ".//HD")))
    } else if (node_name == "SUBCHAP") {
      dummy_df <- dummy_df %>%
        mutate(subchapter_title = xml_text(xml_find_first(node, ".//HD"))) 
      
    } else if (node_name == "PART") {
      dummy_df <- dummy_df %>%
        mutate(part_number = xml_text(xml_find_first(node, ".//EAR")),
               part_title = if_else(xml_text(xml_find_first(node, ".//HD")) == "NA",
                                    xml_text(xml_find_first(node, ".//RESERVED")),
                                    xml_text(xml_find_first(node, ".//HD"))
               )
        )
    } else if (node_name == "SUBPART") {
      dummy_df <- dummy_df %>%
        mutate(subpart_title = if_else(xml_text(xml_find_first(node, ".//HD")) == "NA",
                                       xml_text(xml_find_first(node, ".//RESERVED")),
                                       xml_text(xml_find_first(node, ".//HD"))
        )
        )
    } else if (node_name == "SECTION") {
      dummy_df <- dummy_df  %>%
        mutate(section_number = xml_text(xml_find_first(node, ".//SECTNO")),
               #section_number = str_extract(section_number, "\\d+\\.\\d+"),         # this line gets rid of section symbol
               section_subject = xml_text(xml_find_first(node, ".//SUBJECT")))  
      
    } else if (node_name == "P") {
      dummy_df <- dummy_df  %>%
        mutate(paragraph_text = xml_text(node))
      
    }
  }
  
  return(dummy_df)

}



process_paragraph2 <- function(paragraph){
  
  dummy_df <- data.frame(chapter_title = NA_character_, subchapter_title = NA_character_, part_number = NA_character_,
                         part_title = NA_character_, subpart_title = NA_character_, section_number = NA_character_,
                         section_subject = NA_character_, paragraph_text = NA_character_, CFR_year = NA_character_,
                         stringsAsFactors = FALSE)
  
  ancestors <- xml_find_all(paragraph, ".//ancestor::*")
  
  
  dummy_df <- dummy_df %>%
    mutate(chapter_title = ifelse(("CHAPTER" %in% xml_name(ancestors)) == TRUE ,
                                  xml_text(xml_find_first(ancestors[xml_name(ancestors) == "CHAPTER"], ".//HD")),
                                  chapter_title),
           subchapter_title = ifelse(("SUBCHAP" %in% xml_name(ancestors)) == TRUE ,
                                     xml_text(xml_find_first(ancestors[xml_name(ancestors) == "SUBCHAP"], ".//HD")),
                                     subchapter_title),
           part_number = ifelse(("PART" %in% xml_name(ancestors)) == TRUE,
                                xml_text(xml_find_first(ancestors[xml_name(ancestors) == "PART"], ".//EAR")),
                                part_number),
           
           part_title = ifelse(("PART" %in% xml_name(ancestors)) == TRUE,
                               if_else(xml_text(xml_find_first(ancestors[xml_name(ancestors) == "PART"], ".//HD")) == "NA",
                                       xml_text(xml_find_first(ancestors[xml_name(ancestors) == "PART"], ".//RESERVED")),
                                       xml_text(xml_find_first(ancestors[xml_name(ancestors) == "PART"], ".//HD"))
                               ),
                               part_title),
           subpart_title = ifelse(("SUBPART" %in% xml_name(ancestors)) == TRUE,
                               if_else(xml_text(xml_find_first(ancestors[xml_name(ancestors) == "SUBPART"], ".//HD")) == "NA",
                                       xml_text(xml_find_first(ancestors[xml_name(ancestors) == "SUBPART"], ".//RESERVED")),
                                       xml_text(xml_find_first(ancestors[xml_name(ancestors) == "SUBPART"], ".//HD"))),
                               subpart_title),
           section_number = ifelse(("SECTION" %in% xml_name(ancestors)) == TRUE,
                                xml_text(xml_find_first(ancestors[xml_name(ancestors) == "SECTION"], ".//SECTNO")),
                                section_number),
           section_subject = ifelse(("SECTION" %in% xml_name(ancestors)) == TRUE,
                                   xml_text(xml_find_first(ancestors[xml_name(ancestors) == "SECTION"], ".//SUBJECT")),
                                   section_subject),
           paragraph_text = ifelse(("P" %in% xml_name(ancestors)) == TRUE,
                                   xml_text(ancestors[xml_name(ancestors) == "P"]),
                                   paragraph_text)
    )
  
  
  return(dummy_df)
  
}


process_file <- function(url) {
  
  xml_file <- read_xml(url) %>%
    xml_ns_strip()
  
  file_df <- xml_file %>%
    xml_find_all(., ".//P") %>%
    map_dfr(., process_paragraph2) 
}


# Start the clock!
#ptm <- proc.time()

#test1 <- map_dfr(paragraphs[1:200], process_paragraph1)

# Stop the clock
#proc.time() - ptm


# Start the clock!
#ptm <- proc.time()

#test2 <- future_map_dfr(paragraphs[1:200], process_paragraph2)

# Stop the clock
#proc.time() - ptm

 

#####

#url <- "https://www.govinfo.gov/bulkdata/CFR/2018/title-40/CFR-2018-title40-vol34.xml"


#CFR_sect <- read_xml(url) %>%
#  xml_ns_strip()


#paragraphs <- xml_find_all(CFR_sect, ".//P")

