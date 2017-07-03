library(XML)
library(RCurl)
library(xml2)
library(tidyverse)
# Define function for getting data from clinicaltrials.gov
get_data <- function(id = "NCT02979002"){
  url <- paste0("https://clinicaltrials.gov/show/",
                id,
                "?displayxml=true")
  xData <- getURL(url)
  doc <- xmlParse(xData)
  # x <- xmlToDataFrame(xData)
  xdata <- xData
  
  # get_completion_date <- function(xdata){
  #   dum <- xmlParse(xdata)
  #   xDf <- xmlToDataFrame(nodes = getNodeSet(dum, "//*/completion_date"), stringsAsFactors = FALSE)
  #   xDf$text
  # }
  # 
  # get_lead_sponsor <- function(xdata){
  #   dum <- xmlParse(xdata)
  #   xDf <- xmlToDataFrame(nodes = getNodeSet(dum, "//*/sponsors"), stringsAsFactors = FALSE)
  #   xDf$lead_sponsor
  # }
  
  get_stuff <- function(xdata){
    # dum <- xmlParse(xdata)
    x <- read_xml(x = xdata)
    xc <- xml_children(x)
    n_elements <- length(xc)
    out_list <- list()
    for (i in 1:n_elements){
      message(i)
      this_element <- xc[i]
      this_name <- xml_name(this_element)
      this_content <- xml_contents(this_element)
      col_names <- paste0(this_name,
                          '_',
                          xml_name(this_content))
      values <- as.character(xml_contents(this_content))
      if(length(values) == 0){
        values <- rep(NA, length(col_names))
      }
      if(length(values) > length(col_names)){
        col_names <- paste0(rep(col_names, each = length(values) / length(col_names)))
        col_names[duplicated(col_names)] <- paste0(col_names[duplicated(col_names)],
                                                   '_b')
      }
      try({
        out <- data_frame(key = col_names,
                          value = values)
        out_list[[i]] <- out
      })
      
    }
    out_df <- bind_rows(out_list)
  }
  out <- get_stuff(xdata = xdata)
  out <- out %>% filter(!is.na(value))
  out$study <- out$value[out$key == 'required_header_url']
  return(out)
  #   xml_name(xc[3])
  #   xml_contents(xc[3])
  #   xml_attrs(xc[3])
  #   xl <- as_list(x)
  #   xdf <- unlist(xl)
  #   xdf <- as.data.frame(xdf)
  # 
  # 
  #   xml_text(x)
  #   x <- xmlToDataFrame(doc = dum)
  #   xDf <- xmlToDataFrame(nodes = getNodeSet(dum, paste0("//*/",
  #                                                        text)), 
  #                         stringsAsFactors = FALSE)
  #   xDf[1,1]
  # }
  # 
  # texts <- c('status',
  #            'completion_date_type',
  #            'completion_date',
  #            'sponsors',
  #            'lead_sponsors',
  #            'agency',
  #            'agency_class',
  #            'collaborator',
  #            'agency',
  #            '')
}

a <- get_data()
