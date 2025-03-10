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
      try({
      # message(i)
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

# a <- get_data()

# NTDS
# Read in the actual trials stuff
library(readxl)
xll <- list()
counter <- 0
for (i in 1:19){
  x <- read_excel('data/WHO_ICPRT_NTDs.xlsx', sheet = i)
  if(ncol(x) > 20){ counter <- counter + 1}
  x$Target_size <- as.character(x$Target_size)
  x$Inclusion_gender <- as.character(x$Inclusion_gender)
  xll[[counter]] <- x  
  message(counter)
  print(class(x))
}
xl <- bind_rows(xll)

# Keep only those from clinical trials.gov
xl <- 
  xl %>%
  filter(grepl('clinicaltrials.gov', web_address))

# library(devtools)
# https://github.com/sachsmc/rclinicaltrials
# install_github('sachsmc/rclinicaltrials')
library(rclinicaltrials)

# # Loop through each one, use trial id, get data
# # results_list <- list()
# info_list <- list()
# for (i in 1:nrow(xl)){
#   message(i)
#   try({
#     this_id <- xl$TrialID[i]
#     x <- clinicaltrials_search(query = this_id)
#     y <- clinicaltrials_download(tframe = x, count = 1, include_results = TRUE)
#     # this_result <- get_data(id = this_id)
#     # this_result <- y$study_results
#     z <- y$study_information
#     this_info <- z$study_info
#     if(!is.null(z$outcomes)){
#       if(nrow(z$outcomes) == 1){
#       outcome <- z$outcomes[1]
#       this_info <- cbind(this_info, outcome)
#       }}
#     if(!is.null(z$locations)){
#       if(nrow(z$locations) == 1){
#         location <- z$locations[1]
#         this_info <- cbind(this_info, location)
#       }
#     }
# 
#     # results_list[[i]] <- this_result
#     info_list[[i]] <- this_info
#   })
# }
# results <- bind_rows(info_list)
# write_csv(results, 'results.csv')
# 
# # results <- bind_rows(results_list)
# results <- results[!duplicated(results),]
# results <- results %>%
#   group_by(study,
#            key) %>%
#   summarise(value = first(value))
# wide <- spread(data = results, key = key, value = value)
# flags <- rep(NA, ncol(wide))
# for (j in 1:length(flags)){
#   flags[j] <-
#   length(which(is.na(wide[,j]))) > (0.5 * nrow(wide))
# }
# wide <- wide[,!flags]


# HIV
# Read in the actual trials stuff
library(readxl)
xll <- list()
counter <- 0
for (i in 1:1){
  x <- read_excel('data/HIV_ICTPR_trials.xlsx', sheet = i)
  if(ncol(x) > 20){ counter <- counter + 1}
  # x$Target_size <- as.character(x$Target_size)
  x$Inclusion_gender <- as.character(x$Inclusion_gender)
  xll[[counter]] <- x  
  message(counter)
  print(class(x))
}
xl <- bind_rows(xll)

# Keep only those from clinical trials.gov
xl <- 
  xl %>%
  filter(grepl('clinicaltrials.gov', web_address))

# library(devtools)
# https://github.com/sachsmc/rclinicaltrials
# install_github('sachsmc/rclinicaltrials')
library(rclinicaltrials)

# Loop through each one, use trial id, get data
# results_list <- list()
info_list <- list()
for (i in 1:nrow(xl)){
  message(i)
  try({
    this_id <- xl$TrialID[i]
    x <- clinicaltrials_search(query = this_id)
    y <- clinicaltrials_download(tframe = x, count = 1, include_results = TRUE)
    # this_result <- get_data(id = this_id)
    # this_result <- y$study_results
    z <- y$study_information
    this_info <- z$study_info
    if(!is.null(z$outcomes)){
      if(nrow(z$outcomes) == 1){
        outcome <- z$outcomes[1]
        this_info <- cbind(this_info, outcome)
      }}
    if(!is.null(z$locations)){
      if(nrow(z$locations) == 1){
        location <- z$locations[1]
        this_info <- cbind(this_info, location)
      }
    }
    
    # results_list[[i]] <- this_result
    info_list[[i]] <- this_info
  })
}
results <- bind_rows(info_list)
write_csv(results, 'results_hiv.csv')

hiv <- read_csv('results_hiv.csv')


######################################################
# TB
# Read in the actual trials stuff
library(readxl)
xll <- list()
counter <- 0
for (i in 2:2){
  x <- read_excel('data/WHO_ICTRP_MALARIA_TB.xlsx', sheet = i)
  if(ncol(x) > 20){ counter <- counter + 1}
  x$Target_size <- as.character(x$Target_size)
  x$Inclusion_gender <- as.character(x$Inclusion_gender)
  xll[[counter]] <- x  
  message(counter)
  print(class(x))
}
xl <- bind_rows(xll)

# Keep only those from clinical trials.gov
xl <- 
  xl %>%
  filter(grepl('clinicaltrials.gov', web_address))

# library(devtools)
# https://github.com/sachsmc/rclinicaltrials
# install_github('sachsmc/rclinicaltrials')
library(rclinicaltrials)

# Loop through each one, use trial id, get data
# results_list <- list()
info_list <- list()
for (i in 1:nrow(xl)){
  message(i)
  try({
    this_id <- xl$TrialID[i]
    x <- clinicaltrials_search(query = this_id)
    y <- clinicaltrials_download(tframe = x, count = 1, include_results = TRUE)
    # this_result <- get_data(id = this_id)
    # this_result <- y$study_results
    z <- y$study_information
    this_info <- z$study_info
    if(!is.null(z$outcomes)){
      if(nrow(z$outcomes) == 1){
        outcome <- z$outcomes[1]
        this_info <- cbind(this_info, outcome)
      }}
    if(!is.null(z$locations)){
      if(nrow(z$locations) == 1){
        location <- z$locations[1]
        this_info <- cbind(this_info, location)
      }
    }
    
    # results_list[[i]] <- this_result
    info_list[[i]] <- this_info
  })
}
results <- bind_rows(info_list)
write_csv(results, 'results_tb.csv')



#####################################################

# malaria
# Read in the actual trials stuff
library(readxl)
xll <- list()
counter <- 0
for (i in 1:1){
  x <- read_excel('data/WHO_ICTRP_MALARIA_TB.xlsx', sheet = i)
  if(ncol(x) > 20){ counter <- counter + 1}
  x$Target_size <- as.character(x$Target_size)
  x$Inclusion_gender <- as.character(x$Inclusion_gender)
  xll[[counter]] <- x  
  message(counter)
  print(class(x))
}
xl <- bind_rows(xll)

# Keep only those from clinical trials.gov
xl <- 
  xl %>%
  filter(grepl('clinicaltrials.gov', web_address))

# library(devtools)
# https://github.com/sachsmc/rclinicaltrials
# install_github('sachsmc/rclinicaltrials')
library(rclinicaltrials)

# Loop through each one, use trial id, get data
# results_list <- list()
info_list <- list()
for (i in 1:nrow(xl)){
  message(i)
  try({
    this_id <- xl$TrialID[i]
    x <- clinicaltrials_search(query = this_id)
    y <- clinicaltrials_download(tframe = x, count = 1, include_results = TRUE)
    # this_result <- get_data(id = this_id)
    # this_result <- y$study_results
    z <- y$study_information
    this_info <- z$study_info
    if(!is.null(z$outcomes)){
      if(nrow(z$outcomes) == 1){
        outcome <- z$outcomes[1]
        this_info <- cbind(this_info, outcome)
      }}
    if(!is.null(z$locations)){
      if(nrow(z$locations) == 1){
        location <- z$locations[1]
        this_info <- cbind(this_info, location)
      }
    }
    
    # results_list[[i]] <- this_result
    info_list[[i]] <- this_info
  })
}
results <- bind_rows(info_list)
write_csv(results, 'results_malaria.csv')


# How to get the phase in non-phased studies
get_phase <- function(nct_id = 'NCT02431429'){
  require(rvest)
  
  page <- html(paste0('https://clinicaltrials.gov/ct2/show/',
                      nct_id))
  
  #Scrape for phase
  text <- page %>% 
    html_nodes("div div") %>%
    html_text() 
  
  # Keep only that which mentions phase
  phase_text <- text[grepl('phase ([0-9])', tolower(text))]
  phase_text <- text[grepl('phase ', tolower(text)) &
                       grepl("[[:digit:]]", text)]
  
  # Get only the value right after phase
  phases <- list()
  for(i in 1:length(phase_text)){
    split <- unlist(strsplit(phase_text[i], ' '))
    phase_index <- which(tolower(split) == 'phase')
    phase <- split[phase_index + 1]
    phase <- substr(phase, 1, 1)
    phase <- as.numeric(phase)
    phases[[i]] <- phase
  }
  phase <- sort(unique(unlist(phases)))
  return(paste0(phase, collapse = '; '))
}

# Read and modify phases
files <- c('results_hiv.csv',
           'results_tb.csv',
           'results.csv',
           'results_malaria.csv')
for (i in 1:length(files)){
  this_file <- files[i]
  this_data <- readr::read_csv(this_file)
  this_data$phase_scrape <- NA
  for (r in 1:nrow(this_data)){
    message(this_file, ' ',
            'row ', r,
            ' of ', nrow(this_data))
    if(is.na(this_data$phase[r]) | this_data$phase[r] == 'N/A'){
      try({
        phase_attempt <- get_phase(this_data$nct_id[r])
        this_data$phase_scrape[r] <- phase_attempt
      })
    }
  }
  readr::write_csv(this_data,
                   paste0('phased_', this_file))
}
