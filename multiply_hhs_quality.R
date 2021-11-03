library(kableExtra)
library(redcapAPI)
library(dplyr)
library(stringdist)
library(geosphere)

# Auxiliar functions -------------------------------------------------------------------------------

# Data timestamp
dataTimestamp = function(data_retrieval_mode, file_date = "", file_time = "") {
  if(data_retrieval_mode == "file") {
    data_timestamp = paste(file_date, file_time)
  } else if(data_retrieval_mode == "api") {
    data_timestamp = Sys.time()
  }
  
  return(data_timestamp)
}

# Read data from csv (downloaded from REDCap) or directly through the API
readData = function(data_retrieval_mode, file_prefix = "", file_content = "_DATA_", file_date = "", 
                    file_time = "", api_url = "", api_token = "", non_retrieved_records = "") {
  if(data_retrieval_mode == "file") {
    hhs_data_file =paste0(file_prefix, file_content, file_date, "_", gsub(":", "", file_time), ".csv")
    hhs_data = read.csv(hhs_data_file, stringsAsFactors = FALSE, colClasses = c("household" = "character", "child" = "character"))
  } else if(data_retrieval_mode == "api") {
    rcon = redcapConnection(api_url, api_token)
    field_names = exportFieldNames(rcon)
    fields = unique(
      field_names$original_field_name[! field_names$original_field_name %in% non_retrieved_records])
    hhs_data = exportRecords(rcon, factors = F, fields = fields)
  }
  
  return(hhs_data)
}

# Remove special characters from free text variables
removeSpecialCharacters = function(hhs_data, study_areas_ids) {
  #browser()
  special_characters = c('\n')
  replacement_character = ' '
  
  for(i in 1:length(special_characters)) {
    c = special_characters[i]
    r = replacement_character
    
    hhs_data$why_not_consent_other = gsub(c, r, hhs_data$why_not_consent_other)               # HQ10.1.1.1.1.1
    
    hhs_data$why_not_attend_epi_other = gsub(c, r, hhs_data$why_not_attend_epi_other)         # HQ17.1.1
    hhs_data$u5_card_keeper = gsub(c, r, hhs_data$u5_card_keeper)                             # HQ17.2.1.1
    hhs_data$why_not_u5_card_other = gsub(c, r, hhs_data$why_not_u5_card_other)               # HQ17.2.1.1
    hhs_data$why_not_sp_other = gsub(c, r, hhs_data$why_not_sp_other)                         # HQ19.1.1
    hhs_data$fever_trearment_other = gsub(c, r, hhs_data$fever_trearment_other)               # HQ32.1

    hhs_data$his_hh_other = gsub(c, r, hhs_data$his_hh_other)                                 # HQ34.1
    hhs_data$his_caretaker_relation_other = gsub(c, r, hhs_data$his_caretaker_relation_other) # HQ38.2.1
    hhs_data$religion_other = gsub(c, r, hhs_data$religion_other)                             # HQ42.1
    hhs_data$ethnic_group_other = gsub(c, r, hhs_data$ethnic_group_other)                     # HQ43.1
    
    hhs_data$why_warnings = gsub(c, r, hhs_data$why_warnings)                                 # warning_summary
    hhs_data$comments= gsub(c, r, hhs_data$comments)
  }
  
  return(hhs_data)
}

# Remove those records which are empty
removeEmptyRecords = function(hhs_data) {
  hhs_data = hhs_data[!is.na(hhs_data$district), ]
  
  return(hhs_data)
}

# Get REDCap project information
getProjectInfo = function(api_url, api_token) {
  #browser()
  rcon = redcapConnection(api_url, api_token)
  project_info = exportProjectInformation(rcon)
  
  return(project_info)
}

# Get the timestamp of the last collected record
lastRecordDate = function(hhs_data) {
  return(max(as.character(hhs_data$interview_date), na.rm = T))
}

# Get the number of records uploaded to REDCap
numberOfRecords = function(hhs_data) {
  return(nrow(hhs_data))
}

# Behaves as MySQL UNION statement. Appends a list just below the other.
union = function(...) {
  aux = list(...)
  dat = data.frame()
  for(i in seq(along = aux)) {
    if(length(aux[[i]]) == 0) {
      dat[i,] = rep(0, ncol(dat))
    } else {
      for(j in names(aux[[i]]))
        dat[i,j] = aux[[i]][j] 
    }
  }
  dat = rapply(dat, f = function(x) ifelse(is.na(x), 0, x), how = "replace")
  return(dat)
}

# Converts a data frame of two columns in a list in which one column is used as keys 
# and the other as values.
pivot <- function(indexes, index_column, value_column, df) {
  l = list()
  for(i in indexes) {
    if(length(df[value_column][df[index_column] == i]) == 0)
      l[i] = 0
    else
      l[i] = df[value_column][df[index_column] == i]
  }
  
  return(l)
}

# Compute number of participants who consented the interview
numberOfparticipantsWhoConsented = function(hhs_data) {
  #browser()
  consented = table(hhs_data$ended_pregnancy, hhs_data$resident_during_pregnancy)
  
  if(length(consented) > 0)
    consented = consented[, "1"]
  else
    consented = 0 
  
  return(consented)
}
