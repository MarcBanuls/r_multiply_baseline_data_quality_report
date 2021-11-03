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

    if("his_hh_other" %in% colnames(hhs_data)) 
      hhs_data$his_hh_other = gsub(c, r, hhs_data$his_hh_other)                               # HQ34.1
    if("his_caretaker_relation_other" %in% colnames(hhs_data)) 
      hhs_data$his_caretaker_relation_other = gsub(c, r, hhs_data$his_caretaker_relation_other)# HQ38.2.1
    if("religion_other" %in% colnames(hhs_data)) 
      hhs_data$religion_other = gsub(c, r, hhs_data$religion_other)                           # HQ42.1
    if("ethnic_group_other" %in% colnames(hhs_data)) 
      hhs_data$ethnic_group_other = gsub(c, r, hhs_data$ethnic_group_other)                   # HQ43.1

    
    hhs_data$why_warnings = gsub(c, r, hhs_data$why_warnings)                                 # warning_summary
    hhs_data$comments= gsub(c, r, hhs_data$comments)                                          # Comments
    
    hhs_data$malaria_treatment_name_1 = gsub(c, r, hhs_data$malaria_treatment_name_1)
    hhs_data$malaria_treatment_indication_1 = gsub(c, r, hhs_data$malaria_treatment_indication_1)
    hhs_data$malaria_treatment_dose_1 = gsub(c, r, hhs_data$malaria_treatment_dose_1)
    hhs_data$malaria_treatment_unit_1 = gsub(c, r, hhs_data$malaria_treatment_unit_1)
    hhs_data$malaria_treatment_freq_1 = gsub(c, r, hhs_data$malaria_treatment_freq_1)
    hhs_data$malaria_treatment_route_1 = gsub(c, r, hhs_data$malaria_treatment_route_1)
    
    hhs_data$malaria_treatment_name_2 = gsub(c, r, hhs_data$malaria_treatment_name_2)
    hhs_data$malaria_treatment_indication_2 = gsub(c, r, hhs_data$malaria_treatment_indication_2)
    hhs_data$malaria_treatment_dose_2 = gsub(c, r, hhs_data$malaria_treatment_dose_2)
    hhs_data$malaria_treatment_unit_2 = gsub(c, r, hhs_data$malaria_treatment_unit_2)
    hhs_data$malaria_treatment_freq_2 = gsub(c, r, hhs_data$malaria_treatment_freq_2)
    hhs_data$malaria_treatment_route_2 = gsub(c, r, hhs_data$malaria_treatment_route_2)
    
    hhs_data$malaria_treatment_name_3 = gsub(c, r, hhs_data$malaria_treatment_name_3)
    hhs_data$malaria_treatment_indication_3 = gsub(c, r, hhs_data$malaria_treatment_indication_3)
    hhs_data$malaria_treatment_dose_3 = gsub(c, r, hhs_data$malaria_treatment_dose_3)
    hhs_data$malaria_treatment_unit_3 = gsub(c, r, hhs_data$malaria_treatment_unit_3)
    hhs_data$malaria_treatment_freq_3 = gsub(c, r, hhs_data$malaria_treatment_freq_3)
    hhs_data$malaria_treatment_route_3 = gsub(c, r, hhs_data$malaria_treatment_route_3)
    
    hhs_data$why_warnings_rdt = gsub(c, r, hhs_data$why_warnings_rdt) 
    
    
    
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
#-----
#skip indicators to focus in duplicates

# Compute string distance but token by token rather than the whole string
stringdistByToken =  function(a, b, method) {
  #browser()
  split = " "
  a_tokens = strsplit(as.character(a), split = split)[[1]]
  b_tokens = strsplit(as.character(b), split = split)[[1]]
  
  small_string = a_tokens
  big_string = b_tokens
  if(length(b_tokens) < length(a_tokens)) {
    small_string = b_tokens
    big_string = a_tokens
  }
  
  
  str_dist = 0
  if(length(small_string) > 0) {
    for(i in 1:length(small_string)) {
      min_dist = Inf
      
      for(j in 1:length(big_string)) {
        token_dist = stringdist(small_string[i], big_string[j], method = method)
        if(token_dist < min_dist)
          min_dist = token_dist
      }
      
      str_dist = str_dist + min_dist
    }
  }
  
  # Penalty
  words_diff = abs(length(small_string) - length(big_string))
  order_diff = 0
  if(length(small_string) > 0)
    for(i in 1:length(small_string))
      if(small_string[i] != big_string[i])
        order_diff = order_diff + 1
  
  dist_by_token = str_dist + words_diff + order_diff
  dist = stringdist(a, b, method = method)
  return(min(dist, dist_by_token))
}


# Criteria for deciding when two records are the same interview or a different one:
#
# IF consent_r1 != consent_r2 THEN DIFFERENT_INTERVIEWS
# IF haversine_distance(gps_r1, gps_r2) > P1 THEN DIFFERENT_INTERVIEWS
# ELSE IF days_between(child_birth_r1, child_birth_r2) > P2 THEN DIFFERENT_INTERVIEWS
# ELSE IF levenshtein_distance(hh_initials_r1, hh_initials_r2) > P3 THEN DIFFERENT_INTERVIEWS
# ELSE SAME_INTERVIEW
#
#


sameInterview = function(record1, record2) {
  p1 = 25 # meters
  p2 = 15 # days
  p3 = 3  # levenshtein distance
  #browser()
  consent1 = record1$consent
  if(is.na(consent1))
    consent1 = language$not.asked
  consent2 = record2$consent
  if(is.na(consent2))
    consent2 = language$not.asked
  gps_distance = NA
  # Compute GPS Haversine distance
  if(!is.na(record1$longitude) & !is.na(record1$latitude) & 
     !is.na(record2$longitude) & !is.na(record2$latitude))
    gps_distance = distm(c(record1$longitude, record1$latitude), 
                         c(record2$longitude, record2$latitude), fun = distHaversine)
  
  diff_birth = NA
  # Compute difference between child births
  if(!is.na(record1$child_birth) & !is.na(record2$child_birth)) {
    if(consent1 == 1 & consent2 == 1)
      diff_child_birth_dates = abs(difftime(record1$child_birth, 
                                                   record2$child_birth, units = c("days")))
  } else if(((!is.na(record1$child_birth) & consent1 == 1 ) & 
             is.na(record2$child_birth)) | 
            (is.na(record1$child_birth) & 
             (!is.na(record2$child_birth) & consent2 == 1 ))) {
    diff_child_birth_dates = Inf
  }
  
  initials_distance = NA
  # Compute string Levenshtein distance between household head initials
  if(!is.na(record1$hh_initials) & !is.na(record2$hh_initials))
    initials_distance = stringdistByToken(record1$hh_initials, record2$hh_initials, method = "lv")
  else if((!is.na(record1$hh_initials) & is.na(record2$hh_initials)) | 
          (is.na(record1$hh_initials) & !is.na(record2$hh_initials)))
    initials_distance = Inf
  
  # Criteria 1: CONSENT
  if(consent1 != consent2)
    return(F)
  # Criteria 2: HOUSEHOLD HEAD AVAILABILITY
  # if(record1$hh_available != record2$hh_available)
  #  return(F)
  # Criteria 3: GPS COORDINATES
  else if(!is.na(gps_distance) & gps_distance> p1)
    return(F)
  # Criteria 4: CHILD BIRTH DATE  
  else if(!is.na(diff_child_birth_dates) & diff_child_birth_dates > p2)
    return(F)
  # Criteria 5: HOUSEHOLD HEAD INITIALS  
  else if(!is.na(initials_distance) & initials_distance > p3)
    return(F)
  
  return(T)
}

# Plots --------------------------------------------------------------------------------------------
# Color palette
color_palette = c("gray8", "gray35", "gray90")

#Visited Households per Area not used since Midline of TIPTOP, so not implemented here at the moment

# Visited Households per Cluster in a concrete Area
progressOfArea = function(hhs_data, study_area_column, study_area_label, interval, required_visits_mean, lang = 'EN') {
  #browser()
  
  
  
  column = paste0("cluster_", study_area_column)
  
  visits_number = table(hhs_data[column])
  if(length(visits_number) > 0) {
    max_y_axis = max(visits_number) + interval
    consented_number = table(hhs_data[
      hhs_data$children_2_years == 1 & hhs_data$children_no_icaria == 1, column])
    
    dat = union(visits_number, consented_number)
    par(cex.lab = 1.5, cex.main = 2, cex.axis = 1.05, mar = c(8, 8, 4, 0))
    visits_progress = barplot(
      height = matrix(c(dat[2,], dat[1,] - dat[2,]), nrow = 2, byrow = T),
      main   = sprintf(lang$progress.plot.title, study_area_label),
      xlab   = sprintf(lang$progress.plot.x, study_area_label),
      ylab   = sprintf(lang$progress.plot.y),
      ylim   = c(0, max_y_axis),
      axes   = F,
      col = color_palette[2:3],
      mgp = c(4, 1, 0)
    )
    axis(1, visits_progress, paste0("C", rownames(visits_number)), las = 2)
    axis(2, seq(0, max_y_axis, interval))
    abline(h = required_visits_mean, lwd = 1, lty = 2, col = "red")
    legend("topright", legend = c(lang$progress.plot.s1, lang$progress.plot.s2), fill = color_palette[2:3], cex = 1.5)
    text(x = visits_progress, y = dat[2,], labels = dat[2,], pos = 3, col = color_palette[1])
  } else {
    print(lang$progress.no.data) 
  }
}

# TRIAL PROFILE DATA in progress, focus on duplicates

# SP indicators to be done

# ANC indicators to be done


# Duplicated Records
duplicatedRecords = function(hhs_data, study_area_column, study_area_label) {
  #id_columns = hhs_data[c("cluster_kenge", "cluster_bulungu", "household")]
  column = paste0("cluster_", study_area_column)
  
  x = duplicated(hhs_data[2:ncol(hhs_data)])
  y = duplicated(hhs_data[2:ncol(hhs_data)], fromLast = T)
  duplicated_records = hhs_data[x | y, ]
  
  duplicated_records$cluster = duplicated_records[, column]
  
  columns = c("record_id", "district", "cluster", "household", "latitude", "longitude", 
              "hh_initials", "consent", "interviewer_id", "interview_date","interviewer_id_rdt", "interviewer_date_rdt")
  duplicated_records_summary = duplicated_records[
    order(duplicated_records$district, duplicated_records$cluster, duplicated_records$household), 
    columns]
  
  if(nrow(duplicated_records_summary) > 0) {
    duplicated_records_summary$consent[is.na(duplicated_records_summary$consent)] = language$not.asked
    duplicated_records_summary$consent[duplicated_records_summary$consent == 0]   = language$no
    duplicated_records_summary$consent[duplicated_records_summary$consent == 1]   = language$yes
    
    duplicated_records_summary$district = study_area_label
  }
  
  return(duplicated_records_summary)
}

# Print duplicated records on a table
printDuplicatedRecords = function(hhs_data, study_area_column, study_area_label) {
  #browser()
  duplicated_records_summary = duplicatedRecords(hhs_data, study_area_column, study_area_label)
  
  if(nrow(duplicated_records_summary) > 0) {
    colnames(duplicated_records_summary) = c(
      language$dups.tab.header1, language$dups.tab.header2, language$dups.tab.header3, 
      language$dups.tab.header4, language$dups.tab.header5, language$dups.tab.header6,
      language$dups.tab.header7, language$dups.tab.header8, language$dups.tab.header9,
      language$dups.tab.header10
    )
    
    kable(duplicated_records_summary, "html", row.names = F, escape = F) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                    font_size = 12) %>%
      row_spec(0, bold = T, color = "white", background = "#494949") %>%
      scroll_box(height = "250px")
  } else {
    print(language$dups.no.records)
  }
}

# Duplicated Households













