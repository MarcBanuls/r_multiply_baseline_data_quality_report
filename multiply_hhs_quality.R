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

# Compute number of participants who consented the interview
numberOfparticipantsWhoConsented = function(hhs_data) {
  #browser()
  consented = table(hhs_data$children_2_years, hhs_data$children_no_icaria)
  
  if(length(consented) > 0)
    consented = consented[, "1"]
  else
    consented = 0 
  
  return(consented)
}

# Compute recruitment rate
recruitmentRate = function(hhs_data, sample_size) {
  consented = numberOfparticipantsWhoConsented(hhs_data)
  
  if(is.na(consented)) 
    recruitment = 0 
  else 
    recruitment = floor((consented / sample_size) * 100)
  
  return(recruitment)
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
  
  diff_child_birth_dates = NA
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
      hhs_data$children_2_years > 0 & hhs_data$children_no_icaria > 0, column])
    
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

# Tables ---------------------

trialProfileOfArea = function(hhs_data, study_area_column, lang = 'EN') {
  #browser()
  maximum_number_of_columns = 29
  font_size = 10
  column = paste0("cluster_", study_area_column)
  
  number_hh_selected_visited = table(hhs_data[column])
  if(length(number_hh_selected_visited) > 0) {
    number_hh_selected_interviewed = table(hhs_data[hhs_data$hh_acceptance == 1, column])
    
    number_children_2_years_df = setNames(
      aggregate(children_2_years ~ get(column), FUN = sum, data = hhs_data), 
      c(column, "children_2_years")
    )
    number_children_2_years_list = pivot(
      indexes = names(number_hh_selected_visited), 
      index_column = column, 
      value_column = "children_2_years", 
      df = number_children_2_years_df
    )
    
    number_eligible_children_df = setNames(
      aggregate(children_no_icaria ~ get(column), FUN = sum, data = hhs_data),
      c(column, "children_no_icaria")
    )
    number_eligible_children_list = pivot(
      indexes = names(number_hh_selected_visited),
      index_column = column,
      value_column = "children_no_icaria",
      df = number_eligible_children_df
    )
    
    children_2_years_profile = union(
      number_children_2_years_list, 
      number_eligible_children_list
    )
    
    number_children_interviewed = table(
      subset(hhs_data, children_2_years > 0 & children_no_icaria > 0)[column])
    number_children_interrupt_interview = table(
      subset(hhs_data, consent == 1 & 
               (is.na(children_2_years) | is.na(children_no_icaria)))[column])
    number_children_non_interviewed = table(subset(hhs_data, consent == 0)[column])
    
    eligible_children_selected = union(number_children_interviewed, number_children_interrupt_interview, 
                                       number_children_non_interviewed)
    if(ncol(eligible_children_selected) > 0) {
      eligible_children_selected_totals = eligible_children_selected[1,] + eligible_children_selected[2,] + eligible_children_selected[3,]
    } else {
      eligible_children_selected_totals = number_children_interviewed # empty table 
    }
    
    number_children_denied_consent = table(hhs_data[hhs_data$why_not_consent == 0, column])
    number_children_absent = table(hhs_data[hhs_data$why_not_consent == 2, column])
    number_children_unabled = table(hhs_data[hhs_data$why_not_consent == 1, column])
    number_children_other_reason = table(hhs_data[hhs_data$why_not_consent == 88, column])
    
    number_hh_empty = table(hhs_data[hhs_data$hh_available == 2, column])
    number_hh_head_not_found = table(hhs_data[hhs_data$hh_available == 0, column])
    number_hh_head_refused = table(hhs_data[hhs_data$hh_acceptance == 0, column])
    
    hh_selected_not_interviewed = union(number_hh_empty, number_hh_head_refused)
    if(ncol(hh_selected_not_interviewed) > 0)
      hh_selected_not_interviewed_totals = 
      hh_selected_not_interviewed[1,] + hh_selected_not_interviewed[2,]
    else
      hh_selected_not_interviewed_totals = number_hh_empty # empty table
    
    trial_profile = union(
      number_hh_selected_visited, 
      number_hh_selected_interviewed, 
      number_children_2_years_list, 
      children_2_years_profile[1,] - children_2_years_profile[2,], 
      number_eligible_children_list,
      eligible_children_selected_totals,
      number_children_interviewed,
      number_children_interrupt_interview,
      number_children_non_interviewed,
      number_children_denied_consent,
      number_children_absent,
      number_children_unabled,
      number_children_other_reason,
      hh_selected_not_interviewed_totals,
      number_hh_empty,
      number_hh_head_not_found,
      number_hh_head_refused
    )
    row.names(trial_profile) = c(
      language$profile.row2, 
      language$profile.row3, 
      language$profile.row4,
      paste0(language$profile.row5, footnote_marker_symbol(1, "html")),
      language$profile.row6,
      language$profile.row7,
      language$profile.row8,
      language$profile.row9,
      language$profile.row10,
      language$profile.row11,
      language$profile.row12,
      language$profile.row13,
      language$profile.row14,
      language$profile.row15,
      language$profile.row16,
      paste0(language$profile.row17, footnote_marker_symbol(2, "html")),
      language$profile.row18
    )
    colnames(trial_profile) = paste0("C", colnames(trial_profile))
    #browser()
    # Consistency checks within the trial profile
    trial_profile_checked = trial_profile
    for(i in colnames(trial_profile)) {
      # non_interviewed HH = empty + refused
      trial_profile_checked[c(14, 15, 17), i] = cell_spec(
        x        = trial_profile[c(14, 15, 17),i],
        format   ="html",
        color    = 
          ifelse(trial_profile[15, i] + trial_profile[17, i] != trial_profile[14, i], "red", ""),
        tooltip  = 
          ifelse(trial_profile[15, i] + trial_profile[17, i] != trial_profile[14, i], 
                 language$profile.check1, "")
      )
      
      # children = eligible + non_eligible
      trial_profile_checked[c(3, 4, 5), i] = cell_spec(
        x        = trial_profile[c(3, 4, 5),i],
        format   ="html",
        color    = 
          ifelse(trial_profile[4, i] + trial_profile[5, i] != trial_profile[3, i], "red", ""),
        tooltip  = 
          ifelse(trial_profile[4, i] + trial_profile[5, i] != trial_profile[3, i], 
                 language$profile.check2, "")
      )
      
      # non_interviewed children = denied + absent + unabled + other
      trial_profile_checked[c(9, 10, 11, 12, 13), i] = cell_spec(
        x        = trial_profile[c(9, 10, 11, 12, 13),i],
        format   ="html",
        color    = 
          ifelse(trial_profile[10, i] + trial_profile[11, i] + trial_profile[12, i] + 
                   trial_profile[13, i] != trial_profile[9, i], "red", ""),
        tooltip  = 
          ifelse(trial_profile[10, i] + trial_profile[11, i] + trial_profile[12, i] + 
                   trial_profile[13, i]  != trial_profile[9, i], 
                 language$profile.check3, "")
      )
      
      # children selected = interviewed + interrupted + non_interviewed
      trial_profile_checked[c(6, 7, 8, 9), i] = cell_spec(
        x        = trial_profile[c(6, 7, 8, 9),i],
        format   ="html",
        color    = 
          ifelse(trial_profile[7, i] + trial_profile[8, i] + trial_profile[9, i] 
                 != trial_profile[6, i], "red", ""),
        tooltip  = 
          ifelse(trial_profile[7, i] + trial_profile[8, i] + trial_profile[9, i] 
                 != trial_profile[6, i], 
                 language$profile.check4, "")
      )
      
      # visited HH = interviewed + non_interviewed
      trial_profile_checked[c(1, 2, 14), i] = cell_spec(
        x        = trial_profile[c(1, 2, 14),i],
        format   ="html",
        color    = 
          ifelse(trial_profile[2, i] + trial_profile[14, i] != trial_profile[1, i], "red", ""),
        tooltip  = 
          ifelse(trial_profile[2, i] + trial_profile[14, i] != trial_profile[1, i], 
                 language$profile.check5, "")
      )
    }
    #browser()
    if(ncol(trial_profile_checked) > maximum_number_of_columns) {
      number_of_columns = ncol(trial_profile_checked)
      middle = as.integer(number_of_columns / 2)
      
      print(kable(trial_profile_checked[,1:(middle + 2)], "html", escape = F) %>%
              kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                            font_size = font_size) %>%
              row_spec(0, bold = T, color = "white", background = "#494949") %>%
              row_spec(c(1, 2, 3, 14), bold = T) %>%
              add_indent(c(10, 11, 12, 13))
      )
      print(kable(trial_profile_checked[,(middle + 3):number_of_columns], "html", escape = F) %>%
              kable_styling(bootstrap_options = c("striped", "hover", "responsive"), font_size = 
                              font_size) %>%
              row_spec(0, bold = T, color = "white", background = "#494949") %>%
              row_spec(c(1, 2, 3, 14), bold = T) %>%
              add_indent(c(10, 11, 12, 13)) %>%
              footnote(
                general_title = language$profile.notes.title,
                general = language$profile.notes.desc, 
                symbol = c(
                  language$profile.note1, 
                  language$profile.note2
                )
              )
      )
    } else {
      print(kable(trial_profile_checked, "html", escape = F) %>%
              kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                            font_size = font_size) %>%
              row_spec(0, bold = T, color = "white", background = "#494949") %>%
              row_spec(c(1, 2, 3, 14), bold = T) %>%
              add_indent(c(10, 11, 12, 13)) %>%
              footnote(
                general_title = language$profile.notes.title,
                general = language$profile.notes.desc, 
                symbol = c(
                  language$profile.note1, 
                  language$profile.note2
                )
              )
      )
    }
  } else {
    print(language$progress.no.data)
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
              "hh_initials", "consent", "interviewer_id", "interview_date")
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
duplicatedHouseholds = function(hhs_data, study_area_column, study_area_label) {
  #browser()
  column = paste0("cluster_", study_area_column)
  
  duplicated_records = hhs_data[duplicated(hhs_data[2:ncol(hhs_data)]) | 
                                  duplicated(hhs_data[2:ncol(hhs_data)], fromLast = T), ]
  
  key_columns = c(column, "household")
  id_columns = hhs_data[key_columns]
  duplicated_hh = hhs_data[duplicated(id_columns) | duplicated(id_columns, fromLast = T), ]
  rerecorded_hh = duplicated_hh[!(duplicated_hh$record_id %in% duplicated_records$record_id), ]
  
  # Check if there is reused household IDs which are also duplicates
  rerecorded_and_duplicated = intersect(rerecorded_hh[key_columns], 
                                        duplicated_records[key_columns])
  if(nrow(rerecorded_and_duplicated) > 0) {
    for(i in 1:nrow(rerecorded_and_duplicated)) {
      if(!is.na(rerecorded_and_duplicated[i, column]))
        rerecorded_hh = rbind(rerecorded_hh, duplicated_records[
          duplicated_records[column] == 
            rerecorded_and_duplicated[i, column] &
            duplicated_records$household == rerecorded_and_duplicated$household[i], ][1,])
    }
  }
  
  # Remove duplicates in which the cluster column is NA
  duplicated_hh = duplicated_hh[which(!is.na(duplicated_hh[column])), ]
  rerecorded_hh = rerecorded_hh[which(!is.na(rerecorded_hh[column])), ]
  
  if(nrow(rerecorded_hh) == 0)
    return(NULL)
  
  rerecorded_hh$cluster[!is.na(rerecorded_hh[column])] = 
    rerecorded_hh[!is.na(rerecorded_hh[column]), column]
  
  columns = c("record_id", "district", "cluster", "household", "latitude", "longitude", 
              "hh_initials", "hh_sex", "hh_available", "consent", "child_birth", 
              "interviewer_id", "interview_date", "interviewer_id_rdt", "interview_date_rdt")
  rerecorded_hh_summary = rerecorded_hh[
    order(rerecorded_hh$district, rerecorded_hh$cluster, rerecorded_hh$household, 
          rerecorded_hh$interview_date), columns]
  
  # Remove deleted records
  rerecorded_hh_summary = rerecorded_hh_summary[!is.na(rerecorded_hh_summary$district), ]
  
  #browser()
  # Disambiguate records
  if(nrow(rerecorded_hh_summary) > 0) {
    rerecorded_hh_summary$duplicated = NA
    current_district = rerecorded_hh_summary$district[1]
    current_cluster = rerecorded_hh_summary$cluster[1]
    current_household = rerecorded_hh_summary$household[1]
    records_in_conflict = c(1)
    for(i in 2:nrow(rerecorded_hh_summary)) {
      if(rerecorded_hh_summary$district[i] == current_district & 
         rerecorded_hh_summary$cluster[i] == current_cluster &
         ((is.na(rerecorded_hh_summary$household[i]) & is.na(current_household)) | 
          (!is.na(rerecorded_hh_summary$household[i]) & 
           rerecorded_hh_summary$household[i] == current_household))) {
        records_in_conflict = c(records_in_conflict, i)
      } else {
        for(j in 1:(length(records_in_conflict) - 1)) {
          for(k in (j+1):length(records_in_conflict)) {
            #browser()
            result = sameInterview(rerecorded_hh_summary[records_in_conflict[j], ], 
                                   rerecorded_hh_summary[records_in_conflict[k], ])
            
            if(is.na(rerecorded_hh_summary$duplicated[records_in_conflict[j]]) | 
               rerecorded_hh_summary$duplicated[records_in_conflict[j]] != T)
              rerecorded_hh_summary$duplicated[records_in_conflict[j]] = result
            if(is.na(rerecorded_hh_summary$duplicated[records_in_conflict[k]]) | 
               rerecorded_hh_summary$duplicated[records_in_conflict[k]] != T)
              rerecorded_hh_summary$duplicated[records_in_conflict[k]] = result
          }
        }
        
        current_district = rerecorded_hh_summary$district[i]
        current_cluster = rerecorded_hh_summary$cluster[i]
        current_household = rerecorded_hh_summary$household[i]
        records_in_conflict = c(i)
      }
    }
    
    #browser()
    for(j in 1:(length(records_in_conflict) - 1)) {
      for(k in (j+1):length(records_in_conflict)) {
        #browser()
        result = sameInterview(rerecorded_hh_summary[records_in_conflict[j], ], 
                               rerecorded_hh_summary[records_in_conflict[k], ])
        
        if(is.na(rerecorded_hh_summary$duplicated[records_in_conflict[j]]) | 
           rerecorded_hh_summary$duplicated[records_in_conflict[j]] != T)
          rerecorded_hh_summary$duplicated[records_in_conflict[j]] = result
        if(is.na(rerecorded_hh_summary$duplicated[records_in_conflict[k]]) | 
           rerecorded_hh_summary$duplicated[records_in_conflict[k]] != T)
          rerecorded_hh_summary$duplicated[records_in_conflict[k]] = result
      }
    }
  }
  
  rerecorded_hh_summary$consent[is.na(rerecorded_hh_summary$consent)] = language$not.asked
  rerecorded_hh_summary$consent[rerecorded_hh_summary$consent == 0]   = language$no
  rerecorded_hh_summary$consent[rerecorded_hh_summary$consent == 1]   = language$yes
  
  rerecorded_hh_summary$district = study_area_label
  
  rerecorded_hh_summary$hh_sex[rerecorded_hh_summary$hh_sex == 0] = language$rerecorded.sex1
  rerecorded_hh_summary$hh_sex[rerecorded_hh_summary$hh_sex == 1] = language$rerecorded.sex2
  
  rerecorded_hh_summary$hh_available[rerecorded_hh_summary$hh_available == 0] = language$no
  rerecorded_hh_summary$hh_available[rerecorded_hh_summary$hh_available == 1] = language$yes
  rerecorded_hh_summary$hh_available[rerecorded_hh_summary$hh_available == 2] = language$rerecorded.empty
  
  rerecorded_hh_summary$duplicated[rerecorded_hh_summary$duplicated == F] = language$rerecorded.dup1
  rerecorded_hh_summary$duplicated[rerecorded_hh_summary$duplicated == T] = language$rerecorded.dup2
  
  return(rerecorded_hh_summary)
}

#Print Duplicated Households
printDuplicatedHouseholds = function(hhs_data, study_area_column, study_area_label) {
  #browser()
  rerecorded_hh_summary = duplicatedHouseholds(hhs_data, study_area_column, study_area_label)
  
  if(!is.null(rerecorded_hh_summary)) {
    if(nrow(rerecorded_hh_summary) > 0) {
      colnames(rerecorded_hh_summary) = c(language$rerecorded.dups.h1, language$rerecorded.dups.h2,
                                          language$rerecorded.dups.h3, language$rerecorded.dups.h4,
                                          language$rerecorded.dups.h5, language$rerecorded.dups.h6,
                                          language$rerecorded.dups.h7, language$rerecorded.dups.h8,
                                          language$rerecorded.dups.h9, language$rerecorded.dups.h10,
                                          language$rerecorded.dups.h11, language$rerecorded.dups.h12,
                                          language$rerecorded.dups.h13, language$rerecorded.dups.h14,
                                          language$rerecorded.dups.h15)
      
      kable(rerecorded_hh_summary, "html", row.names = F, escape = F) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                      font_size = 12) %>%
        row_spec(0, bold = T, color = "white", background = "#494949") %>%
        scroll_box(height = "250px")
    } else {
      print(language$dups.no.hh)
    }
  } else {
    print(language$dups.no.hh)
  }
}

#Duplicates Summary
duplicatesSummary = function(hhs_data, study_area_column) {
  #browser()
  font_size = 10
  maximum_number_of_columns = 29
  column = paste0("cluster_", study_area_column)
  
  non_interviewed_visits_number_area = 
    table(hhs_data[is.na(hhs_data$consent) | hhs_data$consent != 1, column])
  interviewed_number_area = 
    table(hhs_data[hhs_data$consent == 1, column])
  
  if(length(non_interviewed_visits_number_area) > 0 | length(interviewed_number_area) > 0) {
    duplicated_records = hhs_data[duplicated(hhs_data[2:ncol(hhs_data)]), ]
    
    non_interviewed_duplicated_records_area = 
      table(duplicated_records[is.na(duplicated_records$consent) | duplicated_records$consent != 1, 
                               column])
    interviewed_duplicated_records_area = 
      table(duplicated_records[duplicated_records$consent == 1, column])
    
    id_columns = hhs_data[c(column, "household")]
    duplicated_hh = hhs_data[duplicated(id_columns) | duplicated(id_columns, fromLast = T), ]
    duplicated_records_from_last = hhs_data[duplicated(hhs_data[2:ncol(hhs_data)]) | 
                                              duplicated(hhs_data[2:ncol(hhs_data)], fromLast = T), ]
    rerecorded_hh = duplicated_hh[!(duplicated_hh$record_id %in% 
                                      duplicated_records_from_last$record_id), ]
    
    rerecorded_hh_area = table(rerecorded_hh[column])
    
    rerecorded_hh_interviewed = rerecorded_hh[rerecorded_hh$consent == 1, ]
    rerecorded_hh_interviewed_area = table(rerecorded_hh_interviewed[column])
    
    non_interviewed = union(non_interviewed_visits_number_area, 
                            non_interviewed_duplicated_records_area)
    if(ncol(non_interviewed) > 0)
      non_interviewed_totals = non_interviewed[1,] - non_interviewed[2,]
    else
      non_interviewed_totals = non_interviewed_visits_number_area # empty table
    
    interviewed = union(interviewed_number_area, interviewed_duplicated_records_area)
    if(ncol(interviewed) > 0)
      interviewed_totals = interviewed[1,] - interviewed[2,]
    else
      interviewed_totals = interviewed_number_area # empty table
    #browser()
    duplicates_summary = union(
      non_interviewed_visits_number_area,
      interviewed_number_area,
      non_interviewed_duplicated_records_area,
      interviewed_duplicated_records_area,
      non_interviewed_totals,
      interviewed_totals,
      rerecorded_hh_area,
      rerecorded_hh_interviewed_area
    )
    row.names(duplicates_summary) = c(
      language$dups.tab.row2,
      language$dups.tab.row3,
      language$dups.tab.row4,
      language$dups.tab.row5,
      language$dups.tab.row6,
      language$dups.tab.row7,
      language$dups.tab.row8,
      language$dups.tab.row9
    )
    colnames(duplicates_summary) = paste0("C", colnames(duplicates_summary))
    
    duplicates_summary_reduced = duplicates_summary[, duplicates_summary[3,] != 0 | 
                                                      duplicates_summary[4,] != 0 | 
                                                      duplicates_summary[7,] != 0, drop = F]
    
    if(ncol(duplicates_summary_reduced) > 0) {
      if(ncol(duplicates_summary_reduced) > maximum_number_of_columns) {
        number_of_columns = ncol(duplicates_summary_reduced)
        middle = as.integer(number_of_columns / 2)
        
        print(kable(duplicates_summary_reduced[,1:(middle + 2)], "html", escape = F) %>%
                kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                              font_size = font_size) %>%
                row_spec(0, bold = T, color = "white", background = "#494949") %>%
                row_spec(c(2, 6), bold = T)
        )
        print(kable(duplicates_summary_reduced[,(middle + 3):number_of_columns], "html", escape = F) %>%
                kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                              font_size = font_size) %>%
                row_spec(0, bold = T, color = "white", background = "#494949") %>%
                row_spec(c(2, 6), bold = T)
        )
      } else {
        print(kable(duplicates_summary_reduced, "html", escape = F) %>%
                kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                              font_size = font_size) %>%
                row_spec(0, bold = T, color = "white", background = "#494949") %>%
                row_spec(c(2, 6), bold = T)
        )
      }
    } else {
      print(language$dups.no.dups) 
    }
  } else {
    print(language$dups.no.data)
  }
}









