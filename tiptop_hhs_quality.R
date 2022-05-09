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
    hhs_data = read.csv(hhs_data_file, stringsAsFactors = FALSE, colClasses = c("household" = "character", "woman" = "character"))
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
    other_facility = paste0("other_facility_", study_areas_ids)
    
    hhs_data$other_why_not_consent = gsub(c, r, hhs_data$other_why_not_consent)       # HQ11.1.1
    hhs_data$hh_other_main_occupation = gsub(c, r, hhs_data$hh_other_main_occupation) # HQ13.1
    hhs_data$household_other_roof = gsub(c, r, hhs_data$household_other_roof)         # HQ15.1
    hhs_data$household_other_walls = gsub(c, r, hhs_data$household_other_walls)       # HQ16.1
    hhs_data$household_other_water = gsub(c, r, hhs_data$household_other_water)       # HQ17.1
    hhs_data$household_other_toilet = gsub(c, r, hhs_data$household_other_toilet)     # HQ18.1
    
    hhs_data$other_why_not_attend_anc = gsub(c, r, hhs_data$other_why_not_attend_anc) # WQ6.1.1
    hhs_data$anc_card_keeper = gsub(c, r, hhs_data$anc_card_keeper)                   # WQ6.2.1.1a
    hhs_data$other_why_not_anc_card = gsub(c, r, hhs_data$other_why_not_anc_card)     # WQ6.2.1.1b
    hhs_data$other_why_not_sp = gsub(c, r, hhs_data$other_why_not_sp)                 # WQ9.1.1
    hhs_data$other_sp_source = gsub(c, r, hhs_data$other_sp_source)                   # WQ9.1.5.1
    #hhs_data$other_antimalarial = gsub(c, r, hhs_data$other_antimalarial)            # Removed in Midline
    #hhs_data$other_non_antimalarial = gsub(c, r, hhs_data$other_non_antimalarial)    # Removed in Midline
    hhs_data$other_birth_place = gsub(c, r, hhs_data$other_birth_place)               # WQ15.4.1
    
    hhs_data$other_main_occupation = gsub(c, r, hhs_data$other_main_occupation)       # WQ19.2.1
    hhs_data[other_facility[1]] = gsub(c, r, hhs_data[,other_facility[1]])            # WQ22.1a
    hhs_data[other_facility[2]] = gsub(c, r, hhs_data[,other_facility[2]])            # WQ22.1b
    hhs_data[other_facility[3]] = gsub(c, r, hhs_data[,other_facility[3]])            # WQ22.1c
    if("other_facility" %in% colnames(hhs_data))
      hhs_data$other_facility = gsub(c, r, hhs_data$other_facility)                   # WQ22.1d
    hhs_data$other_facility_transport = gsub(c, r, hhs_data$other_facility_transport) # WQ23.1
    if("other_religion" %in% colnames(hhs_data))
      hhs_data$other_religion = gsub(c, r, hhs_data$other_religion)                   # WQ25.1
    if("other_ethnic_group" %in% colnames(hhs_data))
      hhs_data$other_ethnic_group = gsub(c, r, hhs_data$other_ethnic_group)           # WQ26.1
    hhs_data$partner_other_occupation = gsub(c, r, hhs_data$partner_other_occupation) # WQ29.1
    
    hhs_data$other_malaria_knowledge_causes = 
      gsub(c, r, hhs_data$other_malaria_knowledge_causes)                             # WQ31.1
    hhs_data$other_malaria_knowledge_consequences = 
      gsub(c, r, hhs_data$other_malaria_knowledge_consequences)                       # WQ32.1
    
    hhs_data$why_warnings = gsub(c, r, hhs_data$why_warnings)
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
    consented = consented[1]
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

# Compute c-IPTp knowledge rate
cIPTpKnowledgeRate = function(hhs_data) {
  #browser()
  consented = numberOfparticipantsWhoConsented(hhs_data)
  
  know_ciptp = table(hhs_data$know_about_ciptp)
  
  if(is.na(know_ciptp[2])) 
    ciptp_knowledge = 0 
  else 
    ciptp_knowledge = floor(know_ciptp[2] / consented * 100)

  return(ciptp_knowledge)
}

# Compute c-IPTp administration: Women who took SP at community level
cIPTpAdministrationRate = function(hhs_data) {
  #browser()
  consented = numberOfparticipantsWhoConsented(hhs_data)
  
  administration_ciptp = table(hhs_data$sp_community)
      
  if(is.na(administration_ciptp[2])) 
    ciptp_administration = 0 
  else 
    ciptp_administration = floor(administration_ciptp[2] / consented[1] * 100)
  
  return(ciptp_administration)
}

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
# OFF - IF hh_available_r1 != hh_available_r2 THEN DIFFERENT_INTERVIEWS
# IF haversine_distance(gps_r1, gps_r2) > P1 THEN DIFFERENT_INTERVIEWS
# ELSE IF days_between(end_last_pregnancy_r1, end_last_pregnancy_r2) > P2 THEN DIFFERENT_INTERVIEWS
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
    consent1 = "Not asked"
  consent2 = record2$consent
  if(is.na(consent2))
    consent2 = "Not asked"
  gps_distance = NA
  # Compute GPS Haversine distance
  if(!is.na(record1$longitude) & !is.na(record1$latitude) & 
     !is.na(record2$longitude) & !is.na(record2$latitude))
    gps_distance = distm(c(record1$longitude, record1$latitude), 
                         c(record2$longitude, record2$latitude), fun = distHaversine)
  
  diff_end_last_pregnancy_dates = NA
  # Compute difference between end of last pregnancy dates
  if(!is.na(record1$end_last_pregnancy) & !is.na(record2$end_last_pregnancy)) {
    if(consent1 == 1 & consent2 == 1)
      diff_end_last_pregnancy_dates = abs(difftime(record1$end_last_pregnancy, 
                                                   record2$end_last_pregnancy, units = c("days")))
  } else if(((!is.na(record1$end_last_pregnancy) & consent1 == 1 ) & 
             is.na(record2$end_last_pregnancy)) | 
            (is.na(record1$end_last_pregnancy) & 
             (!is.na(record2$end_last_pregnancy) & consent2 == 1 ))) {
    diff_end_last_pregnancy_dates = Inf
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
  # Criteria 4: END LAST PREGNANCY DATE  
  else if(!is.na(diff_end_last_pregnancy_dates) & diff_end_last_pregnancy_dates > p2)
      return(F)
  # Criteria 5: HOUSEHOLD HEAD INITIALS  
  else if(!is.na(initials_distance) & initials_distance > p3)
      return(F)

  return(T)
}

# Plots --------------------------------------------------------------------------------------------
# Color palette
color_palette = c("gray8", "gray35", "gray90")

# Visited Households per Area
visitedHouseholdsArea = function(hhs_data, household_to_be_visited, sample_size, 
                                 study_area_name) {
  #browser()
  interval = 100
  max_x_axis = household_to_be_visited + interval * 5
  
  consented = numberOfparticipantsWhoConsented(hhs_data)
  recruitment = recruitmentRate(hhs_data, sample_size)
  
  visits = table(hhs_data$district)
  if(is.na(visits)) 
    visits = 0
  
  completeness = floor((visits / household_to_be_visited) * 100)
  
  par(cex.lab = 1.5, cex.main = 2, cex.axis = 1.05)
  visits_progress = barplot(
    height      = matrix(c(consented, visits), nrow = 2, ncol = 2, byrow = T), 
    horiz       = T, 
    names.arg   = study_area_name, 
    main        = paste("Visited Households in", study_area_name),
    xlab        = "Number of households",
    ylab        = "Study area",
    xlim        = c(0, max_x_axis),
    axes        = F,
    beside      = T,
    col = color_palette[2:3]
  )
  axis(1, seq(0, max_x_axis, interval))
  legend("topright", legend = c("Interviewed", "Visited"), fill = color_palette[2:3], cex = 1.5)
  text(
    x      = c(visits, consented), 
    y      = c(2.5, 5.5, 1.5, 4.5), 
    labels = paste0(c(completeness, recruitment), '%'), 
    pos    = 4, 
    col = "red",
    cex = 1.5
  )
}

# Visited Households per Cluster in a concrete Area
progressOfArea = function(hhs_data, study_area_column, study_area_label, interval, required_visits_mean, lang = 'EN') {
  #browser()
  EN = c("Visited Households per Cluster in", # 1
                    "Cluster in",                        # 2
                    "Number of households",              # 3
                    "Interviewed",                       # 4
                    "Visited",                           # 5
                    "There is no data."                  # 6
                    )
  FR = c("Ménages Visités par Grappe à",      # 1
                    "Grappe à",                          # 2
                    "Nombre de ménages",                 # 3
                    "Interviewés",                       # 4
                    "Visités",                           # 5
                    "Il n'y a pas des données."          # 6
                    )
  strings = data.frame(EN, FR, stringsAsFactors = F)
  
  column = paste0("cluster_", study_area_column)
  
  visits_number = table(hhs_data[column])
  if(length(visits_number) > 0) {
    max_y_axis = max(visits_number) + interval
    consented_number = table(hhs_data[
      hhs_data$ended_pregnancy == 1 & hhs_data$resident_during_pregnancy == 1, column])
    
    dat = union(visits_number, consented_number)
    par(cex.lab = 1.5, cex.main = 2, cex.axis = 1.05, mar = c(8, 8, 4, 0))
    visits_progress = barplot(
      height = matrix(c(dat[2,], dat[1,] - dat[2,]), nrow = 2, byrow = T),
      main   = paste(strings[1, lang], study_area_label),
      xlab   = paste(strings[2, lang], study_area_label),
      ylab   = strings[3, lang],
      ylim   = c(0, max_y_axis),
      axes   = F,
      col = color_palette[2:3],
      mgp = c(4, 1, 0)
    )
    axis(1, visits_progress, paste0("C", rownames(visits_number)), las = 2)
    axis(2, seq(0, max_y_axis, interval))
    abline(h = required_visits_mean, lwd = 1, lty = 2, col = "red")
    legend("topright", legend = c(strings[4, lang], strings[5, lang]), fill = color_palette[2:3], cex = 1.5)
    text(x = visits_progress, y = dat[2,], labels = dat[2,], pos = 3, col = color_palette[1])
  } else {
    print(strings[6, lang]) 
  }
}

# Tables -------------------------------------------------------------------------------------------
trialProfileOfArea = function(hhs_data, study_area_column) {
  #browser()
  maximum_number_of_columns = 29
  font_size = 10
  column = paste0("cluster_", study_area_column)
  
  number_hh_selected_visited = table(hhs_data[column])
  if(length(number_hh_selected_visited) > 0) {
    number_hh_selected_interviewed = table(hhs_data[hhs_data$hh_acceptance == 1, column])
    
    number_women_childbearing_age_df = setNames(
      aggregate(childbearing_age_women ~ get(column), FUN = sum, data = hhs_data), 
      c(column, "childbearing_age_women")
    )
    number_women_childbearing_age_list = pivot(
      indexes = names(number_hh_selected_visited), 
      index_column = column, 
      value_column = "childbearing_age_women", 
      df = number_women_childbearing_age_df
    )
    
    number_eligible_women_df = setNames(
      aggregate(residents_during_pregnancy ~ get(column), FUN = sum, data = hhs_data),
      c(column, "residents_during_pregnancy")
    )
    number_eligible_women_list = pivot(
      indexes = names(number_hh_selected_visited),
      index_column = column,
      value_column = "residents_during_pregnancy",
      df = number_eligible_women_df
    )
    
    childbearing_age_women_profile = union(
      number_women_childbearing_age_list, 
      number_eligible_women_list
    )
    
    number_women_interviewed = table(
      subset(hhs_data, ended_pregnancy == 1 & resident_during_pregnancy == 1)[column])
    number_women_interrupt_interview = table(
      subset(hhs_data, consent == 1 & 
               (is.na(ended_pregnancy) | ended_pregnancy == 0 | 
                  is.na(resident_during_pregnancy) | resident_during_pregnancy == 0))[column])
    number_women_non_interviewed = table(subset(hhs_data, consent ==0)[column])
    
    eligible_women_selected = union(number_women_interviewed, number_women_interrupt_interview, 
                                    number_women_non_interviewed)
    if(ncol(eligible_women_selected) > 0) {
      eligible_women_selected_totals = eligible_women_selected[1,] + eligible_women_selected[2,] + eligible_women_selected[3,]
    } else {
      eligible_women_selected_totals = number_women_interviewed # empty table 
    }
    
    number_women_denied_consent = table(hhs_data[hhs_data$why_not_consent == 0, column])
    number_women_absent = table(hhs_data[hhs_data$why_not_consent == 2, column])
    number_women_unabled = table(hhs_data[hhs_data$why_not_consent == 1, column])
    number_women_other_reason = table(hhs_data[hhs_data$why_not_consent == 88, column])
    
    number_hh_empty = table(hhs_data[hhs_data$hh_available == 2, column])
    number_hh_head_not_found = table(hhs_data[hhs_data$hh_available == 0, column])
    number_hh_head_refused = table(hhs_data[hhs_data$hh_acceptance == 0, column])
    
    hh_selected_not_interviewed = union(number_hh_empty, number_hh_head_refused)
    if(ncol(hh_selected_not_interviewed) > 0)
      hh_selected_not_interviewed_totals = 
        hh_selected_not_interviewed[1,] + hh_selected_not_interviewed[2,]
    else
      hh_selected_not_interviewed_totals = number_hh_empty # emty table
    
    trial_profile = union(
      number_hh_selected_visited, 
      number_hh_selected_interviewed, 
      number_women_childbearing_age_list, 
      childbearing_age_women_profile[1,] - childbearing_age_women_profile[2,], 
      number_eligible_women_list,
      eligible_women_selected_totals,
      number_women_interviewed,
      number_women_interrupt_interview,
      number_women_non_interviewed,
      number_women_denied_consent,
      number_women_absent,
      number_women_unabled,
      number_women_other_reason,
      hh_selected_not_interviewed_totals,
      number_hh_empty,
      number_hh_head_not_found,
      number_hh_head_refused
    )
    row.names(trial_profile) = c(
      "HH selected visited", 
      "HH selected interviewed", 
      "Women of childbearing age",
      paste0("NON eligible women", footnote_marker_symbol(1, "html")),
      "Eligible women",
      "Eligible women selected",
      "Women interviewed",
      "Women that interrupted interview",
      "Women NON interviewed",
      "Denied signed consent/assent",
      "Absent",
      "Not able to respond",
      "Other reason",
      "HH selected NOT interviewed",
      "Empty/destroyed",
      paste0("HH head not found", footnote_marker_symbol(2, "html")),
      "HH head/other refused to consent the interview"
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
                 "NOT interviewed HH must be equal to the sum of empty/destroyed + refused", "")
      )
      
      # women = eligible + non_eligible
      trial_profile_checked[c(3, 4, 5), i] = cell_spec(
        x        = trial_profile[c(3, 4, 5),i],
        format   ="html",
        color    = 
          ifelse(trial_profile[4, i] + trial_profile[5, i] != trial_profile[3, i], "red", ""),
        tooltip  = 
          ifelse(trial_profile[4, i] + trial_profile[5, i] != trial_profile[3, i], 
                 "Women must be equal to the sum of eligibles + NON eligibles", "")
      )
      
      # non_interviwed women = denied + absent + unabled + other
      trial_profile_checked[c(9, 10, 11, 12, 13), i] = cell_spec(
        x        = trial_profile[c(9, 10, 11, 12, 13),i],
        format   ="html",
        color    = 
          ifelse(trial_profile[10, i] + trial_profile[11, i] + trial_profile[12, i] + 
                   trial_profile[13, i] != trial_profile[9, i], "red", ""),
        tooltip  = 
          ifelse(trial_profile[10, i] + trial_profile[11, i] + trial_profile[12, i] + 
                   trial_profile[13, i]  != trial_profile[9, i], 
                 "NON interviewed women must be equal to the sum of denied + absent + unabled", "")
      )
      
      # women selected = interviewed + interrupted + non_interviewed
      trial_profile_checked[c(6, 7, 8, 9), i] = cell_spec(
        x        = trial_profile[c(6, 7, 8, 9),i],
        format   ="html",
        color    = 
          ifelse(trial_profile[7, i] + trial_profile[8, i] + trial_profile[9, i] 
                 != trial_profile[6, i], "red", ""),
        tooltip  = 
          ifelse(trial_profile[7, i] + trial_profile[8, i] + trial_profile[9, i] 
                 != trial_profile[6, i], 
                 "Women selected must be equal to the sum of interviewed + interrupted + NON interviewed", "")
      )
      
      # visited HH = interviewed + non_interviewed
      trial_profile_checked[c(1, 2, 14), i] = cell_spec(
        x        = trial_profile[c(1, 2, 14),i],
        format   ="html",
        color    = 
          ifelse(trial_profile[2, i] + trial_profile[14, i] != trial_profile[1, i], "red", ""),
        tooltip  = 
          ifelse(trial_profile[2, i] + trial_profile[14, i] != trial_profile[1, i], 
                 "Visited HH must be equal to the sum of interviewed + NOT interviewed", "")
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
          general_title = "Notes:",
          general = "Colored cells are consistency errors. Hover over these cells to display a 
          tooltip with the error message. Please, refer to the provided Data Queries Sheet.", 
          symbol = c(
            "Eligible woman: woman that meets selection criteria 1 and selection criteria 2", 
            "HH head availability is not required to proceed with the interview as long as any other 
            adult consents"
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
          general_title = "Notes:",
          general = "Colored cells are consistency errors. Hover over these cells to display a 
          tooltip with the error message. Please, refer to the provided Data Queries Sheet.", 
          symbol = c(
            "Eligible woman: woman that meets selection criteria 1 and selection criteria 2", 
            "HH head availability is not required to proceed with the interview as long as any other 
            adult consents"
          )
        )
      )
    }
  } else {
    print("There is no data.")
  }
  
}

SPIndicators = function(hhs_data, study_areas) {
  #browser()
  consented = numberOfparticipantsWhoConsented(hhs_data)
  
  sp = table(hhs_data$sp)
  #sp = t(union(sp_area_1, sp_area_2))
  
  sp_doses = table(hhs_data$sp_doses_number)
  #sp_doses = t(union(sp_doses_area_1, sp_doses_area_2))
  
  sp_adherence = c(
    consented,
    if('1' %in% rownames(sp)) sp['1'] else 0,
    if('1' %in% rownames(sp_doses)) sp_doses['1'] else 0,
    if('2' %in% rownames(sp_doses)) sp_doses['2'] else 0,
    if('3' %in% rownames(sp_doses)) sp_doses['3'] else 0,
    if('4' %in% rownames(sp_doses)) sp_doses['4'] else 0,
    if('5' %in% rownames(sp_doses)) sp_doses['5'] else 0,
    if('6' %in% rownames(sp_doses)) sp_doses['6'] else 0,
    (if('7' %in% rownames(sp_doses)) sp_doses['7'] else 0) +
      (if('8' %in% rownames(sp_doses)) sp_doses['8'] else 0) +
      (if('9' %in% rownames(sp_doses)) sp_doses['9'] else 0),
    if('0' %in% rownames(sp)) sp['0'] else 0, 
    if('2' %in% rownames(sp)) sp['2'] else 0
  )
  for(i in 2:11) {
    sp_adherence[i] = paste(
      sp_adherence[i],
      paste0("(", round((as.integer(sp_adherence[i]) / consented) * 100, 2), "%", ")")
    )
  }
  sp_adherence = as.data.frame(sp_adherence)
  
  row.names(sp_adherence) = c(
    "Women interviewed",
    "Women that took SP",
    "Women that took exactly 1 dose of SP",
    "Women that took exactly 2 doses of SP",
    "Women that took exactly 3 doses of SP",
    "Women that took exactly 4 doses of SP",
    "Women that took exactly 5 doses of SP",
    "Women that took exactly 6 doses of SP",
    "Women that took more than 6 doses of SP",
    "Women that didn't take SP",
    "Women that didn't know if they took SP"
  )
  colnames(sp_adherence) = study_area_label
  
  kable(sp_adherence, "html", escape = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "responsive"), font_size = 12, 
                  full_width = F, position = "float_right") %>%
    row_spec(0, bold = T, color = "white", background = "#494949") %>%
    row_spec(c(1, 2, 10, 11), bold = T) %>%
    add_indent(c(3, 4, 5, 6, 7, 8, 9))
}

ANCIndicators = function(hhs_data, study_area_label) {
  #browser()
  consented = numberOfparticipantsWhoConsented(hhs_data)
  
  anc = table(hhs_data$attend_anc)
  #anc = t(union(anc_area_1, anc_area_2))
  
  anc_visits = table(hhs_data$anc_visits_number)
  #anc_visits = t(union(anc_visits_area_1, anc_visits_area_2))

  anc_attendance = c(
    consented,
    if('1' %in% rownames(anc)) anc['1'] else 0,
    if('1' %in% rownames(anc_visits)) anc_visits['1'] else 0,
    if('2' %in% rownames(anc_visits)) anc_visits['2'] else 0,
    if('3' %in% rownames(anc_visits)) anc_visits['3'] else 0,
    if('4' %in% rownames(anc_visits)) anc_visits['4'] else 0,
    if('5' %in% rownames(anc_visits)) anc_visits['5'] else 0,
    if('6' %in% rownames(anc_visits)) anc_visits['6'] else 0,
    (if('7' %in% rownames(anc_visits)) anc_visits['7'] else 0) + 
      (if('8' %in% rownames(anc_visits)) anc_visits['8'] else 0) +
      (if('9' %in% rownames(anc_visits)) anc_visits['9'] else 0) +
      (if('10' %in% rownames(anc_visits)) anc_visits['10'] else 0),
    if('0' %in% rownames(anc)) anc['0'] else 0
  )
  for(i in 2:10) {
    anc_attendance[i] = paste(
      anc_attendance[i],
      paste0("(", round((as.integer(anc_attendance[i]) / consented) * 100, 2), "%", ")")
    )
  }
  
  anc_attendance = as.data.frame(anc_attendance)
  
  row.names(anc_attendance) = c(
    "Women interviewed",
    "Women that attended ANC clinic",
    "Women that attended exactly once to ANC clinic",
    "Women that attended exactly twice to ANC clinic",
    "Women that attended exactly 3 times to ANC clinic",
    "Women that attended exactly 4 times to ANC clinic",
    "Women that attended exactly 5 times to ANC clinic",
    "Women that attended exactly 6 times to ANC clinic",
    "Women that attended more than 6 times to ANC clinic",
    "Women that didn't attend to ANC clinic"
  )
  colnames(anc_attendance) = study_area_label
  
  kable(anc_attendance, "html", escape = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "responsive"), font_size = 12, 
                  full_width = T) %>%
    row_spec(0, bold = T, color = "white", background = "#494949") %>%
    row_spec(c(1, 2, 10), bold = T) %>%
    add_indent(c(3, 4, 5, 6, 7, 8, 9))
}

duplicatedRecords = function(hhs_data, study_area_column, study_area_label) {
  #id_columns = hhs_data[c("cluster_kenge", "cluster_bulungu", "household")]
  #browser()
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
    duplicated_records_summary$consent[is.na(duplicated_records_summary$consent)] = "Not asked"
    duplicated_records_summary$consent[duplicated_records_summary$consent == 0]   = "No"
    duplicated_records_summary$consent[duplicated_records_summary$consent == 1]   = "Yes"
    
    duplicated_records_summary$district = study_area_label
  }
  
  return(duplicated_records_summary)
}

printDuplicatedRecords = function(hhs_data, study_areacolumn, study_area_label) {
  #browser()
  duplicated_records_summary = duplicatedRecords(hhs_data, study_area_column, study_area_label)
  
  if(nrow(duplicated_records_summary) > 0) {
    colnames(duplicated_records_summary) = c("ID", "District", "Cluster", "HH ID", "Latitude", 
                                             "Longitude", "Head Initials", "Consent", "Int. ID", 
                                             "Int. Date")
    
    kable(duplicated_records_summary, "html", row.names = F, escape = F) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                    font_size = 12) %>%
      row_spec(0, bold = T, color = "white", background = "#494949") %>%
      scroll_box(height = "250px")
  } else {
    print("There are no duplicated records.")
  }
}

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
    
  if(nrow(rerecorded_hh) == 0)
    return(NULL)
  
  rerecorded_hh$cluster[!is.na(rerecorded_hh[column])] = 
    rerecorded_hh[!is.na(rerecorded_hh[column]), column]
    
  columns = c("record_id", "district", "cluster", "household", "latitude", "longitude", 
              "hh_initials", "hh_sex", "hh_available", "consent", "end_last_pregnancy", 
              "reported_age", "interviewer_id", "interview_date")
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
  
  rerecorded_hh_summary$consent[is.na(rerecorded_hh_summary$consent)] = "Not asked"
  rerecorded_hh_summary$consent[rerecorded_hh_summary$consent == 0]   = "No"
  rerecorded_hh_summary$consent[rerecorded_hh_summary$consent == 1]   = "Yes"
  
  rerecorded_hh_summary$district = study_area_label
  
  rerecorded_hh_summary$hh_sex[rerecorded_hh_summary$hh_sex == 0] = "F"
  rerecorded_hh_summary$hh_sex[rerecorded_hh_summary$hh_sex == 1] = "M"
  
  rerecorded_hh_summary$hh_available[rerecorded_hh_summary$hh_available == 0] = "No"
  rerecorded_hh_summary$hh_available[rerecorded_hh_summary$hh_available == 1] = "Yes"
  rerecorded_hh_summary$hh_available[rerecorded_hh_summary$hh_available == 2] = "Empty HH"
  
  rerecorded_hh_summary$duplicated[rerecorded_hh_summary$duplicated == F] = "F"
  rerecorded_hh_summary$duplicated[rerecorded_hh_summary$duplicated == T] = "T"
  
  return(rerecorded_hh_summary)
}

printDuplicatedHouseholds = function(hhs_data, study_area_column, study_area_label) {
  #browser()
  rerecorded_hh_summary = duplicatedHouseholds(hhs_data, study_area_column, study_area_label)
  
  if(!is.null(rerecorded_hh_summary)) {
    if(nrow(rerecorded_hh_summary) > 0) {
      colnames(rerecorded_hh_summary) = c("ID", "District", "C.", "HH ID", "Lat.", "Lng.", 
                                          "H. Initials", "Sex", "Available", "Cons.", "End Preg.", 
                                          "Age", "Int. ID", "Int. Date", "D.")
    
      kable(rerecorded_hh_summary, "html", row.names = F, escape = F) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                      font_size = 12) %>%
        row_spec(0, bold = T, color = "white", background = "#494949") %>%
        scroll_box(height = "250px")
    } else {
      print("There are no duplicated households.")
    }
  } else {
    print("There are no duplicated households.")
  }
}

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
      "NON interviewed HH", 
      "Interviewed HH", 
      "Duplicated records in NON interviewed HH",
      "Duplicated records in interviewed HH",
      "NON interviewed HH without duplicated records",
      "Interviewed HH without duplicated records",
      "Reused HH IDs",
      "Reused HH IDs in interviewed HH"
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
      print("There are no duplicates.") 
    }
  } else {
    print("There is no data.")
  }
}

recordsByInterviewerPerCluster = function(hhs_data, study_area_column) {
  #browser()
  column = paste0("cluster_", study_area_column)
  records_int_cluster = table(hhs_data[, c(column, 'interviewer_id')], useNA = "ifany")
  women_int_cluster   = table(hhs_data[which(hhs_data$consent == 1), c(column, 'interviewer_id')], useNA = "ifany")
  
  records_int_cluster = as.data.frame(records_int_cluster, stringsAsFactors = F)
  women_int_cluster   = as.data.frame(women_int_cluster, stringsAsFactors = F)
  
  records_int_cluster$interviewer_id[is.na(records_int_cluster$interviewer_id)] = "UNK"
  women_int_cluster$interviewer_id[is.na(women_int_cluster$interviewer_id)] = "UNK"
  
  records_int_cluster = reshape(
    data      = records_int_cluster, 
    direction = "wide", 
    idvar     = column, 
    timevar   = "interviewer_id"
  )
  women_int_cluster   = reshape(
    data      = women_int_cluster,
    direction = "wide",
    idvar     = column,
    timevar   = "interviewer_id" 
  )
  
  row.names(records_int_cluster) = paste0("C", records_int_cluster[[column]])
  row.names(women_int_cluster)   = paste0("C", women_int_cluster[[column]])
  records_int_cluster[column] = NULL
  women_int_cluster[column]   = NULL
  
  records_int_cluster = rbind(records_int_cluster, colSums(records_int_cluster))
  women_int_cluster   = rbind(women_int_cluster, colSums(women_int_cluster))
  row.names(records_int_cluster)[nrow(records_int_cluster)] = "Total"
  row.names(women_int_cluster)[nrow(women_int_cluster)]     = "Total"
  
  colnames(records_int_cluster) = substring(colnames(records_int_cluster), 6)
  colnames(women_int_cluster)   = substring(colnames(women_int_cluster), 6)
  
  if("C0" %in% row.names(records_int_cluster) && !("C0" %in% row.names(women_int_cluster)))
    women_int_cluster = rbind(C0 = 0, women_int_cluster)
  
  records_women_table = as.data.frame(do.call(
    what = cbind, 
    args = lapply(
      X   = colnames(records_int_cluster), 
      FUN = function(c) {
        if(c %in% colnames(women_int_cluster))
          paste0(records_int_cluster[ , c], " (", women_int_cluster[ , c], ")")
        else
          paste0(records_int_cluster[ , c], " (0)")
      }
    )
  ))
  
  records_women_table[records_women_table == "0 (0)"] = NA
  
  row.names(records_women_table) = row.names(records_int_cluster)
  colnames(records_women_table)  = colnames(records_int_cluster)
  
  kable(records_women_table, "html", escape = F) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "responsive"), font_size = 10, 
                  full_width = T) %>%
    row_spec(0, bold = T, color = "white", background = "#494949") %>%
    row_spec(c(18), bold = T)
}