
# INPUTS: API, <API TOKEN>, <STUDY_AREA_1_ID>, <STUDY_AREA_1_NAME>, <STUDY_AREA_2_ID>, 
#         <STUDY_AREA_2_NAME>, <STUDY_AREA_3_ID>, <STUDY_AREA_3_NAME>
# INPUTS: FILE, <API TOKEN>, <STUDY_AREA_1_ID>, <STUDY_AREA_1_NAME>, <STUDY_AREA_2_ID>, 
#         <STUDY_AREA_2_NAME>, <STUDY_AREA_3_ID>, <STUDY_AREA_3_NAME>, <FILE_PREFIX>, 
#         <FILE_CONTENT>, <FILE_DATE>, <FILE_TIME>
#
# OUTPUT: CSV FILE WITH DATA SET WITHOUT DUPLICATES

library(stringr)
source("tiptop_hhs_quality.R")
source("tokens.R")

# Auxiliar functions
renameRecords = function (data, record_ids_to_rename) {
  #browser()
  if(length(record_ids_to_rename) > 0) {
    previous_household_id = -1
    for(i in 1:length(record_ids_to_rename)) {
      new_household_id = paste0(
        data$household[data$record_id == record_ids_to_rename[i]], '_01')
      
      if(new_household_id == previous_household_id | new_household_id %in% data$household)
        new_household_id = paste0(
          data$household[data$record_id == record_ids_to_rename[i]], '_02')
      
      data$household[data$record_id == record_ids_to_rename[i]] = new_household_id
      
      previous_household_id = new_household_id
    }
  }
  
  return(data)
}

# Read arguments
args = commandArgs(T)
source          = args[1] = "API"
api_token       = args[2] = hhs_baseline_togo
study_area_1_id = args[3] = "haho"
study_area_1    = args[4] = "Haho"
#study_area_2_id = args[5] = "meconta"
#study_area_2    = args[6] = "Meconta"
#study_area_3_id = args[7] = "murrupula"
#study_area_3    = args[8] = "Murrupula"
if(source == "FILE") {
  file_prefix   = args[9]  = "DATA/DATA/TIPTOPHHSMidlineMoza"
  file_content  = args[10] = "_DATA_"
  file_date     = args[11] = "2021-03-25"
  file_time     = args[12] = "17:00"
}
  
study_areas_ids = c(study_area_1_id,'test')
study_areas     = c(study_area_1,'test')
study_area_column = 'haho'

# Read data set from REDCap by using the provided token
redcap_api_url = "https://maternal.isglobal.org/redcap/api/"
# For Multiply, we have to merge the hh data with the RDT data
if(source == "API") {
  data = readData("api", api_url = redcap_api_url, api_token = api_token)
} else {
  data = readData("file", file_prefix = file_prefix, file_content = file_content, 
                      file_date = file_date, file_time = file_time)
}
  

# Data separated in two arms. we separate in two dataframes by its arm name
hhs <- data[data$redcap_event_name == 'household_baseline_arm_1',]
rdt <- data[data$redcap_event_name == 'malaria_rdt_arm_1',]

# Remove the variables for each df that are empty (and arm name to avoid interferences) CHANGED due to change of variables in TOGO
hhs <- hhs[,-c(2,163:194)]
rdt <- rdt[,-c(2:162)]

# Merge the two dataframes to have all data in one row per record_id
# (Named hhs_data due to reuse of tiptop functions that only worked with hhs data, not rdt)
hhs_data <- merge(hhs, rdt, by = "record_id", all.x = TRUE)

# Filter by district. Only 1 district in Togo, line not usable.
#hhs_data = hhs_data[hhs_data$district == study_area_id, ]
hhs_data = hhs_data[!is.na(hhs_data$record_id), ]



#not working properly??
#hhs_data = removeSpecialCharacters(hhs_data, 'cluster_haho')
special_characters = c('\n')
replacement_character = ' '

for(i in 1:length(special_characters)) {
  c = special_characters[i]
  r = replacement_character
  other_facility = paste0("other_facility_", study_areas_ids)
  
  hhs_data$why_not_consent_other = gsub(c, r, hhs_data$why_not_consent_other)       # HQ10.1.1.1.1
  hhs_data$why_not_attend_epi_other = gsub(c, r, hhs_data$why_not_attend_epi_other) # HQ16.1.1
  hhs_data$u5_card_keeper = gsub(c, r, hhs_data$u5_card_keeper)                     # HQ16.2.1.1
  hhs_data$why_not_u5_card_other = gsub(c, r, hhs_data$why_not_u5_card_other)       # HQ16.2.1.1
  hhs_data$malaria_knowledge_causes_other = gsub(c, r, hhs_data$malaria_knowledge_causes_other) # HQ24.1
  hhs_data$malaria_knowledge_prevent_other = gsub(c, r, hhs_data$malaria_knowledge_prevent_other) # HQ26.1
  hhs_data$fever_treatment_other = gsub(c, r, hhs_data$fever_treatment_other)       #HQ28.1
  hhs_data$his_caretaker_relation_other = gsub(c, r, hhs_data$his_caretaker_relation_other) # HQ31.1
  hhs_data$religion_other = gsub(c, r, hhs_data$religion_other)                     #HQ36.1
  hhs_data$ethnic_group_other = gsub(c, r, hhs_data$ethnic_group_other)             #HQ37.1
  hhs_data$his_hh_relation_other = gsub(c, r, hhs_data$his_hh_relation_other)       #HQ40.1
  hhs_data$why_warnings = gsub(c, r, hhs_data$why_warnings)                         #WARNINGS
  hhs_data$comments = gsub(c, r, hhs_data$comments)                                 #COMMENTS
  
  

  # if("other_facility" %in% colnames(hhs_data))
  #   hhs_data$other_facility = gsub(c, r, hhs_data$other_facility)                   # WQ22.1d
  # hhs_data$other_facility_transport = gsub(c, r, hhs_data$other_facility_transport) # WQ23.1
  # if("other_religion" %in% colnames(hhs_data))
  #   hhs_data$other_religion = gsub(c, r, hhs_data$other_religion)                   # WQ25.1
  # if("other_ethnic_group" %in% colnames(hhs_data))
  #   hhs_data$other_ethnic_group = gsub(c, r, hhs_data$other_ethnic_group)           # WQ26.1
  # hhs_data$partner_other_occupation = gsub(c, r, hhs_data$partner_other_occupation) # WQ29.1
  
}



#removeEmptyRecords only works when district is a variable, make another function
#hhs_data = removeEmptyRecords(hhs_data)
hhs_data = hhs_data[!is.na(hhs_data$cluster), ]

# ENDLINE BULUNGU SPECIFIC: There was an error in cluster selection and there are records that were not from any cluster
# selected for the Endline. They were allocated in Bulungu and changed the cluster to blank. 
# We do not want to discard them from REDCap but we have to remove them for this study along with the duplicates. 
# hhs_data = hhs_data[!(hhs_data$district == 2 & is.na(hhs_data$cluster_bulungu)),]

# Remove Latitude and Longitude to prevent errors as we do not use this data for the study.
hhs_data$latitude <- NA
hhs_data$longitude <- NA

###-------------###
# Remove duplicated records (where all variables contain exactly the same values except record_id)
#for togo
column = paste0('cluster')

x = duplicated(hhs_data[2:ncol(hhs_data)])
y = duplicated(hhs_data[2:ncol(hhs_data)], fromLast = T)
duplicated_records = hhs_data[x | y, ]
duplicated_records$cluster = duplicated_records[, column]

columns = c("record_id", "cluster", "household", "latitude", "longitude", 
            "hh_initials", "consent", "interviewer_id", "interview_date")

duplicated_records_summary = duplicated_records[
  order(duplicated_records$cluster, duplicated_records$household), 
  columns]

record_ids_to_remove = duplicated_records$record_id[
  duplicated(duplicated_records[2:ncol(duplicated_records)])]
hhs_data_with_no_dups = hhs_data[!(hhs_data$record_id %in% record_ids_to_remove), ]

# Analyze reused household IDs, where they are duplicates or not (added a 'test' to mantain column
# and not change too much the function)
#duplicated_households = duplicatedHouseholds(hhs_data, study_areas_ids[1], study_areas[1])

### duplicated household function changed

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


rerecorded_hh$cluster_haho[!is.na(rerecorded_hh[column])] = 
  rerecorded_hh[!is.na(rerecorded_hh[column]), column]

columns = c("record_id", "cluster", "household", "latitude", "longitude", 
            "hh_initials", "hh_sex", "hh_available", "consent", "interviewer_id", "interview_date")
rerecorded_hh_summary = rerecorded_hh[
  order(rerecorded_hh$cluster, rerecorded_hh$household, 
        rerecorded_hh$interview_date), columns]



# Remove deleted records
rerecorded_hh_summary = rerecorded_hh_summary[!is.na(rerecorded_hh_summary$cluster_haho), ]

# Disambiguate records
if(nrow(rerecorded_hh_summary) > 0) {
  rerecorded_hh_summary$duplicated = NA
  current_cluster = rerecorded_hh_summary$cluster_haho
  current_household = rerecorded_hh_summary$household[1]
  records_in_conflict = c(1)
  for(i in 2:nrow(rerecorded_hh_summary)) {
    if(rerecorded_hh_summary$cluster_haho[i] == current_cluster &
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
      
      current_cluster = rerecorded_hh_summary$cluster_haho[i]
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


rerecorded_hh_summary$hh_sex[rerecorded_hh_summary$hh_sex == 0] = "F"
rerecorded_hh_summary$hh_sex[rerecorded_hh_summary$hh_sex == 1] = "M"

rerecorded_hh_summary$hh_available[rerecorded_hh_summary$hh_available == 0] = "No"
rerecorded_hh_summary$hh_available[rerecorded_hh_summary$hh_available == 1] = "Yes"
rerecorded_hh_summary$hh_available[rerecorded_hh_summary$hh_available == 2] = "Empty HH"

rerecorded_hh_summary$duplicated[rerecorded_hh_summary$duplicated == F] = "F"
rerecorded_hh_summary$duplicated[rerecorded_hh_summary$duplicated == T] = "T"

#rerecorded_hh_summary is then used as duplicated_households variable using a rbind,
#which cannot be usedi in Togo, so we name it and thats all
duplicated_households = rerecorded_hh_summary





# When they are not duplicates, reassing a new household ID (TO THE LATEST RECORDS)
non_duplicated_households = duplicated_households[duplicated_households$duplicated == 'F', ]
record_ids_to_rename = non_duplicated_households$record_id[
  duplicated(non_duplicated_households[2:4])]
hhs_data_with_no_dups = renameRecords(hhs_data_with_no_dups, record_ids_to_rename)

# When they are duplicates, remove the OLDEST RECORDS and keep the LAST ONE
in_fact_duplicated_households = duplicated_households[duplicated_households$duplicated == 'T', ]
record_ids_to_drop = in_fact_duplicated_households$record_id[
  duplicated(in_fact_duplicated_households[2:4], fromLast = T)]
hhs_data_with_no_dups = hhs_data_with_no_dups[!(hhs_data_with_no_dups$record_id %in% 
                                                  record_ids_to_drop), ]

# After renaming non duplicated households and dropping duplicated households, could happen that 
# still few duplicates remain in the dataset, concretely those household IDs which have occurrecies 
# in both non duplicated and duplicated. They have been porcessed independently, now they must be
# treated on the whole by renaming again.
record_ids_to_rename = duplicated_households$record_id[duplicated(duplicated_households[2:4])]
hhs_data_with_no_dups = renameRecords(hhs_data_with_no_dups, record_ids_to_rename)

# Report what have been done
# Pending...

###

# Write file
redcap_project_info = getProjectInfo(redcap_api_url, api_token)
filename = str_replace_all(redcap_project_info$project_title, "[^[:alnum:]]", "")
filename = paste0(filename, "_DATA_WITH_NO_DUPS_")
filename = paste0(filename, Sys.Date())
filename = paste0(filename, "_", format(Sys.time(), "%H%M"))
filename = paste0(filename, ".csv")
write.csv(hhs_data_with_no_dups, file = filename, row.names = F, na = "")
