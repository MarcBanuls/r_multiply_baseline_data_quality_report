
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
  
study_areas_ids = study_area_1_id
study_areas     = study_area_1

# Read data set from REDCap by using the provided token
redcap_api_url = "https://maternal.isglobal.org/redcap/api/"
# For Multiply, we have to merge the hh data with the RDT data
if(source == "API") {
  hhs_data = readData("api", api_url = redcap_api_url, api_token = api_token)
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

#change "cluster" name to "cluster_haho" to facilitate change of code
names(hhs_data)[names(hhs_data) == 'cluster'] <- 'cluster_haho'


#not working properly??
#hhs_data = removeSpecialCharacters(hhs_data, 'cluster_haho')
#removeEmptyRecords only works when district is a variable, make another function
#hhs_data = removeEmptyRecords(hhs_data)
hhs_data = hhs_data[!is.na(hhs_data$cluster_haho), ]

# ENDLINE BULUNGU SPECIFIC: There was an error in cluster selection and there are records that were not from any cluster
# selected for the Endline. They were allocated in Bulungu and changed the cluster to blank. 
# We do not want to discard them from REDCap but we have to remove them for this study along with the duplicates. 
# hhs_data = hhs_data[!(hhs_data$district == 2 & is.na(hhs_data$cluster_bulungu)),]

# Remove Latitude and Longitude to prevent errors as we do not use this data for the study.
hhs_data$latitude <- NA
hhs_data$longitude <- NA

# Remove duplicated records (where all variables contain exactly the same values except record_id)
#for togo
column = paste0("cluster_", study_area_column)

x = duplicated(hhs_data[2:ncol(hhs_data)])
y = duplicated(hhs_data[2:ncol(hhs_data)], fromLast = T)
duplicated_records = hhs_data[x | y, ]
duplicated_records$cluster_haho = duplicated_records[, column]

columns = c("record_id", "cluster_haho", "household", "latitude", "longitude", 
            "hh_initials", "consent", "interviewer_id", "interview_date")

duplicated_records_summary = duplicated_records[
  order(duplicated_records$cluster_haho, duplicated_records$household), 
  columns]

record_ids_to_remove = duplicated_records$record_id[
  duplicated(duplicated_records[2:ncol(duplicated_records)])]
hhs_data_with_no_dups = hhs_data[!(hhs_data$record_id %in% record_ids_to_remove), ]

# Analyze reused household IDs, where they are duplicates or not
duplicated_households_1 = duplicatedHouseholds(hhs_data[hhs_data$district == 1, ], study_areas_ids[1], study_areas[1])
duplicated_households_2 = duplicatedHouseholds(hhs_data[hhs_data$district == 2, ], study_areas_ids[2], study_areas[2])
duplicated_households_3 = duplicatedHouseholds(hhs_data[hhs_data$district == 3, ], study_areas_ids[3], study_areas[3])
duplicated_households = rbind(duplicated_households_1, duplicated_households_2, duplicated_households_3)

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
duplicated_households_1 = duplicatedHouseholds(hhs_data_with_no_dups[hhs_data_with_no_dups$district == 1, ], study_areas_ids[1], study_areas[1])
duplicated_households_2 = duplicatedHouseholds(hhs_data_with_no_dups[hhs_data_with_no_dups$district == 2, ], study_areas_ids[2], study_areas[2])
duplicated_households_3 = duplicatedHouseholds(hhs_data_with_no_dups[hhs_data_with_no_dups$district == 3, ], study_areas_ids[3], study_areas[3])
duplicated_households = rbind(duplicated_households_1, duplicated_households_2, duplicated_households_3)
record_ids_to_rename = duplicated_households$record_id[duplicated(duplicated_households[2:4])]
hhs_data_with_no_dups = renameRecords(hhs_data_with_no_dups, record_ids_to_rename)

# Report what have been done
# Pending...

# In the Mozambique case, cluster values are scattered in multiple variables. So they were collapsed 
# before. Now removed to avoid confusion a keep the data set as it was originally
hhs_data_with_no_dups$cluster_nhamatanda = NULL
hhs_data_with_no_dups$cluster_meconta = NULL
hhs_data_with_no_dups$cluster_murrupula = NULL
###

# Write file
redcap_project_info = getProjectInfo(redcap_api_url, api_token)
filename = str_replace_all(redcap_project_info$project_title, "[^[:alnum:]]", "")
filename = paste0(filename, "_DATA_WITH_NO_DUPS_")
filename = paste0(filename, Sys.Date())
filename = paste0(filename, "_", format(Sys.time(), "%H%M"))
filename = paste0(filename, ".csv")
write.csv(duplicated_households, file = filename, row.names = F, na = "")