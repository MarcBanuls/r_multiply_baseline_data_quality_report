library(dplyr)
dumpcons <- read.csv("dconstantino.csv")
dumpmac <- read.csv("mmacau.csv")
dumpgil <- read.csv("CGIL.csv")

#the import_template no te les variables de les fotos:
#u5_card_name, u5_card_name_2, u5_card_vaccines

#descargar import template i fer un rbind directe de les rows que volem per a cada csv
importemp <- read.csv("import_template.csv")
setdiff(colnames(dumpcons), colnames(importemp))

#remover les tres variables de fotos, els rdt i 
dumpcons <- subset(dumpcons, select=-c(u5_card_name,u5_card_name_2, u5_card_vaccines, interview_duration, interview_duration_rdt))
dumpmac <- subset(dumpmac, select=-c(u5_card_name,u5_card_name_2, u5_card_vaccines, interview_duration, interview_duration_rdt))
dumpgil <- subset(dumpgil, select=-c(u5_card_name,u5_card_name_2, u5_card_vaccines, interview_duration, interview_duration_rdt))
# remou la "X"  del importemplate que surt perque el import template te una coma:
importemp <- subset(importemp, select=-c(X))

#ids que importar
#dconstantino: 2076
cons_filter <- dumpcons %>% 
  filter(record_id == 2076)
importemp <-rbind(cons_filter, importemp)

#mmacau: 1745
macau_filter <- dumpmac %>% 
  filter(record_id == 1745)
importemp <-rbind(macau_filter, importemp)
#cgil: 1068, 1069, 1074, 1063, 1060, 1080
gil_filter <- dumpgil %>% 
  filter(record_id == 1068 | record_id == 1069 | record_id == 1074 | record_id == 1063 | record_id == 1060 | record_id == 1080)
importemp <-rbind(gil_filter, importemp)

write.csv(importemp, "dump_r.csv",row.names = F, na = '')
#it works, yay!!

