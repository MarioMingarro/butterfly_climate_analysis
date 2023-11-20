library(tidyverse)
library(readxl)
library(tidystringdist)

# All Huesca's data
Data <- read_excel("B:/A_ROB/ARAGON/Huesca_Data_1965-75.xlsx")

UTM_Selected <- read_delim("B:/A_ROB/ARAGON/UTM_SELECTED_2.txt", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Selected UTM
UTM_Selected <- UTM_Selected$CUADRICULA

data_selected <- filter(Data, Data$CUTM %in% UTM_Selected)
writexl::write_xlsx(data_selected,"B:/A_ROB/ARAGON/data_selected.xlsx" )

# Names
data_selected$LOCALIDAD <- tolower(data_selected$LOCALIDAD)
tolower(unique(Data$LOCALIDAD))

compare <- tidy_comb_all(Data, LOCALIDAD)

comparisons <- tidy_stringdist(compare) %>% 
  mutate(sim = 1-jaccard) %>% 
  select(V1, V2, sim)

recommendation <- comparisons %>% 
  group_by(V1) %>% 
  summarise(max_sim = max(sim)) %>% 
  ungroup()

kk <- comparisons %>% 
  inner_join(recommendation, by = c("V1" = "V1", "sim" = "max_sim"))

writexl::write_xlsx(kk , "B:/A_ROB/ARAGON/NAMES.xlsx")

# source
unique(data_selected$LOCALIDAD)
Author_contribution <- data_selected %>% 
  group_by(CUTM) %>% 
  summarise(ind_count= n())
