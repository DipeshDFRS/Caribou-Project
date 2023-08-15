#Caribou_project
#required packages 
library(RODBC)# to connect with SQL
library(DBI) # to connect with SQL
library(odbc) # to connect with SQL
library(dplyr) #for data manipulation

# connecting with sQL
MySQL_invq <- odbcConnect("MySQL_DSN")

#importing table from sQL
PLACETTE = sqlQuery(MySQL_invq,"SELECT * FROM placette") #GGeneral characteristics of sample plots
PLACETTE_MES = sqlQuery(MySQL_invq,"SELECT * FROM placette_mes") #Characteristics of sample plot measurements
PLAN_RESEAU = sqlQuery(MySQL_invq,"SELECT * FROM plan_reseau") #Description and number of sample plots per network
PLAN_DESCR_TYPE_PE = sqlQuery(MySQL_invq,"SELECT * FROM PLAN_DESCR_TYPE_PE") #Descriptions of the different types of sample plots,sub-plots and micro-plots
PLAN_REGION_BAS1  = sqlQuery(MySQL_invq,"SELECT * FROM PLAN_REGION_BAS1 ")#Coverage of regions defining the sampling intensity of the sample plots of the BAS1 network
PLAN_REGION_BAS2 = sqlQuery(MySQL_invq,"SELECT * FROM PLAN_REGION_BAS2")#Coverage of regions defining the sampling intensity of the sample plots of the BAS2 network"
PLAN_COMPOSANTE_FACTEXP_REGION = sqlQuery(MySQL_invq,"SELECT * FROM PLAN_COMPOSANTE_FACTEXP_REGION") #Component of the sample plot expansion factor related to the region
PEE_ORI_SOND = sqlQuery(MySQL_invq,"SELECT * FROM PEE_ORI_SOND")#Ecoforest stratification of the original ecoforest stands surveyed
PEE_ETAGE_ORI_SON = sqlQuery(MySQL_invq,"SELECT * FROM PEE_ETAGE_ORI_SON")#Detailed photointerpreted dendrometric variables by floor of the original ecoforest stands surveyed
PEE_ESSENCE_ORI_SOND = sqlQuery(MySQL_invq,"SELECT * FROM PEE_ESSENCE_ORI_SOND")#Detailed species composition of the original ecoforest stands surveyed
STATION_PE = sqlQuery(MySQL_invq,"SELECT * FROM STATION_PE")#Ecoforest stratification and other characteristics of the representative surveyed station
STATION_SEMIS  = sqlQuery(MySQL_invq,"SELECT * FROM STATION_SEMIS ") #Commercial species and seedling height class in the micro-plots 
STATION_SEMIS_NCO = sqlQuery(MySQL_invq,"SELECT * FROM STATION_SEMIS_NCO")#Non-commercial species in the micro-plots (1.13 m)
STATION_SOL = sqlQuery(MySQL_invq,"SELECT * FROM STATION_SOL")#Soil characteristics
STATION_ETAGE = sqlQuery(MySQL_invq,"SELECT * FROM STATION_ETAGE")#Dendrometric variables detailed by etage of the surveyed station
STATION_ESSENCE = sqlQuery(MySQL_invq,"SELECT * FROM STATION_ESSENCE")#Detailed species composition of the surveyed station
STATION_DEBRIS_4 = sqlQuery(MySQL_invq,"SELECT * FROM STATION_DEBRIS_4")#Enumeration of woody debris by decomposition class (first permanent sample plots of the trips)
STATION_DEBRIS_5 = sqlQuery(MySQL_invq,"SELECT * FROM STATION_DEBRIS_5")#Dendrometric variables of individual stems counted
DENDRO_ARBRES = sqlQuery(MySQL_invq,"SELECT * FROM DENDRO_ARBRES")#Dendrometric variables of counted saplings
DENDRO_GAULES = sqlQuery(MySQL_invq,"SELECT * FROM DENDRO_GAULES")#Dendrometric variables of stems selected as study trees
DENDRO_ARBRES_ETUDES = sqlQuery(MySQL_invq,"SELECT * FROM DENDRO_ARBRES_ETUDES")#Quebec Ecological Classification System
CLASSI_ECO_PE = sqlQuery(MySQL_invq,"SELECT * FROM CLASSI_ECO_PE")#Quebec Ecological Classification System
META_NOTES_PE = sqlQuery(MySQL_invq,"SELECT * FROM META_NOTES_PE")#Notes on sample plots
id_study_area = sqlQuery(MySQL_invq,"SELECT * FROM id_study_area")#Id of study area plots created from ArcGIS

# visualizing the dendro_arbres table 
View(DENDRO_ARBRES)
str(DENDRO_ARBRES)
head(DENDRO_ARBRES)
tail(DENDRO_ARBRES)
names(DENDRO_ARBRES)
table(DENDRO_ARBRES$ESSENCE)
max(DENDRO_ARBRES$DHP)
mean(DENDRO_ARBRES$DHP)
min(DENDRO_ARBRES$DHP)
range(DENDRO_ARBRES$ETAT)

# Check for missing values in ETAT column
missing_values <- sum(is.na(DENDRO_ARBRES$ETAT))
if (missing_values > 0) {
  print("There are missing values in the ETAT column.")
} else {
  print("No missing values found in the ETAT column.")
}


library(dplyr)

# Joining three tables using dplyr
merged_table <- table1 %>%
  inner_join(table2, by = "common_column") %>%
  inner_join(table3, by = "common_column")


#select
DENDRO_ARBRES %>% select(32,33) -> DENDRO_ARBRES1_32_33 # selecting columns
View(DENDRO_ARBRES1_32_33)

DENDRO_ARBRES %>% select(12:22) -> DENDRO_ARBRES_12_22 # selecting series of columns
View(DENDRO_ARBRES_12_22)

# Extracting column by name
DENDRO_ARBRES %>% select("ID_PE_MES","ID_ARBRE", "ESSENCE","DHP","TIGE_HA" ,"ST_TIGE","ST_HA","ETAT") -> DENDRO_ARBRES_ATMS
View(DENDRO_ARBRES_ATMS)

names(PEE_ORI_SOND)

PEE_ORI_SOND %>% select(starts_with("A")) -> PEE_ORI_SOND_A # Extracting column with starts with A
View(PEE_ORI_SOND_A)

PEE_ORI_SOND %>% select(ends_with("E")) -> PEE_ORI_SOND_E # extracting coulmn hich starts with E 
View(PEE_ORI_SOND_E)


#filter 
names(DENDRO_ARBRES)
DENDRO_ARBRES %>% filter(NO_MES == 3) -> NO_MES_3
View(NO_MES_3)

DENDRO_ARBRES %>% filter(ESSENCE == "ERS" & DHP > 300) -> ERS_300
View(ERS_300)

#combining select with filter

DENDRO_ARBRES %>% select("ID_PE_MES","ID_ARBRE","ETAT","ESSENCE","DHP", "TIGE_HA","ST_TIGE","ST_TIGE" ) %>% filter(ESSENCE =="ERS" & DHP > 400) -> dendro_arbres_SFA

View(dendro_arbres_SFA)

#Data visualization
library(ggplot2)
#histogram
names(DENDRO_ARBRES)
ggplot(data=DENDRO_ARBRES, aes(x=DHP)) + geom_histogram()
ggplot(data=DENDRO_ARBRES, aes(x=DHP)) + geom_histogram()
ggplot(data=DENDRO_ARBRES, aes(x=DHP)) + geom_histogram()
ggplot(data=DENDRO_ARBRES, aes(x=DHP)) + geom_histogram(fill = "green")
ggplot(data=DENDRO_ARBRES, aes(x=DHP)) + geom_histogram(fill = "blue", col = "black")

#bar_plot

names(PEE_ORI_SOND)

##????
TYPE_COUV_filt <- PEE_ORI_SOND %>% filter(!is.na(TYPE_COUV))
ggplot(data = TYPE_COUV_filt, aes(x = TYPE_COUV_filt)) + geom_bar(stat = "count")
COUV_filt <- PEE_ORI_SOND %>% filter(!is.na(COUV_GAULE))
ggplot(data = PEE_ORI_SOND, aes(x=COUV_filt)) + geom_bar()


ggplot(data = PEE_ORI_SOND, aes(x=TYPE_COUV)) + geom_bar()
ggplot(data = PEE_ORI_SOND, aes(x=TYPE_COUV)) + geom_bar(fill = "palegreen")
ggplot(data = PEE_ORI_SOND, aes(x=TYPE_COUV, fill = TYPE_COUV)) + geom_bar(col = "black")
ggplot(data = PEE_ORI_SOND, aes(x=COUV_GAULE)) + geom_bar()
ggplot(data = PEE_ORI_SOND, aes(x=ORIGINE)) + geom_bar()
ggplot(data = PEE_ORI_SOND, aes(x=AN_ORIGINE)) + geom_bar()
ggplot(data = PEE_ORI_SOND, aes(x=PERTURB)) + geom_bar()
ggplot(data = PEE_ORI_SOND, aes(x=AN_PERTURB)) + geom_bar()
ggplot(data = PEE_ORI_SOND, aes(x=ET_DOMI)) + geom_bar()
ggplot(data = PEE_ORI_SOND, aes(x=CL_DENS)) + geom_bar()
ggplot(data = PEE_ORI_SOND, aes(x=CL_HAUT)) + geom_bar()
ggplot(data = PEE_ORI_SOND, aes(x=CO_TER)) + geom_bar()

#scatter_plot
names(DENDRO_ARBRES)
ggplot(data=DENDRO_ARBRES[1:100, ], aes(x=DHP, y = ST_TIGE)) + geom_point() # subsetting the number of rows
ggplot(data=DENDRO_ARBRES[1:100, ], aes(x=DHP, y = ST_TIGE, col = ESSENCE)) + geom_point()

#Boxplot
ggplot(data=DENDRO_ARBRES[1:100,], aes(x = ESSENCE , y = DHP, fill = ESSENCE)) + geom_boxplot()

#faceting

ggplot(data = DENDRO_ARBRES[1:100,], aes(x=ESSENCE, y = DHP, fill = ETAGE_ARB)) +geom_boxplot() + facet_grid(~ETAGE_ARB)


#merging of tables
library(dplyr)
#PLACETTE = PLACETTE[-2]
View(id_study_area)
id_study_area$id_pe -> ID_PE
# Change the column name from 'id_pe' to 'ID_PE'
#colnames(id_study_area)[colnames(id_study_area) == "id_pe"] <- "ID_PE"
# Increase the memory limit
#memory.limit(size = 8000)  # Specify the desired memory limit in megabytes (MB)
#DENDRO_ARBRES_SA <- merge(x = id_study_area, y = DENDRO_ARBRES, by = ID_PE )
#Merging table using SQL due to memory problem in R
DENDRO_ARBRES_SA = sqlQuery(MySQL_invq,"select * from id_study_area inner join dendro_arbres on id_study_area.id_pe = dendro_arbres.id_pe")
View(DENDRO_ARBRES_SA)
names(DENDRO_ARBRES_SA)

# Convert column names to uppercase
colnames(DENDRO_ARBRES_SA) <- toupper(colnames(DENDRO_ARBRES_SA))
View(DENDRO_ARBRES_SA)

#removing a duplicate column from a database
DENDRO_ARBRES_SA <- DENDRO_ARBRES_SA[-6]

# saving a dataframe from R into SQL database through MySQL_invq connection
sqlSave(MySQL_invq, dat = DENDRO_ARBRES_SA, tablename = "DENDRO_ARBRES_SA", rownames = FALSE, append = TRUE)

names(DENDRO_ARBRES_SA)
names(PEE_ORI_SOND)
SA_DA_POS <- sqlQuery(MySQL_invq, "select * from pee_ori_sond inner join dendro_arbres_sa on pee_ori_sond.id_pe = dendro_arbres_sa.id_pe;")
View(SA_DA_POS)
nrow(SA_DA_POS)
names(SA_DA_POS)

#removing a duplicate column from a database
SA_DA_POS <- SA_DA_POS[, -c(1, 2, 3, 4, 24)]


#saving a dataframe from R into SQL database through MySQL_invq connection
sqlSave(MySQL_invq, dat = SA_DA_POS, tablename = "SA_DA_POS", rownames = FALSE, append = TRUE)

#joining SA_DA_POS and pee_etage_ori_sond
data_1<- sqlQuery(MySQL_invq, "select * from pee_etage_ori_sond as t1 inner join sa_da_pos as t2 on t1.id_pe = t2.id_pe;")
View(data_1)
names(data_1)

#removing a duplicate column from a database
data_1 <- data_1[, -c(1, 2, 3, 4, 12)]

#saving a dataframe from R into SQL database through MySQL_invq connection
sqlSave(MySQL_invq, dat = data_1, tablename = "data_1", rownames = FALSE, append = TRUE)

View(data_1)
names(data_1)


data_2 <- sqlQuery(MySQL_invq, "SELECT objectid, id_pe, id_pe_mes, id_arbre, id_arb_mes, co_ter, latitude, longitude, GEOCODE, dern_sond, essence, dhp, cl_qual, ensoleil,
       etage_arb, tige_ha, st_tige, st_ha, DENSITE, ETAGE, HAUTEUR, ETA_ESS_PC, origine, an_origine, perturb, an_perturb, type_couv, cl_dens, cl_haut, cl_age
FROM data_1;")
View(data_2)



# Load the required package
library(dplyr)
names(data_2)
#Changing the missing species with other species (AES)
data_2$essence[is.na(data_2$essence)] <- AES
View(data_2)
data_2[is.na(data_2)] <- 0
# Group by ID and calculate total basal area for each group
grouped_data <- data_2 %>%
  group_by(id_pe) %>%
  summarise(Total_st_ha = sum(st_ha))
# Merge the total basal area back to the original data
data_2 <- left_join(data_2, grouped_data, by = "id_pe")

# Calculate basal percentage for each tree
data_2$st_pc <- data_2$st_ha / data_2$Total_st_ha * 100

# Print the result
View(data_2)

#grouping the species and adding its basal area percentage

grouped_St_pc <- data_2 %>%
  group_by(essence,id_pe) %>%
  summarise(Gr_st_pc = sum(st_pc))
View(grouped_St_pc)
# merge teh grouped_st_pc back to original data
data_2 <- left_join(data_2, grouped_St_pc, by = "id_pe")

# View the result
View(data_2)


