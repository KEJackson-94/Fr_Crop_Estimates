#library(readxl) # for reading excel files
library(stringr) #str_remove_all()
library(tidyr) # for gather()
library(data.table) #rbindlist() & setnames() & setDT
library(ggplot2)
library(countrycode) # assign/match country iso codes and names
library(ggnewscale) #Any color geom that appears after invoking new_scale_color will get its own scale. https://stackoverflow.com/questions/56306782/how-do-i-add-a-legend-to-identify-vertical-lines-in-ggplot
#library(dplyr) # might not be needed given data.table use (used in Ludemann section)

getwd() # check working directory

####################################################################
#### Organize Cropland Fraction Estimates from Zhang et al 2021 ####
####################################################################

files <- list.files(paste0(getwd(),"/data/Tan_et_al")) 
remove <- c('_FertilizerKgN_21-Jul-2022.csv')
studies_lst <- unique(str_remove_all(files, paste(remove, collapse = "|")))
studies_lst2 <- studies_lst[-which(studies_lst %in% c('FAO'))] # remove Nishina and FAO fronm numerator
FAO <- read.csv(paste0(getwd(),"/data/Tan_et_al/FAO_FertilizerKgN_21-Jul-2022.csv")) # FAO fertilizer consumption data as denominator

df.list <- c()
x = 1
for (study in studies_lst){
  file_nm <- paste0(getwd(),"/data/Tan_et_al/",study,'_FertilizerKgN_21-Jul-2022.csv')
  df1 <- read.csv(file_nm)
  df <- cbind(df1[1],round(df1[-1]/FAO[-1], digits = 6)) # divide study values by FAO values for an estimate of cropland fraction
  names(df)[-1] <- substring(names(df)[-1],2)
  #gather data from columns 2 and 3
  df <- gather(df, key="year", value="Cropland Fraction", 2:ncol(df))
  df$source <- study
  file_new <- paste0("C:/FAO/CroplandFraction/", study,'_CrpFrctn.csv')
  print(file_new)
  df.list[[x]]<-df
  x = x+1
}
Tan = do.call(rbind, df.list)
Tan <- na.omit(Tan)
# Made aware of this packagevia https://github.com/ludemannc/FUBC_1_to_9_2022.git 
# Add ISO3_code
Tan$ISO3_code <- countrycode(Tan$Country, origin = 'country.name', destination = 'iso3c', nomatch = NA)

#### Let's look at models from Tan et al that overrestimate cropland fraction
Tan_subset <- Tan[which(Tan$`Cropland Fraction`>1),]
length(unique(Tan_subset$source))
ggplot(data.frame(Tan_subset), aes(x=source)) +
  geom_bar() + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs (title='Counts of Synthetic Fertilizer Estimates Being Greater Than FAO Estimates by Source')

##########################################################
#### Organize Cropland Fraction Estimates from Rasmus ####
##########################################################

Rasmus <- read.csv(paste0(getwd(),"/data/Rasmus_et_al/synthetic_fertilizer.csv"))
Rasmus_Q <- Rasmus[which(Rasmus$Symbol=='Q'),] # Total quantity of synthetic N fertilizer applied
Rasmus_Q$Index1 <- paste(Rasmus_Q$Region.name, Rasmus_Q$Year) # make index
Rasmus_Q_C <- Rasmus[which(Rasmus$Symbol=='Q_C'),] # Quantity applied on cropland
Rasmus_Q_C$Index2 <- paste(Rasmus_Q_C$Region.name, Rasmus_Q_C$Year) # make index
Rasmus <- merge(Rasmus_Q, Rasmus_Q_C, by.x = 'Index1', by.y = 'Index2') # Merge by index
Rasmus$CroplandFraction <- Rasmus$Value.y/Rasmus$Value.x
Rasmus <- subset(Rasmus, select = c(Region.name.x, Year.x, CroplandFraction))
setnames(Rasmus, old = c('Region.name.x', 'Year.x', 'CroplandFraction'), new = c('Country', 'year', 'Cropland Fraction'))
Rasmus$source <- 'Rasmus'
Rasmus$ISO3_code <- countrycode(Rasmus$Country, origin = 'country.name', destination = 'iso3c', nomatch = NA)
unique(Rasmus[which(is.na(Rasmus$ISO3_code)),]$Country) # excluding "Belgium and Luxembourg", "Former Czechoslovakia", Former EU28 except CY and MT" from analysis           
Rasmus <- na.omit(Rasmus)
df_fnl <- rbind(Rasmus, Tan)

setdiff(unique(Rasmus$ISO3_code),unique(Tan$ISO3_code))# included in Rasmus but excluded from Zhang & FAO
setdiff(unique(Tan$ISO3_code),unique(Rasmus$ISO3_code))# included in Tan but excluded from Rasmus

############################################################
#### Organize Cropland Fraction Estimates from Ludemann ####
############################################################

### upload Ludemann et al data
## as described in metadata: 
## "(includes fertilizer use by crop data from the 8 previously published reports 
## (FUBC 1 to 8), as well as the fertilizer use data from the latest (9th) survey 
## (FUBC 9) carried out by the International Fertilizer Association. It is important 
## to note that the FUBC 9 data have been aggregated by crop categories that align 
## to those used in the previous survey (FUBC 8))."

# We only consider the two most recent data releases because they distinguish grassland and other
Ludemann <- read.csv('data/LUDEMANN_FUBC_DATA_2022//FUBC_1_to_9_data.csv')

##################
#### Report 9 ####
##################

Ludemann_9 <- Ludemann[which(Ludemann$FUBC_report_number == 9),] # report # 9 refers to https://www.nature.com/articles/s41597-022-01592-z.pdf
unique(Ludemann_9$Crop)
unique(Ludemann_9$Year)
unique(Ludemann_9$ISO3_code)
Crop_exclude <- c("Residual",'Grassland')
Ludemann_9$Index <- paste(Ludemann_9$Year,Ludemann_9$FUBC_report_number, Ludemann_9$ISO3_code) # create unique index by country/year/report-year
Indices <- unique(Ludemann_9$Index)
`%!in%` <- Negate(`%in%`) # https://r-lang.com/not-in-r/
df_lst <- c()
row_ttl <- 0
x=1
for (indx in Indices) {
  df <- Ludemann_9[which(Ludemann_9$Index==indx),]
  df$Total_N <- sum(df$N_k_t)
  Total_N <- sum(df$N_k_t)
  Crop_N <- 0
  for (i in 1:nrow(df)){
    if(df$Crop[i] %!in% Crop_exclude){
      Crop_N <- Crop_N + df$N_k_t[i]
    }
  }
  df$N_cropland <- Crop_N
  df$`Cropland Fraction` <- Crop_N/Total_N
  df_lst[[x]] <- df
  x=x+1
}
Ludemann_9 = do.call(rbind, df_lst)
Ludemann_9$source <- 'Ludemann_Report9'
Ludemann_9 <- Ludemann_9[,c("Country","ISO3_code","Year","Cropland Fraction", "source")]
names(Ludemann_9)[names(Ludemann_9) == 'Year'] <- 'year' # rename Year for rbind
Ludemann_9 <- Ludemann_9[!duplicated(Ludemann_9), ]# remove duplicate columns

##################
#### Report 8 ####
##################

Ludemann_8 <- Ludemann[which(Ludemann$FUBC_report_number == 8),] # report # 8 refers to https://www.ifastat.org/plant-nutrition.
unique(Ludemann_8$Crop) # is 'Other' suppose to be 'residual'?
unique(Ludemann_8$Year)
unique(Ludemann_8$ISO3_code)
Crop_exclude <- c("Grassland", 'Other crops')
Ludemann_8$Index <- paste(Ludemann_8$Year,Ludemann_8$FUBC_report_number, Ludemann_8$ISO3_code) # create unique index by country/year/reportyear
Indices <- unique(Ludemann_8$Index)
`%!in%` <- Negate(`%in%`) # https://r-lang.com/not-in-r/
df_lst <- c()
row_ttl <- 0
x=1
for (indx in Indices) {
  df <- Ludemann_8[which(Ludemann_8$Index==indx),]
  df$Total_N <- sum(df$N_k_t)
  Total_N <- sum(df$N_k_t)
  Crop_N <- 0
  for (i in 1:nrow(df)){
    if(df$Crop[i] %!in% Crop_exclude){
      Crop_N <- Crop_N + df$N_k_t[i]
    }
  }
  df$N_cropland <- Crop_N
  df$`Cropland Fraction` <- Crop_N/Total_N
  df_lst[[x]] <- df
  x=x+1
}
Ludemann_8 = do.call(rbind, df_lst)
Ludemann_8$source <- 'Ludemann_Report8'
Ludemann_8 <- Ludemann_8[,c("Country","ISO3_code","Year","Cropland Fraction", "source")]
names(Ludemann_8)[names(Ludemann_8) == 'Year'] <- 'year' # rename Year for rbind
Ludemann_8 <- Ludemann_8[!duplicated(Ludemann_8), ]# remove duplicate columns

df_fnl <- rbind(df_fnl, Ludemann_8, Ludemann_9)

###############################################################
#### Organize Cropland Fraction Estimates from updated FAO ####
###############################################################
# This ultimately didn't go anywhere... but it proved to myself that FAO doesn't distinguish between cropland area and total N use
# I'm keeping the remainder of this code for posterity (it did identify reporting errors in FAO's database*)
# This section doesn't manually include updated FAO fraction estimates for New Zealand and Ireland

# population <- read.csv('data//FAO//FAOSTAT_data_en_9-18-2022_population.csv')
# population$Value <- population$Value*1000 # value originally in '1000 persons'
# population <- population[which(population$Flag == "X"),] # only consider Flag "X" - "International reliable sources"
# #population$Area[population$Area=='Serbia and Montenegro']<-'Montenegro' # reclassify 'Serbia and Montenegro' as 'Montenegro'
# population$iso <- countrycode(population$Area , origin = 'country.name', destination = 'iso3c', nomatch = NA)
# population <- population[,c("iso","Year","Value")]
# names(population)[names(population) == "Value"] <- 'population'
# population <- na.omit(population)
# 
# N_use <- read.csv('data//FAO//FAOSTAT_data_en_9-18-2022_N.csv')
# #N_use$Area[N_use$Area=='Serbia and Montenegro']<-'Montenegro' # reclassify 'Serbia and Montenegro' as 'Montenegro'
# N_use$iso <- countrycode(N_use$Area , origin = 'country.name', destination = 'iso3c', nomatch = NA)
# N_use_total <- N_use[which(N_use$Flag == "E" & N_use$Element == "Use per capita"),] # only consider Flag "E" - "Estimated value"
# N_use_total <- N_use_total[,c('iso',"Year","Value")] # multiply by population for total N
# N_use_total <- na.omit(N_use_total)
# names(N_use_total)[names(N_use_total) == "Value"] <- 'Total_N_per_capita'
# FAO_updt <- merge(x=N_use_total,y=population, by.x=c('iso',"Year"), by.y=c('iso',"Year"))
# 
# N_use_cropland <- N_use[which(N_use$Flag == "E" & N_use$Element == "Use per area of cropland"),] # only consider Flag "E" - "Estimated value"
# N_use_cropland <- N_use_cropland[,c('iso',"Year","Value")] # multiply by cropland area for total use on cropland
# N_use_cropland <- na.omit(N_use_cropland)
# names(N_use_cropland)[names(N_use_cropland) == "Value"] <- 'N_Cropland_per_ha'
# FAO_updt <- merge(x=FAO_updt,y=N_use_cropland, by.x=c('iso',"Year"), by.y=c('iso',"Year"))
# 
# cropland_area <- read.csv('data//FAO//FAOSTAT_data_en_9-18-2022_cropland.csv')
# #cropland_area$Area[cropland_area$Area=='Serbia and Montenegro']<-'Montenegro' # reclassify 'Serbia and Montenegro' as 'Montenegro'
# cropland_area$iso <- countrycode(cropland_area$Area , origin = 'country.name', destination = 'iso3c', nomatch = NA)
# cropland_area <- cropland_area[,c("iso", "Year","Value","Flag")]
# cropland_area <- na.omit(cropland_area)
# # Flags don't appear to overlap temporally
# cropland_area$Value <- cropland_area$Value*1000 # value originally in '1000 ha'
# names(cropland_area)[names(cropland_area) == "Value"] <- 'crop_area'
# cropland_area$iso_yr <- paste(cropland_area$iso,cropland_area$Year)
# setDT(cropland_area)[, crop_area.avg := mean(crop_area), by = iso_yr]
# cropland_area <- cropland_area[!duplicated(cropland_area[ , c("iso_yr")]),]
# cropland_area <- cropland_area[,c("iso", "Year","crop_area.avg")]
# FAO_updt <- merge(x=FAO_updt,y=cropland_area, by.x=c('iso',"Year"), by.y=c('iso',"Year"))
# 
# # Caclcuate Cropland Fraction
# FAO_updt$Total_N <- FAO_updt$Total_N_per_capita*FAO_updt$population
# FAO_updt$N_Cropland <- FAO_updt$crop_area.avg*FAO_updt$N_Cropland_per_ha
# # use cropland area from 2014 for calculation of CF for MNE
# FAO_updt[which(FAO_updt$iso=='MNE' & FAO_updt$Year >= 2006 & FAO_updt$Year <= 2014),]$N_Cropland <-
#   FAO_updt[which(FAO_updt$iso =='MNE' &
#                    FAO_updt$Year== 2014),]$'crop_area.avg'*
#   FAO_updt[which(FAO_updt$iso =='MNE' &
#                    FAO_updt$Year<=2014 & FAO_updt$Year>=2006),]$N_Cropland_per_ha
# FAO_updt$`Cropland.Fraction` <- FAO_updt$N_Cropland/FAO_updt$Total_N
# # use cropland area from 2004 for calculation of CF for NLD
# FAO_updt[which(FAO_updt$iso=='NLD' & FAO_updt$Year >= 1961 & FAO_updt$Year <= 2004),]$N_Cropland <-
#   FAO_updt[which(FAO_updt$iso =='NLD' &
#                    FAO_updt$Year== 2004),]$'crop_area.avg'*
#   FAO_updt[which(FAO_updt$iso =='NLD' &
#                    FAO_updt$Year<=2004 & FAO_updt$Year>=1961),]$N_Cropland_per_ha
# FAO_updt$`Cropland.Fraction` <- FAO_updt$N_Cropland/FAO_updt$Total_N
# # test it out
# blah <- FAO_updt[which(FAO_updt$iso=='NZL'),] # sort out issues with CHINA. not necessary right now
# plot(blah$Year, blah$Cropland.Fraction, ylim = c(0, 2))

FAO_updt <- setNames(data.frame(matrix(ncol = ncol(df_fnl), nrow = 0)), colnames(df_fnl)) # make a df with updated FAO cropland Fraction Estimates
FAO_updt[1,] <- c('New Zealand',2017, 0.08, 'FAO updated', 'NZL') # FAO update on New Zealand
FAO_updt[2,] <- c('Ireland', 2018, 0.23, 'FAO updated', 'IRL') # FAO update on Ireland
df_fnl <- rbind(df_fnl, FAO_updt)

#############################################################
#### After Joining Data, let's clean up and write the df ####
#############################################################

# remove then add country name category using iso3_codes for consistency
df_fnl$year <- as.integer(df_fnl$year)
df_fnl <- df_fnl[which(df_fnl$source !="Nishinawithdoublecropping" & df_fnl$source != "Nishinawithoutdoublecropping" & df_fnl$source != "Zhang2015" & df_fnl$source != "Bodirskywithforage"),]
drops <- c("Country")
df_fnl <- df_fnl[ , !(names(df_fnl) %in% drops)]
df_fnl$Country <- countrycode(df_fnl$ISO3_code,origin = 'iso3c', destination =  'country.name') # while iso codes match, naming conventions for countries might differ between datasets
write.csv(df_fnl,"CroplandFractions.csv")

################################################################################
