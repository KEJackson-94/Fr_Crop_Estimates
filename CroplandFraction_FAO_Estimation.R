library(stringr) #str_remove_all()
library(tidyr) # for gather()
library(data.table) #rbindlist() & setnames() & setDT
library(countrycode) # assign/match country iso codes and names
library(dplyr) # might not be needed given data.table use (used in Ludemann section)
library(imputeTS) # na_interpolation https://cran.r-project.org/web/packages/imputeTS/imputeTS.pdf
library(zoo) # zoo() creates ts object for linear interpolation
library(matrixStats) # rowSDs()
library(ggplot2)

getwd() # check working directory
`%ni%` <- Negate(`%in%`) # this negate of %in% is used in loop

################################################################################
#### Produce FR_Crop time series' for each iso using data from Zhang (2021) ####
################################################################################

#-------------------------------------------------------------------------------
# (Step 0_1) pull Zhang et al., 2021 model data and prepare for screening steps. 

files <- list.files(paste0(getwd(),"/data/Tan_et_al")) 
remove <- c('_FertilizerKgN_21-Jul-2022.csv')
studies_lst <- unique(str_remove_all(files, paste(remove, collapse = "|")))
studies_lst2 <- studies_lst[-which(studies_lst %in% c('FAO'))] # remove FAO from numerator
FAO <- read.csv(paste0(getwd(),"/data/Tan_et_al/FAO_FertilizerKgN_21-Jul-2022.csv")) # FAO fertilizer consumption data as denominator
df.list <- c()
x = 1
for (study in studies_lst){
  file_nm <- paste0(getwd(),"/data/Tan_et_al/",study,'_FertilizerKgN_21-Jul-2022.csv')
  df1 <- read.csv(file_nm)
  df <- cbind(df1[1],round(df1[-1]/FAO[-1], digits = 6)) # divide study values by FAO values for an estimate of cropland fraction
  names(df)[-1] <- substring(names(df)[-1],2)
  df <- gather(df, key="year", value="Cropland Fraction", 2:ncol(df)) #gather data from columns 2 and 3
  df$source <- study
  file_new <- paste0("C:/FAO/CroplandFraction/", study,'_CrpFrctn.csv')
  print(file_new)
  df.list[[x]]<-df
  x = x+1
}
Tan = do.call(rbind, df.list)
Tan <- na.omit(Tan)
Tan$ISO3_code <- countrycode(Tan$Country, origin = 'country.name', destination = 'iso3c', nomatch = NA) # Add ISO3_code
Tan$year <- as.integer(Tan$year)
Tan <- Tan[which(Tan$source !="FAO" & Tan$source !="Nishinawithdoublecropping" & Tan$source != "Nishinawithoutdoublecropping" & Tan$source != "Zhang2015" & Tan$source != "Bodirskywithforage"),]
drops <- c("Country")
Tan <- Tan[ , !(names(Tan) %in% drops)] # remove then add country name category using iso3_codes for consistency
Tan$Country <- countrycode(Tan$ISO3_code,origin = 'iso3c', destination =  'country.name') # while iso codes match, naming conventions for countries might differ between datasets
Zhang_incmplt <- c('CAN','CHL', 'JPN', 'MAR', 'URY', 'ZAF', 'CHE') # list of countries that need post 2011 values removed
Tan[which(Tan$ISO3_code %in% Zhang_incmplt & Tan$year>2011 & Tan$source=="ZhangReorganized"),]$`Cropland Fraction` <- NA # set post 2011 values in "ZhangReorganized" to NA for identified countries
write.csv(Tan,"results//CF_Zhang_Raw_0930.csv")

####################################################################################
#### Produce FR_Crop time series' for each iso using data from Einarsson (2022) ####
####################################################################################

#-------------------------------------------------------------------------------
# (Step 0_2) pull Einarsson, 2022 model data and prepare for screening steps. 

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

################################################################################
#### Combine FR_Crop time series' and filter data and fill gaps accordingly ####
################################################################################

df_cmbnd_raw <- rbind(Rasmus, Tan)
drops <- c("Country")
df_cmbnd_raw <- df_cmbnd_raw[ , !(names(df_cmbnd_raw) %in% drops)]
df_cmbnd_raw$Country <- countrycode(df_cmbnd_raw$ISO3_code,origin = 'iso3c', destination =  'country.name')

#-------------------------------------------------------------------------------
# (Step 1) data screening, for each country, identify data points >100%: 

iso_lst <- unique(df_cmbnd_raw$ISO3_code)
#iso_lst <- c('CHL') # test on chile
fnl_lst <- list()
xx <- 1
for (i in 1:length(iso_lst)){
  #print(iso_lst[i])
  
  #---------------------------------------------------------------------------
  # determine which sources have >30% data coverage of estimates >100% 
  
  sub <- df_cmbnd_raw[which(df_cmbnd_raw$ISO3_code== iso_lst[i]),]
  remove_lst <- c()
  x <- 1
  for (sub_source in unique(sub$source)) {
    if (nrow(sub[which(sub$source == sub_source & sub$`Cropland Fraction`> 1),])/nrow(sub[which(sub$source == sub_source),])>0.3){ # appears Na are already omitted
      remove_lst[x] <- sub_source # for a given source, if the % of CF values >1 is >33% of the data set, add it to this list
      x <- x + 1}}
  #print (remove_lst) # this list should contain all sources for a given ISO that do not meet condition 2
  
  #-------------------------------------------------------------------------------
  # (Step 2) If data points account for >30% of the data, remove time series
  sub <- sub[which(sub$source %ni% remove_lst),] # remove series in remove_lst
  
  #-------------------------------------------------------------------------------
  # (Step 2_2) If data points account for >0% & <=30%, smooth the time series. 
  
  smooth_lst <- c()
  x <- 1
  for (sub_source in unique(sub$source)) {
    if (nrow(sub[which(sub$source == sub_source & sub$`Cropland Fraction`>1),])/nrow(sub[which(sub$source == sub_source),])>0){ # appears Na are already omitted
      smooth_lst[x] <- sub_source # for a given source, if the % of CF values > 1 is >0% of the data set, add it to this list
      x <- x + 1}}
  #print (smooth_lst) # this list should contain all sources for a given ISO that do not meet condition 2
  
  fitted_lst <- list()
  x <- 1
  for (smooth_source in smooth_lst){
    sub_smooth <- sub[which(sub$source == smooth_source),]
    sub_smooth$new.val <- loess(`Cropland Fraction`~`year`, data = sub_smooth)$fitted # let's first smooth available data
    
    #-----------------------------------------------------------------------------
    # (Step 2_3) After smoothing, if there are still data points w/ CF >100%, remove
    is.na(sub_smooth$`new.val`) <- sub_smooth$`new.val` > 1 # set fitted >100% as NA
    is.na(sub_smooth$`new.val`) <- sub_smooth$`new.val` < 0 # set fitted <0% as NA
    
    #-----------------------------------------------------------------------------
    # (Step 2_4 & Step 3) Fill gaps between data points with the average of adjacent data
    
    df <- data.frame(matrix(ncol = 4, nrow = length(1961:2020))) # create dataframe with full years and merge with df_smooth for gap filling
    colnames(df) <- c('year', 'source', 'ISO3_code', 'Country')
    df$year <- c(1961:2020)
    df$source <- unique(sub_smooth$source)
    df$ISO3_code <- unique(sub_smooth$ISO3_code)
    df$Country <- unique(sub_smooth$Country)
    sub_smooth <- merge(x = sub_smooth, y = df, by = c('year', 'source', 'ISO3_code', 'Country'), all.x = TRUE, all.y = TRUE)
    
    time_series <- zoo(sub_smooth$new.val,sub_smooth$year) # create time series object of newly fitted data
    time_series <- na_interpolation(time_series, option = "linear") # interpolate between missing points if they exist
    time_series <- fortify.zoo(time_series)
    sub_smooth$new.val <- time_series$time_series # reset new.val as interpolated values
    if (length(na.omit(sub_smooth$`Cropland Fraction`)) == 1){ # if model only has one time stamp, apply it across entire time series
      sub_smooth$`Cropland Fraction` <- unique(sub_smooth$`Cropland Fraction`)
    }
    fitted_lst[[x]] <- sub_smooth
    x=x+1
  }
  sub_smoothed = do.call(rbind, fitted_lst)
  
  #-----------------------------------------------------------------------------
  # (Step 2_5 & Step 3) Combine this data with any models that didn't need to be smoothed but apply same gap filling techniques
  
  if (length(unique(sub$source)) > length(smooth_lst)){ # if there are any models w/ no data >100%
    #print(iso_lst[i])
    unsmooth_source_lst <- unique(sub[which(sub$source %ni% smooth_lst),]$source) # remove series in smooth_lst
    
    unsmooth_fitted_lst <- list()
    x <- 1
    for (unsmooth_source in unsmooth_source_lst){
      sub_unsmooth <- sub[which(sub$source == unsmooth_source),]
      
      df <- data.frame(matrix(ncol = 4, nrow = length(1961:2020))) # create data frame with full years and merge with df_smooth for gap filling
      colnames(df) <- c('year', 'source', 'ISO3_code', 'Country')
      df$year <- c(1961:2020)
      df$source <- unique(sub_unsmooth$source)
      df$ISO3_code <- unique(sub_unsmooth$ISO3_code)
      df$Country <- unique(sub_unsmooth$Country)
      sub_unsmooth <- merge(x = sub_unsmooth, y = df, by = c('year', 'source', 'ISO3_code', 'Country'), all.x = TRUE, all.y = TRUE)
      sub_unsmooth$`new.val`  <- sub_unsmooth$`Cropland Fraction`
      
      if (length(na.omit(sub_unsmooth$`new.val`)) == 1){ # if model only has one time stamp, apply it across entire time series
        sub_unsmooth$`new.val` <- na.omit(unique(sub_unsmooth$`Cropland Fraction`))
      }
      else{
        
        time_series <- zoo(sub_unsmooth$new.val,sub_unsmooth$year) # create time series object of newly fitted data
        time_series <- na_interpolation(time_series, option = "linear") # interpolate between missing points if they exist
        time_series <- fortify.zoo(time_series)
        sub_unsmooth$new.val <- time_series$time_series # reset new.val as interpolated values
      }
      
      unsmooth_fitted_lst[[x]] <- sub_unsmooth
      x=x+1
    }
    sub_unsmoothed = do.call(rbind, unsmooth_fitted_lst)
  }
  sub_cmbnd <- rbind(sub_unsmoothed, sub_smoothed)
  
  drops <- c("Cropland Fraction")
  sub_cmbnd <- sub_cmbnd[ , !(names(sub_cmbnd) %in% drops)]
  sub_cmbnd <- reshape(sub_cmbnd, idvar = c("year","ISO3_code", "Country" ), timevar = "source", direction = "wide")
  old_names <- colnames(sub_cmbnd)[4:ncol(sub_cmbnd)]
  new_names <- substring(old_names, 9)
  colnames(sub_cmbnd)[which(colnames(sub_cmbnd) %in% old_names)] <- new_names
  fnl_lst[[xx]] <- sub_cmbnd
  xx=xx+1
}
df_fnl = do.call(bind_rows, fnl_lst)
df_long <- melt(setDT(df_fnl), id.vars = c("year","ISO3_code","Country"), variable.name = "source") # make filtered/ gap filled df in long format

#-------------------------------------------------------------------------------
# (Step 4) Produce full time series using models for chosen by experts for select countries

Rasmus <- c('AUT', 'DEU', 'FRA', 'GBR', 'IRL', 'LUX', 'POL', 'SVN', 'NLD') # include NLD
Lassaletta <- c('AUS', 'FIN')
Zhang_cmplt <- c('CAN', 'CHL', 'JPN', 'MAR', 'URY', 'ZAF', 'USA', 'CHE') # include CHE
IMAGE <- c('BRA') # do not include NZL
FAO_updt <- c('NZL')
Other <- c('530', '830')

M49 <- read.csv("data//Data_Request_Countries.csv")
M49$ISO3_code <- countrycode(M49$Country,origin = 'country.name', destination = 'iso3c', nomatch = NA) # "Netherlands Antilles (former)" & "Channel Islands" 
M49[which(M49$Country=='Netherlands Antilles (former)'),]$ISO3_code <- '530'
M49[which(M49$Country=='Channel Islands'),]$ISO3_code <- '830'
M49_iso_lst <- na.omit(unique(M49$ISO3_code)) 
paste(nrow(M49), nrow(na.omit(M49)))
setdiff(M49$Country, na.omit(M49)$Country)

#M49_iso_lst <- c(M49_iso_lst, '530', '830')  # add M49 530 and 830 to the iso list
#M49_iso_lst <- c('CHL') # test with Chile

new_list <- list()
x <- 1
for (iso in M49_iso_lst){
  if (iso %in% Rasmus){
    #print('R')
    df_new <- df_long[which(df_long$ISO3_code==iso & df_long$source=='Rasmus'),]
    new_list[[x]] <- df_new
  }
  else if (iso %in% Lassaletta) {
    #print('L')
    df_new <- df_long[which(df_long$ISO3_code==iso & df_long$source=="Lassaletta"),]
    new_list[[x]] <- df_new
  }
  else if (iso %in% IMAGE) {
    #print('I')
    df_new <- df_long[which(df_long$ISO3_code==iso & df_long$source=="IMAGE"),]
    new_list[[x]] <- df_new
  }
  else if (iso %in% Zhang_cmplt) {
    #print('Z')
    df_new <- df_long[which(df_long$ISO3_code==iso & df_long$source=="ZhangReorganized"),]
    new_list[[x]] <- df_new
  }
  else if (iso %in% FAO_updt){ # create a new df and populate it with CF estimates of 8%
    #print('F')
    df_new <- data.frame(matrix(ncol = ncol(df_long), nrow = length(1961:2020))) # create df w/ full years and populate as default CF
    colnames(df_new) <- colnames(df_long)
    df_new$year <- c(1961:2020)
    df_new$ISO3_code <- 'NZL'
    df_new$Country <- countrycode('NZL',origin = 'iso3c', destination =  'country.name')
    df_new$value <- 0.08
    df_new$source <- 'FAO'
    new_list[[x]] <- df_new
  }
  else if (iso == '830'){ # unique M49 code for "Channel Islands"
    #print('blah')
    df_new <- data.frame(matrix(ncol = ncol(df_long), nrow = length(1961:2020))) # create df w/ full years and populate as default CF
    colnames(df_new) <- colnames(df_long)
    df_new$year <- c(1961:2020)
    df_new$ISO3_code <- iso
    df_new$Country <- "Channel Islands"
    df_new$value <- 1
    df_new$source <- 'default'
    new_list[[x]] <- df_new
  }
  else if (iso == '530'){ # unique M49 code for "Netherlands Antilles (former)"
    #print('blah2')
    df_new <- data.frame(matrix(ncol = ncol(df_long), nrow = length(1961:2020))) # create df w/ full years and populate as default CF
    colnames(df_new) <- colnames(df_long)
    df_new$year <- c(1961:2020)
    df_new$ISO3_code <- iso
    df_new$Country <- "Netherlands Antilles (former)"
    df_new$value <- 1
    df_new$source <- 'default'
    new_list[[x]] <- df_new
  }
  else{ # create a new df and populate it with CF estimates of 1
    #print('E')
    df_new <- data.frame(matrix(ncol = ncol(df_long), nrow = length(1961:2020))) # create df w/ full years and populate as default CF
    colnames(df_new) <- colnames(df_long)
    df_new$year <- c(1961:2020)
    df_new$ISO3_code <- iso
    df_new$Country <- countrycode(df_new$ISO3_code,origin = 'iso3c', destination =  'country.name')
    df_new$value <- 1
    df_new$source <- 'default'
    new_list[[x]] <- df_new
  }
  x <- x +1
}
df_new = do.call(bind_rows, new_list) # I have no clue why Chile is a duplicated df
df_new <- merge(x = df_new, y = M49, by = 'ISO3_code', all.y = TRUE) # match M49 codes to updated df
drops <- c("Country.x")
df_new <- df_new[ , !(names(df_new) %in% drops)] # drop unnecessary columns
df_new <- df_new[,c(5,6,1,2,4,3)]
df_new <- df_new[!duplicated(df_new), ]# remove duplicate rows
names(df_new )[names(df_new ) == 'Country.y'] <- 'Country'
write.csv(df_new,"results//N_Est_0930.csv")

# make a quick plot of non-default values
df_image <- df_new[which(df_new$source != 'default'),]
df_image[which(df_image$ISO3_code=='GBR'),]$Country <- 'United Kingdom'
#names(df_image)[names(df_image) == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
p <-ggplot(df_image,aes(x = year, y = `value`, color = `Country`)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Year",y = "Fraction (%)", title = 'N Fraction Estimates For Major Crops By Country') +
  coord_cartesian(xlim =c(1961,2020), ylim = c(0,NA)) + #cartesian limit doesn't exclude data like ylim()... consider adding ylim
  geom_hline(aes(yintercept= 1, linetype = "100% N Fraction to Major Crop Types"), colour= 'black') +
  scale_linetype_manual(name =  "", values = c(2), 
                        guide = guide_legend(override.aes = list(color = c("black")))) +
  scale_color_discrete(name = "Countries") +
  theme_classic() # make a quick plot of non-default CF estimates of final series
p
ggsave(paste0("results//",'N_Est_img_0930',".png"),p,width=8.5, height=11)

#-----------------------------------------------------------------------------
# (Step 4) take avg & sdev for all data (or med, min-max)
#CF_Zhang <- df_fnl 
CF_Zhang <- melt(setDT(df_fnl), id.vars = c("year","ISO3_code","Country"), variable.name = "source") # make long to remove duplicated rows
CF_Zhang <- CF_Zhang[!duplicated(CF_Zhang), ]# remove duplicate rows
CF_Zhang <- na.omit(CF_Zhang)
CF_Zhang <- reshape(CF_Zhang, idvar = c("year","ISO3_code","Country"), timevar = "source", direction = "wide")# return to wide format
names(CF_Zhang)[4:ncol(CF_Zhang)] <- substring(names(CF_Zhang), 7)[4:ncol(CF_Zhang)] # rename column names
CF_Zhang <- subset(CF_Zhang, select = -c(4)) # remove Rasmus from this stage

CF_Zhang$mean <- rowMeans(CF_Zhang[,4:ncol(CF_Zhang)], na.rm = TRUE)
CF_Zhang$SD <- rowSds(as.matrix(CF_Zhang[,4:(ncol(CF_Zhang)-1)]), na.rm = TRUE)
CF_Zhang$med <- rowMedians(as.matrix(CF_Zhang[,4:(ncol(CF_Zhang)-2)]), na.rm = TRUE)
CF_Zhang$min <- rowMins(as.matrix(CF_Zhang[,4:(ncol(CF_Zhang)-3)]), na.rm = TRUE)
CF_Zhang$max <- rowMaxs(as.matrix(CF_Zhang[,4:(ncol(CF_Zhang)-4)]), na.rm = TRUE)

write.csv(CF_Zhang,"results//CF_Zhang_Stage23_0930_wide.csv")

CF_Zhang <- na.omit(melt(setDT(CF_Zhang), id.vars = c("year","ISO3_code","Country", "SD"), variable.name = "source"))
#is.na(CF_Zhang$SD) <- CF_Zhang$source` %in% c("Changetal.","IMAGE", "Bodirskywithoutforage", "LuandTian", "ZhangReorganized","GerberandMueller","Lassaletta")
CF_Zhang[which(CF_Zhang$source != 'mean'),]$SD <- NA
CF_Zhang[which(CF_Zhang$SD == 0),]$SD <- NA
CF_Zhang$U <- CF_Zhang$value + CF_Zhang$SD*2
CF_Zhang$L <- CF_Zhang$value - CF_Zhang$SD*2

write.csv(CF_Zhang,"results//CF_Zhang_Stage23_0930.csv")

# #############################################################################
# #### Produce FR_Crop time series' using data from remaining data sources ####
# #############################################################################
# 
# Other_df <- read.csv("CroplandFractions.csv")
# Other_df <- Other_df[which(Other_df$source== "Ludemann_Report8" | Other_df$source== "Ludemann_Report9" | Other_df$source== "FAO updated" | Other_df$source== "Rasmus"),]
# Other_df <- Other_df[,-(1)] 
# 
# iso_lst <- unique(Other_df$ISO3_code)
# fnl_lst <- list()
# xx <- 1
# for (i in 1:length(iso_lst)){
#   print(iso_lst[i])
#   Other_sub <- Other_df[which(Other_df$ISO3_code==iso_lst[i]),]
#   
#   Other_unsmooth_fitted_lst <- list()
#   x<-1
#   for (Other_source in unique(Other_sub$source)){
#     Other_unsmooth <- Other_sub[which(Other_sub$source == Other_source),]
#     
#     df <- data.frame(matrix(ncol = 4, nrow = length(1961:2020))) # create data frame with full years and merge with df_smooth for gap filling
#     colnames(df) <- c('year', 'source', 'ISO3_code', 'Country')
#     df$year <- c(1961:2020)
#     df$source <- unique(Other_unsmooth$source)
#     df$ISO3_code <- unique(Other_unsmooth$ISO3_code)
#     df$Country <- unique(Other_unsmooth$Country)
#     Other_unsmooth <- merge(x = Other_unsmooth, y = df, by = c('year', 'source', 'ISO3_code', 'Country'), all.x = TRUE, all.y = TRUE)
#     Other_unsmooth$`new.val`<- Other_unsmooth$`Cropland.Fraction`
#     
#     if (length(na.omit(Other_unsmooth$`new.val`)) == 1){ # if model only has one time stamp, apply it across entire time series
#       Other_unsmooth$`new.val` <- na.omit(unique(Other_unsmooth$`Cropland.Fraction`))
#     }
#     else{ # this is only relevant for Rasmus
#       time_series <- zoo(Other_unsmooth$new.val,Other_unsmooth$year) # create time series object of newly fitted data
#       time_series <- na_interpolation(time_series, option = "linear") # interpolate between missing points if they exist
#       time_series <- fortify.zoo(time_series)
#       Other_unsmooth$new.val <- time_series$time_series # reset new.val as interpolated values
#     }
#     Other_unsmooth_fitted_lst[[x]] <- Other_unsmooth
#     x<-x+1
#   }
#   Other_unsmoothed = do.call(rbind, Other_unsmooth_fitted_lst)
#   
#   drops <- c("Cropland.Fraction")
#   Other_unsmoothed <- Other_unsmoothed[ , !(names(Other_unsmoothed) %in% drops)]
#   Other_unsmoothed <- reshape(Other_unsmoothed, idvar = c("year","ISO3_code", "Country" ), timevar = "source", direction = "wide")
#   old_names <- colnames(Other_unsmoothed)[4:ncol(Other_unsmoothed)]
#   new_names <- substring(old_names, 9)
#   colnames(Other_unsmoothed)[which(colnames(Other_unsmoothed) %in% old_names)] <- new_names
#   
#   fnl_lst[[xx]] <- Other_unsmoothed
#   xx=xx+1
# }
# Other_fnl = do.call(bind_rows, fnl_lst)
# 
# ###################################################################################
# #### Join Tan (or df_nl) with Other_fnl & compute statistics across all models ####
# ###################################################################################
# 
# CF_Est <-merge(x = Other_fnl, y = df_fnl, by = c('year', 'ISO3_code', 'Country'), all.x = TRUE, all.y = TRUE)
# 
# #-----------------------------------------------------------------------------
# # (Step 4) take avg & sdev for all data (or med, min-max)
# 
# CF_Est$mean <- rowMeans(CF_Est[,4:ncol(CF_Est)], na.rm = TRUE)
# CF_Est$SD <- rowSds(as.matrix(CF_Est[,4:(ncol(CF_Est)-1)]), na.rm = TRUE)
# CF_Est$med <- rowMedians(as.matrix(CF_Est[,4:(ncol(CF_Est)-2)]), na.rm = TRUE)
# CF_Est$min <- rowMins(as.matrix(CF_Est[,4:(ncol(CF_Est)-3)]), na.rm = TRUE)
# CF_Est$max <- rowMaxs(as.matrix(CF_Est[,4:(ncol(CF_Est)-4)]), na.rm = TRUE)
# 
# CF_Est <- melt(setDT(CF_Est), id.vars = c("year","ISO3_code","Country"), variable.name = "source")
# 
# write.csv(CF_Est,"results//CF_All_Stage23_0925.csv")