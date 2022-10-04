library(ggplot2)
library(dplyr) # for function mutate()
library(forcats) # for fct_other()
library(RColorBrewer)# create color palette()
library(countrycode)

getwd() # confirm your workspace containing 'CroplandFractions.csv'
`%ni%` <- Negate(`%in%`) # this negate of %in% is used in loop

#-------------------------------------------------------------------------------
# Create Image displaying Raw Image Data (for a given ISO) only covered by 
# Einarsson et al., 2022

CF_df <- read.csv("results//Fr_Crop_Collated.csv") 

drops <- c("FAO", "Nishinawithdoublecropping", "Nishinawithoutdoublecropping", "Zhang2015", "Bodirskywithforage", "Ludemann_Report8", "Ludemann_Report9", "Rasmus") # just keep Zhang et al., 2021 raw
Zhang_Raw <- CF_df[which(CF_df$source !="FAO" & CF_df$source !="Nishinawithdoublecropping" & CF_df$source != "Nishinawithoutdoublecropping" & CF_df$source != "Zhang2015" & CF_df$source != "Bodirskywithforage"),]
Zhang_Raw <- CF_df[which(CF_df$source %ni% drops),]
drops <- c("Country")
Zhang_Raw <- Zhang_Raw[ , !(names(Zhang_Raw) %in% drops)] # remove then add country name category using iso3_codes for consistency
Zhang_Raw$Country <- countrycode(Zhang_Raw$ISO3_code,origin = 'iso3c', destination =  'country.name') # while iso codes match, naming conventions for countries might differ between datasets
Zhang_incmplt <- c('CAN','CHL', 'JPN', 'MAR', 'URY', 'ZAF', 'CHE') # list of countries that need post 2011 values removed
Zhang_Raw[which(Zhang_Raw$ISO3_code %in% Zhang_incmplt & Zhang_Raw$year>2011 & Zhang_Raw$source=="ZhangReorganized"),]$`Cropland.Fraction` <- NA # set post 2011 values in "ZhangReorganized" to NA for identified countries

country_lst <- setdiff(CF_df$ISO3_code,Zhang_Raw$ISO3_code) # only cases with Rasmus and no Zhang et al., 2021 models

source_lst <- unique(CF_df$source) # create a unique color palette for each source
n <- length(source_lst)
clr1 <- palette(brewer.pal(n = 8, name = "Set1"))
clr2 <- palette(brewer.pal(n = 8, name = "Dark2"))
col <- c(clr1, clr2)[1:n]
col[8] <- '#FF3300'
SourceColors <- setNames(col, levels(CF_df$source)) #establish color factors for Zhang et al., 2021 sources

CF_df_sub <- subset(CF_df, source == "Ludemann_Report8" | source == "Ludemann_Report9" | source == "FAO updated")
SourceShape <- setNames(c(8,15,17), levels(CF_df_sub$source))  #establish shape factors for Ludemann_Report8 Ludemann_Report9 & FAO_updated

for (ii in 1:length(country_lst)){
  iso <- country_lst[ii]
  country_name <- paste(countrycode(iso,origin = 'iso3c', destination =  'country.name'),', ',iso)
  print(country_name)
  
  #-------------------------------------------------------------------------------
  # Create Image displaying Raw Image Data (for a given ISO) only covered by Einarsson et al., 2022
  df <- CF_df[which(CF_df$ISO3_code==iso),]
  df_point <- subset(df, source == "Ludemann_Report8" | source == "Ludemann_Report9" | source == "FAO updated")
  df_sub <- subset(df, source != "FAO" & source != "Ludemann_Report8" & source != "Ludemann_Report9" & source != "FAO updated")
  p1 <- ggplot(df_sub,aes(x = year, y = `Cropland.Fraction`)) + 
    geom_line(data = df_sub, aes(color = source), lwd = 1.5) +
    geom_point(data = df_sub, aes(color = source), lwd = 1.5, alpha=0.75) +
    geom_point(data = df_point, aes(shape = source), lwd = 3, alpha=1) +
    labs(x = "Year",y = "Fr_crop (%)", colour = "", title = paste(country_name,'(Unfiltered)')) + 
    coord_cartesian(xlim =c(1961,2020)) + #cartesian limit doesn't exclude data like ylim()... consider adding ylim
    geom_hline(aes(yintercept= 1, linetype = "100% Fr_crop"), colour= 'black') +
    scale_linetype_manual(name = "", values = c(1), guide = guide_legend(override.aes = list(color = c("black")))) +
    scale_shape_manual(values = SourceShape, name = "IFA/FAO Sources") +
    theme_classic()
  p1
  #ggsave(paste0("results//figure0930_2//",iso,".png"),p1,width=8.5, height=11) # make sure this folder exists in your workspace
}

#-------------------------------------------------------------------------------
# Loop creation of Zhang figures showing stages 1-3 - raw data

#see lines 16-23 for Zhang_Raw df (also used in this section)
Zhang_Filtered <- read.csv("results//Fr_Crop_Treated.csv") # originally "results//CF_Zhang_Stage23_0925.csv"
# add SD error bars and reformate table from wide to long format
Zhang_filtered <- na.omit(melt(setDT(Zhang_filtered), id.vars = c("year","ISO3_code","Country", "SD"), variable.name = "source")) # add SD error bars and reformate table from wide to long format
Zhang_filtered[which(Zhang_filtered$source != 'mean'),]$SD <- NA
Zhang_filtered[which(Zhang_filtered$SD == 0),]$SD <- NA
Zhang_filtered$U <- Zhang_filtered$value + Zhang_filtered$SD*2
Zhang_filtered$L <- Zhang_filtered$value - Zhang_filtered$SD*2
keeps <- unique(Zhang_Filtered$source)[1:7] # changed from 7to 8 on 0930
Zhang_Stage2 <- Zhang_Filtered[which(Zhang_Filtered$source %in% keeps),]

Zhang_Stage3 <- Zhang_Filtered[which(Zhang_Filtered$source %in% c('mean', 'med', 'min', 'max')),]

CF_df <- read.csv("Fr_Crop_Estimates.csv")
CF_df_sub <- subset(CF_df, source == "Ludemann_Report8" | source == "Ludemann_Report9" | source == "FAO updated")
SourceShape <- setNames(c(8,15,17), levels(CF_df_sub$source))  #establish shape factors for Ludemann_Report8 Ludemann_Report9 & FAO_updated

iso_lst <- unique(Zhang_Stage3$ISO3_code)
for (iso in iso_lst){
  print(iso)
  
  # Stage 2
  p2 <-ggplot(Zhang_Stage2[which(Zhang_Stage2$ISO3_code==iso),],aes(x = year, y = `value`, color = source)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = "Year",y = "Fr_crop (%)", title = paste(unique(Zhang_Stage2[which(Zhang_Stage2$ISO3_code==iso),]$Country),'Stage 2: filtered (Zhang et al., 2021)')) +
    coord_cartesian(xlim =c(1961,2020), ylim = c(0,NA)) + #cartesian limit doesn't exclude data like ylim()... consider adding ylim
    geom_hline(aes(yintercept= 1, linetype = "100% Fr_crop"), colour= 'black') +
    scale_linetype_manual(name =  "", values = c(2), 
                          guide = guide_legend(override.aes = list(color = c("black")))) +
    theme_classic()
  p2
  
  # Stage 3
  p3 <-ggplot(Zhang_Stage3[which(Zhang_Stage3$ISO3_code==iso),],aes(x = year, y = `value`, color = source)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(color = 'quick stats',x = "Year",y = "Fr_crop (%)", title = paste(unique(Zhang_Stage3[which(Zhang_Stage3$ISO3_code==iso),]$Country),'Stage 3: stats (Zhang et al., 2021)')) +
    coord_cartesian(xlim =c(1961,2020), ylim = c(0,NA)) + #cartesian limit doesn't exclude data like ylim()... consider adding ylim
    geom_hline(aes(yintercept= 1, linetype = "100% Fr_crop"), colour= 'black') +
    scale_linetype_manual(name =  "", values = c(2), 
                          guide = guide_legend(override.aes = list(color = c("black")))) +
    geom_errorbar(aes(ymax = U, ymin = L, width=0.75), show.legend = NA, alpha = 0.5) +
    theme_classic()
  p3
  
  # Stage 1: Create Image displaying Raw Image Data (for a given ISO)
  df <- CF_df[which(CF_df$ISO3_code==iso),]
  df_point <- subset(df, source == "Ludemann_Report8" | source == "Ludemann_Report9" | source == "FAO updated")
  df_sub <- subset(df, source != "FAO" & source != "Ludemann_Report8" & source != "Ludemann_Report9" & source != "FAO updated")
  p1 <- ggplot(df_sub,aes(x = year, y = `Cropland.Fraction`)) + 
    geom_line(data = df_sub, aes(color = source), lwd = 1, alpha=0.5) +
    geom_point(data = df_sub, aes(color = source), lwd = 1, alpha=0.75) +
    geom_point(data = df_point, aes(shape = source), lwd = 3, alpha=1) +
    labs(x = "Year",y = "Fr_crop (%)", colour = "", title =  paste(unique(CF_df[which(CF_df$ISO3_code==iso),]$Country),'Stage 1: all raw data')) + 
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(xlim =c(1961,2020), ylim = c(0,2)) + #cartesian limit doesn't exclude data like ylim()... consider adding ylim
    scale_size_manual(values = c("Rasmus" = 4, "Other Sources" = 1), guide = "none") +
    scale_alpha_manual(values = c("Rasmus" = 1, "Other Sources" = 0.5), guide = "none") +
    scale_shape_manual(values = SourceShape, name = "IFA/FAO Sources") +
    geom_hline(aes(yintercept= 1, linetype = "100% Fr_crop"), colour= 'black') +
    scale_linetype_manual(name =  "", values = c(2), guide = guide_legend(override.aes = list(color = c("black")))) +
    theme_classic()
  p1
  figure <- ggarrange(p1, p2,p3,
                      labels = c("A", "B","C"),
                      ncol = 1, nrow = 3)
  figure
  #ggsave(paste0("results//figure0930//",iso,".png"),figure,width=8.5, height=11) # make sure this folder exists in your workspace
}
