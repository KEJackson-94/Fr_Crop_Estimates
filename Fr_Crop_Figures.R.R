library(stringr) # for function str_length()
library(tidyr) # for gather function
library(countrycode) # assign/match country iso codes and names
library(gridExtra) # for grid.arrange
library(ggplot2)
library(grid) # for textGrob() which adds title to grid.arrange() image
library(scales) # for label_number()
library(dplyr) # for function mutate()
library(forcats) # for fct_other()
library(RColorBrewer)# create color palette

###########################################################################
#### Loop creation if country figures showing just stage 1 - raw data, ####
###########################################################################

# stage 1: CF estimates prior to filtering
CF_df <- read.csv("CroplandFractions.csv")
CF_df$year <- as.numeric(CF_df$year)
class(CF_df$year)

country_lst <- unique(CF_df$ISO3_code)
#country_lst <- c('AUT', 'NZL') # test with AUT and NZL

source_lst <- unique(CF_df$source) # create a unique color palette for each source
n <- length(source_lst)
clr1 <- palette(brewer.pal(n = 8, name = "Set1"))
clr2 <- palette(brewer.pal(n = 8, name = "Dark2"))
col <- c(clr1, clr2)[1:n]
col[8] <- '#FF3300'
SourceColors <- setNames(col, levels(CF_df$source))

CF_df_sub <- subset(CF_df, source == "Ludemann_Report8" | source == "Ludemann_Report9" | source == "FAO updated")
SourceShape <- setNames(c(8,15,17), levels(CF_df_sub$source))

for (ii in 1:length(country_lst)){
  iso <- country_lst[ii]
  country_name <- paste(countrycode(iso,origin = 'iso3c', destination =  'country.name'),', ',iso)
  print(country_name)
      
  #-------------------------------------------------------------------------------
  # Fig. 1: Create Image displaying Raw Image Data (for a given ISO)
  
  df <- CF_df[which(CF_df$ISO3_code==iso),]
  df_point <- subset(df, source == "Ludemann_Report8" | source == "Ludemann_Report9" | source == "FAO updated")
  df_sub <- subset(df, source != "FAO" & source != "Ludemann_Report8" & source != "Ludemann_Report9" & source != "FAO updated")
  df_sub <- df_sub %>% mutate(source_factor = ifelse(source == "Rasmus", 2, 1)) # emphasis Rasmus by making it a factor
  df_sub <- df_sub %>% mutate(source_factor = as.factor(source) %>% fct_other(keep = c("Rasmus"), other_level = "Other Sources"))
  
  p1 <- ggplot(df_sub,aes(x = year, y = `Cropland.Fraction`)) + 
    geom_line(data = df_sub, aes(color = source, size = source_factor)) +
    geom_point(data = df_sub, aes(color = source), lwd = 1.5, alpha=0.75) +
    geom_point(data = df_point, aes(shape = source), lwd = 3, alpha=1) +
    labs(x = "Year",y = "Cropland Fraction (%)", colour = "", title = paste(country_name,'(Unfiltered)')) + 
    coord_cartesian(xlim =c(1961,2020)) + #cartesian limit doesn't exclude data like ylim()... consider adding ylim
    #scale_shape_discrete(name = "IFA/FAO Sources") + 
    #scale_colour_discrete(name = ("Zhang et al. 2021")) + 
    geom_hline(aes(yintercept= 1, linetype = "100% Cropland Fraction"), colour= 'black') +
    scale_linetype_manual(name = "", values = c(1), guide = guide_legend(override.aes = list(color = c("black")))) +
    scale_size_manual(values = c("Rasmus" = 2, "Other Sources" = 1), guide = "none") +
    scale_alpha_manual(values = c("Rasmus" = 1, "Other Sources" = 0.5), guide = "none") +
    scale_color_manual(values = SourceColors, name = 'Additional Sources') +
    scale_shape_manual(values = SourceShape, name = "IFA/FAO Sources") +
    theme_classic()
  p1
  ggsave(paste0("results//figures//",iso,".png"),p1,width=8.5, height=11)
  }

#####################################################################
#### Loop creation if country figures showing stage 1 -raw data, ####
#### stage 2 - filtered data, and stage 3 - best option          ####
#####################################################################

#-------------------------------------------------------------------------------
# Prepare files as dataframes prior to looping the creation of figures per iso

# stage 1: CF estimates prior to filtering
CF_df <- read.csv("CroplandFractions.csv")
CF_df$year <- as.numeric(CF_df$year)
class(CF_df$year)

# stage 2: CF estimates based on options
files <- list.files(paste0(getwd(),"/results/options")) 
file_names <- substr(files,0,str_length(files)-4)
df.list <- c()
x = 1
for (i in 1:length(files)){
  file <- files[i]
  file_name <- file_names[i]
  df <- read.csv(paste0(getwd(),"/results/options/",file))
  names(df)[3:ncol(df)] <- substring(names(df)[3:ncol(df)],2)
  df <- df[,-c(1)]
  df <- gather(df, key="year", value="Cropland Fraction", 2:ncol(df))
  df$option <- file_name
  print(file_name)
  df.list[[x]]<-df
  x = x+1
}
Options_Cmbnd = do.call(rbind, df.list)
Options_Cmbnd <- na.omit(Options_Cmbnd)
Options_Cmbnd$Country <- countrycode(Options_Cmbnd$iso,origin = 'iso3c', destination =  'country.name') # add iso codes to list
Options_Cmbnd$year <- as.numeric(Options_Cmbnd$year)
class(Options_Cmbnd$year)

# stage 3: expert created list for each country choosing optimal option
# create a test df for stage 3
stg3_df <- data.frame(matrix(ncol = 2, nrow = length(unique(Options_Cmbnd$iso)))) 
xx <- c("iso", "Choice") # list of colnames
colnames(stg3_df) <- xx # set colnames
stg3_df$iso <- unique(Options_Cmbnd$iso)
stg3_df$Choice <- unique(Options_Cmbnd$option)[10] # currently let's use the default of CF_option3_Mean2
#stg3_df$Choice <- ''
write.csv(stg3_df, 'C://Git//choice.csv')

#-------------------------------------------------------------------------------
# loop the creation of figures per iso

country_lst <- unique(CF_df$ISO3_code)
#country_lst <- c('USA', 'AUT', 'CHN', 'URY') # test with AUT and USA
for (ii in 1:length(country_lst)){
  iso <- country_lst[ii]
  country_name <- paste(countrycode(iso,origin = 'iso3c', destination =  'country.name'),', ',iso)
  print(country_name)
  
  plist <- list() # create an empty list and add the three plots before merging as a pdf
  for (i in 1:5) { # three different images/steps per country
    if (i == 1){
      
      #-------------------------------------------------------------------------------
      # Fig. 1: Create Image displaying Raw Image Data (for a given ISO)

      df <- CF_df[which(CF_df$ISO3_code==iso),]
      df_sub <- subset(df, source != "Rasmus" & source != "FAO")
      Rasmus_sub <- CF_df[which(CF_df$source=='Rasmus' & CF_df$ISO3_code==iso),]
      FAO_sub <- CF_df[which(CF_df$source=='FAO' & CF_df$ISO3_code==iso),]
      p1 <- ggplot(df_sub,aes(x = year, y = `Cropland.Fraction`)) + 
        geom_line(data = Rasmus_sub, color="red", lwd = 1.5) + 
        geom_line(data = FAO_sub, color="black", lwd = 1.5) +
        geom_line(data = df_sub, aes(color = source), lwd = 1, alpha= 0.5) +
        geom_point(data = df_sub, aes(color = source), lwd = 1.5, alpha=0.75) +
        labs(x = "year",y = "Cropland Fraction (%)", colour = "", title = 'Stage 1: Unfiltered CF Estimates') + 
        coord_cartesian(xlim =c(1961,2020)) + #cartesian limit doesn't exclude data like ylim()... consider adding ylim
        theme_classic()
      plist[[i]] <- p1
    }
    
    else if (i == 2){
      
      #-------------------------------------------------------------------------------
      # Fig. 2: Create Image all 12 options (for a given ISO)
      
      option_subset <- c("CF_option1_Mean1","CF_option1_Mean2","CF_option1_Median1","CF_option1_Median2")
      Options_sub <- Options_Cmbnd[which(Options_Cmbnd$iso==iso & Options_Cmbnd$option %in% option_subset),]
      p2 <- ggplot(Options_sub, aes(x = year, y = `Cropland Fraction`, color = option, group = option)) +  
        geom_line(data = Options_sub, aes(color = option), lwd = 1, alpha=0.5) + 
        geom_point(data = Options_sub, aes(color = option), lwd = 1, alpha=0.75) +
        geom_smooth(data = Options_sub, aes(color = option), se= FALSE, linetype="dashed", lwd = 0.25, alpha=0.25) +
        labs(x = "year",y = "Cropland Fraction (%)", colour = "", title = 'Stage 2: Filtered CF Estimates (Option 1)') +
        scale_y_continuous(labels = label_number(accuracy = 0.01)) +
        coord_cartesian(xlim =c(1961,2020)) + # cartesian limit doesn't exclude data like ylim()
        theme_classic() 
      plist[[i]] <- p2
    }
    
    else if (i == 3){
      
      #-------------------------------------------------------------------------------
      # Fig. 2: Create Image all 12 options (for a given ISO)
      
      option_subset <- c("CF_option2_Mean1","CF_option2_Mean2","CF_option2_Median1","CF_option2_Median2")
      Options_sub <- Options_Cmbnd[which(Options_Cmbnd$iso==iso & Options_Cmbnd$option %in% option_subset),]
      p3 <- ggplot(Options_sub, aes(x = year, y = `Cropland Fraction`, color = option, group = option)) +  
        geom_line(data = Options_sub, aes(color = option), lwd = 1, alpha=0.5) + 
        geom_point(data = Options_sub, aes(color = option), lwd = 1, alpha=0.75) +
        geom_smooth(data = Options_sub, aes(color = option), se= FALSE, linetype="dashed", lwd = 0.25, alpha=0.25) +
        labs(x = "year",y = "Cropland Fraction (%)", colour = "", title = 'Stage 2: Filtered CF Estimates (Option 2)') +
        scale_y_continuous(labels = label_number(accuracy = 0.01)) +
        coord_cartesian(xlim =c(1961,2020)) + # cartesian limit doesn't exclude data like ylim()
        theme_classic() 
      plist[[i]] <- p3
    }
    
    else if (i == 4) {
      
      #-------------------------------------------------------------------------------
      # Fig. 2: Create Image all 12 options (for a given ISO)
      
      option_subset <- c("CF_option3_Mean1","CF_option3_Mean2","CF_option3_Median1","CF_option3_Median2")
      Options_sub <- Options_Cmbnd[which(Options_Cmbnd$iso==iso & Options_Cmbnd$option %in% option_subset),]
      p4 <- ggplot(Options_sub, aes(x = year, y = `Cropland Fraction`, color = option, group = option)) + 
        geom_line(data = Options_sub, aes(color = option), lwd = 1, alpha=0.5) + 
        geom_point(data = Options_sub, aes(color = option), lwd = 1, alpha=0.75) +
        geom_smooth(data = Options_sub, aes(color = option), se= FALSE, linetype="dashed", lwd = 0.25, alpha=0.25) +
        labs(x = "year",y = "Cropland Fraction (%)", colour = "", title = 'Stage 2: Filtered CF Estimates (Option 3)') +
        scale_y_continuous(labels = label_number(accuracy = 0.01)) +
        coord_cartesian(xlim =c(1961,2020)) + # cartesian limit doesn't exclude data like ylim()
        theme_classic() 
      plist[[i]] <- p4
    }
    
    else {
      
      #-------------------------------------------------------------------------------
      # Fig. 3: Create Showing stat_smooth and filtered data for chosen CF for given country

      Choice <- stg3_df[which(stg3_df$iso==iso),]$Choice
      print(Choice)
      Option_sub <- Options_Cmbnd[which(Options_Cmbnd$iso==iso & Options_Cmbnd$option==Choice),]
      p5 <- ggplot(Option_sub, aes(x = year, y = `Cropland Fraction`, color = option, group = option)) +
        geom_line(data = Option_sub, aes(color = option), lwd = 1, alpha=0.5) +
        stat_smooth(data = Option_sub, aes(color = option), inherit.aes = TRUE) +
        geom_point(data = Option_sub, aes(color = option), lwd = 1.5, alpha=0.75) +
        labs(x = "year",y = "Cropland Fraction (%)", colour = "", title = 'Stage 3: Chosen Option for CF Estimates') +
        coord_cartesian(xlim =c(1961,2020)) + # cartesian limit doesn't exclude data like ylim()
        theme_classic()
      plist[[i]] <- p5
    }
  }
  pp <- grid.arrange(grobs = plist, ncol = 1, nrow = length(plist), top=textGrob(country_name, gp=gpar(fontsize=15,font=2))) ## display plot
  ggsave(paste0("results//figures//",iso,".png"),pp,width=8.5, height=11)
  p <- list()
}

# test out pdf combine option
# install.packages("qpdf") # this solution found https://stackoverflow.com/questions/17552917/merging-existing-pdf-files-using-r
# library(qpdf)
# ?qpdf::pdf_combine
# png_list <- list.files(paste0(getwd(),"/results/figures//"), full.names = TRUE) # create list of pngs in figures folder
# pdf_cmbn <- qpdf::pdf_combine(input = png_list,
#                   output = "output.pdf")
