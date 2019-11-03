flag_excludeFIA <- FALSE
PATH_DATA <- "data_20181211"
dir.create(PATH_DATA)
dir.create(paste0(PATH_DATA,"/0_raw"))
# load bien data
bien <- read.table(paste0("data_20181127","/0_raw/bien4.1.0_rarity_20181127.tsv"),
                   sep="\t",
                   quote = "\"",header=TRUE,
                   stringsAsFactors = FALSE)
library(raster)
df_plot <- subset(bien,observation_type== "plot")
df_spen <- subset(bien,observation_type== "specimen")
df_spen_loc <- subset(df_spen,!is.na(latitude) &!is.na(longitude)   )
coordinates(df_spen_loc) <-~ longitude+latitude
crs(df_spen_loc)  <- CRS("+init=epsg:4326")
df_plot_loc <- df_plot[c("latitude","longitude")]
nrow(df_plot_loc) 
df_plot_loc <- unique(df_plot_loc)
nrow(df_plot_loc) 
table( is.na(df_plot_loc$latitude) )
table( is.na(df_plot_loc$longitude) )
df_plot_loc <- subset(df_plot_loc,!is.na(latitude) &!is.na(longitude)   )
coordinates(df_plot_loc) <-~ longitude+latitude
crs(df_plot_loc)  <- CRS("+init=epsg:4326")
df_plot_buff <- buffer(df_plot_loc,width=100000)
over_results <- over(df_plot_buff,df_spen_loc,returnList=TRUE)
nrow(df_spen_loc)     
nrow(over_results$`1`)
specimen_subset <- over_results$`1`
# get freq table for specimen data
sppfreq_spen <- table(specimen_subset$scrubbed_species_binomial)
sppdist_spen <- table(sppfreq_spen) 
sppfreq_spen <- data.frame(sppfreq_spen)
names(sppfreq_spen) <- c("sci_name","obs_num")
sppdist_spen <- data.frame(sppdist_spen)
names(sppdist_spen) <- c("obs_num","spp_num")
specimen_subset_geovalid <- subset(specimen_subset,is_geovalid==1)
sppfreq_spen <- table(specimen_subset_geovalid$scrubbed_species_binomial)
sppdist_spen <- table(sppfreq_spen) 
sppfreq_spen <- data.frame(sppfreq_spen)
names(sppfreq_spen) <- c("sci_name","obs_num")
sppdist_spen <- data.frame(sppdist_spen)
names(sppdist_spen) <- c("obs_num","spp_num")
sppdist_spen$obs_num <- as.numeric(as.character(sppdist_spen$obs_num))
label <- "BIEN_landPlant"
freq_spenTRUE <- read.csv(paste0("sppdist_spen_",
                             "all","_geovalid","TRUE","_1dd",
                             ".csv"))
freq_spenFALSE <- read.csv(paste0("sppdist_spen_",
                                 "all","_geovalid","FALSE","_1dd",
                                 ".csv"))
PATH_DATA <- "data_20181211"
selected_cont="all"
freq_spenOLD <- read.csv(paste0(PATH_DATA,"/1_freq/sppdist_spen_",selected_cont,".csv"))
freq_plot_uniq <- read.csv(paste0(PATH_DATA,"/1_freq/sppdist_plot_",selected_cont,".csv"))
fancy_scientific <- function(l) {
  l <- format(l, scientific = TRUE)
  l <- gsub("^(.*)e", "'\\1'e", l)
  l <- gsub("e", "%*%10^", l)
  parse(text=l)
}
path_freq <- paste0(PATH_DATA,"/1_freq/")
dir.create(path_freq)
calculateSppFrequency <- function(thisCont="all",flag_geovalid=FALSE){
  if(flag_geovalid){
    bien <- subset(bien,is_geovalid==1)
  }
  df_plot <- subset(bien,observation_type== "plot")
  df_spen <- subset(bien,observation_type== "specimen")  
  if(  thisCont!="all"   ){
    if(thisCont=="New World"){
      df_plot <- subset(df_plot,continent=="North America" | continent=="South America")
      df_spen <- subset(df_spen,continent=="North America" | continent=="South America")
    } else {
    df_plot <- subset(df_plot,continent==thisCont)
    df_spen <- subset(df_spen,continent==thisCont)
    }
  }    
  dups <- duplicated(df_plot[c("plot_name","scrubbed_species_binomial")])
  df_plotUNQ <- df_plot[!dups,]  
  save(df_plot,   file=paste0(PATH_DATA,"/0_raw/df_plot_",thisCont,"_geovalid",flag_geovalid,".rdata"))
  save(df_spen,   file=paste0(PATH_DATA,"/0_raw/df_spen_",thisCont,"_geovalid",flag_geovalid,".rdata"))
  save(df_plotUNQ,file=paste0(PATH_DATA,"/0_raw/df_plotUNQ_",thisCont,"_geovalid",flag_geovalid,".rdata"))  
  sppfreq_plot <- table(df_plotUNQ$scrubbed_species_binomial)
  sppdist_plot <- table(sppfreq_plot) 
  sppfreq_plot <- data.frame(sppfreq_plot)
  names(sppfreq_plot) <- c("sci_name","obs_num")
  sppdist_plot <- data.frame(sppdist_plot)
  names(sppdist_plot) <- c("obs_num","spp_num")
  write.csv(sppfreq_plot,paste0(path_freq,
                                "sppfreq_plot_",
                                thisCont,"_geovalid",flag_geovalid,
                                ".csv"),row.names =F )
  write.csv(sppdist_plot,paste0(path_freq,
                                "sppdist_plot_",
                                thisCont,"_geovalid",flag_geovalid,
                                ".csv"),row.names =F )  
  sppfreq_spen <- table(df_spen$scrubbed_species_binomial)
  sppdist_spen <- table(sppfreq_spen) 
  sppfreq_spen <- data.frame(sppfreq_spen)
  names(sppfreq_spen) <- c("sci_name","obs_num")
  sppdist_spen <- data.frame(sppdist_spen)
  names(sppdist_spen) <- c("obs_num","spp_num")
  write.csv(sppfreq_spen,paste0(path_freq,
                                "sppfreq_spen_",
                                thisCont,"_geovalid",flag_geovalid,
                                ".csv"),row.names = F)
  write.csv(sppdist_spen,paste0(path_freq,
                                "sppdist_spen_",
                                thisCont,"_geovalid",flag_geovalid,
                                ".csv"),row.names = F)  
  sppfreq_combine <- merge(sppfreq_spen,sppfreq_plot,by="sci_name",all=T)
  nrow(sppfreq_combine)
  table(is.na(sppfreq_combine$obs_num.x))
  table(is.na(sppfreq_combine$obs_num.y))
  sppfreq_combine$obs_num.x[is.na(sppfreq_combine$obs_num.x)] <- 0
  sppfreq_combine$obs_num.y[is.na(sppfreq_combine$obs_num.y)] <- 0
  sppfreq_combine$obs_num <- sppfreq_combine$obs_num.x + sppfreq_combine$obs_num.y
  names(sppfreq_combine)[2:3] <- c("obs_num_specimen","obs_num_plot")  
  sppdist_combine <- data.frame( table(sppfreq_combine$obs_num) )
  names(sppdist_combine) <- c("obs_num","spp_num")  
  write.csv(sppfreq_combine,paste0(path_freq,
                                   "sppfreq_combine_",
                                   thisCont,"_geovalid",flag_geovalid,
                                   ".csv"),row.names = F)
  write.csv(sppdist_combine,paste0(path_freq,
                                   "sppdist_combine_",
                                   thisCont,"_geovalid",flag_geovalid,
                                   ".csv"),row.names = F)  
  sppfreq_combine$flag_rare <- FALSE
  sppfreq_combine$flag_rare[ which(sppfreq_combine$obs_num <=3 )] <- TRUE
  table(sppfreq_combine$flag_rare)
  table(sppfreq_combine$flag_rare)/nrow(sppfreq_combine)
  write.csv(sppfreq_combine,paste0(path_freq,
                                   "sppfreq_combine_update1",
                                   thisCont,"_geovalid",flag_geovalid,
                                   ".csv"),row.names = F)  
  save(sppfreq_combine,file=paste0(path_freq,
                                   "sppfreq_combine_update1",
                                   thisCont,"_geovalid",flag_geovalid,
                                   ".rdata"))
}
calculateSppFrequency(thisCont="all")

