freq_spenTRUE <- read.csv("sppdist_spen_all_geovalidTRUE_1dd.csv", stringsAsFactors = FALSE)
freq_spenFALSE <- read.csv("sppdist_spen_all_geovalidFALSE_1dd.csv", stringsAsFactors = FALSE)
freq_spenOLD <- read.csv("sppdist_spen_all.csv", stringsAsFactors = FALSE)
freq_plot_uniq <- read.csv("sppdist_plot_all.csv", stringsAsFactors = FALSE)


fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}

base_breaks <- function(n = 6){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

mymethod = "auto"
my_pt_size = 2
my_pt_alpha = 0.7
library(ggplot2)
fig_rarity<- ggplot(freq_spenTRUE, aes(obs_num,spp_num)) +
  geom_point(col = "#c65999", fill = "#c65999", shape = 25, alpha = my_pt_alpha,size=my_pt_size)+
  
  geom_point(data=freq_plot_uniq,aes(obs_num,spp_num), shape = 22,
             col = "#7aa456", fill = "#7aa456", alpha = my_pt_alpha,size=my_pt_size)+
  
  geom_point(data=freq_spenOLD,aes(obs_num,spp_num), shape = 21,
             col = "#777acd", fill = "#777acd", alpha = my_pt_alpha,size=my_pt_size)+
  
  geom_point(data=freq_spenFALSE,aes(obs_num,spp_num), shape = 2,
             col = "#c96d44", fill = "#0000ffcc",alpha = my_pt_alpha,size=my_pt_size)+  
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE)+  
  scale_y_log10(labels=fancy_scientific, breaks = base_breaks())+
  scale_x_log10(labels=fancy_scientific, breaks = base_breaks())+
  labs(x = "Number of observations")+
  labs(y = "Number of species")+
  
  # legend ########################################################################
  geom_point(aes(x = 9000, y = 60000), col = "#777acd", fill = "#777acd", size = 7, shape = 21) +
  geom_point(aes(x = 9000, y = 35000), col = "#7aa456", fill = "#7aa456", size = 7, shape = 22) +
  geom_point(aes(x = 9000, y = 20000), col = "#c65999", fill = "#c65999", size = 6, shape = 25) +
  geom_point(aes(x = 9000, y = 10000), col = "#c96d44", size = 6, shape = 2) +
  annotate("text", x = 12000, y = 60000, label = "all specimens", size = 6, hjust = 0) +
  annotate("text", x = 12000, y = 35000, label = "plots", size = 6, hjust = 0) +
  annotate("text", x = 12000, y = 20000, label = "geovalid specimens", size = 6, hjust = 0) +
  annotate("text", x = 12000, y = 10000, label = "specimens within 1°", size = 6, hjust = 0) +
  #################################################################################
  
  
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(colour="black", size = 20),
        axis.text.x = element_text(colour="black", size = 20),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        panel.grid.major.y = element_line(linetype = "dotted", size=.1, color="grey70"), 
        panel.grid.major.x = element_line(linetype = "dotted", size=.1, color="grey70"), 
        panel.border = element_rect(colour = "black",fill=NA))

#fig_rarity

ggsave("C:/Users/Dan/Downloads/rarity.pdf", plot = fig_rarity, width = 12, height = 12)


