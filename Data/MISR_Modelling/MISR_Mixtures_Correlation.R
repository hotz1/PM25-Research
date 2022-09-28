#############
# An R script to create a "correlation heatmap" among the 74 AOD mixture models in the MISR data wihch was 
# collected in California from 2000-2022.
# Last updated: September 27, 2022
#############

library(tidyverse)
library(data.table)

# setwd("C:/Users/johot/Desktop/Joey's Files/Work/NSERC 2022/PM25-Research")

# Get local file directory name
misr.datasets.dir = paste0(getwd(), '/Data/MISR/MISR_datasets/') # Sub-folder containing yearly MISR datasets

# Collect local MISR filenames
misr.annual.filenames <- list.files(misr.datasets.dir, pattern = ".csv", full.names = T)

# Create empty (for now) lists which will store annual merged MISR datasets 
misr.annual.mixtures <- vector("list", length = length(misr.annual.filenames))


# For each MISR file, load in the MISR dataset, and merge with the AQS/CSN/IMPROVE datasets based on spatial matching and time matching
for(i in 1:length(misr.annual.filenames)){
  # Select year from the filename
  year <- substr(misr.annual.filenames[i], nchar(misr.annual.filenames[i]) - 7, nchar(misr.annual.filenames[i]) - 4)
  cat('- Loading MISR Data from ', year, '......', sep = '')
  start = Sys.time()
  
  # Read in the csv file containing MISR data
  misr.annual <- read_csv(misr.annual.filenames[i], guess_max = Inf, show_col_types = FALSE)
  cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' seconds\n', sep = '')
  
  # Select 74 AOD mixtures
  mixtures.annual <- misr.annual %>% select(c(32:105))
  misr.annual.mixtures[[i]] <- mixtures.annual
}

# Combine 74 mixtures for each year into one large dataframe
misr.all.mixtures <- do.call("rbind", misr.annual.mixtures)

# Remove any rows containing missing observations 
# (to cut down on space, as these would be discarded later anyways for computing correlations)
misr.all.mixtures <- misr.all.mixtures %>%
  drop_na()

# Remove data for individual years (no reason in keeping duplicates of the data)
remove(misr.annual.mixtures)

# Create a matrix containing correlation coefficients for each pair of mixtures
mixture.corr <- round(cor(misr.all.mixtures), digits = 3)

# Reshape correlation matrix into a large table (required for plotting the heatmap)
melted_mixture.corr <- reshape2::melt(mixture.corr)

# Save the melted mixture correlation matrix as a csv file locally
write.csv(melted_mixture.corr, paste0(getwd(), "/Data/MISR_Modelling/Mixtures_Correlation_Melted.csv"), row.names = F)


# # Create the correlation heatmap, and save it as an image locally.
# mixtures_heatmap <- melted_mixture.corr %>%
#   ggplot(aes(x = Var1, y = Var2, fill = value)) +
#   labs(title = "Pearson Correlation between pairs of 74 AOD Mixtures from the MISR dataset",
#        subtitle = "MISR Data collected from 2000-2022 in California") +
#   geom_tile(color = "black") +
#   scale_fill_gradient2(low = "#ff0000", high = "#00ff00", mid = "#ffffff",
#                        midpoint = 0, limit = c(-1,1), name = "Correlation") +
#   geom_text(aes(Var2, Var1, label = value), color = "black", size = 1.5) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 25),
#         plot.subtitle = element_text(hjust = 0.5, size = 20))
# 
# 
# 
# # Save the plot created above as a local PNG file
# ggsave(filename = paste0(getwd(), "/Data/MISR_Modelling/Mixtures_Correlation_Heatmap.png"),
#        plot = mixtures_heatmap, device = "png", width = 18, height = 16)

