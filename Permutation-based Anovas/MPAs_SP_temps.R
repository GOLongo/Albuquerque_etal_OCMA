############################################ 
# Comparing coral occurrence probability  #
# in strict protected MPAs between times  #
###########################################
#---releasing packages----
library(dplyr)
library(raster)
library(lmPerm)
library(rcompanion)
library(yarrr)
library(readr)
#------
#Probability values for 2020
coral_occ2020_UCsPI_mask <- raster ("dados_sig/ucs_novas/AMPs_pi/coral_occ2020_UCsPI_mask_ras.asc")
coral_occ2020_UCsPI_values <- getValues(coral_occ2020_UCsPI_mask)
hist(coral_occ2020_UCsPI_values)

#creating a data frame with the occurrence probabilities values 
coral_occ2020_UCsPI_values_df <- as.data.frame(coral_occ2020_UCsPI_values)
coral_occ2020_UCsPI_values_df$temp <- 2020
names(coral_occ2020_UCsPI_values_df) <- c("values", "temp")


#Probability values for 2050
coral_occ2050_UCsPI_mask <- raster ("dados_sig/2050/UCsPI/coral_occ2050_UCsPI_mask_ras.asc")
coral_occ2050_UCsPI_values <- getValues(coral_occ2050_UCsPI_mask)
hist(coral_occ2050_UCsPI_values)

#creating a data frame with the occurrence probabilities values 
coral_occ2050_UCsPI_values_df <- as.data.frame(coral_occ2050_UCsPI_values)
coral_occ2050_UCsPI_values_df$temp <- 2050
names(coral_occ2050_UCsPI_values_df) <- c("values", "temp")


#Probability values for 2100
coral_occ2100_UCsPI_mask <- raster ("dados_sig/2100/UCsPI/coral_occ2100_UCsPI_mask_ras.asc")
coral_occ2100_UCsPI_values <- getValues(coral_occ2100_UCsPI_mask)
hist(coral_occ2100_UCsPI_values)

#creating a data frame with the occurrence probabilities values 
coral_occ2100_UCsPI_values_df <- as.data.frame(coral_occ2100_UCsPI_values)
coral_occ2100_UCsPI_values_df$temp <- 2100
names(coral_occ2100_UCsPI_values_df) <- c("values", "temp")

#joining data frames and save database

ucs_pi_all_df <- rbind(coral_occ2020_UCsPI_values_df, 
                    coral_occ2050_UCsPI_values_df,
                    coral_occ2100_UCsPI_values_df)
ucs_pi_all_df <- ucs_pi_all_df %>% na.omit()
ucs_pi_all_df$temp <- as.factor(ucs_pi_all_df$temp)
write_csv(ucs_pi_all_df, "base_tratada/ucs_pi_all.csv")

#--------------Running permanova -------------
#
ucs_pi_all_df <- read_csv("base_tratada/ucs_pi_all.csv", 
                       col_types = cols(values = col_double(),
                                        temp = col_factor())) %>%
  mutate(pxl = rep(1:290, 3))

# the pernamova test

ucspi_aovp <- aovp(values ~ temp + pxl, ucs_pi_all_df)
summary(ucspi_aovp)

# post hoc test
posthoc_ucpi <- pairwisePermutationTest(values ~ temp + pxl, ucs_pi_all_df)
posthoc_ucpi

# Violin plot
pirateplot((formula = values ~ temp), 
           data = ucs_pi_all_df,
           main = "",
           xlab = "",
           ylab = "Occurence probability",
           ylim = c(0,1),
           theme = 0,
           pal = c("#FFE5CE", "#FDAE6B", "#E5550E"), # southpark color palette
           bean.f.o = .8, # Bean fill
           bean.b.col = c("#FFE5CE", "#FDAE6B", "#E5550E"),
           bean.b.o = 1,
           point.o = .3, # Points
           avg.line.o = 1, # Average line
           avg.line.fun = median, 
           #bar.f.o = .4, # Bar
           inf.f.col = "white", # Inf fill col
           inf.b.col = "black", # Inf border col
           avg.line.col = "#3F3F3F", # avg line col
           avg.line.lwd = 2,
           bar.f.col = gray(.8), # bar filling color
           point.pch = 21,
           point.bg = "white",
           point.col = "black",
           point.cex = .7,
           adjust = 1,
           width.max = .49,
           gl.col = gray(.6), 
           gl.lty = 0,
           gl.lwd = c(.5, 0.5))

#text(x = ucs_pi_all$temp, y = c("ab", "ab", "c"), col="black",cex=2)
