############################################ 
# Comparing coral occurrence probability  #
# in sustainable use MPAs between times   #
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
coral_occ2020_UCsUS_mask <- raster ("dados_sig/ucs_novas/AMPs_us/coral_occ2020_UCsUS_mask.asc")
coral_occ2020_UCsUS_values <- getValues(coral_occ2020_UCsUS_mask)
hist(coral_occ2020_UCsUS_values)

#creating a data frame with the occurrence probabilities values 
coral_occ2020_UCsUS_values_df <- as.data.frame(coral_occ2020_UCsUS_values)
coral_occ2020_UCsUS_values_df$temp <- 2020
names(coral_occ2020_UCsUS_values_df) <- c("values", "temp")


#Probability values for 2050
coral_occ2050_UCsUS_mask <- raster ("dados_sig/2050/UCsUS/coral_occ2050_UCsUS_mask.asc")
coral_occ2050_UCsUS_values <- getValues(coral_occ2050_UCsUS_mask)
hist(coral_occ2050_UCsUS_values)

#creating a data frame with the occurrence probabilities values 
coral_occ2050_UCsUS_values_df <- as.data.frame(coral_occ2050_UCsUS_values)
coral_occ2050_UCsUS_values_df$temp <- 2050
names(coral_occ2050_UCsUS_values_df) <- c("values", "temp")


#Probability values for 2100
coral_occ2100_UCsUS_mask <- raster ("dados_sig/2100/UCsUS/coral_occ2100_UCsUS_mask.asc")
coral_occ2100_UCsUS_values <- getValues(coral_occ2100_UCsUS_mask)
hist(coral_occ2100_UCsUS_values)

#creating a data frame with the occurrence probabilities values 
coral_occ2100_UCsUS_values_df <- as.data.frame(coral_occ2100_UCsUS_values)
coral_occ2100_UCsUS_values_df$temp <- 2100
names(coral_occ2100_UCsUS_values_df) <- c("values", "temp")

#joining data frames and save database

ucs_us_all <- rbind(coral_occ2020_UCsUS_values_df, 
                    coral_occ2050_UCsUS_values_df,
                    coral_occ2100_UCsUS_values_df)
ucs_us_all <- ucs_us_all %>% na.omit()
ucs_us_all$temp <- as.factor(ucs_us_all$temp)
ucs_us_all$pxl <- rep(1:911, 3)
ucs_us_all_df <- write_csv(ucs_us_all, "base_tratada/ucs_us_all.csv")

#--------------Running permanova -------------
#
ucs_us_all <- read_csv("base_tratada/ucs_us_all.csv", 
                       col_types = cols(values = col_double(),
                                        temp = col_factor(),
                                        pxl = col_double()))

# the pernamova test
ucsus_aovp <- aovp(values ~ temp + pxl, ucs_us_all)
summary(ucsus_aovp)

# post hoc test
posthoc_ucus <- pairwisePermutationTest(values ~ temp + Error(pxl), ucs_us_all)
posthoc_ucus


# Violin plot
pirateplot((formula = values ~ temp), 
           data = ucs_us_all,
           #main = "AMPs de US entre tempos",
           xlab = "",
           ylab = "",
           ylim = c(0,1),
           theme = 0,
           pal = c("#FFE5CE", "#FDAE6B", "#E5550E"), 
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
