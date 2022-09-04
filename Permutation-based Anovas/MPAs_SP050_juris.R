################################################ 
# Comparing coral occurrence probability       #
# between jurisdiction in strict protected     #
# MPAs  in 2040-2050                           #
################################################
#---releasing packages----
library(dplyr)
library(raster)
library(lmPerm)
library(rcompanion)
library(yarrr)
library(readr)

#------

#Probability values for federal jurisdiction
coral_occ2050_UCsPI_fdrl_mask <- raster ("dados_sig/2050/UCsPI/UCsPI_fdrl/coral_occ2050_UCsPI_fdrl_mask.asc")
coral_occ2050_UCsPI_fdrl_values <- getValues(
  coral_occ2050_UCsPI_fdrl_mask )
hist(coral_occ2050_UCsPI_fdrl_values)

#creating a data frame with the occurrence probabilities values
coral_occ2050_UCsPI_fdrl_values_df <- as.data.frame(coral_occ2050_UCsPI_fdrl_values)
coral_occ2050_UCsPI_fdrl_values_df$juris <- "Federal"
names(coral_occ2050_UCsPI_fdrl_values_df) <- c("values", "juris")

#Probability values for state jurisdiction

coral_occ2050_UCsPI_estd_mask <- raster ("dados_sig/2050/UCsPI/UCsPI_estd/coral_occ2050_UCsPI_estd_mask.asc")
coral_occ2050_UCsPI_estd_values <- getValues(
  coral_occ2050_UCsPI_estd_mask )
hist(coral_occ2050_UCsPI_estd_values)

#creating a data frame with the occurrence probabilities values
coral_occ2050_UCsPI_estd_values_df <- as.data.frame(coral_occ2050_UCsPI_estd_values)
coral_occ2050_UCsPI_estd_values_df$juris <- "Estadual"
names(coral_occ2050_UCsPI_estd_values_df) <- c("values", "juris")

#Probability values for municipal jurisdiction

coral_occ2050_UCsPI_muni_mask <- raster ("dados_sig/2050/UCsPI/UCsPI_muni/coral_occ2050_UCsPI_muni_mask.asc")
coral_occ2050_UCsPI_muni_values <- getValues(
  coral_occ2050_UCsPI_muni_mask )
hist(coral_occ2050_UCsPI_muni_values)

#creating a data frame with the occurrence probabilities values
coral_occ2050_UCsPI_muni_values_df <- as.data.frame(coral_occ2050_UCsPI_muni_values)
coral_occ2050_UCsPI_muni_values_df$juris <- "Municipal"
names(coral_occ2050_UCsPI_muni_values_df) <- c("values", "juris")

#joining data frames and save database

ucspi_juris_050 <- rbind(coral_occ2050_UCsPI_fdrl_values_df,
                         coral_occ2050_UCsPI_estd_values_df,
                         coral_occ2050_UCsPI_muni_values_df)

ucspi_juris_050 <- ucspi_juris_050 %>% na.omit()
write_csv(ucspi_juris_050, "base_tratada/ucspi_juris_050.csv")

##--------------Running permanova -------

ucspi_juris_050 <- read_csv("base_tratada/ucspi_juris_050.csv",
                         col_types = cols(values = col_double(),
                                          juris = col_factor()))

# pernamova test

ucspi50_juris_aovp <- aovp(values ~ juris, iter = 5000, ucspi_juris_050)
summary(ucspi50_juris_aovp)

# post hoc test
posthoc_ucspi50_juris <- pairwisePermutationTest(values ~ juris,
                                               ucspi_juris_050)
posthoc_ucspi50_juris


#----Violin plot----

pirateplot((formula = values ~ juris), 
           data = ucspi_juris_050,
           main = "",
           xlab = "",
           ylab = "",
           ylim = c(0,1),
           theme = 0,
           pal = c("#D40404", "#FF2F2F", "#F57926"), # southpark color palette
           bean.f.o = .8, # Bean fill
           bean.b.col = c("#D40404", "#FF2F2F", "#F57926"),
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
