################################################ 
# Comparing coral occurrence probability       #
# between jurisdiction in sustainable use      #
# MPAs  in current time                        #
################################################

#---releasing packages----
library(dplyr)
library(raster)
library(lmPerm)
library(rcompanion)
library(ggplot2)
library(readr)
library(yarrr)

#------

#Probability values for federal jurisdiction
coral_occ2020_UCsUS_fdrl_mask <- raster ("dados_sig/ucs_novas/AMPs_us/ucs_us_fdrl/coral_occ2020_UCsUS_fdrl_mask.asc")
coral_occ2020_UCsUS_values <- getValues(
  coral_occ2020_UCsUS_fdrl_mask )
hist(coral_occ2020_UCsUS_values)
coral_occ2020_UCsUS_fdrl_values_df <- as.data.frame(coral_occ2020_UCsUS_values)
coral_occ2020_UCsUS_fdrl_values_df$juris <- "Federal"
names(coral_occ2020_UCsUS_fdrl_values_df) <- c("values", "juris")

##Probability values for state jurisdiction

coral_occ2020_UCsUS_estd_mask <- raster ("dados_sig/ucs_novas/AMPs_us/ucs_us_estd/coral_occ2020_UCsUS_estd_mask.asc")
coral_occ2020_UCsUS_estd_values <- getValues(
  coral_occ2020_UCsUS_estd_mask )
hist(coral_occ2020_UCsUS_estd_values)

#creating a data frame with the occurrence probabilities values
coral_occ2020_UCsUS_estd_values_df <- as.data.frame(coral_occ2020_UCsUS_estd_values)
coral_occ2020_UCsUS_estd_values_df$juris <- "Estadual"
names(coral_occ2020_UCsUS_estd_values_df) <- c("values", "juris")

#Probability values for municipal jurisdiction

coral_occ2020_UCsUS_muni_mask <- raster ("dados_sig/ucs_novas/AMPs_us/ucs_us_muni/coral_occ2020_UCsUS_muni_mask.asc")
coral_occ2020_UCsUS_muni_values <- getValues(
  coral_occ2020_UCsUS_muni_mask )
hist(coral_occ2020_UCsUS_muni_values)

#creating a data frame with the occurrence probabilities values
coral_occ2020_UCsUS_muni_values_df <- as.data.frame(coral_occ2020_UCsUS_muni_values)
coral_occ2020_UCsUS_muni_values_df$juris <- "Municipal"
names(coral_occ2020_UCsUS_muni_values_df) <- c("values", "juris")

#joining data frames and save database

ucsus_juris_020 <- rbind(coral_occ2020_UCsUS_fdrl_values_df,
                         coral_occ2020_UCsUS_estd_values_df,
                         coral_occ2020_UCsUS_muni_values_df)

ucsus_juris_020 <- ucsus_juris_020 %>% na.omit()
write_csv(ucsus_juris_020, "base_tratada/ucsus_juris_020.csv")

##--------------Running permanova -------

ucsus_juris_020 <- read_csv("base_tratada/ucsus_juris_020.csv",
                      col_types = cols(values = col_double(),
                                       juris = col_factor()))

# pernamova test

ucsus20_juris_aovp <- aovp(values ~ juris, ucsus_juris_020)
summary(ucsus20_juris_aovp)

# post hoc test
posthoc_ucsus20_juris <- pairwisePermutationTest(values ~ juris,
                                                 ucsus_juris_020)
posthoc_ucsus20_juris


# Violin plot

pirateplot((formula = values ~ juris), 
           data = ucsus_juris_020,
           main = "",
           xlab = "",
           ylab = "",
           ylim = c(0,1),
           theme = 0,
           pal = c("#D5AC04", "#FECD04", "#FFE783"), # southpark color palette
           bean.f.o = .8, # Bean fill
           bean.b.col = c("#D5AC04", "#FECD04", "#FFE783"),
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

