################################################ 
# Comparing coral occurrence probability       #
# between strict protected and sustainable use #
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

#Probability values for strict protected MPAs
coral_occ2050_UCsPI_mask <- raster ("dados_sig/2050/UCsPI/coral_occ2050_UCsPI_mask_ras.asc")
coral_occ2050_UCsPI_values <- getValues(coral_occ2050_UCsPI_mask)
hist(coral_occ2050_UCsPI_values)

#creating a data frame with the occurrence probabilities values
coral_occ2050_UCsPI_values_df <- as.data.frame(coral_occ2050_UCsPI_values)
coral_occ2050_UCsPI_values_df$catg <- "Proteção integral"
names(coral_occ2050_UCsPI_values_df) <- c("values", "catg")


#Probability values for sustainable use MPAs
coral_occ2050_UCsUS_mask <- raster ("dados_sig/2050/UCsUS/coral_occ2050_UCsUS_mask.asc")
coral_occ2050_UCsUS_values <- getValues(coral_occ2050_UCsUS_mask)
hist(coral_occ2050_UCsUS_values)

#creating a data frame with the occurrence probabilities values
coral_occ2050_UCsUS_values_df <- as.data.frame(coral_occ2050_UCsUS_values)
coral_occ2050_UCsUS_values_df$catg <- "Uso sustentável"
names(coral_occ2050_UCsUS_values_df) <- c("values", "catg")

#joining data frames and save database

ucs_cat_2050 <- rbind(coral_occ2050_UCsPI_values_df,
                      coral_occ2050_UCsUS_values_df)
ucs_cat_2050 <- ucs_cat_2050 %>% na.omit()
write_csv(ucs_cat_2050, "base_tratada/ucs_catg_050.csv")

#--------------Running permanova -------

ucs_cat_050 <- read_csv("base_tratada/ucs_catg_050.csv",
                         col_types = cols(values = col_double(),                                  catg = col_factor())) 

# pernamova test
ucs050_aovp <- aovp(values ~ catg, ucs_cat_050)
summary(ucs050_aovp)

# post hoc test
posthoc_ucscatg050 <- pairwisePermutationTest(values ~ catg, 
                                              ucs_cat_050)
posthoc_ucscatg050

# Violin plot

pirateplot((formula = values ~ catg), 
           data = ucs_cat_050,
           main = "",
           xlab = "",
           ylab = "Occurrence probability",
           ylim = c(0,1),
           theme = 0,
           pal = c("#FF0302", "#FFDD57"), 
           bean.f.o = .8, # Bean fill
           bean.b.col = c("#FF0302", "#FFDD57"),
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