################################################ 
# Comparing coral occurrence probability       #
# between strict protected and sustainable use #
# MPAs  in current time                        #
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
coral_occ2020_UCsPI_mask <- raster ("dados_sig/ucs_novas/AMPs_pi/coral_occ2020_UCsPI_mask_ras.asc")
coral_occ2020_UCsPI_values <- getValues(coral_occ2020_UCsPI_mask)
hist(coral_occ2020_UCsPI_values)

#creating a data frame with the occurrence probabilities values
coral_occ2020_UCsPI_values_df <- as.data.frame(coral_occ2020_UCsPI_values)
coral_occ2020_UCsPI_values_df$catg <- "Proteção integral"
names(coral_occ2020_UCsPI_values_df) <- c("values", "catg")

#Probability values for sustainable use MPAs
coral_occ2020_UCsUS_mask <- raster ("dados_sig/ucs_novas/AMPs_us/coral_occ2020_UCsUS_mask.asc")
coral_occ2020_UCsUS_values <- getValues(coral_occ2020_UCsUS_mask)
hist(coral_occ2020_UCsUS_values)

#creating a data frame with the occurrence probabilities values
coral_occ2020_UCsUS_values_df <- as.data.frame(coral_occ2020_UCsUS_values)
coral_occ2020_UCsUS_values_df$catg <- "Uso sustentável"
names(coral_occ2020_UCsUS_values_df) <- c("values", "catg")

#joining data frames and save database

ucs_cat_2020 <- rbind(coral_occ2020_UCsPI_values_df,
                      coral_occ2020_UCsUS_values_df)
ucs_cat_2020 <- ucs_cat_2020 %>% na.omit()
ucs_cat_2020$catg <- as.factor(ucs_cat_2020$catg)

write_csv(ucs_cat_2020, "base_tratada/ucs_catg_020.csv")


##--------------Running permanova -------
#

ucs_cat_2020 <- read_csv("base_tratada/ucs_catg_020.csv",
                          col_types = cols(values = col_double(),                                  catg = col_factor())) 

# pernamova test
ucs20_aovp <- aovp(values ~ catg, ucs_cat_2020)
summary(ucs20_aovp)

# post hoc test
posthoc_ucscatg020 <- pairwisePermutationTest(values ~ catg, 
ucs_cat_2020)
posthoc_ucscatg020


# Violin plot
pirateplot((formula = values ~ catg), 
           data = ucs_cat_2020,
        #  main = "Diferença entrea as categorias de AMPs em 2020",
           xlab = "",
           ylab = "Occurence probability",
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
