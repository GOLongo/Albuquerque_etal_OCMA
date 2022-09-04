##################################################
# Extracting occurrence probability values for   #
# Amazonia MPAs                                  #
##################################################

#---releasing packages----
library(sf)
library(yarrr)
library(dplyr)
library(raster)
library(readr)
library(lmPerm)
library(rcompanion)
#----------

# opening ecoregions shapefile
ecorrg <-st_read(dsn = "dados_sig/ucs_novas/ecorreg/ecorr_disert.shp", layer = 'ecorr_disert', crs = 4326)
st_crs(ecorrg) 
plot(ecorrg$geometry)

#filtering amazonia ecoregion
amazonia <- dplyr::filter(ecorrg,  name_ecor == "Amazonia")
plot(amazonia$geometry)

#---------------------------------------------------
## opening MPAs shapefile of amazonia
#Strict Protection
ucspi_amazn <-st_read(dsn = "dados_sig/ucs_novas/ecorreg/amps_amazonia/amps_amaz_pi.shp", layer = "amps_amaz_pi", crs = 4326)
st_crs(ucspi_amazn) 
plot(ucspi_amazn$geometry)

#Sustainable Use
ucsus_amazn <-st_read(dsn = "dados_sig/ucs_novas/ecorreg/amps_amazonia/amps_amaz_us.shp", layer = "amps_amaz_us", crs = 4326)
st_crs(ucsus_amazn) 
plot(ucsus_amazn$geometry)

#Opening raster layer for probability of occurrence
#"coral_occurrence_2020"
coral_occ2020 <- raster ("dados_sig/Coral_Occurrence_Posterior_predictive_mean.asc")
crs(coral_occ2020) <- "+proj=longlat +datum=WGS84 +no_defs"
plot(coral_occ2020)

#Opening raster layer for probability of occurrence
#"coral_occurrence_2050"
coral_occ2050 <- raster ("dados_sig/2050/Coral_Occurrence_Posterior_predictive_mean_2050.asc")
crs(coral_occ2050) <- "+proj=longlat +datum=WGS84 +no_defs"
plot(coral_occ2050)

#Opening raster layer for probability of occurrence
#"coral_occurrence_2100"
coral_occ2100 <- raster ("dados_sig/2100/Coral_Occurrence_Posterior_predictive_mean_2100.asc")
crs(coral_occ2100) <- "+proj=longlat +datum=WGS84 +no_defs"
plot(coral_occ2100)


#---------Strict Protection------
#---extracting occurrence probability values 
#for SP MPAs in the Amazonia in 2020

# Cutting the raster from "coral_occurrence_2020" to ecoregion area
coral_occ2020_ucspi_amazn_crop <- crop(coral_occ2020, extent(amazonia))
plot(coral_occ2020_ucspi_amazn_crop)

# Converting MPAs' shapefile to raster
coral_occ2020_ucspi_amazn_ras <- rasterize(ucspi_amazn,
                                           coral_occ2020_ucspi_amazn_crop,
                                           getCover=TRUE)
coral_occ2020_ucspi_amazn_ras[
  coral_occ2020_ucspi_amazn_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2020_ucspi_amazn_mask <- mask(coral_occ2020_ucspi_amazn_crop,                                coral_occ2020_ucspi_amazn_ras,
    filename = "dados_sig/ucs_novas/ecorreg/amps_amazonia/coral_occ2020_ucspi_amazn_mask.asc")

plot(coral_occ2020_ucspi_amazn_mask)

hist(coral_occ2020_ucspi_amazn_mask)

#---extracting occurrence probability values----
#for SP MPAs in the Amazonia in 2050

# Cutting the raster from "coral_occurrence_2050" to ecoregion area
coral_occ2050_ucspi_amazn_crop <- crop(coral_occ2050, extent(amazonia))
plot(coral_occ2050_ucspi_amazn_crop)

# Converting MPAs' shapefile to raster
coral_occ2050_ucspi_amazn_ras <- rasterize(ucspi_amazn,
                                           coral_occ2050_ucspi_amazn_crop,
                                           getCover=TRUE)
coral_occ2050_ucspi_amazn_ras[
  coral_occ2050_ucspi_amazn_ras == 0] <- NA

#Extracting and Saving occurrence probability values within MPAs
coral_occ2050_ucspi_amazn_mask <- mask(coral_occ2050_ucspi_amazn_crop,                                   coral_occ2050_ucspi_amazn_ras,
       filename = "dados_sig/ucs_novas/ecorreg/amps_amazonia/coral_occ2050_ucspi_amazn_mask.asc")

plot(coral_occ2050_ucspi_amazn_mask)

hist(coral_occ2050_ucspi_amazn_mask)

#---extracting occurrence probability values----
#for SP MPAs in the Amazonia in 2100

# Cutting the raster from "coral_occurrence_2100" to ecoregion area
coral_occ2100_ucspi_amazn_crop <- crop(coral_occ2100, extent(amazonia))
plot(coral_occ2100_ucspi_amazn_crop)

# Converting MPAs' shapefile to raster
coral_occ2100_ucspi_amazn_ras <- rasterize(ucspi_amazn,
                                           coral_occ2100_ucspi_amazn_crop,
                                           getCover=TRUE)
coral_occ2100_ucspi_amazn_ras[
  coral_occ2100_ucspi_amazn_ras == 0] <- NA

#Extracting and Saving occurrence probability values within MPAs
coral_occ2100_ucspi_amazn_mask <- mask(coral_occ2100_ucspi_amazn_crop,
         coral_occ2100_ucspi_amazn_ras,
         filename = "dados_sig/ucs_novas/ecorreg/amps_amazonia/coral_occ2100_ucspi_amazn_mask.asc")

plot(coral_occ2100_ucspi_amazn_mask)

hist(coral_occ2100_ucspi_amazn_mask)

#--------------------Sustainable use-------------------------------

#---extracting occurrence probability values----
#for SP MPAs in the Amazon in 2020

# Cutting the raster from "coral_occurrence_2020" to ecoregion area
coral_occ2020_ucsus_amazn_crop <- crop(coral_occ2020, extent(amazonia))
plot(coral_occ2020_ucsus_amazn_crop)

# Converting MPAs' shapefile to raster
coral_occ2020_ucsus_amazn_ras <- rasterize(ucsus_amazn,
                                           coral_occ2020_ucsus_amazn_crop,
                                           getCover=TRUE)
coral_occ2020_ucsus_amazn_ras[
  coral_occ2020_ucsus_amazn_ras == 0] <- NA

#Extracting and Saving occurrence probability values within MPAs
coral_occ2020_ucsus_amazn_mask <- mask(coral_occ2020_ucsus_amazn_crop,                                coral_occ2020_ucsus_amazn_ras,
    filename = "dados_sig/ucs_novas/ecorreg/amps_amazonia/coral_occ2020_ucsus_amazn_mask.asc")

plot(coral_occ2020_ucsus_amazn_mask)

hist(coral_occ2020_ucsus_amazn_mask)

#---extracting occurrence probability values----
#for SP MPAs in the Amazon in 2050

# Cutting the raster from "coral_occurrence_2050" to ecoregion area
coral_occ2050_ucsus_amazn_crop <- crop(coral_occ2050, extent(amazonia))
plot(coral_occ2050_ucsus_amazn_crop)

# Converting MPAs' shapefile to raster
coral_occ2050_ucsus_amazn_ras <- rasterize(ucsus_amazn, 
                                           coral_occ2050_ucspi_amazn_crop, 
                                           getCover=TRUE)
coral_occ2050_ucsus_amazn_ras[
  coral_occ2050_ucsus_amazn_ras == 0] <- NA

#Extracting and Saving occurrence probability values within MPAs
coral_occ2050_ucsus_amazn_mask <- mask(coral_occ2050_ucsus_amazn_crop,                                   coral_occ2050_ucsus_amazn_ras,
       filename = "dados_sig/ucs_novas/ecorreg/amps_amazonia/coral_occ2050_ucsus_amazn_mask.asc")

plot(coral_occ2050_ucsus_amazn_mask)

hist(coral_occ2050_ucsus_amazn_mask)

#---extracting occurrence probability values----
#for SP MPAs in the Amazon in 2100

# Cutting the raster from "coral_occurrence_2100" to ecoregion area
coral_occ2100_ucsus_amazn_crop <- crop(coral_occ2100, extent(amazonia))
plot(coral_occ2100_ucsus_amazn_crop)

# Converting MPAs' shapefile to raster
coral_occ2100_ucsus_amazn_ras <- rasterize(ucsus_amazn,
                                           coral_occ2100_ucspi_amazn_crop,
                                           getCover=TRUE)
coral_occ2100_ucsus_amazn_ras[
  coral_occ2100_ucsus_amazn_ras == 0] <- NA

#Extracting and Saving occurrence probability values within MPAs
coral_occ2100_ucsus_amazn_mask <- mask(coral_occ2100_ucsus_amazn_crop,
                                       coral_occ2100_ucsus_amazn_ras,
                                       filename = "dados_sig/ucs_novas/ecorreg/amps_amazonia/coral_occ2100_ucsus_amazn_mask.asc")

plot(coral_occ2100_ucsus_amazn_mask)

hist(coral_occ2100_ucsus_amazn_mask)

#--------Preparing Data for Analysis----------------

#SP
#2020
coral_occ2020_ucspi_amazn_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_amazonia/coral_occ2020_ucspi_amazn_mask.asc")
coral_occ2020_ucspi_amazn_values <- getValues(coral_occ2020_ucspi_amazn_mask)
hist(coral_occ2020_ucspi_amazn_values)
coral_occ2020_ucspi_amazn_values_df <- as.data.frame(coral_occ2020_ucspi_amazn_values)
coral_occ2020_ucspi_amazn_values_df$temp <- 2020
names(coral_occ2020_ucspi_amazn_values_df) <- c("values", "temp")

#2050
coral_occ2050_ucspi_amazn_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_amazonia/coral_occ2050_ucspi_amazn_mask.asc")
coral_occ2050_ucspi_amazn_values <- getValues(coral_occ2050_ucspi_amazn_mask)
hist(coral_occ2050_ucspi_amazn_values)
coral_occ2050_ucspi_amazn_values_df <- as.data.frame(coral_occ2050_ucspi_amazn_values)
coral_occ2050_ucspi_amazn_values_df$temp <- 2050
names(coral_occ2050_ucspi_amazn_values_df) <- c("values", "temp")

#2100
coral_occ2100_ucspi_amazn_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_amazonia/coral_occ2100_ucspi_amazn_mask.asc")
coral_occ2100_ucspi_amazn_values <- getValues(coral_occ2100_ucspi_amazn_mask)
hist(coral_occ2100_ucspi_amazn_values)
coral_occ2100_ucspi_amazn_values_df <- as.data.frame(coral_occ2100_ucspi_amazn_values)
coral_occ2100_ucspi_amazn_values_df$temp <- 2100
names(coral_occ2100_ucspi_amazn_values_df) <- c("values", "temp")

#joining databases SP

ucspi_amazonia_df <- rbind(coral_occ2020_ucspi_amazn_values_df, coral_occ2050_ucspi_amazn_values_df, coral_occ2100_ucspi_amazn_values_df)
ucspi_amazonia_df <- ucspi_amazonia_df %>% na.omit()
ucspi_amazonia_df$temp <- as.factor(ucspi_amazonia_df$temp)
write_csv(ucspi_amazonia_df, "base_tratada/ucspi_amazonia.csv")
#----------------------------------

#SU
#2020
coral_occ2020_ucsus_amazn_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_amazonia/coral_occ2020_ucsus_amazn_mask.asc")
coral_occ2020_ucsus_amazn_values <- getValues(coral_occ2020_ucsus_amazn_mask)
hist(coral_occ2020_ucsus_amazn_values)
coral_occ2020_ucsus_amazn_values_df <- as.data.frame(coral_occ2020_ucsus_amazn_values)
coral_occ2020_ucsus_amazn_values_df$temp <- 2020
names(coral_occ2020_ucsus_amazn_values_df) <- c("values", "temp")

#2050
coral_occ2050_ucsus_amazn_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_amazonia/coral_occ2050_ucsus_amazn_mask.asc")
coral_occ2050_ucsus_amazn_values <- getValues(coral_occ2050_ucsus_amazn_mask)
hist(coral_occ2050_ucsus_amazn_values)
coral_occ2050_ucsus_amazn_values_df <- as.data.frame(coral_occ2050_ucsus_amazn_values)
coral_occ2050_ucsus_amazn_values_df$temp <- 2050
names(coral_occ2050_ucsus_amazn_values_df) <- c("values", "temp")

#2100
coral_occ2100_ucsus_amazn_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_amazonia/coral_occ2100_ucsus_amazn_mask.asc")
coral_occ2100_ucsus_amazn_values <- getValues(coral_occ2100_ucsus_amazn_mask)
hist(coral_occ2100_ucsus_amazn_values)
coral_occ2100_ucsus_amazn_values_df <- as.data.frame(coral_occ2100_ucsus_amazn_values)
coral_occ2100_ucsus_amazn_values_df$temp <- 2100
names(coral_occ2100_ucsus_amazn_values_df) <- c("values", "temp")

#joining databases SU

ucsus_amazonia_df <- rbind(coral_occ2020_ucsus_amazn_values_df, coral_occ2050_ucsus_amazn_values_df, coral_occ2100_ucsus_amazn_values_df)
ucsus_amazonia_df <- ucsus_amazonia_df %>% na.omit()
ucsus_amazonia_df$temp <- as.factor(ucsus_amazonia_df$temp)
write_csv(ucsus_amazonia_df, "base_tratada/ucsus_amazonia.csv")

#---permanova test SP
ucspi_amazonia_df <- read_csv("base_tratada/ucspi_amazonia.csv",
                              col_types = cols(
                                values = col_double(),
                                temp = col_factor()
                              )) %>% 
  mutate(pxl = rep(1:109, 3))

#----permanova
ucspi_amazonia_aovp <- aovp(values ~ temp + pxl, 
                            data = ucspi_amazonia_df)

summary(ucspi_amazonia_aovp)

#post hoc test
posthoc_ucspi_amazonia <- pairwisePermutationTest(values ~ temp + pxl, data = ucspi_amazonia_df)

posthoc_ucspi_amazonia

#plot

pirateplot((formula = values ~ temp), 
           data = ucspi_amazonia_df,
           main = "",
           xlab = "",
           ylab = "Occurrence probability",
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


#---------permanova SU--------------

ucsus_amazonia_df <- read_csv("base_tratada/ucsus_amazonia.csv",
                              col_types = cols(
                                values = col_double(),
                                temp = col_factor()
                              )) %>% 
  mutate(pxl = rep(1:263, 3))


#permanova
ucsus_amazonia_aovp <- aovp(values ~ temp + pxl, 
                            data = ucsus_amazonia_df)

summary(ucsus_amazonia_aovp)

#post hoc test
posthoc_ucsus_amazonia <- pairwisePermutationTest(values ~ temp + pxl, data = ucsus_amazonia_df)

posthoc_ucsus_amazonia

#plot

pirateplot((formula = values ~ temp), 
           data = ucsus_amazonia_df,
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
