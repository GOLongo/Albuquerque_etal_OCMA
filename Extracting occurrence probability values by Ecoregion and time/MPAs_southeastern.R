##################################################
# Extracting occurrence probability values for   #
# southeastern MPAs                                  #
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

#filtering southeastern ecoregion
southsthern <- dplyr::filter(ecorrg,  name_ecor == "Sudeste")
plot(southsouthern$geometry)

#---------------------------------------------------
## opening MPAs shapefile of southeastern
#Strict Protection
ucspi_south <-st_read(dsn = "dados_sig/ucs_novas/ecorreg/amps_sudst/amps_sudst_pi.shp", layer = "amps_sudst_pi", crs = 4326)
st_crs(ucspi_south) 
plot(ucspi_south$geometry)

#Sustainable Use
ucsus_south <-st_read(dsn = "dados_sig/ucs_novas/ecorreg/amps_sudst/amps_sudst_us.shp", layer = "amps_sudst_us", crs = 4326)
st_crs(ucsus_south) 
plot(ucsus_south$geometry)

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
#for SP MPAs in the southeastern in 2020

# Cutting the raster from "coral_occurrence_2020" to ecoregion area
coral_occ2020_ucspi_south_crop <- crop(coral_occ2020, extent(southsthern))
plot(coral_occ2020_ucspi_south_crop)

# Converting MPAs' shapefile to raster
coral_occ2020_ucspi_south_ras <- rasterize(ucspi_south,
                                           coral_occ2020_ucspi_south_crop, 
                                           getCover=TRUE)
coral_occ2020_ucspi_south_ras[
  coral_occ2020_ucspi_south_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2020_ucspi_south_mask <- mask(coral_occ2020_ucspi_south_crop, coral_occ2020_ucspi_south_ras,filename = "dados_sig/ucs_novas/ecorreg/amps_sudst/coral_occ2020_ucspi_south_mask.asc")

plot(coral_occ2020_ucspi_south_mask)

hist(coral_occ2020_ucspi_south_mask)

#---extracting occurrence probability values----
#for SP MPAs in the southeastern in 2050

# Cutting the raster from "coral_occurrence_2050" to ecoregion area
coral_occ2050_ucspi_south_crop <- crop(coral_occ2050, extent(southsthern))
plot(coral_occ2050_ucspi_south_crop)

# Converting MPAs' shapefile to raster
coral_occ2050_ucspi_south_ras <- rasterize(ucspi_south,
                                           coral_occ2050_ucspi_south_crop,
                                           getCover=TRUE)

coral_occ2050_ucspi_south_ras[
  coral_occ2050_ucspi_south_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2050_ucspi_south_mask <- mask(coral_occ2050_ucspi_south_crop, coral_occ2050_ucspi_south_ras,filename = "dados_sig/ucs_novas/ecorreg/amps_sudst/coral_occ2050_ucspi_south_mask.asc")

plot(coral_occ2050_ucspi_south_mask)

hist(coral_occ2050_ucspi_south_mask)

#---extracting occurrence probability values----
#for SP MPAs in the southeastern in 2100

# Cutting the raster from "coral_occurrence_2100" to ecoregion area
coral_occ2100_ucspi_south_crop <- crop(coral_occ2100, extent(southsthern))
plot(coral_occ2100_ucspi_south_crop)

# Converting MPAs' shapefile to raster
coral_occ2100_ucspi_south_ras <- rasterize(ucspi_south, 
                                           coral_occ2100_ucspi_south_crop, 
                                           getCover=TRUE)
coral_occ2100_ucspi_south_ras[
  coral_occ2100_ucspi_south_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2100_ucspi_south_mask <- mask(coral_occ2100_ucspi_south_crop,
                                      coral_occ2100_ucspi_south_ras,
                                      filename = "dados_sig/ucs_novas/ecorreg/amps_sudst/coral_occ2100_ucspi_south_mask.asc")

plot(coral_occ2100_ucspi_south_mask)

hist(coral_occ2100_ucspi_south_mask)

#--------Preparing Data for Analysis----------------

#SP
#2020
coral_occ2020_ucspi_south_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_sudst/coral_occ2020_ucspi_south_mask.asc")
coral_occ2020_ucspi_south_values <- getValues(coral_occ2020_ucspi_south_mask)
hist(coral_occ2020_ucspi_south_values)
coral_occ2020_ucspi_south_values_df <- as.data.frame(coral_occ2020_ucspi_south_values)
coral_occ2020_ucspi_south_values_df$temp <- 2020
names(coral_occ2020_ucspi_south_values_df) <- c("values", "temp")

#2050
coral_occ2050_ucspi_south_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_sudst/coral_occ2050_ucspi_south_mask.asc")
coral_occ2050_ucspi_south_values <- getValues(coral_occ2050_ucspi_south_mask)
hist(coral_occ2050_ucspi_south_values)
coral_occ2050_ucspi_south_values_df <- as.data.frame(coral_occ2050_ucspi_south_values)
coral_occ2050_ucspi_south_values_df$temp <- 2050
names(coral_occ2050_ucspi_south_values_df) <- c("values", "temp")

#2100
coral_occ2100_ucspi_south_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_sudst/coral_occ2100_ucspi_south_mask.asc")
coral_occ2100_ucspi_south_values <- getValues(coral_occ2100_ucspi_south_mask)
hist(coral_occ2100_ucspi_south_values)
coral_occ2100_ucspi_south_values_df <- as.data.frame(coral_occ2100_ucspi_south_values)
coral_occ2100_ucspi_south_values_df$temp <- 2100
names(coral_occ2100_ucspi_south_values_df) <- c("values", "temp")

#joining databases SP

ucspi_southeastern_df <- rbind(coral_occ2020_ucspi_south_values_df, coral_occ2050_ucspi_south_values_df, coral_occ2100_ucspi_south_values_df)
ucspi_southeastern_df <- ucspi_southeastern_df %>% na.omit()
ucspi_southeastern_df$temp <- as.factor(ucspi_southeastern_df$temp)
write_csv(ucspi_southeastern_df, "base_tratada/ucspi_southeastern.csv")
#---------------------------------------------------

#--------------------Sustainable use-------------------------------

#---extracting occurrence probability values----
#for SP MPAs in the southeastern in 2020

# Cutting the raster from "coral_occurrence_2020" to ecoregion area
coral_occ2020_ucsus_south_crop <- crop(coral_occ2020, extent(southsthern))
plot(coral_occ2020_ucsus_south_crop)

# Converting MPAs' shapefile to raster
coral_occ2020_ucsus_south_ras <- rasterize(ucsus_south,
                                           coral_occ2020_ucsus_south_crop,
                                           getCover=TRUE)

coral_occ2020_ucsus_south_ras[
  coral_occ2020_ucsus_south_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2020_ucsus_south_mask <- mask(coral_occ2020_ucsus_south_crop, coral_occ2020_ucsus_south_ras,filename = "dados_sig/ucs_novas/ecorreg/amps_sudst/coral_occ2020_ucsus_south_mask.asc")

plot(coral_occ2020_ucsus_south_mask)

hist(coral_occ2020_ucsus_south_mask)

#---extracting occurrence probability values----
#for SP MPAs in the southeastern in 2050

# Cutting the raster from "coral_occurrence_2050" to ecoregion area
coral_occ2050_ucsus_south_crop <- crop(coral_occ2050, extent(southsthern))
plot(coral_occ2050_ucsus_south_crop)

# Converting MPAs' shapefile to raster
coral_occ2050_ucsus_south_ras <- rasterize(ucsus_south, 
                                           coral_occ2050_ucspi_south_crop, 
                                           getCover=TRUE)
coral_occ2050_ucsus_south_ras[
  coral_occ2050_ucsus_south_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2050_ucsus_south_mask <- mask(coral_occ2050_ucsus_south_crop, coral_occ2050_ucsus_south_ras,filename = "dados_sig/ucs_novas/ecorreg/amps_sudst/coral_occ2050_ucsus_south_mask.asc")

plot(coral_occ2050_ucsus_south_mask)

hist(coral_occ2050_ucsus_south_mask)

#---extracting occurrence probability values----
#for SP MPAs in the southeastern in 2100

# Cutting the raster from "coral_occurrence_2100" to ecoregion area
coral_occ2100_ucsus_south_crop <- crop(coral_occ2100, extent(southsthern))
plot(coral_occ2100_ucsus_south_crop)

# Converting MPAs' shapefile to raster
coral_occ2100_ucsus_south_ras <- rasterize(ucsus_south,
                                           coral_occ2100_ucsus_south_crop, 
                                           getCover=TRUE)

coral_occ2100_ucsus_south_ras[
  coral_occ2100_ucsus_south_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2100_ucsus_south_mask <- mask(coral_occ2100_ucsus_south_crop,
                                      coral_occ2100_ucsus_south_ras,
                                      filename = "dados_sig/ucs_novas/ecorreg/amps_sudst/coral_occ2100_ucsus_south_mask.asc")

plot(coral_occ2100_ucsus_south_mask)

hist(coral_occ2100_ucsus_south_mask)

#----------------------------------

#SU
#2020
coral_occ2020_ucsus_south_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_sudst/coral_occ2020_ucsus_south_mask.asc")
coral_occ2020_ucsus_south_values <- getValues(coral_occ2020_ucsus_south_mask)
hist(coral_occ2020_ucsus_south_values)
coral_occ2020_ucsus_south_values_df <- as.data.frame(coral_occ2020_ucsus_south_values)
coral_occ2020_ucsus_south_values_df$temp <- 2020
names(coral_occ2020_ucsus_south_values_df) <- c("values", "temp")

#2050
coral_occ2050_ucsus_south_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_sudst/coral_occ2050_ucsus_south_mask.asc")
coral_occ2050_ucsus_south_values <- getValues(coral_occ2050_ucsus_south_mask)
hist(coral_occ2050_ucsus_south_values)
coral_occ2050_ucsus_south_values_df <- as.data.frame(coral_occ2050_ucsus_south_values)
coral_occ2050_ucsus_south_values_df$temp <- 2050
names(coral_occ2050_ucsus_south_values_df) <- c("values", "temp")

#2100
coral_occ2100_ucsus_south_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_sudst/coral_occ2100_ucsus_south_mask.asc")
coral_occ2100_ucsus_south_values <- getValues(coral_occ2100_ucsus_south_mask)
hist(coral_occ2100_ucsus_south_values)
coral_occ2100_ucsus_south_values_df <- as.data.frame(coral_occ2100_ucsus_south_values)
coral_occ2100_ucsus_south_values_df$temp <- 2100
names(coral_occ2100_ucsus_south_values_df) <- c("values", "temp")

#joining databases SU

ucsus_southeastern_df <- rbind(coral_occ2020_ucsus_south_values_df, coral_occ2050_ucsus_south_values_df, coral_occ2100_ucsus_south_values_df)
ucsus_southeastern_df <- ucsus_southeastern_df %>% na.omit()
ucsus_southeastern_df$temp <- as.factor(ucsus_southeastern_df$temp)
write_csv(ucsus_southeastern_df, "base_tratada/ucsus_southeastern.csv")
#----------------------

#---permanova test SP
ucspi_southeastern_df <- read_csv("base_tratada/ucspi_southeastern.csv",
                  col_types = cols(values = col_double(),                                           temp = col_factor() )) %>%
  mutate( pixel = rep(1:109, 3) ) %>% 
  mutate(grp = "pi")



#permanova
ucspi_southeastern_aovp <- aovp(values ~ temp + pixel, 
                                ucspi_southeastern_df)
summary(ucspi_southeastern_aovp)


#post hoc test
posthoc_ucspisouth <- pairwisePermutationTest(values ~ temp + Error(pixel), ucspi_southeastern_df)
posthoc_ucspisouth


#---------permanova SU--------------

ucsus_southeastern_df <- read_csv("base_tratada/ucsus_southeastern.csv",
                   col_types = cols(values = col_double(),
                                    temp = col_factor())) %>%
  mutate( pixel = rep(1:273, 3) ) %>% 
  mutate(grp = "us")



#permanova
ucsus_southeastern_aovp <- aovp(values ~ temp + pixel, 
                                ucsus_southeastern_df)
summary(ucsus_southeastern_aovp)


#post hoc test
posthoc_ucsussouth <- pairwisePermutationTest(values ~ temp + pixel, ucsus_southeastern_df)
posthoc_ucsussouth


#plot

#SP
pirateplot((formula = values ~ temp), 
           data = ucspi_southeastern_df,
           main = "",
           xlab = "",
           ylab = "Probabilidades de ocorrência",
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


#SU
pirateplot((formula = values ~ temp), 
           data = ucsus_southeastern_df,
           main = "",
           xlab = "",
           ylab = "",
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

