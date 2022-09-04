##################################################
# Extracting occurrence probability values for   #
# trindade MPAs                                  #
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

#filtering trindade ecoregion
trindade <- dplyr::filter(ecorrg, name_ecor == "Trindade")
plot(trindade$geometry)

#---------------------------------------------------
## opening MPAs shapefile of trindade
#Strict Protection
ucspi_trind <-st_read(dsn = "dados_sig/ucs_novas/ecorreg/amps_trind/amps_trind_pi.shp", layer = "amps_trind_pi", crs = 4326)
st_crs(ucspi_trind) 
plot(ucspi_trind$geometry)

#Sustainable Use
ucsus_trind <-st_read(dsn = "dados_sig/ucs_novas/ecorreg/amps_trind/amps_trind_us.shp", layer = "amps_trind_us", crs = 4326)
st_crs(ucsus_trind) 
plot(ucsus_trind$geometry)

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
#for SP MPAs in the trindade in 2020

# Cutting the raster from "coral_occurrence_2020" to ecoregion area
coral_occ2020_ucspi_trind_crop <- crop(coral_occ2020, extent(trindade))
plot(coral_occ2020_ucspi_trind_crop)

# Converting MPAs' shapefile to raster
coral_occ2020_ucspi_trind_ras <- rasterize(ucspi_trind, 
                                           coral_occ2020_ucspi_trind_crop,
                                           getCover=TRUE)

coral_occ2020_ucspi_trind_ras[
  coral_occ2020_ucspi_trind_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2020_ucspi_trind_mask <- mask(coral_occ2020_ucspi_trind_crop, coral_occ2020_ucspi_trind_ras,filename = "dados_sig/ucs_novas/ecorreg/amps_trind/coral_occ2020_ucspi_trind_mask.asc", overwrite = TRUE)

plot(coral_occ2020_ucspi_trind_mask)

hist(coral_occ2020_ucspi_trind_mask)

#---extracting occurrence probability values----
#for SP MPAs in the trindade in 2050

# Cutting the raster from "coral_occurrence_2050" to ecoregion area
coral_occ2050_ucspi_trind_crop <- crop(coral_occ2050, extent(trindade))
plot(coral_occ2050_ucspi_trind_crop)

# Converting MPAs' shapefile to raster
coral_occ2050_ucspi_trind_ras <- rasterize(ucspi_trind, 
                                           coral_occ2050_ucspi_trind_crop,
                                           getCover=TRUE)
coral_occ2050_ucspi_trind_ras[
  coral_occ2050_ucspi_trind_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2050_ucspi_trind_mask <- mask(coral_occ2050_ucspi_trind_crop, coral_occ2050_ucspi_trind_ras,filename = "dados_sig/ucs_novas/ecorreg/amps_trind/coral_occ2050_ucspi_trind_mask.asc", overwrite = TRUE)

plot(coral_occ2050_ucspi_trind_mask)

hist(coral_occ2050_ucspi_trind_mask)

#---extracting occurrence probability values----
#for SP MPAs in the trindade in 2100

# Cutting the raster from "coral_occurrence_2100" to ecoregion area
coral_occ2100_ucspi_trind_crop <- crop(coral_occ2100, extent(trindade))
plot(coral_occ2100_ucspi_trind_crop)

# Converting MPAs' shapefile to raster
coral_occ2100_ucspi_trind_ras <- rasterize(ucspi_trind, 
                                           coral_occ2100_ucspi_trind_crop,
                                           getCover=TRUE)

coral_occ2100_ucspi_trind_ras[
  coral_occ2100_ucspi_trind_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2100_ucspi_trind_mask <- mask(coral_occ2100_ucspi_trind_crop,
                                      coral_occ2100_ucspi_trind_ras,
                                      filename = "dados_sig/ucs_novas/ecorreg/amps_trind/coral_occ2100_ucspi_trind_mask.asc", overwrite = TRUE)

plot(coral_occ2100_ucspi_trind_mask)

hist(coral_occ2100_ucspi_trind_mask)


#--------------------Sustainable use-------------------------------

#---extracting occurrence probability values----
#for SU MPAs in the trindade in 2020

# Cutting the raster from "coral_occurrence_2020" to ecoregion area
coral_occ2020_ucsus_trind_crop <- crop(coral_occ2020, extent(trindade))
plot(coral_occ2020_ucsus_trind_crop)

# Converting MPAs' shapefile to raster
coral_occ2020_ucsus_trind_ras <- rasterize(ucsus_trind, 
                                           coral_occ2020_ucsus_trind_crop,
                                           getCover=TRUE)
coral_occ2020_ucsus_trind_ras[
  coral_occ2020_ucsus_trind_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2020_ucsus_trind_mask <- mask(coral_occ2020_ucsus_trind_crop, coral_occ2020_ucsus_trind_ras,filename = "dados_sig/ucs_novas/ecorreg/amps_trind/coral_occ2020_ucsus_trind_mask.asc", overwrite = TRUE)

plot(coral_occ2020_ucsus_trind_mask)

hist(coral_occ2020_ucsus_trind_mask)

#---extracting occurrence probability values----
#for SU MPAs in the trindade in 2050

# Cutting the raster from "coral_occurrence_2050" to ecoregion area
coral_occ2050_ucsus_trind_crop <- crop(coral_occ2050, extent(trindade))
plot(coral_occ2050_ucsus_trind_crop)

# Converting MPAs' shapefile to raster
coral_occ2050_ucsus_trind_ras <- rasterize(ucsus_trind, 
                                           coral_occ2050_ucsus_trind_crop, 
                                           getCover=TRUE)
coral_occ2050_ucsus_trind_ras[
  coral_occ2050_ucsus_trind_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2050_ucsus_trind_mask <- mask(coral_occ2050_ucsus_trind_crop, coral_occ2050_ucsus_trind_ras,filename = "dados_sig/ucs_novas/ecorreg/amps_trind/coral_occ2050_ucsus_trind_mask.asc", overwrite = TRUE)

plot(coral_occ2050_ucsus_trind_mask)

hist(coral_occ2050_ucsus_trind_mask)

#---extracting occurrence probability values----
#for SU MPAs in the trindade in 2100

# Cutting the raster from "coral_occurrence_2100" to ecoregion area
coral_occ2100_ucsus_trind_crop <- crop(coral_occ2100, extent(trindade))
plot(coral_occ2100_ucsus_trind_crop)

# Converting MPAs' shapefile to raster
coral_occ2100_ucsus_trind_ras <- rasterize(ucsus_trind, 
                                           coral_occ2100_ucsus_trind_crop, 
                                           getCover=TRUE)
coral_occ2100_ucsus_trind_ras[
  coral_occ2100_ucsus_trind_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2100_ucsus_trind_mask <- mask(coral_occ2100_ucsus_trind_crop,
                                      coral_occ2100_ucsus_trind_ras,
                                      filename = "dados_sig/ucs_novas/ecorreg/amps_trind/coral_occ2100_ucsus_trind_mask.asc", overwrite = TRUE)

plot(coral_occ2100_ucsus_trind_mask)

hist(coral_occ2100_ucsus_trind_mask)


#--------Preparing Data for Analysis----------------

#SP
#2020
coral_occ2020_ucspi_trind_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_trind/coral_occ2020_ucspi_trind_mask.asc")
coral_occ2020_ucspi_trind_values <- getValues(coral_occ2020_ucspi_trind_mask)
hist(coral_occ2020_ucspi_trind_values)
coral_occ2020_ucspi_trind_values_df <- as.data.frame(coral_occ2020_ucspi_trind_values)
coral_occ2020_ucspi_trind_values_df$temp <- 2020
names(coral_occ2020_ucspi_trind_values_df) <- c("values", "temp")

#2050
coral_occ2050_ucspi_trind_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_trind/coral_occ2050_ucspi_trind_mask.asc")
coral_occ2050_ucspi_trind_values <- getValues(coral_occ2050_ucspi_trind_mask)
hist(coral_occ2050_ucspi_trind_values)
coral_occ2050_ucspi_trind_values_df <- as.data.frame(coral_occ2050_ucspi_trind_values)
coral_occ2050_ucspi_trind_values_df$temp <- 2050
names(coral_occ2050_ucspi_trind_values_df) <- c("values", "temp")

#2100
coral_occ2100_ucspi_trind_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_trind/coral_occ2100_ucspi_trind_mask.asc")
coral_occ2100_ucspi_trind_values <- getValues(coral_occ2100_ucspi_trind_mask)
hist(coral_occ2100_ucspi_trind_values)
coral_occ2100_ucspi_trind_values_df <- as.data.frame(coral_occ2100_ucspi_trind_values)
coral_occ2100_ucspi_trind_values_df$temp <- 2100
names(coral_occ2100_ucspi_trind_values_df) <- c("values", "temp")


#----------------------------------

#SU
#2020
coral_occ2020_ucsus_trind_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_trind/coral_occ2020_ucsus_trind_mask.asc")
coral_occ2020_ucsus_trind_values <- getValues(coral_occ2020_ucsus_trind_mask)
hist(coral_occ2020_ucsus_trind_values)
coral_occ2020_ucsus_trind_values_df <- as.data.frame(coral_occ2020_ucsus_trind_values)
coral_occ2020_ucsus_trind_values_df$temp <- 2020
names(coral_occ2020_ucsus_trind_values_df) <- c("values", "temp")

#2050
coral_occ2050_ucsus_trind_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_trind/coral_occ2050_ucsus_trind_mask.asc")
coral_occ2050_ucsus_trind_values <- getValues(coral_occ2050_ucsus_trind_mask)
hist(coral_occ2050_ucsus_trind_values)
coral_occ2050_ucsus_trind_values_df <- as.data.frame(coral_occ2050_ucsus_trind_values)
coral_occ2050_ucsus_trind_values_df$temp <- 2050
names(coral_occ2050_ucsus_trind_values_df) <- c("values", "temp")

#2100
coral_occ2100_ucsus_trind_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_trind/coral_occ2100_ucsus_trind_mask.asc")
coral_occ2100_ucsus_trind_values <- getValues(coral_occ2100_ucsus_trind_mask)
hist(coral_occ2100_ucsus_trind_values)
coral_occ2100_ucsus_trind_values_df <- as.data.frame(coral_occ2100_ucsus_trind_values)
coral_occ2100_ucsus_trind_values_df$temp <- 2100
names(coral_occ2100_ucsus_trind_values_df) <- c("values", "temp")

#joining databases SP
ucspi_trindade_df <- rbind(coral_occ2020_ucspi_trind_values_df, coral_occ2050_ucspi_trind_values_df, coral_occ2100_ucspi_trind_values_df)
ucspi_trindade_df <- ucspi_trindade_df %>% na.omit()
ucspi_trindade_df$temp <- as.factor(ucspi_trindade_df$temp)
write_csv(ucspi_trindade_df, "base_tratada/ucspi_trind.csv")

#joining databases SU
ucsus_trindade_df <- rbind(coral_occ2020_ucsus_trind_values_df, coral_occ2050_ucsus_trind_values_df, coral_occ2100_ucsus_trind_values_df)
ucsus_trindade_df <- ucsus_trindade_df %>% na.omit()
ucsus_trindade_df$temp <- as.factor(ucsus_trindade_df$temp)
write_csv(ucsus_trindade_df, "base_tratada/ucsus_trind.csv")

#---permanova test
#data SP
ucspi_trindade_df <- read_csv("base_tratada/ucspi_trind.csv",
                            col_types = cols(
                              values = col_double(),
                              temp = col_factor())) %>% 
  mutate(pxl = rep(1:9, 3)) %>% 
  mutate(grp = "pi")

#data SU
ucsus_trindade_df <- read_csv("base_tratada/ucsus_trind.csv",
                            col_types = cols(
                              values = col_double(),
                              temp = col_factor())) %>% 
  mutate(pxl = rep(1:2, 3)) %>% 
  mutate(grp = "us")

#---permanova test SP
ucspi_trindade_aovp <- aovp(values ~ temp + pxl, 
                            data = ucspi_trindade_df)
summary(ucspi_trindade_aovp)

#post hoc test
(ucspi_trindade_paiw <- pairwisePermutationTest(values ~ temp, data = ucspi_trindade_df))

#---permanova test SU
ucsus_trindade_aovp <- aovp(values ~ temp, 
                            data = ucsus_trindade_df)
summary(ucsus_trindade_aovp)

#post hoc test
(ucsus_trindade_paiw <- pairwisePermutationTest(values ~ temp, data = ucsus_trindade_df))




#--------------plots------------------
#SP
pirateplot((formula = values ~ temp), 
           data = ucspi_trindade_df,
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
           data = ucsus_trindade_df,
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

