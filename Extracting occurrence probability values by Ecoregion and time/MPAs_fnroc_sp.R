##################################################
# Extracting occurrence probability values for   #
# Noronha, Rocas, SPSP MPAs                                  #
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

#filtering Noronha, Rocas, SPSP ecoregion
fn_roc <- dplyr::filter(ecorrg,  name_ecor == "Noronha")
plot(fn_roc$geometry)

#---------------------------------------------------
##---------------------------------------------------
## opening MPAs shapefile of Noronha, Rocas, SPSP
#Strict Protection
ucspi_fnroc <-st_read(dsn = "dados_sig/ucs_novas/ecorreg/amps_noronha/amps_noronha_pi.shp", layer = "amps_noronha_pi", crs = 4326)
st_crs(ucspi_fnroc) 
plot(ucspi_fnroc$geometry)

#Sustainable Use
ucsus_fnroc <-st_read(dsn = "dados_sig/ucs_novas/ecorreg/amps_noronha/amps_noronha_us.shp", layer = "amps_noronha_us", crs = 4326)
st_crs(ucsus_fnroc) 
plot(ucsus_fnroc$geometry)

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
#for SP MPAs in the Noronha, Rocas, SPSP in 2020

# Cutting the raster from "coral_occurrence_2020" to ecoregion area
coral_occ2020_ucspi_fnroc_crop <- crop(coral_occ2020, extent(fn_roc))
plot(coral_occ2020_ucspi_fnroc_crop)

coral_occ2020_ucspi_fnroc_ras <- rasterize(ucspi_fnroc,
                                           coral_occ2020_ucspi_fnroc_crop,
                                           getCover=TRUE)

# Converting MPAs' shapefile to raster
coral_occ2020_ucspi_fnroc_ras[ 
  coral_occ2020_ucspi_fnroc_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2020_ucspi_fnroc_mask <- mask(coral_occ2020_ucspi_fnroc_crop,
    coral_occ2020_ucspi_fnroc_ras,
    filename = "dados_sig/ucs_novas/ecorreg/amps_noronha/coral_occ2020_ucspi_fnroc_mask.asc", overwrite=TRUE)

plot(coral_occ2020_ucspi_fnroc_mask)

hist(coral_occ2020_ucspi_fnroc_mask)

#---extracting occurrence probability values----
#for SP MPAs in the Noronha, Rocas, SPSP in 2050

# Cutting the raster from "coral_occurrence_2050" to ecoregion area
coral_occ2050_ucspi_fnroc_crop <- crop(coral_occ2050, extent(fn_roc))
plot(coral_occ2050_ucspi_fnroc_crop)

# Converting MPAs' shapefile to raster
coral_occ2050_ucspi_fnroc_ras <- rasterize(ucspi_fnroc,
                                           coral_occ2050_ucspi_fnroc_crop,
                                           getCover=TRUE)

#Extracting and Saving occurrence probability values within MPAs
coral_occ2050_ucspi_fnroc_ras[
  coral_occ2050_ucspi_fnroc_ras == 0] <- NA

coral_occ2050_ucspi_fnroc_mask <- mask(coral_occ2050_ucspi_fnroc_crop,
                                       coral_occ2050_ucspi_fnroc_ras,
                                       filename = "dados_sig/ucs_novas/ecorreg/amps_noronha/coral_occ2050_ucspi_fnroc_mask.asc", overwrite = TRUE)

plot(coral_occ2050_ucspi_fnroc_mask)

hist(coral_occ2050_ucspi_fnroc_mask)

#---extracting occurrence probability values----
#for SP MPAs in the Noronha, Rocas, SPSP in 2100
coral_occ2100_ucspi_fnroc_crop <- crop(coral_occ2100, extent(fn_roc))
plot(coral_occ2100_ucspi_fnroc_crop)

# Converting MPAs' shapefile to raster
coral_occ2100_ucspi_fnroc_ras <- rasterize(ucspi_fnroc, 
                                           coral_occ2100_ucspi_fnroc_crop,
                                           getCover=TRUE)
coral_occ2100_ucspi_fnroc_ras[
  coral_occ2100_ucspi_fnroc_ras == 0] <- NA

#Extracting and Saving occurrence probability values within MPAs
coral_occ2100_ucspi_fnroc_mask <- mask(coral_occ2100_ucspi_fnroc_crop,
                                      coral_occ2100_ucspi_fnroc_ras,
                                      filename = "dados_sig/ucs_novas/ecorreg/amps_noronha/coral_occ2100_ucspi_fnroc_mask.asc", overwrite = TRUE)

plot(coral_occ2100_ucspi_fnroc_mask)

hist(coral_occ2100_ucspi_fnroc_mask)


#--------------------Sustainable use-------------------------------

#---extracting occurrence probability values----
#for SP MPAs in the Noronha, Rocas, SPSP in 2020

# Cutting the raster from "coral_occurrence_2020" to ecoregion area

coral_occ2020_ucsus_fnroc_crop <- crop(coral_occ2020, extent(fn_roc))
plot(coral_occ2020_ucsus_fnroc_crop)

# Converting MPAs' shapefile to raster
coral_occ2020_ucsus_fnroc_ras <- rasterize(ucsus_fnroc, 
                                           coral_occ2020_ucsus_fnroc_crop,
                                           getCover=TRUE)
coral_occ2020_ucsus_fnroc_ras[
  coral_occ2020_ucsus_fnroc_ras == 0] <- NA

#Extracting and Saving occurrence probability values within MPAs
coral_occ2020_ucsus_fnroc_mask <- mask(coral_occ2020_ucsus_fnroc_crop, coral_occ2020_ucsus_fnroc_ras,filename = "dados_sig/ucs_novas/ecorreg/amps_noronha/coral_occ2020_ucsus_fnroc_mask.asc", overwrite = TRUE)

plot(coral_occ2020_ucsus_fnroc_mask)

hist(coral_occ2020_ucsus_fnroc_mask)

#---extracting occurrence probability values----
#for SP MPAs in the Noronha, Rocas, SPSP in 2050

# Cutting the raster from "coral_occurrence_2050" to ecoregion area
coral_occ2050_ucsus_fnroc_crop <- crop(coral_occ2050, extent(fn_roc))
plot(coral_occ2050_ucsus_fnroc_crop)

# Converting MPAs' shapefile to raster
coral_occ2050_ucsus_fnroc_ras <- rasterize(ucsus_fnroc,
                                           coral_occ2050_ucsus_fnroc_crop,
                                           getCover=TRUE)
coral_occ2050_ucsus_fnroc_ras[
  coral_occ2050_ucsus_fnroc_ras == 0] <- NA

#Extracting and Saving occurrence probability values within MPAs
coral_occ2050_ucsus_fnroc_mask <- mask(coral_occ2050_ucsus_fnroc_crop, coral_occ2050_ucsus_fnroc_ras,filename = "dados_sig/ucs_novas/ecorreg/amps_noronha/coral_occ2050_ucsus_fnroc_mask.asc", overwrite = TRUE)

plot(coral_occ2050_ucsus_fnroc_mask)

hist(coral_occ2050_ucsus_fnroc_mask)

#---extracting occurrence probability values----
#for SP MPAs in the Noronha, Rocas, SPSP in 2100

# Cutting the raster from "coral_occurrence_2100" to ecoregion area
coral_occ2100_ucsus_fnroc_crop <- crop(coral_occ2100, extent(fn_roc))
plot(coral_occ2100_ucsus_fnroc_crop)

# Converting MPAs' shapefile to raster
coral_occ2100_ucsus_fnroc_ras <- rasterize(ucsus_fnroc, 
                                           coral_occ2100_ucsus_fnroc_crop, 
                                           getCover=TRUE)
coral_occ2100_ucsus_fnroc_ras[
  coral_occ2100_ucsus_fnroc_ras == 0] <- NA

#Extracting and Saving occurrence probability values within MPAs
coral_occ2100_ucsus_fnroc_mask <- mask(coral_occ2100_ucsus_fnroc_crop,
      coral_occ2100_ucsus_fnroc_ras,
      filename = "dados_sig/ucs_novas/ecorreg/amps_noronha/coral_occ2100_ucsus_fnroc_mask.asc", overwrite = TRUE)

plot(coral_occ2100_ucsus_fnroc_mask)

hist(coral_occ2100_ucsus_fnroc_mask)


#--------Preparing Data for Analysis----------------

#SP
#2020
coral_occ2020_ucspi_fnroc_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_noronha/coral_occ2020_ucspi_fnroc_mask.asc")
coral_occ2020_ucspi_fnroc_values <- getValues(coral_occ2020_ucspi_fnroc_mask)
hist(coral_occ2020_ucspi_fnroc_values)
coral_occ2020_ucspi_fnroc_values_df <- as.data.frame(coral_occ2020_ucspi_fnroc_values)
coral_occ2020_ucspi_fnroc_values_df$temp <- 2020
names(coral_occ2020_ucspi_fnroc_values_df) <- c("values", "temp")

#2050
coral_occ2050_ucspi_fnroc_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_noronha/coral_occ2050_ucspi_fnroc_mask.asc")
coral_occ2050_ucspi_fnroc_values <- getValues(coral_occ2050_ucspi_fnroc_mask)
hist(coral_occ2050_ucspi_fnroc_values)
coral_occ2050_ucspi_fnroc_values_df <- as.data.frame(coral_occ2050_ucspi_fnroc_values)
coral_occ2050_ucspi_fnroc_values_df$temp <- 2050
names(coral_occ2050_ucspi_fnroc_values_df) <- c("values", "temp")

#2100
coral_occ2100_ucspi_fnroc_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_noronha/coral_occ2100_ucspi_fnroc_mask.asc")
coral_occ2100_ucspi_fnroc_values <- getValues(coral_occ2100_ucspi_fnroc_mask)
hist(coral_occ2100_ucspi_fnroc_values)
coral_occ2100_ucspi_fnroc_values_df <- as.data.frame(coral_occ2100_ucspi_fnroc_values)
coral_occ2100_ucspi_fnroc_values_df$temp <- 2100
names(coral_occ2100_ucspi_fnroc_values_df) <- c("values", "temp")

#----------------------------------

#SU
#2020
coral_occ2020_ucsus_fnroc_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_noronha/coral_occ2020_ucsus_fnroc_mask.asc")
coral_occ2020_ucsus_fnroc_values <- getValues(coral_occ2020_ucsus_fnroc_mask)
hist(coral_occ2020_ucsus_fnroc_values)
coral_occ2020_ucsus_fnroc_values_df <- as.data.frame(coral_occ2020_ucsus_fnroc_values)
coral_occ2020_ucsus_fnroc_values_df$temp <- 2020
names(coral_occ2020_ucsus_fnroc_values_df) <- c("values", "temp")

#2050
coral_occ2050_ucsus_fnroc_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_noronha/coral_occ2050_ucsus_fnroc_mask.asc")
coral_occ2050_ucsus_fnroc_values <- getValues(coral_occ2050_ucsus_fnroc_mask)
hist(coral_occ2050_ucsus_fnroc_values)
coral_occ2050_ucsus_fnroc_values_df <- as.data.frame(coral_occ2050_ucsus_fnroc_values)
coral_occ2050_ucsus_fnroc_values_df$temp <- 2050
names(coral_occ2050_ucsus_fnroc_values_df) <- c("values", "temp")

#2100
coral_occ2100_ucsus_fnroc_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_noronha/coral_occ2100_ucsus_fnroc_mask.asc")
coral_occ2100_ucsus_fnroc_values <- getValues(coral_occ2100_ucsus_fnroc_mask)
hist(coral_occ2100_ucsus_fnroc_values)
coral_occ2100_ucsus_fnroc_values_df <- as.data.frame(coral_occ2100_ucsus_fnroc_values)
coral_occ2100_ucsus_fnroc_values_df$temp <- 2100
names(coral_occ2100_ucsus_fnroc_values_df) <- c("values", "temp")


#joining databases SP

ucspi_fn_roc_df <- rbind(coral_occ2020_ucspi_fnroc_values_df, coral_occ2050_ucspi_fnroc_values_df, coral_occ2100_ucspi_fnroc_values_df)
ucspi_fn_roc_df <- ucspi_fn_roc_df %>% na.omit()
ucspi_fn_roc_df$temp <- as.factor(ucspi_fn_roc_df$temp)
write_csv(ucspi_fn_roc_df, "base_tratada/ucspi_fnroc.csv")

#joining databases SU

ucsus_fn_roc_df <- rbind(coral_occ2020_ucsus_fnroc_values_df, coral_occ2050_ucsus_fnroc_values_df, coral_occ2100_ucsus_fnroc_values_df)
ucsus_fn_roc_df <- ucsus_fn_roc_df %>% na.omit()
ucsus_fn_roc_df$temp <- as.factor(ucsus_fn_roc_df$temp)
write_csv(ucsus_fn_roc_df, "base_tratada/ucsus_fnroc.csv")

#---permanova test 
#SP
ucspi_fn_roc_df <- read_csv("base_tratada/ucspi_fnroc.csv",
                             col_types = cols(
                               values = col_double(),
                               temp = col_factor())) %>% 
  mutate(pxl = rep(1:11, 3)) %>% 
  mutate(grp = "pi")

#SU
ucsus_fn_roc_df <- read_csv("base_tratada/ucsus_fnroc.csv",
                            col_types = cols(
                              values = col_double(),
                              temp = col_factor())) %>% 
  mutate(pxl = rep(1:15, 3)) %>% 
  mutate(grp = "us")

#----------------permanova-----------------------------
#SP
ucspi_fn_roc_aovp <- aovp(values ~ temp + pxl, data = ucspi_fn_roc_df)
summary(ucspi_fn_roc_aovp)

#post hoc test
(ucspi_fn_roc_paiw <- pairwisePermutationTest(values ~ temp + pxl, data = ucspi_fn_roc_df))

#SU
ucsus_fn_roc_aovp <- aovp(values ~ temp + pxl, data = ucsus_fn_roc_df)
summary(ucsus_fn_roc_aovp)

#post hoc test
(ucsus_fn_roc_paiw <- pairwisePermutationTest(values ~ temp + pxl, data = ucsus_fn_roc_df))
#---------------------------------------------

#plots
#SP
pirateplot((formula = values ~ temp), 
            data = ucspi_fn_roc_df,
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
           data = ucsus_fn_roc_df,
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
