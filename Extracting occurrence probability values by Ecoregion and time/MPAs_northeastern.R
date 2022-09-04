
##################################################
# Extracting occurrence probability values for   #
# northeastern MPAs                                  #
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

#filtering northeastern ecoregion
northeastern <- dplyr::filter(ecorrg,  name_ecor == "Nordeste")
plot(northeastern$geometry)

#---------------------------------------------------
## opening MPAs shapefile of northeastern
#Strict Protection
ucspi_northst <-st_read(dsn = "dados_sig/ucs_novas/ecorreg/amps_nordeste/amps_northest_pi.shp", layer = "amps_northest_pi", crs = 4326)
st_crs(ucspi_northst) 
plot(ucspi_northst$geometry)

#Sustainable Use
ucsus_northst <-st_read(dsn = "dados_sig/ucs_novas/ecorreg/amps_nordeste/amps_northest_us.shp", layer = "amps_northest_us", crs = 4326)
st_crs(ucsus_northst) 
plot(ucsus_northst$geometry)

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
#for SP MPAs in the northeastern in 2020

# Cutting the raster from "coral_occurrence_2020" to ecoregion area
coral_occ2020_ucspi_northst_crop <- crop(coral_occ2020, extent(northeastern))
plot(coral_occ2020_ucspi_northst_crop)

# Converting MPAs' shapefile to raster
coral_occ2020_ucspi_northst_ras <- rasterize(ucspi_northst, 
                                             coral_occ2020_ucspi_northst_crop, 
                                             getCover=TRUE)
coral_occ2020_ucspi_northst_ras[
  coral_occ2020_ucspi_northst_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2020_ucspi_northst_mask <- mask(coral_occ2020_ucspi_northst_crop,                coral_occ2020_ucspi_northst_ras,
         filename = "dados_sig/ucs_novas/ecorreg/amps_nordeste/coral_occ2020_ucspi_northst_mask.asc")

plot(coral_occ2020_ucspi_northst_mask)

hist(coral_occ2020_ucspi_northst_mask)

#---extracting occurrence probability values----
#for SP MPAs in the northeastern in 2050

# Cutting the raster from "coral_occurrence_2050" to ecoregion area
coral_occ2050_ucspi_northst_crop <- crop(coral_occ2050, extent(northeastern))
plot(coral_occ2050_ucspi_northst_crop)

# Converting MPAs' shapefile to raster
coral_occ2050_ucspi_northst_ras <- rasterize(ucspi_northst, 
                                             coral_occ2050_ucspi_northst_crop, 
                                             getCover=TRUE)
coral_occ2050_ucspi_northst_ras[
  coral_occ2050_ucspi_northst_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2050_ucspi_northst_mask <- mask(coral_occ2050_ucspi_northst_crop,                coral_occ2050_ucspi_northst_ras,
         filename = "dados_sig/ucs_novas/ecorreg/amps_nordeste/coral_occ2050_ucspi_northst_mask.asc")

plot(coral_occ2050_ucspi_northst_mask)

hist(coral_occ2050_ucspi_northst_mask)

#---extracting occurrence probability values----
#for SP MPAs in the northeastern in 2100

# Cutting the raster from "coral_occurrence_2100" to ecoregion area
coral_occ2100_ucspi_northst_crop <- crop(coral_occ2100, extent(northeastern))
plot(coral_occ2100_ucspi_northst_crop)

# Converting MPAs' shapefile to raster
coral_occ2100_ucspi_northst_ras <- rasterize(ucspi_northst, 
                                             coral_occ2100_ucspi_northst_crop, 
                                             getCover=TRUE)
coral_occ2100_ucspi_northst_ras[
  coral_occ2100_ucspi_northst_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2100_ucspi_northst_mask <- mask(coral_occ2100_ucspi_northst_crop,
                                       coral_occ2100_ucspi_northst_ras,
                                       filename = "dados_sig/ucs_novas/ecorreg/amps_nordeste/coral_occ2100_ucspi_northst_mask.asc")

plot(coral_occ2100_ucspi_northst_mask)

hist(coral_occ2100_ucspi_northst_mask)

#--------------------Sustainable use-------------------------------

#---extracting occurrence probability values----
#for SP MPAs in the northeastern in 2020

# Cutting the raster from "coral_occurrence_2020" to ecoregion area
coral_occ2020_ucsus_northst_crop <- crop(coral_occ2020, extent(northeastern))
plot(coral_occ2020_ucsus_northst_crop)

# Converting MPAs' shapefile to raster
coral_occ2020_ucsus_northst_ras <- rasterize(ucsus_northst,
                                             coral_occ2020_ucsus_northst_crop, 
                                             getCover=TRUE)
coral_occ2020_ucsus_northst_ras[
  coral_occ2020_ucsus_northst_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2020_ucsus_northst_mask <- mask(coral_occ2020_ucsus_northst_crop,                coral_occ2020_ucsus_northst_ras,
         filename = "dados_sig/ucs_novas/ecorreg/amps_nordeste/coral_occ2020_ucsus_northst_mask.asc")

plot(coral_occ2020_ucsus_northst_mask)

hist(coral_occ2020_ucsus_northst_mask)

#---extracting occurrence probability values----
#for SP MPAs in the northeastern in 2050

# Cutting the raster from "coral_occurrence_2050" to ecoregion area
coral_occ2050_ucsus_northst_crop <- crop(coral_occ2050, extent(northeastern))
plot(coral_occ2050_ucsus_northst_crop)

# Converting MPAs' shapefile to raster
coral_occ2050_ucsus_northst_ras <- rasterize(ucsus_northst, 
                                             coral_occ2050_ucsus_northst_crop, 
                                             getCover=TRUE)
coral_occ2050_ucsus_northst_ras[
  coral_occ2050_ucsus_northst_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2050_ucsus_northst_mask <- mask(coral_occ2050_ucsus_northst_crop,              coral_occ2050_ucsus_northst_ras,
       filename = "dados_sig/ucs_novas/ecorreg/amps_nordeste/coral_occ2050_ucsus_northst_mask.asc")

plot(coral_occ2050_ucsus_northst_mask)

hist(coral_occ2050_ucsus_northst_mask)

#---extracting occurrence probability values----
#for SP MPAs in the northeastern in 2100

# Cutting the raster from "coral_occurrence_2100" to ecoregion area
coral_occ2100_ucsus_northst_crop <- crop(coral_occ2100, extent(northeastern))
plot(coral_occ2100_ucsus_northst_crop)


# Converting MPAs' shapefile to raster
coral_occ2100_ucsus_northst_ras <- rasterize(ucsus_northst,
                                             coral_occ2100_ucsus_northst_crop,
                                             getCover=TRUE)
coral_occ2100_ucsus_northst_ras[
  coral_occ2100_ucsus_northst_ras == 0] <- NA

##Extracting and Saving occurrence probability values within MPAs
coral_occ2100_ucsus_northst_mask <- mask(coral_occ2100_ucsus_northst_crop,
                                         coral_occ2100_ucsus_northst_ras,
                                         filename = "dados_sig/ucs_novas/ecorreg/amps_nordeste/coral_occ2100_ucsus_northst_mask.asc")

plot(coral_occ2100_ucsus_northst_mask)

hist(coral_occ2100_ucsus_northst_mask)

#--------Preparing Data for Analysis----------------

#SP
#2020
coral_occ2020_ucspi_northst_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_nordeste/coral_occ2020_ucspi_northst_mask.asc")
coral_occ2020_ucspi_northst_values <- getValues(coral_occ2020_ucspi_northst_mask)
hist(coral_occ2020_ucspi_northst_values)
coral_occ2020_ucspi_northst_values_df <- as.data.frame(coral_occ2020_ucspi_northst_values)
coral_occ2020_ucspi_northst_values_df$temp <- 2020
names(coral_occ2020_ucspi_northst_values_df) <- c("values", "temp")

#2050
coral_occ2050_ucspi_northst_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_nordeste/coral_occ2050_ucspi_northst_mask.asc")
coral_occ2050_ucspi_northst_values <- getValues(coral_occ2050_ucspi_northst_mask)
hist(coral_occ2050_ucspi_northst_values)
coral_occ2050_ucspi_northst_values_df <- as.data.frame(coral_occ2050_ucspi_northst_values)
coral_occ2050_ucspi_northst_values_df$temp <- 2050
names(coral_occ2050_ucspi_northst_values_df) <- c("values", "temp")

#2100
coral_occ2100_ucspi_northst_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_nordeste/coral_occ2100_ucspi_northst_mask.asc")
coral_occ2100_ucspi_northst_values <- getValues(coral_occ2100_ucspi_northst_mask)
hist(coral_occ2100_ucspi_northst_values)
coral_occ2100_ucspi_northst_values_df <- as.data.frame(coral_occ2100_ucspi_northst_values)
coral_occ2100_ucspi_northst_values_df$temp <- 2100
names(coral_occ2100_ucspi_northst_values_df) <- c("values", "temp")

#joining databases SP

ucspi_northeastern_df <- rbind(coral_occ2020_ucspi_northst_values_df, coral_occ2050_ucspi_northst_values_df, coral_occ2100_ucspi_northst_values_df)
ucspi_northeastern_df <- ucspi_northeastern_df %>% na.omit()
ucspi_northeastern_df$temp <- as.factor(ucspi_northeastern_df$temp)
write_csv(ucspi_northeastern_df, "base_tratada/ucspi_northeastern.csv")
#----------------------------------

#SU
#2020
coral_occ2020_ucsus_northst_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_nordeste/coral_occ2020_ucsus_northst_mask.asc")
coral_occ2020_ucsus_northst_values <- getValues(coral_occ2020_ucsus_northst_mask)
hist(coral_occ2020_ucsus_northst_values)
coral_occ2020_ucsus_northst_values_df <- as.data.frame(coral_occ2020_ucsus_northst_values)
coral_occ2020_ucsus_northst_values_df$temp <- 2020
names(coral_occ2020_ucsus_northst_values_df) <- c("values", "temp")

#2050
coral_occ2050_ucsus_northst_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_nordeste/coral_occ2050_ucsus_northst_mask.asc")
coral_occ2050_ucsus_northst_values <- getValues(coral_occ2050_ucsus_northst_mask)
hist(coral_occ2050_ucsus_northst_values)
coral_occ2050_ucsus_northst_values_df <- as.data.frame(coral_occ2050_ucsus_northst_values)
coral_occ2050_ucsus_northst_values_df$temp <- 2050
names(coral_occ2050_ucsus_northst_values_df) <- c("values", "temp")

#2100
coral_occ2100_ucsus_northst_mask <- raster ("dados_sig/ucs_novas/ecorreg/amps_nordeste/coral_occ2100_ucsus_northst_mask.asc")
coral_occ2100_ucsus_northst_values <- getValues(coral_occ2100_ucsus_northst_mask)
hist(coral_occ2100_ucsus_northst_values)
coral_occ2100_ucsus_northst_values_df <- as.data.frame(coral_occ2100_ucsus_northst_values)
coral_occ2100_ucsus_northst_values_df$temp <- 2100
names(coral_occ2100_ucsus_northst_values_df) <- c("values", "temp")

#joining databases SU

ucsus_northeastern_df <- rbind(coral_occ2020_ucsus_northst_values_df, coral_occ2050_ucsus_northst_values_df, coral_occ2100_ucsus_northst_values_df)
ucsus_northeastern_df <- ucsus_northeastern_df %>% na.omit()
ucsus_northeastern_df$temp <- as.factor(ucsus_northeastern_df$temp)
write_csv(ucsus_northeastern_df, "base_tratada/ucsus_northeastern.csv")

#---permanova test SP-----
ucspi_northeastern_df <- read_csv("base_tratada/ucspi_northeastern.csv",
                                  col_types = cols(
                                    values = col_double(),
                                    temp = col_factor())) %>% 
  mutate(pxl = rep(1:9, 3)) %>% 
  mutate(grp = "pi")

#permanova
ucspi_northeast_aovp <- aovp(values ~ temp + pxl, ucspi_northeastern_df)
summary(ucspi_northeast_aovp)

#post hoc test
posthoc_ucspi_northeast <- pairwisePermutationTest(values ~ temp + pxl, ucspi_northeastern_df)
posthoc_ucspi_northeast

#---permanova test SU-----
ucsus_northeastern_df <- read_csv("base_tratada/ucsus_northeastern.csv",
                                  col_types = cols(
                                    values = col_double(),
                                    temp = col_factor()))%>% 
  mutate(pxl = rep(1:189, 3))  %>% 
  mutate(grp = "us")

#Permanova
ucsus_northeast_aovp <- aovp(values ~ temp + pxl, ucsus_northeastern_df)
summary(ucsus_northeast_aovp)

#post hoc test
posthoc_ucsus_northeast <- pairwisePermutationTest(values ~ temp + pxl, ucsus_northeastern_df)
posthoc_ucsus_northeast


#plot

#SP
pirateplot((formula = values ~ temp), 
           data = ucspi_northeastern_df,
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
           data = ucsus_northeastern_df,
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
