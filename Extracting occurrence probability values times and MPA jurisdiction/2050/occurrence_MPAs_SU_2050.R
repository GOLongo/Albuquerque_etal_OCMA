##################################################
# Extracting occurrence probability values for   #
# sustainable use  MPAs for 2040-2050            #
##################################################

#---releasing packages----
library(ggplot2)
library(dplyr)
library(raster)
library(sf)
#---------

# Opening raster layer for probability of occurrence 
#"coral_occurrence_2050.asc"
coral_occ2050 <- raster ("dados_sig/2050/Coral_Occurrence_Posterior_predictive_mean_2050.asc")
crs(coral_occ2050) <- "+proj=longlat +datum=WGS84 +no_defs"
plot(coral_occ2050)


# opening MPAs shapefile
UCsUS <-st_read(dsn = "dados_sig/ucs_novas/AMPs_us/amps_us_wgs84.shp", layer = 'amps_us_wgs84', crs = 4326)
#st_crs(UCsUS) <- 4326
st_crs(UCsUS) 
plot(UCsUS$geometry)

#--------------------------------------
# Cutting the raster from "coral_occurrence" to MPAs areas
coral_occ2050_UCsUS_crop <- crop(coral_occ2050, extent(UCsUS))
plot(coral_occ2050_UCsUS_crop)

# Converting MPAs' shapefile to raster
coral_occ2050_UCsUS_ras <- rasterize(UCsUS, coral_occ2050, getCover=TRUE)
coral_occ2050_UCsUS_ras[coral_occ2050_UCsUS_ras==0] <- NA

#Extracting and Saving occurrence probability values within MPAs
coral_occ2050_UCsUS_mask <- mask(coral_occ2050,
                                 coral_occ2050_UCsUS_ras, 
                                 filename = "dados_sig/2050/UCsUS/coral_occ2050_UCsUS_mask.asc", overwrite=TRUE)

plot(coral_occ2050_UCsUS_mask)
#---------------
#ucstd_2050 <- rbind(UCsPI, UCsUS)

#coral_occ2050_ucstd_ras <- rasterize(ucstd_2050, coral_occ2050, getCover=TRUE)
#coral_occ2050_ucstd_ras[coral_occ2050_ucstd_ras==0] <- NA

#coral_occ2050_ucstd_mask <- mask(coral_occ2050,
#                                 coral_occ2050_ucstd_ras, 
#                                filename = "dados_sig/2050/coral_occ2050_ucstd_mask.asc")

#plot(coral_occ2050_UCsUS_mask)

#----------
# Looking at data

coral_occ2050_ucstd_mask <- raster ("dados_sig/2050/coral_occ2050_ucstd_mask.asc")
coral_occ2050_ucstd_values <- getValues(coral_occ2050_ucstd_mask)
hist(coral_occ2050_ucstd_values)

coral_occ2050_UCsUS_mask <- raster ("dados_sig/2050/UCsUS/coral_occ2050_UCsUS_mask.asc")
coral_occ2050_UCsUS_values <- getValues(coral_occ2050_UCsUS_mask)
hist(coral_occ2050_UCsUS_values)

# Observando os valores em coral_occurence_2050 - todo
#coral_occ2050
#coral_occ2050_values <- getValues(coral_occ2050)
#length(coral_occ2050_values)
#hist(coral_occ2050_values)

# Observando os valores em coral_occurence_2050 - apenas quando occ >= 0,4069
#coral_occ2050_A <- coral_occ2050
#plot(coral_occ2050_A)
#coral_occ2050_A[Which(coral_occ2050_A <= 0.4069)] <- NA
#plot(coral_occ2050_A)
#coral_occ2050_A_values <- getValues(coral_occ2050_A)
#length(coral_occ2050_A_values)
#hist(coral_occ2050_A_values)

# Observando dois histogramas juntos:
# Esquerda: distribui??o de todos os valores em coral_occ
# Direita:  distribui??o de todos os valores em coral_occ dentro de UCsPI
#par(mfrow=c(1,2))    # dividindo a ?rea do gr?fico em 1 linha e 2 colunas
#hist(coral_occ2050_values)
#hist(coral_occ2050_UCsUS_values)

#length(na.omit(coral_occ2050_UCsPI_values)) # Aqui, s? para visualizar o numero de pixels em UCsPI



#creating a data frame with the occurrence probabilities values
coral_occ2050_UCsUS_values_df <- as.data.frame(coral_occ2050_UCsUS_values)
hist(coral_occ2050_UCsUS_values)


#plot
windowsFonts(Times=windowsFont("Times New Roman"))

gph_ucsUS_2050 <- ggplot(coral_occ2050_UCsUS_values_df, 
                    aes(coral_occ2050_UCsUS_values)) + 
  geom_histogram(bins = 15,
                 fill = "#bdbdbd", 
                 colour = "#636363") + 
  #geom_vline(xintercept = 0.3743, 
   #          lty = "dashed", 
    #         colour = "red", 
     #        size = .8) + 
  scale_x_continuous(breaks = seq(from = 0,
                                  to = 1,
                                  by = 0.2), 
                     limits = c(0, 1)) +
  theme_bw() +
  labs(title = "SU",
       x = "Occurence probability", 
       y = "") +
  theme(plot.title = element_text(face = "bold",size = 12, 
                                  family = "Times",
                                  hjust = 1),
        #legend.background = element_rect(fill = "white", 
        #size = 4, colour = "white"),
        # legend.justification = c(0, 1),
        # legend.position = c(0, 1),
        axis.ticks = element_line(colour = "#252525", size = 0.2),
        panel.grid.major = element_line(colour = "#f0f0f0", 
                                        size = 0.2),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "#bdbdbd", size = 1),
        axis.line = element_line(colour = "#252525", size = 1))

#-------------
coral_occ2050_ucstd_values_df <- as.data.frame(coral_occ2050_ucstd_values)
hist(coral_occ2050_values)



gph_ucstd_2050 <- ggplot(coral_occ2050_ucstd_values_df, 
                  aes(coral_occ2050_ucstd_values)) + 
  geom_histogram(bins = 15,
                 fill = "#bdbdbd", 
                 colour = "#636363") + 
  geom_vline(xintercept = 0.3734, 
             lty = "dashed", 
             colour = "red", 
             size = .8) + 
  scale_x_continuous(breaks = seq(from = 0,
                                  to = 1,
                                  by = 0.2), 
                     limits = c(0, 1)) +
  theme_light() +
  labs(title = "UCs",
       x = "Probabilidade de ocorrência de corais", 
       y = "Frequência") +
  theme(plot.title = element_text(face = "bold",size = 12, 
                                  family = "Times",
                                  hjust = 1),
        #legend.background = element_rect(fill = "white", 
        #size = 4, colour = "white"),
        # legend.justification = c(0, 1),
        # legend.position = c(0, 1),
        axis.ticks = element_line(colour = "#252525", size = 0.2),
        panel.grid.major = element_line(colour = "#f0f0f0", 
                                        size = 0.2),
        panel.grid.minor = element_blank(),
        
        #axis.line.y.left = element_line(color = "red"),
        #axis.line.x.bottom = element_line(colour = "red"),
        #axis.line.x.top = element_line(colour = "red"),
        panel.border = element_rect(colour = "#bdbdbd", size = 1),
        axis.line = element_line(colour = "#252525", size = 1)
  )

gph_tot_2050 / gph_ucstd_2050

