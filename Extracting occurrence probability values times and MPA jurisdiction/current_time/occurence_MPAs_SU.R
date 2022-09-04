##################################################
# Extracting occurrence probability values for   #
# sustainable use MPAs in current time           #
##################################################

#---releasing packages----
library(ggplot2)
library(dplyr)
library(raster)
library(sf)
# Opening raster layer for probability of occurrence 

coral_occ2020 <- raster ("dados_sig/Coral_Occurrence_Posterior_predictive_mean.asc")
crs(coral_occ2020) <- "+proj=longlat +datum=WGS84 +no_defs"
plot(coral_occ2020)

# opening MPAs shapefile
UCsUS <- st_read(dsn = "dados_sig/ucs_novas/AMPs_us/amps_us_wgs84.shp", layer = "amps_us_wgs84", crs = 4326)
st_crs(UCsUS) 
plot(UCsUS$geometry)

#--------------------------------------
# Cutting the raster from "coral_occurrence_2020.asc" to MPAs areas 
coral_occ2020_UCsUS_crop <- crop(coral_occ2020, extent(UCsUS))
plot(coral_occ2020_UCsUS_crop)

# Converting MPAs' shapefile to raster
coral_occ2020_UCsUS_ras <- rasterize(UCsUS, coral_occ2020, getCover=TRUE)
coral_occ2020_UCsUS_ras[coral_occ2020_UCsUS_ras==0] <- NA

#Extracting and Saving occurrence probability values within MPAs
coral_occ2020_UCsUS_mask <- mask(coral_occ2020,
                                 coral_occ2020_UCsUS_ras, 
                                 filename = "dados_sig/ucs_novas/AMPs_us/coral_occ2020_UCsUS_mask.asc")

plot(coral_occ2020_UCsUS_mask)
#----------

# Looking at data
coral_occ2020_UCsUS_mask <- raster ("dados_sig/ucs_novas/AMPs_us/coral_occ2020_UCsUS_mask.asc")
coral_occ2020_UCsUS_values <- getValues(coral_occ2020_UCsUS_mask)
hist(coral_occ2020_UCsUS_values)

# Observando os valores em coral_occurence_2020 - todo
#coral_occ2020
#coral_occ2020_values <- getValues(coral_occ2020)
#length(coral_occ2020_values)
#hist(coral_occ2020_values)

# Observando os valores em coral_occurence_2020 - apenas quando occ >= 0,4069
#coral_occ2020_A <- coral_occ2020
#plot(coral_occ2020_A)
#coral_occ2020_A[Which(coral_occ2020_A <= 0.4069)] <- NA
#plot(coral_occ2020_A)
#coral_occ2020_A_values <- getValues(coral_occ2020_A)
#length(coral_occ2020_A_values)
#hist(coral_occ2020_A_values)

# Observando dois histogramas juntos:
# Esquerda: distribui??o de todos os valores em coral_occ
# Direita:  distribui??o de todos os valores em coral_occ dentro de UCsPI
#par(mfrow=c(1,2))    # dividindo a ?rea do gr?fico em 1 linha e 2 colunas
#hist(coral_occ2020_values)
#hist(coral_occ2020_UCsUS_values)
#length(na.omit(coral_occ2020_UCsPI_values)) 


#creating a data frame with the occurrence probabilities values
coral_occ2020_UCsUS_values_df <- as.data.frame(coral_occ2020_UCsUS_values)
hist(coral_occ2020_UCsUS_values)


#Plot
windowsFonts(Times=windowsFont("Times New Roman"))

(gph_ucsUS <- ggplot(coral_occ2020_UCsUS_values_df, 
       aes(coral_occ2020_UCsUS_values)) + 
  geom_histogram(bins = 15,
                 fill = "#bdbdbd", 
                 colour = "#636363") + 
  #geom_vline(xintercept = 0.4069, 
   #          lty = "dashed", 
    #         colour = "red", 
     #        size = .8) + 
  scale_x_continuous(breaks = seq(from = 0,
                                  to = 1,
                                  by = 0.2), 
                     limits = c(0, 1)) +
  theme_bw() +
  labs(title = "",
       x = "", 
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
)

