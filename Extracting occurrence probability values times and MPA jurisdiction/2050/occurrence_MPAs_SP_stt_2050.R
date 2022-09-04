##################################################
# Extracting occurrence probability values for   #
# strict protected MPAs under state jurisdiction #
# in 2040-2050                                   #
##################################################

#---releasing packages----
library(ggplot2)
library(patchwork)
library(dplyr)
library(raster)
library(sf)
#--------

# Opening raster layer for probability of occurrence 
coral_occ2050 <- raster ("dados_sig/2050/Coral_Occurrence_Posterior_predictive_mean_2050.asc")
crs(coral_occ2050) <- "+proj=longlat +datum=WGS84 +no_defs"
plot(coral_occ2050)

# opening MPAs shapefile
UCsPI_estd  <- st_read(dsn = "dados_sig/ucs_novas/AMPs_pi/ucs_pi_estd/ucs_pi_estd.shp", layer = "ucs_pi_estd", crs = 4326)
st_crs(UCsPI)
#st_crs(UCsPI) <- 4326

#UCsPI_estd <- UCsPI %>% filter(ESFERA5 == "estadual")
plot(UCsPI_estd$geometry)

#----------------

# Cutting the raster from "coral_occurrence_2020.asc" to MPAs areas
coral_occ2050_UCsPI_crop <- crop(coral_occ2050, extent(UCsPI_estd))
plot(coral_occ2050_UCsPI_crop)

# Converting MPAs' shapefile to raster
coral_occ2050_UCsPI_estd_ras <- rasterize(UCsPI_estd, coral_occ2050, getCover=TRUE)
coral_occ2050_UCsPI_estd_ras[coral_occ2050_UCsPI_estd_ras==0] <- NA

#Extracting and Saving occurrence probability values within MPAs
coral_occ2050_UCsPI_estd_mask <- mask(coral_occ2050, coral_occ2050_UCsPI_estd_ras, filename = "dados_sig/2050/UCsPI/UCsPI_estd/coral_occ2050_UCsPI_estd_mask.asc", overwrite = T)

plot(coral_occ2050_UCsPI_estd_mask)
#----------

# Looking at data
coral_occ2050_UCsPI_estd_mask <- raster ("dados_sig/2050/UCsPI/UCsPI_estd/coral_occ2050_UCsPI_estd_mask.asc")
coral_occ2050_UCsPI_estd_values <- getValues(
  coral_occ2050_UCsPI_estd_mask )
hist(coral_occ2050_UCsPI_estd_values)

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
#hist(coral_occ2050_UCsPI_values)
#length(na.omit(coral_occ2050_UCsPI_values)) 


#creating a data frame with the occurrence probabilities values
coral_occ2050_UCsPI_estd_values_df <- as.data.frame(coral_occ2050_UCsPI_estd_values)
hist(coral_occ2050_UCsPI_values)

#Plot
windowsFonts(Times=windowsFont("Times New Roman"))

(gph_UCSPI_estd_2050 <- ggplot(coral_occ2050_UCsPI_estd_values_df, 
                         aes(coral_occ2050_UCsPI_estd_values)) + 
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
  labs(title = "State",
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
        #panel.background = element_rect(fill = "#ae017e", 
        #colour = NA)
        panel.border = element_rect(colour = "#bdbdbd", 
                                    size = 1),
        axis.line = element_line(colour = "#252525", 
                                 size = 1))
)




