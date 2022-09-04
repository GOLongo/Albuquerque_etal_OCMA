##################################################
# Extracting occurrence probability values for   #
# strict protected MPAs for 2090-2100            #
##################################################

#---releasing packages----
library(ggplot2)
library(dplyr)
library(raster)
library(sf)
#---------

# Opening raster layer for probability of occurrence 
#"coral_occurrence_2100"
coral_occ2100 <- raster ("dados_sig/2100/Coral_Occurrence_Posterior_predictive_mean_2100.asc")
crs(coral_occ2100) <- "+proj=longlat +datum=WGS84 +no_defs"
plot(coral_occ2100)

# opening MPAs shapefile
UCsPI <-st_read(dsn = "dados_sig/ucs_novas/AMPs_pi/amps_pi_wgs84.shp", layer = 'amps_pi_wgs84', crs = 4326)

st_crs(UCsPI) 
plot(UCsPI$geometry)

#----------------

# Cutting the raster from "coral_occurrence_2100" to MPAs areas
coral_occ2100_UCsPI_crop <- crop(coral_occ2100, extent(UCsPI))
plot(coral_occ2100_UCsPI_crop)

# Converting MPAs' shapefile to raster
coral_occ2100_UCsPI_ras <- rasterize(UCsPI, coral_occ2100, getCover=TRUE)
coral_occ2100_UCsPI_ras[coral_occ2100_UCsPI_ras==0] <- NA

#Extracting and Saving occurrence probability values within MPAs
coral_occ2100_UCsPI_mask <- mask(coral_occ2100,
                                 coral_occ2100_UCsPI_ras,
                                 filename = "dados_sig/2100/UCsPI/coral_occ2100_UCsPI_mask.asc", overwrite = T)

plot(coral_occ2100_UCsPI_mask)
#----------

# Looking at data
coral_occ2100_UCsPI_mask <- raster ("dados_sig/2100/UCsPI/coral_occ2100_UCsPI_mask.asc")
coral_occ2100_UCsPI_values <- getValues(coral_occ2100_UCsPI_mask)
hist(coral_occ2100_UCsPI_values)

# Observando os valores em coral_occurence_2100 - todo
#coral_occ2100
#coral_occ2100_values <- getValues(coral_occ2100)
#length(coral_occ2100_values)
#hist(coral_occ2100_values)
# Observando os valores em coral_occurence_2100 - apenas quando occ >= 0,4069
#coral_occ2100_A <- coral_occ2100
#plot(coral_occ2100_A)
#coral_occ2100_A[Which(coral_occ2100_A <= 0.4069)] <- NA
#plot(coral_occ2100_A)
#coral_occ2100_A_values <- getValues(coral_occ2100_A)
#length(coral_occ2100_A_values)
#hist(coral_occ2100_A_values)

# Observando dois histogramas juntos:
# Esquerda: distribui??o de todos os valores em coral_occ
# Direita:  distribui??o de todos os valores em coral_occ dentro de UCsPI
#par(mfrow=c(1,2))    # dividindo a ?rea do gr?fico em 1 linha e 2 colunas
#hist(coral_occ2100_values)
#hist(coral_occ2100_UCsPI_values)

#length(na.omit(coral_occ2100_UCsPI_values)) # Aqui, s? para visualizar o numero de pixels em UCsPI

##creating a data frame with the occurrence probabilities values
coral_occ2100_UCsPI_values_df <- as.data.frame(coral_occ2100_UCsPI_values)
hist(coral_occ2100_values)

#plot
windowsFonts(Times=windowsFont("Times New Roman"))

(gph_UCsPI_2100 <- ggplot(coral_occ2100_UCsPI_values_df, 
                         aes(coral_occ2100_UCsPI_values)) + 
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
  theme_light() +
  labs(title = "2100",
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
        
        #axis.line.y.left = element_line(color = "red"),
        #axis.line.x.bottom = element_line(colour = "red"),
        #axis.line.x.top = element_line(colour = "red"),
        panel.border = element_rect(colour = "#bdbdbd", size = 1),
        axis.line = element_line(colour = "#252525", size = 1))
)
