#---releasing packages----
library(dplyr)
library(raster)
library(lmPerm)
library(rcompanion)
library(ggplot2)
library(readr)
library(yarrr)
#--------

ucspi_juris_020 <- read_csv("base_tratada/ucspi_juris_020.csv",
                     col_types = cols(values = col_double(),
                                     juris = col_factor())) %>% 
  mutate(catg = "pi")


ucsus_juris_020 <- read_csv("base_tratada/ucsus_juris_020.csv",
                    col_types = cols(values = col_double(),
                                     juris = col_factor())) %>% 
  mutate(catg = "us")

ucs_juris <- union_all(ucspi_juris_020, ucsus_juris_020)

ucs_juris$catg <- as.factor(ucs_juris$catg)

pirateplot((formula = values ~ juris + catg), 
           data = ucs_juris,
           main = "Jurisdição das UCs por categoria",
           xlab = "Locais",
           ylab = "values",
           ylim = c(0,1),
           theme = 0,
           pal = "southpark", # southpark color palette
           bean.f.o = .6, # Bean fill
           point.o = .3, # Points
           avg.line.o = 1, # Average line
           avg.line.fun = median, 
           bar.f.o = .4, # Bar
           avg.line.col = "black", # avg line col
           bar.f.col = gray(.8), # bar filling color
           point.pch = 21,
           point.bg = "white",
           point.col = "black",
           point.cex = .7,
           adjust = 1,
           width.max = .45
           )
text(x = ucs_juris$juris, c("a", "b", "bc", "d", "e"), col="blue",cex=2)


ucs_juris_aovp <- aovp(values ~ juris * catg,
                       ucs_juris)
summary(ucs_juris_aovp)

ucs_juris_pairw <- pairwisePermutationTest(values ~ juris + catg,
                                           ucs_juris)
ucs_juris_pairw


ucst <- filter(ucs_juris, catg == "pi" & juris == "fdrl")
ucsv <- filter(ucs_juris, catg == "us" & juris == "estd")
ucsb <- union_all(ucst, ucsv)


ucsb_aovp <- aovp(values ~ juris,
                       ucsb)
summary(ucsb_aovp)

ucsb_pairw <- pairwisePermutationTest(values ~ juris, ucsb)
ucsb_pairw


pirateplot((formula = values ~ juris), 
           data = ucsb,
           main = "Jurisdição das UCs por categoria",
           xlab = "Locais",
           ylab = "values",
           ylim = c(0,1),
           theme = 0,
           pal = "southpark", # southpark color palette
           bean.f.o = .6, # Bean fill
           point.o = .3, # Points
           avg.line.o = 1, # Average line
           avg.line.fun = median, 
           bar.f.o = .4, # Bar
           avg.line.col = "black", # avg line col
           bar.f.col = gray(.8), # bar filling color
           point.pch = 21,
           point.bg = "white",
           point.col = "black",
           point.cex = .7,
           adjust = 1,
           width.max = .45)
