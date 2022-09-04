library(vegan)
library(ggplot2)
library(dplyr)
library(pairwiseAdonis)
#reading data:
corais_amps <- read.csv("base_tratada/corais_amps_tot.csv", 
                        header = T, dec = ",")


#Species richness
rich <- corais_amps[,5:25]

corais_amps$richness <- specnumber(corais_amps[, 5:25])

#writexl::write_xlsx(corais_amps, "base_tratada/corais_amps_tot_excel.xlsx")

##
#distance matrix:
rich_jac <- vegdist(rich, method="jaccard")

#nMDS
resultado.nmds_rich <- metaMDS(rich_jac)

#----------------------------------

#Permanova protection level
pl.adonis<-adonis2(
  rich_jac ~ corais_amps$prot_level, permutations = 2000, method = "jaccard")
pl.adonis

#Permanova jurisdiction
juris.adonis<-adonis2(
  rich_jac ~ corais_amps$jurisdiction, permutations = 2000, method = "jacard")
juris.adonis

pairwise.adonis2(rich ~ jurisdiction, data = corais_amps)
#--------------------#------------#-----------

#####################
## Endemic species ##
####################

endem_amps <- corais_amps[,c(2:4, 19:25)]
endem_amps$tot <- specnumber(endem_amps[, 4:10])


endem_amps <- filter(endem_amps, tot != 0) |> 
  select(-tot)

endem <- endem_amps[,4:10]
##
#distance matrix:
endem_jac <- vegdist(endem, method="jaccard")

#nMDS
resultado.nmds_endem <- metaMDS(endem_jac)

#----------------------------------

#Permanova protection level
pl.adonis_endm<-adonis2(
  endem_jac ~ endem_amps$prot_level, permutations = 2000, method = "jaccard")
pl.adonis_endm

#Permanova jurisdiction
juris.adonis_endem<-adonis2(
  endem_jac ~ endem_amps$jurisdiction, permutations = 2000, method = "jacard")
juris.adonis_endem

pairwise.adonis2(endem ~ jurisdiction, data = endem_amps)
#--------------#-----------------#----

########################
## threatened species ##
#######################

threat_amps <- corais_amps |> 
  select(NOME_UC1, prot_level, jurisdiction, 
         "Millepora_laboreli",   
         "Mussismilia_braziliensis", 
         "Mussismilia_harttii")

threat_amps$tot <- specnumber(threat_amps[, 4:6])


threat_amps <- filter(threat_amps, tot != 0) |> 
  select(-tot)

threat <- threat_amps[,4:6]
##
#distance matrix:
threat_jac <-vegdist(threat, method="jaccard")

#nMDS
resultado.nmds_threat <- metaMDS(threat_jac)

#----------------------------------

#Permanova protection level
pl.adonis_endm<-adonis2(
  threat_jac ~ threat_amps$prot_level, permutations = 2000, method = "jaccard")
pl.adonis_endm

#Permanova jurisdiction
juris.adonis_threat<-adonis2(
  threat_jac ~ threat_amps$jurisdiction, permutations = 2000, method = "jacard")
juris.adonis_threat

pairwise.adonis2(threat ~ jurisdiction, data = threat_amps)

