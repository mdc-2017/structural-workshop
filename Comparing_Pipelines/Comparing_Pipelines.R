####################################################
##       Comparing FreeSurfer cross-sectional     ##
##           and longitudinal pipelines           ##
##       By Kate Mills & Anne-Lise Goddings       ##
####################################################

## This code will load data that have been 
## sample by participant.

## Load required packages 
packages <- c("lme4", "nlme", "ggplot2", "dplyr", "tidyr", "knitr",
                "parallel", "data.table", "lubridate")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
lapply(packages, library, character.only = TRUE)

## Load your sample ##
load(paste0(getwd(),"/Comparing_pipelines/exampledata.RData"))


# Graph Nucleus Accumbens by Age for the cross-sectional pipeline
Nucleus_Accumbens_Xsectional<-ggplot(data=subcorticaldata_anonymized,
                                     aes(x=age,
                                         y=nacc_vol_cross))+
  xlim(9,23)+
  ylim(750,2100)+
  xlab("Age (years)")+
  ylab("NAcc volume (cross-sectional)")+
  geom_line(aes(colour=sex, group=subid),size=.3,alpha=0.3)+
  geom_point(aes(colour=sex, group=subid),size=2,alpha=0.3)+
  theme_bw() +
  theme_minimal(base_size = 12, base_family = "Arial") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none")

# Take a look
Nucleus_Accumbens_Xsectional


# Graph Nucleus Accumbens by Age for the longitudinal pipeline
Nucleus_Accumbens_Longitudinal<-ggplot(data=subcorticaldata_anonymized,
                         aes(x=age,
                             y=nacc_vol_long))+
  xlim(9,23)+
  ylim(750,2100)+
  xlab("Age (years)")+
  ylab("NAcc volume (longitudinal)")+
  geom_line(aes(colour=sex, group=subid),size=.3,alpha=0.3)+
  geom_point(aes(colour=sex, group=subid),size=2,alpha=0.3)+
  theme_bw() +
  theme_minimal(base_size = 12, base_family = "Arial") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none")

# Take a look
Nucleus_Accumbens_Longitudinal

# Subtract difference and graph it
subcorticaldata_anonymized <- subcorticaldata_anonymized %>%
  mutate(difference_nacc=(nacc_vol_long-nacc_vol_cross))

Nucleus_Accumbens_Difference<-ggplot(data=subcorticaldata_anonymized,
                                       aes(x=age,
                                           y=difference_nacc))+
  xlim(9,23)+
  xlab("Age (years)")+
  ylab("Difference between volumes (longitudinal - cross-secitonal")+
  geom_line(aes(colour=sex, group=subid),size=.3,alpha=0.3)+
  geom_point(aes(colour=sex, group=subid),size=2,alpha=0.3)+
  theme_bw() +
  theme_minimal(base_size = 12, base_family = "Arial") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none")

# Take a look
Nucleus_Accumbens_Difference
