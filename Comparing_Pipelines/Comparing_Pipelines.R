####################################################
##       Comparing FreeSurfer cross-sectional     ##
##           and longitudinal pipelines           ##
##       By Kate Mills & Anne-Lise Goddings       ##
####################################################

## Load required packages 
packages <- c("lme4", "nlme", "ggplot2", "dplyr", "tidyr", "knitr",
                "parallel", "data.table", "lubridate", "gridExtra")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
lapply(packages, library, character.only = TRUE)

## Load your sample ##
load(paste0(getwd(),"/Comparing_pipelines/exampledata.RData"))

exampledata_comparing$subid<-as.factor(exampledata_comparing$subid)

# Set a sample sub (try "84732" or "86124")
samplesub<-84948

# Graph PFC Volume by Age for the cross-sectional pipeline
Prefrontal_Volume_Xsectional<-ggplot(data=exampledata_comparing,
                                     aes(x=age,
                                         y=prefrontal_vol_cross))+
  xlim(9,23)+
  ylim(125000,225000)+
  xlab("Age (years)")+
  ylab("PFC volume (cross-sectional)")+
  geom_line(aes(colour=subid, group=subid),size=.3,alpha=0.3)+
  geom_point(aes(colour=subid, group=subid),size=2,alpha=0.3)+
  geom_line(data=(exampledata_comparing %>%
                    filter(subid==paste0(samplesub))),aes(colour="dodgerblue", group=subid),size=1,alpha=0.8)+
  geom_point(data=(exampledata_comparing %>%
                     filter(subid==paste0(samplesub))),aes(colour="dodgerblue", group=subid),size=2,alpha=0.8)+
  theme_bw() +
  theme_minimal(base_size = 12, base_family = "Arial") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none")

# Take a look
Prefrontal_Volume_Xsectional


# Graph Prefrontal volume by Age for the longitudinal pipeline
Prefrontal_Volume_Longitudinal<-ggplot(data=exampledata_comparing,
                                     aes(x=age,
                                         y=prefrontal_vol_long))+
  xlim(9,23)+
  ylim(125000,225000)+
  xlab("Age (years)")+
  ylab("PFC volume (longitudinal)")+
  geom_line(aes(colour=subid, group=subid),size=.3,alpha=0.3)+
  geom_point(aes(colour=subid, group=subid),size=2,alpha=0.3)+
  geom_line(data=(exampledata_comparing %>%
                    filter(subid==paste0(samplesub))),aes(colour="dodgerblue", group=subid),size=1,alpha=0.8)+
  geom_point(data=(exampledata_comparing %>%
                     filter(subid==paste0(samplesub))),aes(colour="dodgerblue", group=subid),size=2,alpha=0.8)+
  theme_bw() +
  theme_minimal(base_size = 12, base_family = "Arial") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none")

# Take a look
Prefrontal_Volume_Longitudinal

# Subtract difference and graph it
exampledata_comparing <- exampledata_comparing %>%
  mutate(difference_PFC=(prefrontal_vol_long-prefrontal_vol_cross))

Prefrontal_Volume_Difference<-ggplot(data=exampledata_comparing,
                                       aes(x=age,
                                           y=difference_PFC))+
  xlim(9,23)+
  xlab("Age (years)")+
  ylab("Difference between volumes (longitudinal - cross-secitonal")+
  geom_line(aes(colour=subid, group=subid),size=.3,alpha=0.3)+
  geom_point(aes(colour=subid, group=subid),size=2,alpha=0.3)+
  theme_bw() +
  theme_minimal(base_size = 12, base_family = "Arial") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none")

# Take a look
Prefrontal_Volume_Difference

grid.arrange(Prefrontal_Volume_Xsectional,Prefrontal_Volume_Longitudinal,ncol=2)
