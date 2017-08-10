####################################################
##       Visualize your longitudinal sample       ##
##       By Kate Mills & Anne-Lise Goddings       ##
####################################################

## This code will let you visualize your longitudinal
## sample by participant.

## Load required packages 
packages <- c("dplyr", "ggplot2")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
lapply(packages, library, character.only = TRUE)

## Load your sample ##
# Option 1: Load your own data
thedata<-read.csv("/Path/to/your/data.csv")
# Option 2: Load the example data, which contains:
# participant, scan, age, and gender 
load(paste0(getwd(),"/Visualize_Sample/exampledata.RData"))

## Make participant, scan, and gender factors ##
thedata <- thedata %>%
  mutate(participant=factor(participant),
         scan=factor(scan),
         gender=factor(gender))

## Create new variable ordering participants by age
thedata$participantordered <- factor(thedata$participant,
                                     levels = thedata$participant[order(thedata$age)])

# Graph it
AgebyParticipant<-ggplot(thedata,
                         aes(colour=gender,
                             y=participantordered,
                             x=age))+
  geom_line(size=.7) + 
  ylab("Participants") +
  geom_point(size=3) +
  theme_bw() +
  theme_minimal(base_size = 18,
                base_family = "Arial") +
  theme(axis.line = element_line(colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# Take a look
AgebyParticipant

# Save it
ggsave(filename=paste0(getwd(),"/Visualize_Sample/AgebyParticipant.jpg"),
       plot=AgebyParticipant,
       width=5,
       height=7)

