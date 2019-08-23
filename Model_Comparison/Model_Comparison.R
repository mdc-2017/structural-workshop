##################################################
##        Model Comparison                      ##
##       By Kate Mills & Nandi Vijayakumar      ##
##################################################

## Load required packages 
packages <- c("lme4", "nlme", "ggplot2", "dplyr", "tidyr", "knitr",
                "parallel", "data.table", "lubridate")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
lapply(packages, library, character.only = TRUE)

## Set some variables ##
ctrl <- lmeControl(opt='optim')
options(scipen=999)

## Load your sample ##
load(paste0(getwd(),"/Comparing_pipelines/exampledata.RData"))

thedata<-exampledata_comparing

# Make sure subid is considered a factor
thedata$subid<-as.factor(thedata$subid)

# Center age
thedata$agecent<-((thedata$age)-(round(mean(thedata$age),0)))
thedata$agecentsq<-(thedata$agecent*thedata$agecent)
thedata$agecentcu<-(thedata$agecent*thedata$agecent*thedata$agecent)

##############################
### Model PFC development ####

## Null model
null_PFCmodel=(lme(prefrontal_vol_long ~ 1,
                   method="ML",
                   random = ~1|subid,
                   data=thedata))

## Linear age model
lin_PFCmodel=(lme(prefrontal_vol_long ~ agecent,
                  method="ML",
                  random = ~1|subid,
                  data=thedata))

## Quadratic age model
quad_PFCmodel=(lme(prefrontal_vol_long ~ agecent+agecentsq,
                   method="ML",
                   random = ~1|subid,
                   data=thedata))

## Cubic age model
cub_PFCmodel=(lme(prefrontal_vol_long ~ agecent+agecentsq+agecentcu,
                  method="ML",
                  random = ~1|subid,
                  data=thedata))

## If using top-down approach to model selection.
# Examine p-value of highest order term, and move down age terms if not significant
summary(cub_PFCmodel) 
summary(quad_PFCmodel)
summary(lin_PFCmodel)

## Using a model fit approach
# Compare model fit using anova
age_predict_PFC_table<-anova(null_PFCmodel,
                          lin_PFCmodel,
                          quad_PFCmodel,
                          cub_PFCmodel)
# take a look
age_predict_PFC_table

#compare cubic model and linear model
age_predict_PFC_linvscub_table<-anova(lin_PFCmodel,cub_PFCmodel)
age_predict_PFC_linvscub_table

# Looks like the cubic model is the best fit
bestagemodel_PFC<-cub_PFCmodel

#######################################
# Graph PFC development
agecent<-round(seq(min(thedata$agecent,na.rm = TRUE),
                   max(thedata$agecent,na.rm = TRUE),by=1),
               2)
agecentsq=agecent*agecent
agecentcu=agecent*agecent*agecent
data.pred = data.frame(agecent=agecent,agecentsq=agecentsq,agecentcu=agecentcu)
data.pred$age<-(data.pred$agecent+mean(thedata$age))
y.pred = predict(bestagemodel_PFC,
                 data.pred,
                 level=0)
data.pred = cbind.data.frame(data.pred,
                             y.pred)
scale = 1.96
designmat<-model.matrix(eval(eval(bestagemodel_PFC$call$fixed)[-2]),
                        data.pred[-3]) #make design matrix
SDvalue<-sqrt(diag(designmat %*% bestagemodel_PFC$varFix %*% t(designmat))) #calculate standard deviation for each point for each model
y.lower<-y.pred-(scale*SDvalue) #calculate confidence intervals - lower
y.upper<-y.pred+(scale*SDvalue) #calculate confidence intervals - upper
data.pred = cbind.data.frame(data.pred,
                             y.lower,
                             y.upper)
data.pred$prefrontal_vol_long<-data.pred$y.pred


# Graph it
PFC_Age<-ggplot(data=thedata,
                aes(x=age,
                    y=prefrontal_vol_long))+
  xlim(9,23)+
  ylim(125000,225000)+
  xlab("Age (years)")+
  ylab("PFC volume (longitudinal)")+
  geom_line(data=data.pred,
            aes(x=age, y=prefrontal_vol_long), size=.7, colour="deeppink")+
  geom_ribbon(data=data.pred,
              aes(ymin=y.lower, ymax=y.upper), alpha=0.2, fill="deeppink")+
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
PFC_Age


###############################
### Model Nacc development ####

## Null model
null_NAccmodel=(lme(nacc_vol_long ~ 1,
                   method="ML",
                   random = ~1|subid,
                   data=thedata))

## Linear age model
lin_NAccmodel=(lme(nacc_vol_long ~ agecent,
                  method="ML",
                  random = ~1|subid,
                  data=thedata))

## Generate a quadratic age model
quad_NAccmodel=(lme(nacc_vol_long ~ agecent+agecentsq,
                   method="ML",
                   random = ~1|subid,
                   data=thedata))

## Generate a cubic age model
cub_NAccmodel=(lme(nacc_vol_long ~ agecent+agecentsq+agecentcu,
                  method="ML",
                  random = ~1|subid,
                  data=thedata))

## If using a top-down approach to model selection
summary(cub_NAccmodel) 
summary(quad_NAccmodel)
summary(lin_NAccmodel)

## Compare models using anova
age_predict_NAcc_table<-anova(null_NAccmodel,
                             lin_NAccmodel,
                             quad_NAccmodel,
                             cub_NAccmodel)

## Complete model comparisons for NAcc and graph trajectory...
# note: always check if final "best-fit" model is better fit than null model

