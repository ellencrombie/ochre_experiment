#soil stats
soils_p<-read.csv("Soil_P.csv",row.names=1,stringsAsFactors = TRUE)
summary(soils_p)
hist(soils_p$mass_normalised)
hist(soils_p$normal_percent)
lm_phos<-lm(normal_percent~phosphorous_loading_conc, data=soils_p)
plot(lm_phos)
lm_phos_resids<-resid(lm_phos)
shapiro.test(lm_phos_resids)
bartlett.test(normal_percent~phosphorous_loading_conc, data=soils_p)
anova(lm_phos)lm_phosanova(lm_phos)anova(lm_phos)lm_phosanovsoils_pa(lm_phos)
summary(lm_phos)
lm_ochre<-lm(normal_percent~iron_ochre_amendement_percent, data=soils_p)
summary(lm_ochre)
plot(lm_ochre)
lm_ochre_resids<-resid(lm_ochre)
shapiro.test(lm_ochre_resids)

##this model is the one we used 
lm_ochre_p<-lm(normal_percent~iron_ochre_amendement_percent+phosphorous_loading_conc, data=soils_p)
summary(lm_ochre_p)
plot(lm_ochre_p)
lm_ochre_p_resids<-resid(lm_ochre_p)
shapiro.test(lm_ochre_p_resids)
bartlett.test(lm_ochre_p_resids)
summary(lm_ochre_p)
 AIC(lm_phos,lm_ochre, lm_ochre_p)

##ignore this for now this was us trying to plot our mixed model
#partial effect of phosphorous
lm_ochre_p
soil_p_NA<-na.omit(soils_p)
soil_p_NA$predicted <- predict(lm_ochre_p)


ggplot(soil_p_NA, aes(x=predicted, y=normal_percent))+
  geom_point(alpha=0.6)+
  geom_line(data=soil_p_NA, aes(x=predicted, y=predicted), color="blue", )+
  labs(title="Effect of Iron Ochre Amendement on Sorption of Phosphorus",
       x="Iron Ochre Percentage (%)",
       y="Mean Phosphorous Sorption(Mg/g)")+
  theme_minimal(base_size=14)

grid <-data.frame(
  phosphorous_loading_conc = seq(min(soil_p_NA$phosphorous_loading_conc, na.rm = TRUE),
                  max(soil_p_NA$phosphorous_loading_conc, na.rm = TRUE),
                  length.out=100),
  iron_ochre_amendement_percent = mean(soil_p_NA$iron_ochre_amendement_percent, na.rm = TRUE)
  
)

preds <- predict(lm_ochre_p, newdata = grid, interval = "confidence")
grid$fit <- preds [, "fit"]
grid$lwr <- preds [, "lwr"]
grid$upr <- preds [, "upr"]

ggplot(soil_p_NA, aes(x=predicted, y=normal_percent))+
  geom_point(alpha=0.6)+
  geom_line(data=soil_p_NA, aes(x=predicted, y=predicted), color="blue", )+
  labs(title="Effect of Iron Ochre Amendement on Sorption of Phosphorus",
       x="Iron Ochre Percentage (%)",
       y="Mean Phosphorous Sorption(Mg/g)")+
  theme_minimal(base_size=14)



install.packages(("matrix"))
library(matrix)


#pH
soil_pH<-read.csv("soil_ph.csv",row.nam=1,stringsAsFactors = TRUE)
summary(soil_pH)
lm_pH<-lm(pH~PO4removed_p, data=soil_pH)
summary(lm_pH)
