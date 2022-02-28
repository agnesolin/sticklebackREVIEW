# ~ # ~ # ~ # ~ # ~ # ~ # ~ # ~ # ~ # ~ # ~ #
# ~~~ code to reproduce figs 1, 2 and 4 ~~~ #
# ~ # ~ # ~ # ~ # ~ # ~ # ~ # ~ # ~ # ~ # ~ #

# Agnes Olin agnesolin@gmail.com 23 December 2021
# R version 4.1.0 


# Data for Fig 1 (BIAS_map) and Fig 2a-c (BIAS_ts) come from the ICES Baltic International Acoustic Survey (see Olsson et al. 2019 for details).

# Data for Fig 2d-f (juvs) come from juveniles surveys during the spawning season using underwater detonations (see Eklöf et al. 2020 for details).

# Data for Fig 2g-h (forsmark) come from sticklebacks caught in the cooling water intake of the Forsmark nuclear power plant on the Swedish coast (see Adill et al. 2018 for details).

# Data for Fig 2i (finland) come from traps set out during the spawning season along the Finnish coast, and were extracted from Candolin and Voigt (2020) using https://apps.automeris.io/wpd/.

# Data for Fig 2j-k (latvia) come from beach seines during the spawning season along the Latvian coast (see Olsson et al. 2015 for details).

# Data for Fig 4a were extracted from Olsson (2019). Data for Fig 4b-f were obtained from https://standardgraphs.ices.dk. Data for Fig 4g were obtained from https://metadata.helcom.fi/geonetwork/srv/eng/catalog.search#/metadata/a30a77d1-12b6-47b4-a520-a54331bdbf41. Data for Fig 4h were extracted from Andersen et al. (2017) using https://apps.automeris.io/wpd/. Data for Fig 4i were obtained from doi.org/10.48670/moi-00206. 

# Data for Fig 5 (wave) come from juveniles surveys during the spawning season using underwater detonations (see Eklöf et al. 2020 for details).

# Coastline shapefile used in Figs 1 and 4 can be found at: https://www.eea.europa.eu/data-and-maps/data/eea-coastline-for-analysis-1/gis-data/europe-coastline-shapefile

# ICES subdivision shapefile used in Figs 1 and 4 can be found at: https://gis.ices.dk/sf/index.html?widget=StatRec


# load required libraries
library(cowplot) # version 1.1.1
library(ggplot2) # version 3.3.5
library(ggpubr) # version 0.4.0
library(grid) # version 4.1.0
library(jpeg) # version 0.1-9
library(MuMIn) # version 1.43.17
library(mgcv) # version 1.8-36
library(RColorBrewer) # version 1.1-2
library(raster) # version 3.4-13
library(scales) # version 1.1.1
library(sf) # version 1.0-2
library(showtext) # version 0.9-4


# add custom font
font_add_google(name = "Raleway", family = "spec-font")
showtext_auto()

#### FIGURE 1 ####

## load data ##

# stickleback densities
BIAS_map = read.csv("BIAS_map.csv")

# coastline
coast = st_read("Europe_coastline_poly.shp")
geom_coast = st_geometry(coast)
geom_coast = st_transform(geom_coast, "+proj=longlat +datum=WGS84")

# ICES SDs
ICES_lines = st_read("ICES_Areas_20160601_cut_dense_3857.shp")
ICES_lines = st_geometry(ICES_lines$geometry)
ICES_lines = st_transform(ICES_lines, "+proj=longlat +datum=WGS84")


### MAP1: locations ###

# locations for time series in fig 2
study_locs = data.frame(
  x = c(18.20760200108532, 23, 22.853619236144617, 21.020235593235224), 
  y = c(60.40798399098644, 60, 57.47993141699425, 56.27747746321472)
)


mapPLOT1 = ggplot(data = geom_coast) + # coastline
  
  labs(x = "Longitude", y = "Latitude") +
  
  
  geom_sf(fill = "#4393C3" , colour = "#4393C3" , size = 0.2) +
  coord_sf(xlim = c(4, 29), ylim = c(53.8,65.8)) +
  
  # locations
  annotate("text",
           x = c(23,
                 19.2,
                 18.8,
                 27,
                 23.7,
                 19,
                 5), 
           y = c(64.7,
                 61.8,
                 56.7,
                 61.3,
                 56.5,
                 53.5,
                 56), 
           label = c("Bothnian Bay",
                     "Bothnian Sea",
                     "Baltic Proper",
                     "Gulf of Finland",
                     "Gulf of Riga",
                     "Gulf of Gdansk",
                     "North Sea"),
           size = 7.5, 
           family = "spec-font",
           angle = c(
             45,
             45,
             45,
             0,
             0,
             0,
             0
           )) +
  
  
  # arrows for text
  geom_segment(aes(x = c(19), y = c(53.8), xend = c(19.2), yend = c(54.6)),
               arrow = arrow(length = unit(0.15, "cm"))) +
  
  geom_segment(aes(x = c(23.5), y = c(56.7), xend = c(23.8), yend = c(57.6)),
               arrow = arrow(length = unit(0.15, "cm"))) +
  
  geom_segment(aes(x = c(27), y = c(61.1), xend = c(26.5), yend = c(59.7)),
               arrow = arrow(length = unit(0.15, "cm"))) +
  
  # add time series locations
  geom_point(data = study_locs, aes(x = x, y = y), size = 4, pch = 21, fill = "white", col = "black") + 
  annotate("text", x = study_locs$x+c(0.0, 0.0, -0.01, 0.01), y = study_locs$y+c(0.05, 0.05, 0.025, 0.025), label = c("A", "B", "C", "D"), size = 7, family = "spec-font") + 
  
  # map label
  annotate("text", x = 29.2, y = 66, label = "a.", size = 14, family = "spec-font") + 
  
  
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "spec-font"),
    panel.background = element_rect(fill = "#D1E5F0",
                                    colour = "#D1E5F0",
                                    size = 0, linetype = "solid"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(face = "plain", size = 25),
    axis.title.y = element_text(face = "plain", size = 25),
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22))



### MAP2: stickleback data ###

## sort data into categories ##
BIAS_map$biomass_cat = NA
x = BIAS_map$biomass_density_tonnes_per_km2
BIAS_map$biomass_cat[x > 2   & !is.na(x)] = 6
BIAS_map$biomass_cat[x > 1 & x <= 2 & !is.na(x)] = 5
BIAS_map$biomass_cat[x > 0.5 & x <= 1 & !is.na(x)] = 4
BIAS_map$biomass_cat[x > 0.1 & x <= 0.5 & !is.na(x)] = 3
BIAS_map$biomass_cat[x > 0 & x <= 0.1 & !is.na(x)] = 2
BIAS_map$biomass_cat[x == 0 & !is.na(x)] = 1
BIAS_map$biomass_cat = as.factor(BIAS_map$biomass_cat)


mapPLOT2 = ggplot() +
  
  # add stickleback densities
  geom_raster(data = BIAS_map, aes(x = long_mid, y = lat_mid, fill = biomass_cat)) +
  scale_fill_manual(values = c("white", brewer.pal(5, "YlOrRd")), 
                    labels = c("0", ">0-0.1", ">0.1-0.5", ">0.5-1", ">1-2", ">2")) +
  labs(fill = "", 
       x = "Longitude", y = "Latitude") +
  
  
  # add ICES SDs
  geom_sf(data = ICES_lines, fill = NA, size = 0.5, colour = "#4393C3") + 
  geom_hline(yintercept = c(63.5,  56.5)) +
  geom_segment(aes(x = c(18, 19, 19, 23, 15, 16, 14, 21.7),
                   y = c(53, 58, 58.5, 59, 53, 60.5, 55.45, 57.5),
                   xend = c(18, 19, 25, 23, 15, 24, 14.75, 21.95),
                   yend = c(57, 59.5, 58.5, 60, 55, 60.5, 55.3, 58))) +
  
  
  # add coastline
  geom_sf(data = geom_coast, fill = "#4393C3",  colour = "#4393C3" , size = 0.005) +
  coord_sf(xlim = c(14, 30), ylim = c(53.8,65.8)) +
  
  annotate("text",
           label = c("30", "32", "29", "26", "27", "28","25"),
           x = c(19.5, 26.5, 21.5, 19.5, 17.5, 20.5, 16.5),
           y = c(62, 59.8, 59.25, 55.25, 57.75, 57.75, 55.25),
           size = 8,
           family = "spec-font"
  ) +
  
  # add label
  annotate("text", x = 29.5, y = 66, label = "b.", size = 14, family = "spec-font") +
  
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "spec-font"),
    panel.background = element_rect(fill = "#D1E5F0",
                                    colour = "#D1E5F0",
                                    size = 0, linetype = "solid"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.spacing.y = unit(0.04, "cm"),
    legend.key = element_rect(fill = NA, color = "black", size = 1.1),
    legend.key.width = unit(0.25,"cm"),
    legend.key.height = unit(0.05,"cm"),
    legend.text = element_text(size = 19.3, lineheight = 0.1),
    legend.title = element_text(size = 18, lineheight = 0.4),
    legend.position = c(1.04, 0.275),
    legend.background = element_rect(fill = NA, color = NA),
    axis.title.x = element_text(face = "plain", size = 25),
    axis.title.y = element_text(face = "plain", size = 25),
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22)) +
  
  # title legend
  annotate("text", x = 23.5, y = 56.5, label = "Biomass", size = 7.5, family = "spec-font") +
  annotate("text", x = 23.5, y = 56, label = expression(paste("(tonnes km"^"-2", ")")), size = 7.5, family = "spec-font") 





## initialise fig ##
jpeg("fig1.jpeg", 
    width = 17, height = 11, units = 'cm', res = 300, pointsize = 9, family = "sans")


## print fig
print(ggarrange(mapPLOT1, mapPLOT2, 
                ncol = 2, nrow = 1, 
                widths = c(1,0.7)))
## close fig ##
dev.off()







#### FIGURE 2 ####

## load data ##
BIAS = read.csv("BIAS_ts.csv")
juvs = read.csv("SwedenJuvenile.csv")
forsmark = read.csv("Forsmark.csv")
finland = read.csv("Finland.csv")
latvia = read.csv("Latvia.csv", sep = ";")


## graphical settings ##

# custom colours for different ICES SDs
cols = c("#1c9099", "#d8b365", "sienna")

# custom graphical settings for ggplot
theme_sets = theme(
  text = element_text(family = "spec-font"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title.x = element_text(face = "plain", size = 50),
  axis.title.y = element_text(face = "plain", size = 50),
  axis.text.x = element_text(size = 45),
  axis.text.y = element_text(size = 45))



### BIAS hydroacoustic data ###

# make SD group factor
BIAS$group = factor(BIAS$group, levels = c("SD30", "SD27_29", "SD25"))


#### 2a ####
## SD 25 ##

# subset data
dat = BIAS[BIAS$group == "SD25",] 

# fit and check gam model
gam.mod = gam(biomass_density_tonnes_per_km2 ~ s(year, k = 3),
              data = dat,
              family = Gamma(link = "log"),
              method = "REML")
gam.check(gam.mod)
summary(gam.mod)

# save p-value
p.value = signif(summary(gam.mod)$s.table[,4], digits = 1)

# check temporal autocorrelation
acf(residuals(gam.mod), lag.max = 2) 

# fit null model without year effect
gam.mod.null = gam(biomass_density_tonnes_per_km2 ~ 1,
                   data = dat,
                   family = Gamma(link = "log"),
                   method = "REML")

# calculate delta-AIC
dAIC = signif(AICc(gam.mod)-AICc(gam.mod.null), digits = 2)

# added penalty variable selection
gam.mod.select = gam(biomass_density_tonnes_per_km2 ~ s(year),
                     data = dat,
                     family = Gamma(link = "log"),
                     select = T,
                     method = "REML")
summary(gam.mod.select)
select = "selected"


# make predictions from gam model
p = predict(gam.mod, 
            type = "link",
            se.fit = TRUE)



# add predictions to df, including 95% CI
dat$pred = gam.mod$family$linkinv(p$fit)
dat$ymin = gam.mod$family$linkinv(p$fit - 1.96 * p$se.fit)
dat$ymax = gam.mod$family$linkinv(p$fit + 1.96 * p$se.fit)


fig2a = 
  
  ggplot(data = dat, aes(x = year, y = biomass_density_tonnes_per_km2)) +
  
  # point and lines for average densities
  geom_point(col = cols[1], size = 0.75) +
  geom_line(col = cols[1], size = 0.25) +
  
  # error bars for average densities
  geom_errorbar(aes(ymin = low_ci, ymax = high_ci), 
                width = 0, size = 0.25, col = cols[1], alpha = 0.5) +
  
  # line for GAM predictions
  geom_line(aes(y = pred)) +
  
  # CI interval GAM predictions
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) +
  
  # add label for SD
  annotate(geom = "text", x = 1993.5, y = 5.30, label = "SD 25", hjust = "left", family = "spec-font", size = 21) + 
  
  # add p value to plot
  annotate(geom = "text", x = 1993.5, y = 1.5, 
           label = paste("p =", p.value), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add deltaAIC value to plot
  annotate(geom = "text", x = 1993.5, y = 0.95, 
           label = bquote(Delta*AIC[C] ~ "=" ~ .(dAIC)),
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add result of variable selection
  annotate(geom = "text", x = 1993.5, y = 0.5, 
           label = select, 
           hjust = "left", family = "spec-font", size = 18, fontface = "bold") + 
  
  # fix labels
  labs(
    x = "Year",
    y = expression(paste("Biomass (tonnes km"^"-2", ")"))) +
  
  # adjust plot limits
  lims( x = c(1993,2020)) +
  #scale_y_continuous(limits=c(0, 7.3),oob=squish) +
  
  # graphical settings
  theme_bw(base_size = 12) +
  theme_sets


#### 2b ####
## SD 27-29 ##

# subset data
dat = BIAS[BIAS$group == "SD27_29",]

# fit and check gam model
gam.mod = gam(biomass_density_tonnes_per_km2 ~ s(year, k = 3),
              data = dat,
              family = Gamma(link = "log"),
              method = "REML")
gam.check(gam.mod)
summary(gam.mod)

# save p-value
p.value = signif(summary(gam.mod)$s.table[,4], digits = 1)

# check temporal autocorrelation
acf(residuals(gam.mod), lag.max = 2) 
# some signs of neg corr 2 years, but this is unlikely to be due to stickleback demographics - we would expect positive corrs (a lot of stickleback year t -> high recruitment -> plenty of stickleback in two years)

# fit null model without year effect
gam.mod.null = gam(biomass_density_tonnes_per_km2 ~ 1,
                   data = dat,
                   family = Gamma(link = "log"),
                   method = "REML")

# calculate delta-AIC
dAIC = signif(AICc(gam.mod)-AICc(gam.mod.null), digits = 2)

# added penalty variable selection
gam.mod.select = gam(biomass_density_tonnes_per_km2 ~ s(year),
                     data = dat,
                     family = Gamma(link = "log"),
                     select = T,
                     method = "REML")
summary(gam.mod.select)
select = "selected"


# make predictions from gam model
p = predict(gam.mod, 
            type = "link",
            se.fit = TRUE)


# add predictions to df, including 95% CI
dat$pred = gam.mod$family$linkinv(p$fit)
dat$ymin = gam.mod$family$linkinv(p$fit - 1.96 * p$se.fit)
dat$ymax = gam.mod$family$linkinv(p$fit + 1.96 * p$se.fit)


fig2b = 
  
  ggplot(data = dat, aes(x = year, y = biomass_density_tonnes_per_km2)) +
  
  # point and lines for average densities
  geom_point(col = cols[2], size = 0.75) +
  geom_line(col = cols[2], size = 0.25) +
  
  # error bars for average densities
  geom_errorbar(aes(ymin = low_ci, ymax = high_ci), 
                width = 0, size = 0.25, col = cols[2], alpha = 0.5) +
  
  # line for GAM predictions
  geom_line(aes(y = pred)) +
  
  # CI interval GAM predictions
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) +
  
  # add label for SD
  annotate(geom = "text", x = 1993.5, y = 6.97, label = "SD 27–29", hjust = "left", family = "spec-font", size = 20) + 
  
  # add p value to plot
  annotate(geom = "text", x = 1993.5, y = 3.78, 
           label = paste("p =", p.value), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add deltaAIC value to plot
  annotate(geom = "text", x = 1993.5, y = 3.08, 
           label = bquote(Delta*AIC[C] ~ "=" ~ .(dAIC)), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add result of variable selection
  annotate(geom = "text", x = 1993.5, y = 2.5, 
           label = select, 
           hjust = "left", family = "spec-font", size = 18, fontface = "bold") + 
  
  
  # fix labels
  labs(
    x = "Year",
    y = expression(paste("Biomass (tonnes km"^"-2", ")"))) +
  
  # adjust plot limits
  lims( x = c(1993,2020)) +
  #scale_y_continuous(limits=c(0, 7.3),oob=squish) +
  
  # graphical settings
  theme_bw(base_size = 12) +
  theme_sets


#### 2c ####
## SD 30 ##

# subset data
dat = BIAS[BIAS$group == "SD30",]

# fit and check gam model
gam.mod = gam(biomass_density_tonnes_per_km2 ~ s(year, k = 3),
              data = dat,
              family = Gamma(link = "log"),
              method = "REML")
gam.check(gam.mod)
summary(gam.mod)

# save p-value
p.value = signif(summary(gam.mod)$s.table[,4], digits = 1)

# check temporal autocorrelation
acf(residuals(gam.mod), lag.max = 2)

# fit null model without year effect
gam.mod.null = gam(biomass_density_tonnes_per_km2 ~ 1,
                   data = dat,
                   family = Gamma(link = "log"),
                   method = "REML")

# calculate delta-AIC
dAIC = signif(AICc(gam.mod)-AICc(gam.mod.null), digits = 2)

# added penalty variable selection
gam.mod.select = gam(biomass_density_tonnes_per_km2 ~ s(year),
                     data = dat,
                     family = Gamma(link = "log"),
                     select = T,
                     method = "REML")
summary(gam.mod.select)
select = "selected"

# make predictions from gam model
p = predict(gam.mod, 
            type = "link",
            se.fit = TRUE)

# add predictions to df, including 95% CI
dat$pred = gam.mod$family$linkinv(p$fit)
dat$ymin = gam.mod$family$linkinv(p$fit - 1.96 * p$se.fit)
dat$ymax = gam.mod$family$linkinv(p$fit + 1.96 * p$se.fit)


fig2c = 
  
  ggplot(data = dat, aes(x = year, y = biomass_density_tonnes_per_km2)) +
  
  # point and lines for average densities
  geom_point(col = cols[3], size = 0.75) +
  geom_line(col = cols[3], size = 0.25) +
  
  # error bars for average densities
  geom_errorbar(aes(ymin = low_ci, ymax = high_ci), 
                width = 0, size = 0.25, col = cols[3], alpha = 0.5) +
  
  # line for GAM predictions
  geom_line(aes(y = pred)) +
  
  # CI interval GAM predictions
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) +
  
  # add label for SD
  annotate(geom = "text", x = 1993.5, y = 2.9, label = "SD 30", hjust = "left", family = "spec-font", size = 20) + 
  
  # add p value to plot
  annotate(geom = "text", x = 1995.5, y = 0.95, 
           label = paste("p =", p.value), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add deltaAIC value to plot
  annotate(geom = "text", x = 1995.5, y = 0.65, 
           label = bquote(Delta*AIC[C] ~ "=" ~ .(dAIC)), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add result of variable selection
  annotate(geom = "text", x = 1995.5, y = 0.4, 
           label = select, 
           hjust = "left", family = "spec-font", size = 18, fontface = "bold") + 
  
  
  # fix labels
  labs(
    x = "Year",
    y = expression(paste("Biomass (tonnes km"^"-2", ")"))) +
  
  # adjust plot limits
  lims( x = c(1993,2020)) +
  scale_y_continuous(limits=c(0, 3.1),oob=squish) +
  
  # graphical settings
  theme_bw(base_size = 12) +
  theme_sets



### Swedish juvenile data ###

# make SD group factor
juvs$area = factor(juvs$area, levels = c("SD30", "SD27_29", "SD25"))


#### 2d ####
## SD 25 ##

# subset data
dat = juvs[juvs$area == "SD25",]

# fit and check gam model
gam.mod = gam(abundance ~ s(year, k = 5),
              data = dat,
              family = Gamma(link = "log"),
              method = "REML")
gam.check(gam.mod)
summary(gam.mod)

# save p-value
p.value = signif(summary(gam.mod)$s.table[,4], digits = 1)

# check temporal autocorrelation
acf(residuals(gam.mod), lag.max = 2) 

# fit null model without year effect
gam.mod.null = gam(abundance ~ 1,
                   data = dat,
                   family = Gamma(link = "log"),
                   method = "REML")

# calculate delta_AIC
dAIC = signif(AICc(gam.mod)-AICc(gam.mod.null), digits = 2)

# added penalty variable selection
gam.mod.select = gam(abundance ~ s(year),
                     data = dat,
                     family = Gamma(link = "log"),
                     select = T,
                     method = "REML")
summary(gam.mod.select)
select = "selected"

# make predictions from gam model
p = predict(gam.mod, 
            type = "link",
            se.fit = TRUE)

# add predictions to df, including 95% CI
dat$pred = gam.mod$family$linkinv(p$fit)
dat$ymin = gam.mod$family$linkinv(p$fit - 1.96 * p$se.fit)
dat$ymax = gam.mod$family$linkinv(p$fit + 1.96 * p$se.fit)


fig2d = 
  
  ggplot(data = dat, aes(x = year, y = abundance)) +
  
  # point and lines for average densities
  geom_point(col = cols[1], size = 0.75) +
  geom_line(col = cols[1], size = 0.25) +
  
  # error bars for average densities
  geom_errorbar(aes(ymin = low_ci, ymax = high_ci), 
                width = 0, size = 0.25, col = cols[1], alpha = 0.5) +
  
  # line for GAM predictions
  geom_line(aes(y = pred)) +
  
  # CI interval GAM predictions
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) +
  
  # add label for SD
  annotate(geom = "text", x = 1993.5, y = 477, label = "SD 25", hjust = "left", family = "spec-font", size = 20) + 
  
  # add p value to plot
  annotate(geom = "text", x = 1996, y = 95, 
           label = paste("p =", p.value), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add deltaAIC value to plot
  annotate(geom = "text", x = 1996, y = 52, 
           label = bquote(Delta*AIC[C] ~ "=" ~ .(dAIC)), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add result of variable selection
  annotate(geom = "text", x = 1996, y = 17, 
           label = select, 
           hjust = "left", family = "spec-font", size = 18, fontface = "bold") + 
  
  # fix labels
  labs(
    x = "Year",
    y = expression(paste("CPUE (fish detonation"^"-1", ")"))) +
  
  # adjust plot limits
  lims( x = c(1993,2020)) +
  scale_y_continuous(breaks = c(0, 100, 200, 300, 400, 500),
                     labels = c("0", "100", "200", "300", "400", ""),
                     limits = c(0, 500)) +
  
  # graphical settings
  theme_bw(base_size = 12) +
  theme_sets

#### 2e ####
## SD 27-29 ##

# subset data
dat = juvs[juvs$area == "SD27_29",]

# fit and check gam model
gam.mod = gam(abundance ~ s(year, k = 3),
              data = dat,
              family = Gamma(link = "log"),
              method = "REML")
gam.check(gam.mod)
summary(gam.mod)

# save p-value
p.value = signif(summary(gam.mod)$s.table[,4], digits = 1)

# check temporal autocorrelation
acf(residuals(gam.mod), lag.max = 2) 

# fit null model without year effect
gam.mod.null = gam(abundance ~ 1,
                   data = dat,
                   family = Gamma(link = "log"),
                   method = "REML")

# calculate delta-AIC
dAIC = signif(AICc(gam.mod)-AICc(gam.mod.null), digits = 2)

# added penalty variable selection
gam.mod.select = gam(abundance ~ s(year),
                     data = dat,
                     family = Gamma(link = "log"),
                     select = T,
                     method = "REML")
summary(gam.mod.select)
select = "selected"

# make predictions from gam model
p = predict(gam.mod, 
            type = "link",
            se.fit = TRUE)

# add predictions to df, including 95% CI
dat$pred = gam.mod$family$linkinv(p$fit)
dat$ymin = gam.mod$family$linkinv(p$fit - 1.96 * p$se.fit)
dat$ymax = gam.mod$family$linkinv(p$fit + 1.96 * p$se.fit)

fig2e = 
  
  ggplot(data = dat, aes(x = year, y = abundance)) +
  
  # point and lines for average densities
  geom_point(col = cols[2], size = 0.75) +
  geom_line(col = cols[2], size = 0.25) +
  
  # error bars for average densities
  geom_errorbar(aes(ymin = low_ci, ymax = high_ci), 
                width = 0, size = 0.25, col = cols[2], alpha = 0.5) +
  
  # line for GAM predictions
  geom_line(aes(y = pred)) +
  
  # CI interval GAM predictions
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) +
  
  # add label for SD
  annotate(geom = "text", x = 1993.5, y = 210, label = "SD 27 & 29", hjust = "left", family = "spec-font", size = 20) + 
  
  
  # add p value to plot
  annotate(geom = "text", x = 1993, y = 77, 
           label = paste("p =", p.value), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add deltaAIC value to plot
  annotate(geom = "text", x = 1993, y = 56, 
           label = bquote(Delta*AIC[C] ~ "=" ~ .(dAIC)), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add result of variable selection
  annotate(geom = "text", x = 1993, y = 40, 
           label = select, 
           hjust = "left", family = "spec-font", size = 18, fontface = "bold") + 
  
  
  # fix labels
  labs(
    x = "Year",
    y = expression(paste("CPUE (fish detonation"^"-1", ")"))) +
  
  # adjust plot limits
  lims( x = c(1993,2020)) +
  #scale_y_continuous(limits=c(0,780),oob=squish) +
  
  
  # graphical settings
  theme_bw(base_size = 12) +
  theme_sets



#### 2f ####
## SD 30 ##

# subset data
dat = juvs[juvs$area == "SD30",]

dat$abundance[dat$abundance == 0] = 2e-16 # set to very small value as Gamma doesn't allow 0s


# fit and check gam model
gam.mod = gam(abundance ~ s(year, k = 3),
              data = dat,
              family = Gamma(link = "log"),
              method = "REML")
gam.check(gam.mod)
summary(gam.mod)

# save p-value
p.value = signif(summary(gam.mod)$s.table[,4], digits = 1)

# check temporal autocorrelation
acf(residuals(gam.mod), lag.max = 2) 

# fit null model without year effect
gam.mod.null = gam(abundance ~ 1,
                   data = dat,
                   family = Gamma(link = "log"),
                   method = "REML")

# calculate delta-AIC
dAIC = signif(AICc(gam.mod)-AICc(gam.mod.null), digits = 2)

# added penalty variable selection
gam.mod.select = gam(abundance ~ s(year),
                     data = dat,
                     family = Gamma(link = "log"),
                     select = T,
                     method = "REML")
summary(gam.mod.select)
select = "not selected"


# make predictions from gam model
p = predict(gam.mod, 
            type = "link",
            se.fit = TRUE)

# add predictions to df, including 95% CI
dat$pred = gam.mod$family$linkinv(p$fit)
dat$ymin = gam.mod$family$linkinv(p$fit - 1.96 * p$se.fit)
dat$ymax = gam.mod$family$linkinv(p$fit + 1.96 * p$se.fit)


fig2f = 
  
  ggplot(data = dat, aes(x = year, y = abundance)) +
  
  # point and lines for average densities
  geom_point(col = cols[3], size = 0.75) +
  geom_line(col = cols[3], size = 0.25) +
  
  # error bars for average densities
  geom_errorbar(aes(ymin = low_ci, ymax = high_ci), 
                width = 0, size = 0.25, col = cols[3], alpha = 0.5) +
  
  # line for GAM predictions
  geom_line(aes(y = pred)) +
  
  # CI interval GAM predictions
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) + 
  
  # add label for SD
  annotate(geom = "text", x = 1993.5, y = 695, label = "SD 30", hjust = "left", family = "spec-font", size = 20) + 
  
  
  # add p value to plot
  annotate(geom = "text", x = 1994, y = 395, 
           label = paste("p =", p.value), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add deltaAIC value to plot
  annotate(geom = "text", x = 1994, y = 320, 
           label = bquote(Delta*AIC[C] ~ "=" ~ .(dAIC)), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add result of variable selection
  annotate(geom = "text", x = 1994, y = 258, 
           label = select, 
           hjust = "left", family = "spec-font", size = 18, fontface = "bold") + 
  
  # fix labels
  labs(
    x = "Year",
    y = expression(paste("CPUE (fish detonation"^"-1", ")"))) +
  
  # adjust plot limits
  lims( x = c(1993,2020)) +
  #scale_y_continuous(limits=c(0,780),oob=squish) +
  
  # graphical settings
  theme_bw(base_size = 12) +
  theme_sets




### cooling water data from Forsmark ###

#### 2g ####
# fit and check gam model spring
gam.mod.spring = gam(density_inds_per_m3 ~ s(year, k = 13),
                     data = forsmark[forsmark$season == "spring",],
                     family = Gamma(link = "log"),
                     method = "REML")
gam.check(gam.mod.spring)
summary(gam.mod.spring)

# save p-value
p.value = signif(summary(gam.mod.spring)$s.table[,4], digits = 1)

# check temporal autocorrelation
acf(residuals(gam.mod.spring), lag.max = 2) 

# fit null model without year effect
gam.mod.null = gam(density_inds_per_m3 ~ 1,
                   data = forsmark[forsmark$season == "spring",],
                   family = Gamma(link = "log"),
                   method = "REML")

# calculate delta-AIC
dAIC = signif(AICc(gam.mod.spring)-AICc(gam.mod.null), digits = 2)

# added penalty variable selection
gam.mod.select = gam(density_inds_per_m3 ~ s(year),
                     data = forsmark[forsmark$season == "spring",],
                     family = Gamma(link = "log"),
                     select = T,
                     method = "REML")
summary(gam.mod.select)
select = "selected"



# make predictions from gam model spring
p =   predict(gam.mod.spring, 
              newdata =  data.frame(year = forsmark$year[forsmark$season == "spring"]),
              type = "link",
              se.fit = TRUE)

# add predictions to df, including 95% CI
forsmark$pred[forsmark$season == "spring"] = gam.mod.spring$family$linkinv(p$fit)
forsmark$ymin[forsmark$season == "spring"] = gam.mod.spring$family$linkinv(p$fit - 1.96 * p$se.fit)
forsmark$ymax[forsmark$season == "spring"] = gam.mod.spring$family$linkinv(p$fit + 1.96 * p$se.fit)


fig2g = 
  
  ggplot(data = forsmark[forsmark$season == "spring",], aes(x = year, y = density_inds_per_m3)) +
  
  # point and lines for average densities
  geom_point(size = 0.75, col = "grey50") +
  geom_line(size = 0.25, col = "grey50") +
  
  # error bars for average densities
  geom_errorbar(aes(ymin = low_ci, ymax = high_ci), 
                width = 0, size = 0.25, col = "grey50", alpha = 0.5) +
  
  # line for GAM predictions
  geom_line(aes(y = pred)) +
  
  # CI interval GAM predictions
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) + 
  
  # add label for location + season
  annotate(geom = "text", x = 1993, y = 0.114, label = "Forsmark, Sweden", hjust = "left", family = "spec-font", size = 20) + 
  annotate(geom = "text", x = 1993, y = 0.105, label = "Spring", hjust = "left", family = "spec-font", size = 20) + 
  
  
  # add p value to plot
  annotate(geom = "text", x = 1997, y = 0.036, 
           label = paste("p =", p.value), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add deltaAIC value to plot
  annotate(geom = "text", x = 1997, y = 0.025, 
           label = bquote(Delta*AIC[C] ~ "=" ~ .(dAIC)), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add result of variable selection
  annotate(geom = "text", x = 1997, y = 0.016, 
           label = select, 
           hjust = "left", family = "spec-font", size = 18, fontface = "bold") + 
  
  
  # fix labels
  labs(
    x = "Year",
    y = expression(paste("Density (fish m"^"-3", ")"))) +
  
  # adjust plot limits
  lims( x = c(1993,2020)) +
  scale_y_continuous(breaks = c(0, 0.03, 0.06, 0.09, 0.12),
                     labels = c("0", "0.03", "0.06", "0.09", ""),
                     limits = c(0, 0.12)) +
  
  # graphical settings
  theme_bw(base_size = 12) +
  theme_sets 


#### 2h ####
# fit and check gam model autumn
gam.mod.autumn = gam(density_inds_per_m3 ~ s(year, k = 3),
                     data = forsmark[forsmark$season == "autumn",],
                     family = Gamma(link = "log"),
                     method = "REML")
gam.check(gam.mod.autumn)
summary(gam.mod.autumn)

# save p-value
p.value = signif(summary(gam.mod.spring)$s.table[,4], digits = 1)

# check temporal autocorrelation
acf(residuals(gam.mod.autumn), lag.max = 2) 

# fit null model without year effect
gam.mod.null = gam(density_inds_per_m3 ~ 1,
                   data = forsmark[forsmark$season == "autumn",],
                   family = Gamma(link = "log"),
                   method = "REML")

# save delta-AIC
dAIC = signif(AICc(gam.mod.autumn)-AICc(gam.mod.null), digits = 2)

# added penalty variable selection
gam.mod.select = gam(density_inds_per_m3 ~ s(year),
                     data = forsmark[forsmark$season == "autumn",],
                     family = Gamma(link = "log"),
                     select = T,
                     method = "REML")
summary(gam.mod.select)
select = "selected"


# make predictions from gam model autumn
p =   predict(gam.mod.autumn, 
              newdata =  data.frame(year = forsmark$year[forsmark$season == "autumn"]),
              type = "link",
              se.fit = TRUE)

# add predictions to df, including 95% CI
forsmark$pred[forsmark$season == "autumn"] = gam.mod.autumn$family$linkinv(p$fit)
forsmark$ymin[forsmark$season == "autumn"] = gam.mod.autumn$family$linkinv(p$fit - 1.96 * p$se.fit)
forsmark$ymax[forsmark$season == "autumn"] = gam.mod.autumn$family$linkinv(p$fit + 1.96 * p$se.fit)



fig2h = 
  
  
  ggplot(data = forsmark[forsmark$season == "autumn",], aes(x = year, y = density_inds_per_m3)) +
  
  # point and lines for average densities
  geom_point(size = 0.75, col = "grey50") +
  geom_line(size = 0.25, col = "grey50") +
  
  # error bars for average densities
  geom_errorbar(aes(ymin = low_ci, ymax = high_ci), 
                width = 0, size = 0.25, col = "grey50", alpha = 0.5) +
  
  # line for GAM predictions
  geom_line(aes(y = pred)) +
  
  # CI interval GAM predictions
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) + 
  
  # add label for location and season
  annotate(geom = "text", x = 1993, y = 0.130, label = "Forsmark, Sweden", hjust = "left", family = "spec-font", size = 20) + 
  annotate(geom = "text", x = 1993, y = 0.119, label = "Autumn", hjust = "left", family = "spec-font", size = 20) + 
  
  
  # add p value to plot
  annotate(geom = "text", x = 1993, y = 0.038, 
           label = paste("p =", p.value), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add deltaAIC value to plot
  annotate(geom = "text", x = 1993, y = 0.025, 
           label = bquote(Delta*AIC[C] ~ "=" ~ .(dAIC)), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add result of variable selection
  annotate(geom = "text", x = 1993, y = 0.014, 
           label = select, 
           hjust = "left", family = "spec-font", size = 18, fontface = "bold") + 
  
  # fix labels
  labs(
    x = "Year",
    y = expression(paste("Density (fish m"^"-3", ")"))) +
  
  # adjust plot limits
  lims( x = c(1993,2020)) +
  #scale_y_continuous(limits=c(0,0.135),oob=squish) +
  
  # graphical settings
  theme_bw(base_size = 12) +
  theme_sets 



#### 2i ####
### trapping data from Finland ###

# fix values for CI
low = finland$stick[finland$value == "mid"] - finland$stick[finland$value == "low"]
high = finland$stick[finland$value == "high"] - finland$stick[finland$value == "mid"]
dat = finland[finland$value == "mid",]

# add year column
dat$year = as.numeric(dat$year)

# fit and check gam model
gam.mod = gam(stick ~ s(year, k = 3), # k adjusted due to few values
              data = dat,
              family = Gamma(link = "log"),
              method = "REML")
gam.check(gam.mod)
summary(gam.mod)

# save p-value
p.value = signif(summary(gam.mod)$s.table[,4], digits = 1)

# check temporal autocorrelation
acf(residuals(gam.mod), lag.max = 2) 

# fit null model without year effect
gam.mod.null = gam(stick ~ 1,
                   data = dat,
                   family = Gamma(link = "log"),
                   method = "REML")

# calculate delta-AIC
dAIC = signif(AICc(gam.mod)-AICc(gam.mod.null), digits = 2)

# added penalty variable selection
gam.mod.select = gam(stick ~ s(year, k = 9), # k adjusted due to few values
                     data = dat,
                     family = Gamma(link = "log"),
                     select = T,
                     method = "REML")
summary(gam.mod.select)
select = "selected"


# make predictions from gam model
p = predict(gam.mod, 
            type = "link",
            se.fit = TRUE)

dat$pred = gam.mod$family$linkinv(p$fit)
dat$ymin = gam.mod$family$linkinv(p$fit - 1.96 * p$se.fit)
dat$ymax = gam.mod$family$linkinv(p$fit + 1.96 * p$se.fit)


fig2i = 
  
  ggplot(data = dat, aes(x = year, y = stick)) +
  
  # point and lines for average densities
  geom_point(size = 0.80, col = "grey50") +
  geom_line(size = 0.25, col = "grey50") +
  
  # error bars for average densities
  geom_errorbar(aes(ymin = stick - low, ymax = stick + high), width = 0, size = 0.25, col = "grey50") +
  
  # line for GAM predictions
  geom_line(aes(y = pred)) +
  
  # CI interval GAM predictions
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) + 
  
  # add label for location
  annotate(geom = "text", x = 1993, y = 8.5, label = "Tvärminne, Finland", hjust = "left", family = "spec-font", size = 20) + 
  
  
  # add p value to plot
  annotate(geom = "text", x = 2001, y = 4.1, 
           label = paste("p =", p.value), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add deltaAIC value to plot
  annotate(geom = "text", x = 2001, y = 3.5, 
           label = bquote(Delta*AIC[C] ~ "=" ~ .(dAIC)), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add result of variable selection
  annotate(geom = "text", x = 2001, y = 3.0, 
           label = select, 
           hjust = "left", family = "spec-font", size = 18, fontface = "bold") +
  
  # fix labels
  labs(
    x = "Year",
    y = expression(paste("CPUE (fish trap"^"-1", ")"))) +
  
  # adjust plot limits
  lims( x = c(1993,2020)) +
  
  # graphical settings
  theme_bw(base_size = 12) +
  theme_sets 



### trapping data from Latvia ###


latvia$stickleback[latvia$stickleback == 0] = 2e-16 # set to very small value as Gamma doesn't alow 0s

#### 2j ####
# fit and check gam model Pape
gam.mod.pape = gam(stickleback ~ s(year, k = 3), # gam.check suggest k too low but straight line no matter choice of k - keep at minimum
                   data = latvia[latvia$area == "Pape",],
                   family = Gamma(link = "log"),
                   method = "REML")
gam.check(gam.mod.pape)
summary(gam.mod.pape)

# save p-value
p.value = signif(summary(gam.mod.pape)$s.table[,4], digits = 1)

# check temporal autocorrelation
acf(residuals(gam.mod.pape), lag.max = 2) 
# some hints of a positive correlation at 1 year, which would be sensible, but we are not making any claims of a trend here

# fit null model without year effect
gam.mod.null = gam(stickleback ~ 1,
                   data = latvia[latvia$area == "Pape",],
                   family = Gamma(link = "log"),
                   method = "REML")

# calculate delta-AIC
dAIC = signif(AICc(gam.mod.pape)-AICc(gam.mod.null), digits = 2)

# added penalty variable selection
gam.mod.select = gam(stickleback ~ s(year),
                     data = latvia[latvia$area == "Pape",],
                     family = Gamma(link = "log"),
                     select = T,
                     method = "REML")
summary(gam.mod.select)
select = "not selected"

# make predictions from gam model Pape
p =   predict(gam.mod.pape, 
              newdata =  data.frame(year = latvia$year[latvia$area == "Pape"]),
              type = "link",
              se.fit = TRUE)

# add predictions to df, including 95% CI
latvia$pred[latvia$area == "Pape"] = gam.mod.pape$family$linkinv(p$fit)
latvia$ymin[latvia$area == "Pape"] = gam.mod.pape$family$linkinv(p$fit - 1.96 * p$se.fit)
latvia$ymax[latvia$area == "Pape"] = gam.mod.pape$family$linkinv(p$fit + 1.96 * p$se.fit)


fig2j = 
  
  ggplot(data = latvia[latvia$area == "Pape",], aes(x = year, y = stickleback)) +
  
  # point and lines for average densities
  geom_point(size = 0.75, col = "grey50") +
  geom_line(size = 0.25, col = "grey50") +
  
  # line for GAM predictions
  geom_line(aes(y = pred)) +
  
  # CI interval GAM predictions
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) + 
  
  # add label for location
  annotate(geom = "text", x = 1993, y = 0.63, label = "Pape, Latvia", hjust = "left", family = "spec-font", size = 20) + 
  
  
  # add p value to plot
  annotate(geom = "text", x = 2008, y = 0.62, 
           label = paste("p =", p.value), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add deltaAIC value to plot
  annotate(geom = "text", x = 2008, y = 0.56, 
           label = bquote(Delta*AIC[C] ~ "=" ~ .(dAIC)), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add result of variable selection
  annotate(geom = "text", x = 2008, y = 0.51, 
           label = select, 
           hjust = "left", family = "spec-font", size = 18, fontface = "bold") +
  
  # fix labels
  labs(
    x = "Year",
    y = expression(paste("CPUE (fish haul"^"-1", ")"))) +
  
  # adjust plot limits
  lims( x = c(1993,2020)) +
  
  # graphical settings
  theme_bw() +
  theme_sets 


#### 2k ####
# fit and check gam model Kolka
gam.mod.kolka = gam(stickleback ~ s(year, k = 10),
                    data = latvia[latvia$area == "Kolka",],
                    family = Gamma(link = "log"),
                    method = "REML")
gam.check(gam.mod.kolka)
summary(gam.mod.kolka)

# save p-value
p.value = signif(summary(gam.mod.kolka)$s.table[,4], digits = 1)

# check temporal autocorrelation
acf(residuals(gam.mod.kolka), lag.max = 2) 

# fit null model without year effect
gam.mod.null = gam(stickleback ~ 1,
                   data = latvia[latvia$area == "Kolka",],
                   family = Gamma(link = "log"),
                   method = "REML")

# calculate delta-AIC
dAIC = signif(AICc(gam.mod.kolka)-AICc(gam.mod.null), digits = 2)

# added penalty variable selection
gam.mod.select = gam(stickleback ~ s(year),
                     data = latvia[latvia$area == "Kolka",],
                     family = Gamma(link = "log"),
                     select = T,
                     method = "REML")
summary(gam.mod.select)
select = "selected"


# make predictions from gam model Kolka
p =   predict(gam.mod.kolka, 
              newdata =  data.frame(year = latvia$year[latvia$area == "Kolka"]),
              type = "link",
              se.fit = TRUE)

# add predictions to df, including 95% CI
latvia$pred[latvia$area == "Kolka"] = gam.mod.kolka$family$linkinv(p$fit)
latvia$ymin[latvia$area == "Kolka"] = gam.mod.kolka$family$linkinv(p$fit - 1.96 * p$se.fit)
latvia$ymax[latvia$area == "Kolka"] = gam.mod.kolka$family$linkinv(p$fit + 1.96 * p$se.fit)


fig2k = 
  
  ggplot(data = latvia[latvia$area == "Kolka",], aes(x = year, y = stickleback)) +
  
  # point and lines for average densities
  geom_point(size = 0.75, col = "grey50") +
  geom_line(size = 0.25, col = "grey50") +
  
  # line for GAM predictions
  geom_line(aes(y = pred)) +
  
  # CI interval GAM predictions
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) + 
  
  # add label for location
  annotate(geom = "text", x = 2007, y = 2.43, label = "Kolka, Latvia", hjust = "left", family = "spec-font", size = 20) + 
  
  
  # add p value to plot
  annotate(geom = "text", x = 1993, y = 0.46, 
           label = paste("p =", p.value), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add deltaAIC value to plot
  annotate(geom = "text", x = 1993, y = 0.22, 
           label = bquote(Delta*AIC[C] ~ "=" ~ .(dAIC)), 
           hjust = "left", family = "spec-font", size = 18) + 
  
  # add result of variable selection
  annotate(geom = "text", x = 1993, y = 0.01, 
           label = select, 
           hjust = "left", family = "spec-font", size = 18, fontface = "bold") +
  
  # fix labels
  labs(
    x = "Year",
    y = expression(paste("CPUE (fish haul"^"-1", ")"))) +
  
  # adjust plot limits
  lims( x = c(1993,2020)) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5, 2, 2.5),
                     labels = c("0.5", "1.0", "1.5", "2.0", ""),
                     limits = c(0, 2.6)) +
  
  # graphical settings
  theme_bw() +
  theme_sets 








# align plots
plots1 = align_plots(fig2a, fig2d, fig2i, fig2g, align = 'v', axis = 'l')
plots2 = align_plots(fig2b, fig2e, fig2j, align = 'v', axis = 'l')
plots3 = align_plots(fig2c, fig2f, fig2k, align = 'v', axis = 'l')


# order plots and fix panel settings
top_row = plot_grid(plots1[[1]], plots2[[1]], plots3[[1]],
                    labels = c('a.', 'b.', 'c.'),
                    label_size = 70,
                    label_fontfamily = "spec-font",
                    label_fontface = "plain", 
                    label_x = c(0.16, 0.12, 0.12),
                    ncol = 3)

mid_row1 = plot_grid(plots1[[2]], plots2[[2]], plots3[[2]],
                     labels = c('d.', 'e.', 'f.'),
                     label_size = 70,
                     label_fontfamily = "spec-font",
                     label_fontface = "plain", 
                     label_x = c(0.15, 0.12, 0.15), 
                     ncol = 3)

mid_row2 =   plot_grid(plots1[[4]], fig2h, labels = c('g.', 'h.'), 
                       label_size = 70,
                       label_fontfamily = "spec-font",
                       label_fontface = "plain", 
                       label_x = c(0.1, 0.09), 
                       ncol = 2)

bottom_row =  plot_grid(plots1[[3]], plots2[[3]], plots3[[3]], 
                        labels = c('i.', 'j.', 'k.'), 
                        label_size = 75,
                        label_fontfamily = "spec-font",
                        label_fontface = "plain", 
                        label_x = c(0.19, 0.13, 0.12),
                        ncol = 3)



## initialise fig ##
jpeg("fig2.jpeg", 
    width = 17, height = 20, units = 'cm', res = 600, pointsize = 9, family = "sans")

## print fig

print(plot_grid(top_row, mid_row1, mid_row2, bottom_row, 
                
                ncol = 1))

## close fig ##
dev.off()



#### FIGURE 4 ####

# polygon indicating area of increase
stickleback_polygon = data.frame(
  x = c(14, 19, 22, 24, 22, 17.5, 14),
  y = c(55.75, 55.75, 59, 59.5, 63.5,63.5, 55.75)
  
)


### pike and perch ###

# load data
pp = read.csv("PikePerch.csv", sep = ";")

# add symbols and figures to illustrate perch trends
pp$arrow_perch = '\u2191'; pp$perch_col = "darkgreen"
pp$arrow_perch[pp$trend_perch == "no_trend"] = '\u2192'; pp$perch_col[pp$trend_perch == "no_trend"] = "black"
pp$arrow_perch[pp$trend_perch == "neg"] = '\u2193'; pp$perch_col[pp$trend_perch == "neg"] = "darkred"
pp$long_perch = pp$long
pp$long_perch[(pp$trend_pike) != ""] = pp$long[(pp$trend_pike) != ""] - 0.23
pp$add_circle = FALSE
pp$add_circle[pp$location %in% c("SD31", "The Quark", "SD30", "SD29", "SD32")] = TRUE

# add symbols and figures to illustrate pike trends
pp$arrow_pike = '\u21d1'; pp$pike_col = "darkgreen"
pp$arrow_pike[pp$trend_pike == "no_trend"] = '\u21d2'; pp$pike_col[pp$trend_pike == "no_trend"] = "black"
pp$arrow_pike[pp$trend_pike == "neg"] = '\u21d3'; pp$pike_col[pp$trend_pike == "neg"] = "darkred"
pp$long_pike = NA
pp$long_pike[(pp$trend_pike) != ""] = pp$long[(pp$trend_pike) != ""] + 0.23


# plot
pp_plot =
  ggplot(data = geom_coast) + # coastline
  
  labs(x = "Longitude", y = "Latitude", title = "a. Pike and perch population trends") + # add labels
  
  geom_sf(fill = "grey",  colour = "grey" , size = 0.005) + # fix colours coastline
  
  
  # add ICES SDs
  geom_sf(data = ICES_lines, fill = NA, size = 0.5, colour = "grey") + 
  coord_sf(xlim = c(14, 30), ylim = c(53.8,65.8)) +
  
  annotate("text",
           label = c("30", "32", "29", "26", "27", "28","25", "31"),
           x = c(19.5, 26.5, 20.5, 19.5, 17.5, 20, 16.5, 23),
           y = c(62, 59.8, 59.25, 55.2, 57.6, 57.6, 55.2, 64.75),
           size = 18,
           family = "spec-font"
  ) +
  
  # arrows for pike and perch
  annotate("text", x = pp$long_perch, y = pp$lat, label = sprintf(pp$arrow_perch), colour = pp$perch_col, size = 16) +
  annotate("text", x = pp$long_pike, y = pp$lat, label = sprintf(pp$arrow_pike), colour = pp$pike_col, size = 16) +
  
  # indicate which data points refer to a larger area
  annotate("text", x = pp$long_perch[pp$add_circle], y = pp$lat[pp$add_circle]-0.1, label = "_", size = 18, family = "spec-font") +
  
  # indicate areas of stickleback increase
  geom_path(data = stickleback_polygon, aes(x = x, y = y), linetype = "longdash", size = 0.3, colour = "grey39") +
  
  # legend
  annotate("text", x = 24.3, y = 55, label = sprintf('\u2191'), colour = "darkgreen", size = 18) +
  annotate("text", x = 25, y = 55, label = sprintf('\u2193'), colour = "darkred", size = 18) +
  annotate("text", x = 25.7, y = 55, label = sprintf('\u2192'), size = 18) +
  annotate("text", x = 27.8, y = 55, label = "perch", size = 18, family = "spec-font") +
  
  annotate("text", x = 24.3, y = 54, label = sprintf('\u21d1'), colour = "darkgreen", size = 18) +
  annotate("text", x = 25, y = 54, label = sprintf('\u21d3'), colour = "darkred", size = 18) +
  annotate("text", x = 25.7, y = 54, label = sprintf('\u21d2'), size = 18) +
  annotate("text", x = 27.4, y = 54, label = "pike", size = 18, family = "spec-font") +
  
  # graphical settings
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "spec-font"),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0, linetype = "solid"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.spacing.y = unit(0.04, "cm"),
    legend.key = element_rect(fill = NA, color = "black", size = 1.1),
    legend.key.width = unit(0.25,"cm"),
    legend.key.height = unit(0.05,"cm"),
    legend.text = element_text(size = 19.3, lineheight = 0.1),
    legend.title = element_text(size = 18, lineheight = 0.4),
    legend.position = c(1.04, 0.275),
    legend.background = element_rect(fill = NA, color = NA),
    plot.title = element_text(size = 50),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank(),) 

img = readJPEG("pike.jpg", native = TRUE) # add image
img2 = readJPEG("perch.jpg", native = TRUE) # add image
pp_plot = pp_plot +                  
  annotation_custom(rasterGrob(img), 
                    ymin = 63.7, ymax = 65.7, xmin = 25.5, xmax = 30.1) +
  annotation_custom(rasterGrob(img2), 
                    ymin = 64.5, ymax = 66.5, xmin = 25.5, xmax = 30.1)


### offshore fish ###

# load data
ices_df = read.csv("ICES_otherSPP.csv", sep = ";")

# cod plot
cod = ggplot(data = ices_df[ices_df$species == "eastern_cod",], aes(x = year, y = SSB_mid/1000000)) +
  geom_line(size = 0.2) +
  geom_vline(xintercept = 2000, linetype = "dotted", size = 0.2) +
  labs(x = "Year", y = "SSB (million tonnes)", title = "b. Cod SD 24–32") + 
  xlim(1980, 2020) +
  theme_bw() +
  theme_sets +
  theme(plot.title = element_text(size = 50))

img = readJPEG("cod.jpg", native = TRUE) # add image
cod = cod +                  
  annotation_custom(rasterGrob(img), 
                    ymin = 0.39, ymax = 0.47, xmin = 2008, xmax = 2020)

# sprat plot
sprat = ggplot(data = ices_df[ices_df$species == "sprat",], aes(x = year, y = SSB_mid/1000000)) +
  geom_line(size = 0.2) +
  geom_vline(xintercept = 2000, linetype = "dotted", size = 0.2) +
  labs(x = "Year", y = "SSB (million tonnes)", title = "c. Sprat whole Baltic Sea") + 
  xlim(1980, 2020) +
  theme_bw() +
  theme_sets +
  theme(plot.title = element_text(size = 50))

img = readJPEG("sprat.jpg", native = TRUE) # add image
sprat = sprat +                  
  annotation_custom(rasterGrob(img), 
                    ymin = 1.70, ymax = 1.85, xmin = 2010, xmax = 2020)


# herring Bothnian Sea and Bothnian Bay
herring_bothnia = ggplot(data = ices_df[ices_df$species == "herring_Bothnia",], aes(x = year, y = SSB_mid/1000000)) +
  geom_line(size = 0.2) +
  geom_vline(xintercept = 2000, linetype = "dotted", size = 0.2) +
  labs(x = "Year", y = "SSB (million tonnes)", title = "d. Herring SD 30–31") + 
  xlim(1980, 2020) +
  theme_bw() +
  theme_sets +
  theme(plot.title = element_text(size = 50))

img = readJPEG("herring.jpg", native = TRUE) # add image
herring_bothnia = herring_bothnia +
  annotation_custom(rasterGrob(img), 
                               ymin = 1.20, ymax = 1.32, xmin = 2010, xmax = 2020)


# herring Baltic Proper
herring_25_29 = ggplot(data = ices_df[ices_df$species == "herring_25_29",], aes(x = year, y = SSB_mid/1000000)) +
  geom_line(size = 0.2) +
  geom_vline(xintercept = 2000, linetype = "dotted", size = 0.2) +
  labs(x = "Year", y = "SSB (million tonnes)", title = "e. Herring SD 25–29 & 32") + 
  xlim(1980, 2020) +
  theme_bw() +
  theme_sets +
  theme(plot.title = element_text(size = 50))

img = readJPEG("herring.jpg", native = TRUE) # add image
herring_25_29 = herring_25_29 +                  
  annotation_custom(rasterGrob(img), 
                    ymin = 1.65, ymax = 1.90, xmin = 2010, xmax = 2020)


# herring Gulf of Riga
herring_riga = ggplot(data = ices_df[ices_df$species == "herring_Riga",], aes(x = year, y = SSB_mid/1000000)) +
  geom_line(size = 0.2) +
  labs(x = "Year", y = "SSB (million tonnes)", title = "f. Herring Gulf of Riga") + 
  xlim(1980, 2020) +
  theme_bw() +
  theme_sets +
  theme(plot.title = element_text(size = 50))

img = readJPEG("herring.jpg", native = TRUE) # add image
herring_riga = herring_riga +                  
  annotation_custom(rasterGrob(img), 
                    ymin = 0.155, ymax = 0.175, xmin = 2010, xmax = 2020)


### eutrophication ###

# colours to indicate eutrophication status
eu_colours = c("#238b45", "#ffeda0", "#fd8d3c", "#bd0026")

# load shapefile with eutrophication status
eutrophication_status = st_read("eutrophication_map/IntegratedEutrophicationAssessment2018.shp")

#
eutrophication_status = st_transform(eutrophication_status, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# subset to relevant area
eu_sub = subset(eutrophication_status, STATUS %in% c("1.0-1.5", "1.5-2.0", "0.5-1", ">2.0"))

# fix order of levels
eu_sub$STATUS = factor(eu_sub$STATUS, levels = c( "0.5-1", "1.0-1.5", "1.5-2.0", ">2.0"))

# plot of eutrophication status
eu_map = 
  
  # plot polygon data on status
  ggplot(eu_sub) + 
  geom_sf(aes(fill = STATUS), size = 0.1) +
  scale_fill_manual(values = eu_colours, name =  "", labels = c("", "", "", "")) +
  
  # indicate areas of stickleback increase
  geom_path(data = stickleback_polygon, aes(x = x, y = y), linetype = "longdash", size = 0.3, colour = "grey39") +
  
  # add labels
  labs(title = "g. Eutrophication status") +
  
  # custom-made legend labels
  annotate("text", x = 10.8, y = 66, label = "Status", size = 16, family = "spec-font") +
  annotate("text", x = 11.7, y = 64.95, label = "0.5-1 (good)", size = 13, family = "spec-font", hjust = 0) +
  annotate("text", x = 11.7, y = 63.95, label = "1-1.5 (moderate)", size = 13, family = "spec-font", hjust = 0) +
  annotate("text", x = 11.7, y = 62.95, label = "1.5-2 (poor)", size = 13, family = "spec-font", hjust = 0) +
  annotate("text", x = 11.7, y = 61.95, label = ">2.0 (bad)", size = 13, family = "spec-font", hjust = 0) +

  
  # graphical settings
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "spec-font"),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0, linetype = "solid"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.spacing.y = unit(0.04, "cm"),
    legend.key.width = unit(0.45,"cm"),
    legend.key.height = unit(0.45,"cm"),
    legend.position = c(0.1, 0.8),
    legend.background = element_rect(fill = NA, color = NA),
    plot.title = element_text(size = 50),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank()) 


# load trend data
eu_trend_df = read.csv("eutroph_trend.csv", sep = ";")

# round extracted year values 
eu_trend_df$year = round(eu_trend_df$year)

# eutrophication trend plot
eu_trend = ggplot(data = eu_trend_df, aes(x = year, y = status)) +
  
  # background areas with same status scale as map
  annotate("rect", xmin = 1980, xmax = 2020, ymin = 1, ymax = 1.5, alpha = 1, fill = eu_colours[2]) +
  annotate("rect", xmin = 1980, xmax = 2020, ymin = 1.5, ymax = 2, alpha = 1, fill = eu_colours[3]) +
  annotate("rect", xmin = 1980, xmax = 2020, ymin = 2, ymax = 3, alpha = 1, fill = eu_colours[4]) +
  
  geom_line(size = 0.2) +
  geom_vline(xintercept = 2000, linetype = "dotted", size = 0.2) +
  labs(x = "Year", y = "Eutrophication status", title = "h. Trend eutrophication status") + 
  xlim(1980, 2020) +
  ylim(1, 3) +
  theme_bw() +
  theme_sets +
  theme(plot.title = element_text(size = 50))


### temperature ###

# load trend data
temp_trend = brick("baltic_omi_tempsal_sst_trend/baltic_omi_tempsal_sst_trend_2020_P20210903_R19932014.nc", 
      varname="sst_trend")

# crop to relevant area and values
temp_trend = crop(temp_trend, extent(14, 32, 54, 66))
values(temp_trend)[values(temp_trend) <= 0] = NaN

# convert to data frame
temp_trend_df = data.frame(rasterToPoints(temp_trend, spatial = TRUE))

# plot
temp_trend = 
  ggplot() +
  geom_raster(data = temp_trend_df , aes(x = x, y = y, fill = layer/28)) +
  scale_fill_gradientn(colours=c(brewer.pal(9, "YlOrRd")), name = "") +
  
  # indicate areas of stickleback increase
  geom_path(data = stickleback_polygon, aes(x = x, y = y), linetype = "longdash", size = 0.3, colour = "grey39") +
  
  # add labels
  labs(title = "i. SST trend 1993–2020") +
  
  xlim(10,30) +
  
  # custom-made legend labels
  annotate("text", x = 12, y = 66, label = "\u00B0C/year", size = 16, family = "spec-font") +
  annotate("text", x = 12, y = 65.5, label = "0.08", size = 13, family = "spec-font", hjust = 0) +
  annotate("text", x = 12, y = 63.9, label = "0.06", size = 13, family = "spec-font", hjust = 0) +
  annotate("text", x = 12, y = 62.3, label = "0.04", size = 13, family = "spec-font", hjust = 0) +
  annotate("text", x = 12, y = 60.7, label = "0.02", size = 13, family = "spec-font", hjust = 0) +

  # graphical settings
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "spec-font"),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0, linetype = "solid"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.text = element_blank(),
    legend.key.width = unit(0.25,"cm"),
    legend.position = c(0.107, 0.72),
    legend.background = element_rect(fill = NA, color = NA),
    plot.title = element_text(size = 50),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank()) 


### arrange and save plots ###

# order plots and fix panel settings for row 1
plots1 = align_plots(cod, sprat,  align = 'v', axis = 'l')
col1 = plot_grid(plots1[[1]], plots1[[2]], ncol = 1)
plots2 = align_plots(herring_bothnia, herring_25_29, herring_riga, align = 'v', axis = 'l')
col2 = plot_grid(plots2[[1]], plots2[[2]], plots2[[3]], ncol = 1)


# collect plots for row 1
row1 = 
plot_grid(pp_plot, col1,  col2, 
          
          nrow = 1)


# collect plots for row 2
row2 = plot_grid(eu_map, eu_trend, temp_trend, nrow = 1)

## initialise fig ##
jpeg("fig4.jpeg", 
    width = 17, height = 18, units = 'cm', res = 600, pointsize = 9, family = "sans")

## print fig ##
plot_grid(row1, row2, rel_heights = c(3,2), ncol = 1)

## close fig ##
dev.off()



#### FIGURE 5 ####

wave = read.csv("SticklebackWave.csv", sep = ";") # detonation data from Eklöf et al


# clean up file
wave = wave[wave$Included == 1 & 
              !is.na(wave$RPD) & 
              !is.na(wave$SWM), ]


# fit final best model in Eklöf et al
modEklof = glm(RPD ~ Year * Distance + log10(SWM) + DD_N,
               data = wave,
               family = binomial)

# year groups for plot
wave$year_group = 1
wave$year_group[wave$Year %in% 2000:2009] = 2
wave$year_group[wave$Year %in% 2010:2017] = 3

# make predictions based on model
newdat = expand.grid(Distance = 0:ceiling(max(wave$Distance)), 
                     Year = 1979:2017, 
                     SWM = mean(wave$SWM),
                     DD_N = mean(wave$DD_N))

newdat$RPD =
  predict(
    modEklof,
    newdata = newdat,
    type = "response"
  )

# year groups for plot
newdat$year_group = 1
newdat$year_group[newdat$Year %in% 2000:2009] = 2
newdat$year_group[newdat$Year %in% 2010:2017] = 3

# for rectangles indicating range of values for which probability is equal
rec_lims = data.frame(
  year_group = 1:3,
  ymin = 0, ymax = 1,
  
  xmin = c(newdat$Distance[newdat$Year == 1979][ which.min(abs( newdat$RPD[newdat$Year == 1979]-0.5)  ) ]/1000 , 
           newdat$Distance[newdat$Year == 2000][ which.min(abs( newdat$RPD[newdat$Year == 2000]-0.5)  ) ]/1000, 
           newdat$Distance[newdat$Year == 2011][ which.min(abs( newdat$RPD[newdat$Year == 2011]-0.5)  ) ]/1000),
  
  xmax = c(newdat$Distance[newdat$Year == 1999][ which.min(abs( newdat$RPD[newdat$Year == 1999]-0.5)  ) ]/1000 , 
           newdat$Distance[newdat$Year == 2010][ which.min(abs( newdat$RPD[newdat$Year == 2010]-0.5)  ) ]/1000, 
           newdat$Distance[newdat$Year == 2017][ which.min(abs( newdat$RPD[newdat$Year == 2017]-0.5)  ) ]/1000)
  
)


# WAVE PLOT #
wave_plot = 
  
  ggplot(data = wave, aes(x = Distance/1000, y = RPD)) +  # raw data
  
  # group per year group
  facet_wrap(~ year_group, ncol = 1,
             labeller = labeller(year_group = 
                                   c("1" = "a. 1979-1999",
                                     "2" = "b. 2000-2010",
                                     "3" = "c. 2011-2017"))
             
  ) + 
  
  # add rectangles indicating range of values for which probability is equal
  geom_rect(data = rec_lims, aes(x = NULL,y = NULL,xmin = xmin, xmax = xmax, 
                                 ymin = ymin, ymax = ymax, 
                                 fill = "grey90"), fill = "grey90") +
  
  
  
  # add points for raw data
  geom_point(alpha = 0.3, size = 0.8) +
  
  # add lines for predictions
  geom_line(data = newdat, aes(x = Distance/1000, y = RPD, group = Year, col = Year), size = 0.3) +
  scale_color_gradientn(colours = rev(brewer.pal(9, "Spectral")), trans = 'reverse') + 
  guides(colour = guide_colourbar(barwidth = 0.75, barheight = 10.5)) +
  
  
  labs(
    x = "Distance to open sea (km)",
    y = "Relative predator dominance") +
  
  theme_bw(base_size = 14) +
  theme(
    text = element_text(family = "spec-font"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    strip.background =element_rect(fill = "grey90"),
    legend.text = element_text(size = 23),
    legend.title = element_text(size = 24),
    strip.text.x = element_text(size = 27),
    axis.text.x = element_text(size = 21),
    axis.text.y = element_text(size = 21))


## initialise fig ##
jpeg("fig5.jpeg", 
    width = 8.5, height = 17, units = 'cm', res = 300, pointsize = 12, family = "sans")

## print fig
print(wave_plot)

## close fig ##
dev.off()


