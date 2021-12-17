# ~ # ~ # ~ # ~ # ~ # ~ # ~ # ~ # ~ # ~ # ~ #
# ~~~ code to reproduce figs 1, 2 and 4 ~~~ #
# ~ # ~ # ~ # ~ # ~ # ~ # ~ # ~ # ~ # ~ # ~ #

# Agnes Olin agnesolin@gmail.com 17 December 2021
# R version 4.1.0 


# Data for Fig 1 and Fig 2a-c come from the ICES Baltic International Acoustic Survey (see Olsson et al. 2019 for details).
 
# Data for Fig 2d-f come from juveniles surveys during the spawning season using underwater detonations (see Eklöf et al. 2020 for details).
 
# Data for Fig 2g-h come from sticklebacks caught in the cooling water intake of the Forsmark nuclear power plant on the Swedish coast (see Adill et al. 2018 for details).
 
# Data for Fig 2i come from traps set out during the spawning season along the Finnish coast, and were extracted from Candolin and Voigt (2020) using https://apps.automeris.io/wpd/.
 
# Data for Fig 2j-k come from beach seines during the spawning season along the Latvian coast (see Olsson et al. 2015 for details).
 
# Data for Fig 4 come from juveniles surveys during the spawning season using underwater detonations (see Eklöf et al. 2020 for details).
 
# Coastline shapefile used in Fig 1 can be found at: https://www.eea.europa.eu/data-and-maps/data/eea-coastline-for-analysis-1/gis-data/europe-coastline-shapefile
 
# ICES subdivision shapefile used in Fig 1 can be found at: https://gis.ices.dk/sf/index.html?widget=StatRec


# load required libraries
library(cowplot) # version 1.1.1
library(ggplot2) # version 3.3.5
library(ggpubr) # version 0.4.0
library(mgcv)
library(raster) # version 3.4-5
library(RColorBrewer) # version 1.1-2
library(scales)
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
                     "Gulf of latvia",
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
png("fig1.png", 
    width = 17, height = 17, units = 'cm', res = 300, pointsize = 9, family = "sans")


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

cols = c("#1c9099", "#d8b365", "sienna")

theme_sets = theme(
  text = element_text(family = "spec-font"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.spacing.y = unit(0, 'cm'),
  legend.spacing.x = unit(0, 'cm'),
  legend.margin=margin(t = 0, unit='cm'),
  legend.box.margin=margin(t = 0, unit='cm'),
  legend.title=element_text(size = 1),
  legend.text=element_text(size = 8),
  legend.justification = c("right", "top"),
  legend.key.width = unit(1,"cm"),
  legend.background=element_blank(),
  axis.title.x = element_text(size = 9),
  axis.title.y = element_text(size = 9),
  axis.text.x = element_text(size = 8),
  axis.text.y = element_text(size = 8))



## BIAS hydroacoustic data ##
BIAS$group = factor(BIAS$group, levels = c("SD30", "SD27_29", "SD25_26"))

dat = BIAS[BIAS$group == "SD25_26",]

gam.mod = gam(biomass_density_tonnes_per_km2 ~ s(year),
              data = dat,
              method = "REML")
summary(gam.mod)
gam.check(gam.mod)

p = predict(gam.mod, 
            type = "response",
            se.fit = TRUE)

dat$pred = p$fit
dat$ymin = p$fit -  1.96 * p$se.fit
dat$ymax = p$fit +  1.96 * p$se.fit


ts1a = 
  
  ggplot(data = dat, aes(x = year, y = biomass_density_tonnes_per_km2)) +
  
  geom_point(col = cols[1], size = 0.75) +
  geom_line(col = cols[1], size = 0.25) +
  
  geom_errorbar(aes(ymin = biomass_density_tonnes_per_km2 - CI, ymax = biomass_density_tonnes_per_km2 + CI), 
                width = 0, size = 0.25, col = cols[1], alpha = 0.5) +
  
  geom_line(aes(y = pred)) +
  
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) +
  
  annotate(geom = "text", x = 1993.5, y = 5.4, label = "SD 25-26", hjust = "left", family = "spec-font", size = 3) + 
  
  annotate(geom = "text", x = 1993.5, y = 0.2, label = "p = 0.5", hjust = "left", family = "spec-font", size = 2.7) + 
  
  labs(
    x = "Year",
    y = expression(paste("Biomass (tonnes km"^"-2", ")"))) +
  
  lims( x = c(1993,2020)) +
  scale_y_continuous(limits=c(0,5.6),oob=squish) +
  
  theme_bw(base_size = 12) +
  theme_sets


dat = BIAS[BIAS$group == "SD27_29",]

gam.mod = gam(biomass_density_tonnes_per_km2 ~ s(year),
              data = dat,
              method = "REML")
summary(gam.mod)
gam.check(gam.mod)

p = predict(gam.mod, 
            type = "response",
            se.fit = TRUE)

dat$pred = p$fit
dat$ymin = p$fit -  1.96 * p$se.fit
dat$ymax = p$fit +  1.96 * p$se.fit




ts1b = 
  
  ggplot(data = dat, aes(x = year, y = biomass_density_tonnes_per_km2)) +
  
  geom_point(col = cols[2], size = 0.75) +
  geom_line(col = cols[2], size = 0.25) +
  
  geom_errorbar(aes(ymin = biomass_density_tonnes_per_km2 - CI, ymax = biomass_density_tonnes_per_km2 + CI), 
                width = 0, size = 0.25, col = cols[2], alpha = 0.5) +
  
  geom_line(aes(y = pred)) +
  
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) +
  
  annotate(geom = "text", x = 1993.5, y = 5.4, label = "SD 27-29", hjust = "left", family = "spec-font", size = 3) + 
  
  annotate(geom = "text", x = 1993.2, y = 0.4, label = "p = 0.01", hjust = "left", family = "spec-font", size = 2.7) + 
  
  
  labs(
    x = "Year",
    y = expression(paste("Biomass (tonnes km"^"-2", ")"))) +
  
  lims( x = c(1993,2020)) +
  scale_y_continuous(limits=c(0,5.6),oob=squish) +
  
  theme_bw(base_size = 12) +
  theme_sets


dat = BIAS[BIAS$group == "SD30",]

gam.mod = gam(biomass_density_tonnes_per_km2 ~ s(year),
              data = dat,
              method = "REML")
summary(gam.mod)
gam.check(gam.mod)

p = predict(gam.mod, 
            type = "response",
            se.fit = TRUE)

dat$pred = p$fit
dat$ymin = p$fit -  1.96 * p$se.fit
dat$ymax = p$fit +  1.96 * p$se.fit



ts1c = 
  
  ggplot(data = dat, aes(x = year, y = biomass_density_tonnes_per_km2)) +
  
  geom_point(col = cols[3], size = 0.75) +
  geom_line(col = cols[3], size = 0.25) +
  
  geom_errorbar(aes(ymin = biomass_density_tonnes_per_km2 - CI, ymax = biomass_density_tonnes_per_km2 + CI), 
                width = 0, size = 0.25, col = cols[3], alpha = 0.5) +
  
  geom_line(aes(y = pred)) +
  
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) +
  
  
  
  annotate(geom = "text", x = 1993.5, y = 5.4, label = "SD 30", hjust = "left", family = "spec-font", size = 3) + 
  
  
  annotate(geom = "text", x = 1997, y = 0.4, label = "p = 0.002", hjust = "left", family = "spec-font", size = 2.7) + 
  
  
  labs(
    x = "Year",
    y = expression(paste("Biomass (tonnes km"^"-2", ")"))) +
  
  lims( x = c(1993,2020)) +
  scale_y_continuous(limits=c(0,5.6),oob=squish) +
  
  theme_bw(base_size = 12) +
  theme_sets



## Swedish juvenile data ##
juvs$area = factor(juvs$area, levels = c("SD30", "SD27_29", "SD25"))

dat = juvs[juvs$area == "SD25",]

gam.mod = gam(abundance ~ s(year),
              data = dat,
              method = "REML")
summary(gam.mod)
gam.check(gam.mod)

p = predict(gam.mod, 
            type = "response",
            se.fit = TRUE)

dat$pred = p$fit
dat$ymin = p$fit -  1.96 * p$se.fit
dat$ymax = p$fit +  1.96 * p$se.fit




ts2a = 
  
  ggplot(data = dat, aes(x = year, y = abundance)) +
  
  geom_point(col = cols[1], size = 0.75) +
  geom_line(col = cols[1], size = 0.25) +
  
  geom_errorbar(aes(ymin = abundance - CI, ymax = abundance + CI), 
                width = 0, size = 0.25, col = cols[1], alpha = 0.5) +
  
  geom_line(aes(y = pred)) +
  
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) +
  
  
  annotate(geom = "text", x = 1993.5, y = 260, label = "SD 25", hjust = "left", family = "spec-font", size = 3) + 
  
  annotate(geom = "text", x = 2000, y = 20, label = "p = 0.08", hjust = "left", family = "spec-font", size = 2.7) + 
  
  
  labs(
    x = "Year",
    y = expression(paste("CPUE (fish detonation"^"-1", ")"))) +
  
  lims( x = c(1993,2020)) +
  scale_y_continuous(limits=c(0,270),oob=squish) +
  
  
  theme_bw(base_size = 12) +
  theme_sets


dat = juvs[juvs$area == "SD27_29",]

gam.mod = gam(abundance ~ s(year),
              data = dat,
              method = "REML")
summary(gam.mod)
gam.check(gam.mod)

p = predict(gam.mod, 
            type = "response",
            se.fit = TRUE)

dat$pred = p$fit
dat$ymin = p$fit -  1.96 * p$se.fit
dat$ymax = p$fit +  1.96 * p$se.fit




ts2b = 
  
  ggplot(data = dat, aes(x = year, y = abundance)) +
  
  geom_point(col = cols[2], size = 0.75) +
  geom_line(col = cols[2], size = 0.25) +
  
  
  geom_errorbar(aes(ymin = abundance - CI, ymax = abundance + CI), 
                width = 0, size = 0.25, col = cols[2], alpha = 0.5) +
  
  geom_line(aes(y = pred)) +
  
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) +
  
  annotate(geom = "text", x = 1993.5, y = 260, label = "SD 27 & 29", hjust = "left", family = "spec-font", size = 3) + 
  
  annotate(geom = "text", x = 1993.5, y = 20, label = "p = 0.1", hjust = "left", family = "spec-font", size = 2.7) + 
  
  
  labs(
    x = "Year",
    y = expression(paste("CPUE (fish detonation"^"-1", ")"))) +
  
  lims( x = c(1993,2020)) +
  scale_y_continuous(limits=c(0,270),oob=squish) +
  
  
  
  theme_bw(base_size = 12) +
  theme_sets


dat = juvs[juvs$area == "SD30",]
gam.mod = gam(abundance ~ s(year),
              data = dat,
              method = "REML")
summary(gam.mod)
gam.check(gam.mod)

p = predict(gam.mod, 
            type = "response",
            se.fit = TRUE)

dat$pred = p$fit
dat$ymin = p$fit -  1.96 * p$se.fit
dat$ymax = p$fit +  1.96 * p$se.fit



ts2c = 
  
  ggplot(data = dat, aes(x = year, y = abundance)) +
  
  geom_point(col = cols[3], size = 0.75) +
  geom_line(col = cols[3], size = 0.25) +
  
  
  geom_errorbar(aes(ymin = abundance - CI, ymax = abundance + CI), 
                width = 0, size = 0.25, col = cols[3], alpha = 0.5) +
  
  geom_line(aes(y = pred)) +
  
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) + 
  
  annotate(geom = "text", x = 1993.5, y = 260, label = "SD 30", hjust = "left", family = "spec-font", size = 3) + 
  
  annotate(geom = "text", x = 1995, y = 20, label = "p = 0.3", hjust = "left", family = "spec-font", size = 2.7) + 
  
  
  labs(
    x = "Year",
    y = expression(paste("CPUE (fish detonation"^"-1", ")"))) +
  
  lims( x = c(1993,2020)) +
  scale_y_continuous(limits=c(0,270),oob=squish) +
  
  
  
  theme_bw(base_size = 12) +
  theme_sets






## trapping data from Finland ##

low = finland$stick[finland$value == "mid"] - finland$stick[finland$value == "low"]
high = finland$stick[finland$value == "high"] - finland$stick[finland$value == "mid"]

dat = finland[finland$value == "mid",]
dat$year = as.numeric(dat$year)

gam.mod = gam(stick ~ s(year, k = 3),
              data = dat,
              method = "REML")
summary(gam.mod)
gam.check(gam.mod)


p = predict(gam.mod, 
            type = "response",
            se.fit = TRUE)

dat$pred = p$fit
dat$ymin = p$fit -  1.96 * p$se.fit
dat$ymax = p$fit +  1.96 * p$se.fit







ts3 = 
  
  ggplot(data = dat, aes(x = year, y = stick)) +
  
  geom_point(size = 0.80, col = "grey50") +
  geom_line(size = 0.25, col = "grey50") +
  
  geom_errorbar(aes(ymin = stick - low, ymax = stick + high), width = 0, size = 0.25, col = "grey50") +
  
  geom_line(aes(y = pred)) +
  
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) + 
  
  annotate(geom = "text", x = 1993.5, y = 8.2, label = "Tv?rminne,\nFinland", hjust = "left", family = "spec-font", size = 3) + 
  
  annotate(geom = "text", x = 2001, y = 4.6, label = "p = 0.05", hjust = "left", family = "spec-font", size = 2.7) + 
  
  
  
  labs(
    x = "Year",
    y = expression(paste("CPUE (fish trap"^"-1", ")"))) +
  
  lims( x = c(1993,2020)) +
  
  theme_bw(base_size = 12) +
  theme_sets 



## trapping data from Gulf of latvia ##



gam.mod.pape = gam(stickleback ~ s(year),
                   data = latvia[latvia$area == "Pape",],
                   method = "REML")
summary(gam.mod.pape)
gam.check(gam.mod.pape)


gam.mod.kolka = gam(stickleback ~ s(year),
                    data = latvia[latvia$area == "Kolka",],
                    method = "REML")
summary(gam.mod.kolka)
gam.check(gam.mod.kolka)


p =   predict(gam.mod.pape, 
              newdata =  data.frame(year = latvia$year[latvia$area == "Pape"]),
              type = "response",
              se.fit = TRUE)

latvia$pred[latvia$area == "Pape"] = p$fit
latvia$ymin[latvia$area == "Pape"] = p$fit -  1.96 * p$se.fit
latvia$ymax[latvia$area == "Pape"] = p$fit +  1.96 * p$se.fit


p =   predict(gam.mod.kolka, 
              newdata =  data.frame(year = latvia$year[latvia$area == "Kolka"]),
              type = "response",
              se.fit = TRUE)

latvia$pred[latvia$area == "Kolka"] = p$fit
latvia$ymin[latvia$area == "Kolka"] = p$fit -  1.96 * p$se.fit
latvia$ymax[latvia$area == "Kolka"] = p$fit +  1.96 * p$se.fit



ts4 = 
  
  ggplot(data = latvia[latvia$area == "Pape",], aes(x = year, y = stickleback)) +
  
  
  geom_point(size = 0.75, col = "grey50") +
  geom_line(size = 0.25, col = "grey50") +
  
  geom_line(aes(y = pred)) +
  
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) + 
  
  annotate(geom = "text", x = 2013, y = 0.6, label = "Pape,\nLatvia", hjust = "left", family = "spec-font", size = 3) + 
  
  annotate(geom = "text", x = 1993, y = 0.05, label = "p = 0.3", hjust = "left", family = "spec-font", size = 2.7) + 
  
  labs(
    x = "Year",
    y = expression(paste("CPUE (fish haul"^"-1", ")"))) +
  
  lims( x = c(1993,2020)) +
  
  theme_bw() +
  theme_sets 


ts5 = 
  
  ggplot(data = latvia[latvia$area == "Kolka",], aes(x = year, y = stickleback)) +
  
  
  geom_point(size = 0.75, col = "grey50") +
  geom_line(size = 0.25, col = "grey50") +
  
  geom_line(aes(y = pred)) +
  
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) + 
  
  annotate(geom = "text", x = 2013, y = 2.3, label = "Kolka,\nLatvia", hjust = "left", family = "spec-font", size = 3) + 
  
  annotate(geom = "text", x = 1998, y = 1.9, label = "p < 0.001", hjust = "left", family = "spec-font", size = 2.7) + 
  
  labs(
    x = "Year",
    y = expression(paste("CPUE (fish haul"^"-1", ")"))) +
  
  scale_y_continuous(breaks = c(0.5, 1, 1.5, 2, 2.5),
                     labels = c("0.5", "1.0", "1.5", "2.0", "")) +
  
  lims( x = c(1993,2020)) +
  
  theme_bw() +
  theme_sets 



## cooling water data from Forsmark ##



gam.mod.spring = gam(density_inds_per_m3 ~ s(year),
                     data = forsmark[forsmark$season == "spring",],
                     method = "REML")
summary(gam.mod.spring)


gam.mod.autumn = gam(density_inds_per_m3 ~ s(year),
                     data = forsmark[forsmark$season == "autumn",],
                     method = "REML")
summary(gam.mod.autumn)
gam.check(gam.mod)



p =   predict(gam.mod.spring, 
              newdata =  data.frame(year = forsmark$year[forsmark$season == "spring"]),
              type = "response",
              se.fit = TRUE)

forsmark$pred[forsmark$season == "spring"] = p$fit
forsmark$ymin[forsmark$season == "spring"] = p$fit -  1.96 * p$se.fit
forsmark$ymax[forsmark$season == "spring"] = p$fit +  1.96 * p$se.fit

p =   predict(gam.mod.autumn, 
              newdata =  data.frame(year = forsmark$year[forsmark$season == "autumn"]),
              type = "response",
              se.fit = TRUE)

forsmark$pred[forsmark$season == "autumn"] = p$fit
forsmark$ymin[forsmark$season == "autumn"] = p$fit -  1.96 * p$se.fit
forsmark$ymax[forsmark$season == "autumn"] = p$fit +  1.96 * p$se.fit



ts6a = 
  
  ggplot(data = forsmark[forsmark$season == "spring",], aes(x = year, y = density_inds_per_m3)) +
  
  geom_point(size = 0.75, col = "grey50") +
  geom_line(size = 0.25, col = "grey50") +
  
  
  geom_errorbar(aes(ymin = density_inds_per_m3 - CI, ymax = density_inds_per_m3 + CI), 
                width = 0, size = 0.25, col = "grey50", alpha = 0.5) +
  
  geom_line(aes(y = pred)) +
  
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) + 
  
  
  annotate(geom = "text", x = 1993, y = 0.070, label = "Forsmark, Sweden\nSpring", hjust = "left", family = "spec-font", size = 3) + 
  
  annotate(geom = "text", x = 1997, y = 0.013, label = "p < 0.001", hjust = "left", family = "spec-font", size = 2.7) + 
  
  
  
  labs(
    x = "Year",
    y = expression(paste("Density (fish m"^"-3", ")"))) +
  
  lims( x = c(1993,2020)) +
  scale_y_continuous(limits=c(0,0.08),oob=squish,
                     breaks = c(0.00, 0.02, 0.04, 0.06, 0.08),
                     labels = c("0.00", "0.02", "0.04", "0.06", "")) +
  
  
  theme_bw(base_size = 12) +
  theme_sets +
  theme(    legend.position = c(0.5, 1))


ts6b = 
  
  ggplot(data = forsmark[forsmark$season == "autumn",], aes(x = year, y = density_inds_per_m3)) +
  
  geom_point(size = 0.75, col = "grey50") +
  geom_line(size = 0.25, col = "grey50") +
  
  geom_errorbar(aes(ymin = density_inds_per_m3 - CI, ymax = density_inds_per_m3 + CI), 
                width = 0, size = 0.25, col = "grey50", alpha = 0.5) +
  
  
  geom_line(aes(y = pred)) +
  
  geom_ribbon(aes(ymin = ymin, ymax = ymax),  alpha = .15) + 
  
  
  annotate(geom = "text", x = 1993, y = 0.095, label = "Forsmark, Sweden\nAutumn", hjust = "left", family = "spec-font", size = 3) + 
  
  annotate(geom = "text", x = 1993, y = 0.015, label = "p < 0.001", hjust = "left", family = "spec-font", size = 2.7) + 
  
  labs(
    x = "Year",
    y = expression(paste("Density (fish m"^"-3", ")"))) +
  
  lims( x = c(1993,2020)) +
  scale_y_continuous(limits=c(0,0.11),oob=squish) +
  
  
  theme_bw(base_size = 12) +
  theme_sets +
  theme(    legend.position = c(0.5, 1))







# align plots
plots1 = align_plots(ts1a, ts2a, ts3, ts6a, align = 'v', axis = 'l')
plots2 = align_plots(ts1b, ts2b, ts4, align = 'v', axis = 'l')
plots3 = align_plots(ts1c, ts2c, ts5, align = 'v', axis = 'l')

top_row = plot_grid(plots1[[1]], plots2[[1]], plots3[[1]],
                    labels = c('a.', 'b.', 'c.'),
                    label_size = 13,
                    label_fontfamily = "spec-font",
                    label_fontface = "plain", 
                    label_x = 0.13,
                    ncol = 3)

mid_row1 = plot_grid(plots1[[2]], plots2[[2]], plots3[[2]],
                     labels = c('d.', 'e.', 'f.'),
                     label_size = 13,
                     label_fontfamily = "spec-font",
                     label_fontface = "plain", 
                     label_x = c(0.14, 0.13, 0.15), 
                     ncol = 3)

mid_row2 =   plot_grid(plots1[[4]], ts6b, labels = c('g.', 'h.'), 
                       label_size = 13,
                       label_fontfamily = "spec-font",
                       label_fontface = "plain", 
                       label_x = 0.09, 
                       ncol = 2)

bottom_row =  plot_grid(plots1[[3]], plots2[[3]], plots3[[3]], 
                        labels = c('i.', 'j.', 'k.'), 
                        label_size = 13,
                        label_fontfamily = "spec-font",
                        label_fontface = "plain", 
                        label_x = c(0.16, 0.15, 0.12),
                        ncol = 3)








## initialise fig ##
png("fig2.png", 
    width = 17, height = 20, units = 'cm', res = 600, pointsize = 9, family = "sans")

## print fig

print(plot_grid(top_row, mid_row1, mid_row2, bottom_row, 
                
                ncol = 1))

## close fig ##
dev.off()


#### FIGURE 4 ####

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
png("fig4.png", 
    width = 8.5, height = 17, units = 'cm', res = 300, pointsize = 12, family = "sans")

## print fig
print(wave_plot)

## close fig ##
dev.off()


