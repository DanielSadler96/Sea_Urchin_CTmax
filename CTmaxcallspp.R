library(tidyverse)
library(ggdist)   
library(gghalves)
library(ggprism)
library(patchwork)
library(rphylopic)
library(ggpubr)
library(ggdist)
library(scales)
library(EnvStats)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(ordinal)


# readfiles ---------------------------------------------------------------

setwd("~/Documents/Postdoc/CTmaxUrchin")

CTmaxpurp<-read.csv("~/Documents/Postdoc/CTmaxUrchin/Purpleurchins/CTmaxurchin.csv")
CTmaxgreen<-read.csv("~/Documents/Postdoc/CTmaxUrchin/Greenurchins/CTmaxGreens.csv")
CTmaxvar<-read.csv("CTmaxVar.csv")

CTmaxvar$Treatment<-as.factor(CTmaxvar$Treatment)
CTmaxpurp$Treatment<-as.factor(CTmaxpurp$Treatment)
CTmaxgreen$Treatment<-as.factor(CTmaxgreen$Treatment)



# Plotting ----------------------------------------------------------------

##Obtain images from phylopic for plots
##obtain id for species
uuid <- rphylopic::get_uuid(name = "Strongylocentrotus purpuratus", n = 1)
uuid
##download image (note using same urchin model and recolouring for each)
urchin<- image_data("06119510-7641-4a0b-8585-93ccb5ca9447", size=1024)[[1]]
uuid <- rphylopic::get_uuid(name = "Strongylocentrotus", n = 1)


###remove NA values
CTmaxvar <- CTmaxvar %>%
  filter(!is.na(LossofAdhesion))

###create IDs based on phenotype colour
CTmaxvar %>% 
  unite(col = "FullColour", Test, Spine, sep = "_") -> CTmaxvarunite



###plot CTmax for all species
p1<-ggplot(CTmaxpurp, aes(x = Treatment, y = LossOfAdhesion, color = Treatment , fill =Treatment)) +
  geom_boxplot(aes(fill = Treatment),
               width = 0.2, 
               outlier.shape = NA,
               alpha=0.8,
               size = 2
  ) +
  geom_jitter(
    size = 3, 
    alpha = 0.5,
    shape = 21,
    colour = "black",
    stroke = 1.5,
    width = 0,
      ) +
  scale_fill_manual(values = c("lightblue1","firebrick3","blue3","firebrick")) +
  scale_color_manual(values = c("lightblue3", "red4","blue", "red")) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_blank(),       
    strip.background = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "grey50"),
    axis.ticks = element_line(colour = "grey50"),
    axis.text = element_text(size = 20, face = "bold", colour = "black"),
    axis.title.x = element_text(margin = margin(t = 8), face = "bold", size = 22), ##changes elements of x axis label, face = bold, italic, underlined
    axis.title.y = element_text(margin = margin(r = 8), face = "bold", size = 22),
    title = element_text(size = 20, face = "bold")
  )+
  labs(
    title = expression(paste(italic("Strongylocentrotus purpuratus"))),
    x = "Acclimation Temperature (°C)",
    y = "CTmax (°C)"
  ) +
  theme(
    legend.position = "none"
  )+
  ylim(17,38)

###add phylopic image
p1<-ggpubr::ggarrange(p1)+
  add_phylopic(uuid = "a6125e0d-c627-40e3-9966-5ad288277545", 
               x=0.85, 
               y=0.25, 
               alpha=0.6,
               ysize=0.2, 
               fill = "purple")


p1

p2<-ggplot(CTmaxgreen, aes(x = Treatment, y = LossofAdhesion, color = Treatment)) +
  geom_boxplot(aes(fill = Treatment),
               width = 0.2, 
               outlier.shape = NA,
               alpha=0.8,
               size = 2
  ) +
  geom_jitter(aes(fill = Treatment),
              
              size = 3, 
              alpha = 0.5,
              shape = 21,
              colour = "black",
              stroke = 1.5,
              width = 0,
  ) +
  scale_fill_manual(values = c("8" = "lightblue1", "14" = "firebrick3")) +
  scale_color_manual(values = c("8" = "lightblue3", "14" = "red4")) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_blank(),       
    strip.background = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "grey50"),
    axis.ticks = element_line(colour = "grey50"),
    axis.text = element_text(size = 20, face = "bold", colour = "black"),
    axis.title.x = element_text(margin = margin(t = 8), face = "bold", size =22), ##changes elements of x axis label, face = bold, italic, underlined
    axis.title.y = element_text(margin = margin(r = 8), face = "bold", size =22),
    title = element_text(size = 20, face = "bold")
  )+
  labs(
    title = expression(paste(italic("Strongylocentrotus droebachiensis"))),
    x = "Acclimation Temperature (°C)",
    y = "CTmax (°C)"
  ) +
  theme(
    legend.position = "none"
  )+
  ylim(17,38)

p2<-ggpubr::ggarrange(p2)+
  add_phylopic(uuid = "a6125e0d-c627-40e3-9966-5ad288277545", 
               x=0.9, 
               y=0.25, 
               alpha=0.6,
               ysize=0.2, 
               fill = "green4")

p2

p3<-ggplot(CTmaxvar, aes(x = Treatment, y = LossofAdhesion, color = Treatment , fill =Treatment)) +
  geom_boxplot(aes(fill = Treatment),
               width = 0.2, 
               outlier.shape = NA,
               alpha=0.15,
               #position = position_nudge(x = 0.2),
               size = 2
  ) +
  geom_jitter(
    
    size = 3, 
    alpha = 0.5,
    shape = 21,
    colour = "black",
    stroke = 1.5,
    width = 0,
      ) +
  scale_fill_manual(values = c("lightblue1","firebrick3","blue3","firebrick")) +
  scale_color_manual(values = c("lightblue3", "red4","blue", "red")) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_blank(),       
    strip.background = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "grey50"),
    axis.ticks = element_line(colour = "grey50"),
    axis.text = element_text(size = 20, face = "bold", colour = "black"),
    axis.title.x = element_text(margin = margin(t = 8), face = "bold", size = 22), ##changes elements of x axis label, face = bold, italic, underlined
    axis.title.y = element_text(margin = margin(r = 8), face = "bold", size = 22),
    title = element_text(size = 20, face = "bold")
  )+
  labs(
    title = expression(paste(italic("Lytechinus variegatus"))),
    x = "Acclimation Temperature (°C)",
    y = "CTmax (°C)"
  ) +
  theme(
    legend.position = "none"
  )+
  ylim(17,38)

p3<-ggpubr::ggarrange(p3)+
  add_phylopic(uuid = "a6125e0d-c627-40e3-9966-5ad288277545", 
               x=0.85, 
               y=0.25, 
               alpha=0.6,
               ysize=0.2, 
               fill = "tan2")


p3

###combine using patchwork
p4<-(p1 | p2) /
  (p3 | plot_spacer())
ggsave(p4, file = "panelct.png", width = 14, height = 12, dpi = 500)



# validation --------------------------------------------------------------
###Probability of phenotype occuring, note loss of adhesion occurs 100% of the time as the end point of the assay.

CTmaxgreen %>% summarise(probtube=sum(TubeProb, na.rm=T)/n(),
                         probspine=sum(SpineProb, na.rm=T)/n())
#probtube probspine
#  0.875    0.3875
CTmaxvar %>% summarise(probtube=sum(TubeProb, na.rm=T)/n(),
                         probspine=sum(SpineProb, na.rm=T)/n())

CTmaxpurp %>% summarise(probtube=sum(TubeProb, na.rm=T)/n(),
                       probspine=sum(SpineProb, na.rm=T)/n())



p1<-ggplot(CTmaxgreen, aes(x = Treatment, y = TubeProb, color = Treatment)) +
  
  # Raw data points jittered
  geom_jitter(aes(fill = Treatment),
              height = 0,
              size = 3, 
              alpha = 0.3,
              shape = 21,
              colour = "black",
              stroke = 1.5,
              width = 0.1,
              #position = position_nudge(x = +0.05)
  ) +
  stat_summary(aes(fill = Treatment),fun.data = mean_cl_normal, #lets look at mean instead of median
               geom = "pointrange", # could also do crossbar, errorbar, point
               size = 2, 
               linewidth = 2,
               shape = 21)+
  scale_fill_manual(values = c("8" = "lightblue1", "14" = "firebrick3")) +
  scale_color_manual(values = c("8" = "lightblue3", "14" = "red4")) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_blank(),       
    strip.background = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "grey50"),
    axis.ticks = element_line(colour = "grey50"),
    axis.text = element_text(size = 20, face = "bold", colour = "black"),
    axis.title.x = element_text(margin = margin(t = 8), face = "bold", size = 22), ##changes elements of x axis label, face = bold, italic, underlined
    axis.title.y = element_text(margin = margin(r = 8), face = "bold", size = 22),
    title = element_text(size = 20, face = "bold")
  )+
  labs(
    title = "",
    x = "Acclimation Temperature (°C)",
    y = "Probability of tube feet retraction"
  ) +
  theme(
    legend.position = "none"
  )

p1

p2<-ggplot(CTmaxgreen, aes(x = Treatment, y = SpineProb, color = Treatment)) +
  
  # Raw data points jittered
  geom_jitter(aes(fill = Treatment),
              height = 0,
              size = 3, 
              alpha = 0.3,
              shape = 21,
              colour = "black",
              stroke = 1.5,
              width = 0.1,
              #position = position_nudge(x = +0.05)
  ) +
  stat_summary(aes(fill = Treatment),fun.data = mean_cl_normal, #lets look at mean instead of median
               geom = "pointrange", # could also do crossbar, errorbar, point
               size = 2, 
               linewidth = 2,
               shape = 21)+
  scale_fill_manual(values = c("8" = "lightblue1", "14" = "firebrick3")) +
  scale_color_manual(values = c("8" = "lightblue3", "14" = "red4")) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_blank(),       
    strip.background = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "grey50"),
    axis.ticks = element_line(colour = "grey50"),
    axis.text = element_text(size = 20, face = "bold", colour = "black"),
    axis.title.x = element_text(margin = margin(t = 8), face = "bold", size = 22), ##changes elements of x axis label, face = bold, italic, underlined
    axis.title.y = element_text(margin = margin(r = 8), face = "bold", size =22),
    title = element_text(size = 20, face = "bold")
  )+
  labs(
    title = "",
    x = "Acclimation Temperature (°C)",
    y = "Probability of spine compression"
  ) +
  theme(
    legend.position = "none"
  )

p2


p3<-p1+p2


ggsave(p3, file = "probplots.png", width = 12, height = 8, dpi = 1000)

# Repeatability -----------------------------------------------------------

CTmaxgreen <- CTmaxgreen %>%
  mutate(
    ID = interaction(Colour, Tank, drop = TRUE)   # unique ID per individual
  )
CTmaxgreen$Trial<-as.factor(CTmaxgreen$Trial)

CTmaxgreen$Treatment

facet_names <- c(
  `8` = "8°C",
  `14` = "14°C")

p1<-ggplot(CTmaxgreen, aes(x = Trial, y = LossofAdhesion, color = Treatment)) +
  geom_line(aes(group = ID), alpha = 0.2, linewidth = 1) +
  # Raw data points jittered
  geom_jitter(aes(fill = Treatment),
              height = 0,
              size = 3, 
              alpha = 0.3,
              shape = 21,
              colour = "black",
              stroke = 1.5,
              width = 0,
              #position = position_nudge(x = +0.05)
  ) +  
  geom_boxplot(aes(group = Trial, fill = Treatment),
               width = 0.2, alpha = 0.15, outlier.shape = NA, size = 1.5) + 
  
  ###size changes size of dot
  scale_fill_manual(values = c("8" = "lightblue1", "14" = "firebrick3")) +
  scale_color_manual(values = c("8" = "lightblue3", "14" = "red4")) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 20, face ="bold"),       
    strip.background = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "grey50"),
    axis.ticks = element_line(colour = "grey50"),
    axis.text = element_text(size = 20, face = "bold", colour = "black"),
    axis.title.x = element_text(margin = margin(t = 8), face = "bold", size =22), ##changes elements of x axis label, face = bold, italic, underlined
    axis.title.y = element_text(margin = margin(r = 8), face = "bold", size =22),
    title = element_text(size = 22, face = "bold")
  )+
  labs(
    title = "",
    x = "Trial no.",
    y = "CTmax (°C)"
  ) +
  scale_y_continuous(labels = comma_format(accuracy = 1)) +
  theme(
    legend.position = "none"
  )+
  facet_wrap(~Treatment, labeller = as_labeller(facet_names))



CTmaxvar <- CTmaxvar %>%
  mutate(
    ID = interaction(TagColour, Tank, drop = TRUE)   # unique ID per individual
  )
CTmaxvar$Trial<-as.factor(CTmaxvar$Trial)


facet_names <- c(
  `21` = "21°C",
  `27` = "27°C")

p2<-ggplot(CTmaxvar, aes(x = Trial, y = LossofAdhesion, colour = Treatment)) +
  geom_line(aes(group = ID), alpha = 0.2, linewidth = 1) +
  # Raw data points jittered
  geom_jitter(aes(fill = Treatment),
              height = 0,
              size = 3, 
              alpha = 0.3,
              shape = 21,
              colour = "black",
              stroke = 1.5,
              width = 0,
              #position = position_nudge(x = +0.05)
  ) +  
  geom_boxplot(aes(group = Trial, fill = Treatment),
               width = 0.2, alpha = 0.15, outlier.shape = NA, size = 1.5) + 
  
  ###size changes size of dot
  scale_fill_manual(values = c("21" = "lightblue1", "27" = "firebrick3")) +
  scale_color_manual(values = c("21" = "lightblue3", "27" = "red4")) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 20, face ="bold"),       
    strip.background = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "grey50"),
    axis.ticks = element_line(colour = "grey50"),
    axis.text = element_text(size = 20, face = "bold", colour = "black"),
    axis.title.x = element_text(margin = margin(t = 8), face = "bold", size =22), ##changes elements of x axis label, face = bold, italic, underlined
    axis.title.y = element_text(margin = margin(r = 8), face = "bold", size =22),
    title = element_text(size = 22, face = "bold")
  )+
  labs(
    title = "",
    x = "Trial no.",
    y = "CTmax (°C)"
  ) +
  theme(
    legend.position = "none"
  )+
  facet_wrap(~Treatment, labeller = as_labeller(facet_names))

p3<-p1+p2


# weight ------------------------------------------------------------------


p1<-ggplot(CTmaxvar, aes(x = Weight, y = LossofAdhesion, fill = Treatment))+
  geom_point(shape = 21, size =4)+
  geom_smooth(method = "lm")+
  theme_classic()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))+
  scale_fill_manual(values = c("darkgreen", "pink2"))+
  facet_wrap(~Treatment)
p1

CTmaxpurp

p2<-ggplot(CTmaxpurp, aes(x = Weight, y = LossOfAdhesion, fill = Treatment))+
  geom_point(shape = 21, size =4)+
  geom_smooth(method = "lm")+
  theme_classic()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))+
  scale_fill_manual(values = c("darkgreen", "pink2"))+
  facet_wrap(~Treatment)
p2

p3<-ggplot(CTmaxgreen, aes(x = Weight, y = LossofAdhesion, fill = Treatment))+
  geom_point(shape = 21, size =4)+
  geom_smooth(method = "lm")+
  theme_classic()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))+
  scale_fill_manual(values = c("darkgreen", "pink2"))+
  facet_wrap(~Treatment)
p3


cor.test(CTmaxgreen$Weight,CTmaxgreen$LossofAdhesion) #0.03
cor.test(CTmaxpurp$Weight,CTmaxpurp$LossOfAdhesion) #0.13
cor.test(CTmaxvar$Weight,CTmaxvar$LossofAdhesion) #0.07

CTmaxgreen %>% group_by(Trial, Treatment) %>% summarise(meanweight = mean(Weight, na.rm = TRUE))
CTmaxvar %>% group_by(Trial, Treatment) %>% summarise(meanweight = mean(Weight, na.rm = TRUE))
CTmaxpurp %>% group_by(Trial, Treatment) %>% summarise(meanweight = mean(Weight, na.rm = TRUE))


# variance ----------------------------------------------------------------


CTmaxgreen %>%
  drop_na() %>%
  summarise(CVAdhesion=cv(LossofAdhesion),
            CVSpine = cv(Spines),
            CVtube = cv(Tubefeet))
CTmaxpurp %>%
  drop_na() %>%
  summarise(CVAdhesion=cv(LossOfAdhesion),
            CVSpine = cv(SpineDec),
            CVtube = cv(TubeFeet))
CTmaxvar %>%
  drop_na() %>%
  summarise(CVAdhesion=cv(LossofAdhesion),
            CVSpine = cv(Spines),
            CVtube = cv(Tubefeet))




# Righting ----------------------------------------------------------------

righting<-read_csv("~/Documents/Postdoc/CTmaxUrchin/Greenurchins/rightingrespGreens.csv")
righting$Treatment<-as.factor(righting$Treatment)
right<-read.csv("~/Documents/Postdoc/CTmaxUrchin/Lytechinus/rightingrespLyt.csv")
right %>% group_by(UrchinID) %>% drop_na(UrchinID) %>% ungroup() -> right
right$Week<-as.factor(right$Week)
right$Treatment<-as.factor(right$Treatment)


rightingplot<-righting %>% 
  dplyr::select(Treatment,Week,Righting_time_s) %>% ##selects only columns we are interesed in plotting
  drop_na() ### removes NAs (deaths)
rightingplot %>% dim() ##checking size of the dataframe to make sure it didnt remove everything

rightingplot
rightingplot<-rightingplot %>%
  mutate(treatmentweek = paste(Righting_time_s, Week, sep="_"))
###plot for Righting time by week
righting_summary <- rightingplot %>%
  dplyr::summarise(
    mean_rt = mean(Righting_time_s, na.rm = TRUE),
    sd_rt   = sd(Righting_time_s, na.rm = TRUE),
    n       = length(Righting_time_s),
    se_rt   = sd_rt / sqrt(n),
    .by = c(Week, Treatment)
  )
righting_summary$Week<-as.factor(righting_summary$Week)
righting_summary$Treatment<-as.factor(righting_summary$Treatment)

p1<-ggplot(righting_summary, aes(
  x = Week,
  y = mean_rt,
  color = Treatment,
  group = Treatment
)) +
  geom_line(size = 1.2) +
  geom_point(size = 5) +
  geom_rect(aes(xmin = 5.1, xmax = 5.9, ymin = 0, ymax = 140),
            inherit.aes = FALSE, fill = "red", alpha = 0.01)+
  geom_text(
    aes(x = 5.5, y = 130, label = "CTmax"),
    inherit.aes = FALSE,
    size = 6, face = "bold"
  ) +  
  geom_errorbar(aes(ymin = mean_rt - se_rt, ymax = mean_rt + se_rt),
                width = 0.2, linewidth = 0.8) +
  theme_minimal(base_size = 14) +
  ylab("Righting time (s)") +
  xlab("Week") +
  scale_color_manual(values = c("cadetblue1", "red4")) +
  #scale_x_continuous(breaks = unique(rightingplot$Week)) +
  theme(
    strip.text = element_blank(),       
    strip.background = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "grey50"),
    axis.ticks = element_line(colour = "grey50"),
    axis.text = element_text(size = 20, face = "bold", colour = "black"),
    axis.title.x = element_text(margin = margin(t = 8), face = "bold", size =22), ##changes elements of x axis label, face = bold, italic, underlined
    axis.title.y = element_text(margin = margin(r = 8), face = "bold", size = 22),
    title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 18, face = "bold", colour = "black"),
    legend.title = element_text(size = 18, face = "bold", colour = "black"),
    legend.position = "top"
  )+
  labs(color = "Acclimation (°C)")


rightingplot<-right %>% 
  dplyr::select(Treatment,Week,Righting_time) %>% ##selects only columns we are interesed in plotting
  drop_na() ### removes NAs (deaths)
rightingplot %>% dim() ##checking size of the dataframe to make sure it didnt remove everything
tail(rightingplot)

rightingplot %>% filter(Righting_time < 590) ->rightingplot


rightingplot<-rightingplot %>%
  mutate(treatmentweek = paste(Righting_time, Week, sep="_"))
###plot for Righting time by week
righting_summary <- rightingplot %>%
  dplyr::summarise(
    mean_rt = mean(Righting_time, na.rm = TRUE),
    sd_rt   = sd(Righting_time, na.rm = TRUE),
    n       = length(Righting_time),
    se_rt   = sd_rt / sqrt(n),
    .by = c(Week, Treatment)
  )
righting_summary$Week<-as.factor(righting_summary$Week)
righting_summary$Treatment<-as.factor(righting_summary$Treatment)
righting_summary 


p2<-ggplot(righting_summary, aes(
  x = Week,
  y = mean_rt,
  color = Treatment,
  group = Treatment
)) +
  geom_line(size = 1.2) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = mean_rt - se_rt, ymax = mean_rt + se_rt),
                width = 0.2, linewidth = 0.8) +
  geom_rect(aes(xmin = 5.1, xmax = 5.9, ymin = 100, ymax = Inf),
            inherit.aes = FALSE, fill = "red", alpha = 0.01)+
  geom_text(
    aes(x = 5.5, y = 250, label = "CTmax"),
    inherit.aes = FALSE,
    size = 6, face = "bold"
  ) +
  theme_minimal(base_size = 14) +
  ylab("Righting time (s)") +
  xlab("Week") +
  scale_color_manual(values = c("cadetblue1", "red4")) +
  #scale_x_continuous(breaks = unique(rightingplot$Week)) +
  theme(
    strip.text = element_text(colour = "black", size= 18, face ="bold"),       
    strip.background = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "grey50"),
    axis.ticks = element_line(colour = "grey50"),
    axis.text = element_text(size = 20, face = "bold", colour = "black"),
    axis.title.x = element_text(margin = margin(t = 8), face = "bold", size =22), ##changes elements of x axis label, face = bold, italic, underlined
    axis.title.y = element_text(margin = margin(r = 8), face = "bold", size = 22),
    title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 18, face = "bold", colour = "black"),
    legend.title = element_text(size = 18, face = "bold", colour = "black"),
    legend.position = "top"
  )+
  labs(color = "Acclimation (°C)")


p1+p2




p1<-ggplot(right, aes(x = Weight_g, y = Righting_time, fill = Treatment))+
  geom_point(shape = 21, size =4)+
  theme_classic()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))+
  scale_fill_manual(values = c("darkgreen", "pink2"))
p1


###bins for variegated due to inds exceeding 600s frequently
hist(right$Righting_time)
hist(righting$Righting_time_s)
###also performed for greens for comparisons
tr <- righting %>%
  filter(!is.na(Righting_time_s)) %>%
  mutate(
    RT_bin = cut(
      Righting_time_s,
      breaks = c(-Inf, 60, 120, 180, Inf),
      labels = c("0-60", "61-120", "121-180", ">180"),
      ordered_result = TRUE
    )
  )

plotdat <- tr %>%
  count(Week, Treatment, RT_bin) %>%
  group_by(Week, Treatment) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(plotdat,
       aes(x = factor(Week), y = prop, fill = RT_bin)) +
  geom_col() +
  facet_wrap(~Treatment) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Week",
       y = "Individuals (%)",
       fill = "Righting time (s)") +
  theme_classic()

ggplot(plotdat,
       aes(Week, prop,
           colour = RT_bin,
           group = RT_bin)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~Treatment) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Week",
       y = "Individuals (%)",
       colour = "Righting time (s)") +
  theme_classic()


tr$Week <- factor(tr$Week)
###no sig effect of tank
mod <- clm(RT_bin ~ Treatment * Week, data = tr)
summary(mod)

anova(mod)

####variegated
binvar <- right %>%
  filter(!is.na(Righting_time)) %>%
  mutate(
    RT_bin = cut(
      Righting_time,
      breaks = c(-Inf, 60, 120, 180, Inf),
      labels = c("0-60", "61-120", "121-180", ">180"),
      ordered_result = TRUE
    )
  )

plotbin <- binvar %>%
  count(Week, Treatment, RT_bin) %>%
  group_by(Week, Treatment) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(plotbin,
       aes(x = factor(Week), y = prop, fill = RT_bin)) +
  geom_col() +
  facet_wrap(~Treatment) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Week",
       y = "Individuals (%)",
       fill = "Righting time (s)") +
  theme_classic()

ggplot(plotbin,
       aes(Week, prop,
           colour = RT_bin,
           group = RT_bin)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~Treatment) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Week",
       y = "Individuals (%)",
       colour = "Righting time (s)") +
  theme_classic()


binvar$Week <- factor(binvar$Week)
###no sig effect of tank
mod <- clm(RT_bin ~ Treatment * Week, data = binvar)
summary(mod)

anova(mod)



# Stats -------------------------------------------------------------------

###mean CTmax
CTmaxgreen %>% select(Treatment,TestSize, Weight, Tubefeet, Spines, LossofAdhesion,TubeProb,SpineProb) ->greenfilt
CTmaxpurp %>% select(Treatment,Size, Weight, TubeFeet, SpineDec, LossOfAdhesion,TubeProb,SpineProb) ->purpfilt
CTmaxvar %>% select(Treatment, Weight, Tubefeet, Spines, LossofAdhesion,Spines) ->varfilt


purpfilt %>% group_by(Treatment) %>% summarise(meanct = mean(LossOfAdhesion))
greenfilt %>% select(Treatment, LossofAdhesion) %>% group_by(Treatment) %>% drop_na() %>% summarise(meanct = mean(LossofAdhesion))
varfilt %>% select(Treatment, LossofAdhesion) %>% group_by(Treatment) %>% drop_na() %>% summarise(meanct = mean(LossofAdhesion))

right
righting

m1<-lmer(LossofAdhesion~Treatment + Trial +(1|ID), data = CTmaxgreen)
m1<-lmer(LossofAdhesion~Treatment + (1|ID), data = CTmaxgreen)
m2<-lmer(Spines~Treatment + (1|ID), data = CTmaxgreen)
m3<-lmer(Tubefeet~Treatment + (1|ID), data = CTmaxgreen)

qqnorm(resid(m1))
qqnorm(resid(m2))
qqnorm(resid(m3))
hist(resid(m1))
hist(resid(m2))
hist(resid(m3))

summary(m1)
anova(m1)
anova(m2)
anova(m3)

m1<-lm(LossOfAdhesion~Treatment, data = CTmaxpurp)
m2<-lm(SpineDec~Treatment, data = CTmaxpurp)
m3<-lm(TubeFeet~Treatment , data = CTmaxpurp)

qqnorm(resid(m1))
qqnorm(resid(m2))
qqnorm(resid(m3))
hist(resid(m1))
hist(resid(m2))
hist(resid(m3))

anova(m1)
anova(m2)
anova(m3)

str(righting)


m1 <- glmmTMB(
  Righting_time_s ~ Treatment * Week +
    (1 | UrchinID),
  family = Gamma(link = "log"),
  data = righting
)

m2 <- glmmTMB(
  Righting_time_s ~ Treatment + Week +
    (1 | UrchinID),
  family = Gamma(link = "log"),
  data = righting
)

AIC(m1, m2)
anova(m2, m1)
car::Anova(m2)
summary(m2)

m3 <- glmmTMB(
  Righting_time_s ~ ExpPhase * Treatment +
    (1 | UrchinID),
  family = Gamma(link = "log"),
  data = righting
)
m4 <- glmmTMB(
  Righting_time_s ~ ExpPhase + Treatment +
    (1 | UrchinID),
  family = Gamma(link = "log"),
  data = righting
)

m5 <- glmmTMB(
  Righting_time_s ~ ExpPhase + 
    (1 | UrchinID),
  family = Gamma(link = "log"),
  data = righting
)

anova(m4, m5)

summary(m5)


m1<-aov(Righting_time~Colour*Weight_g, data = right)
m1
anova(m1)



m1<-lmer(LossofAdhesion~Treatment + Trial +(1|ID), data = CTmaxvar)
m1<-lmer(LossofAdhesion~Treatment + (1|ID), data = CTmaxvar)
m2<-lmer(Spines~Treatment + (1|ID), data = CTmaxvar)
m3<-lmer(Tubefeet~Treatment + (1|ID), data = CTmaxvar)

qqnorm(resid(m1))
qqnorm(resid(m2))
qqnorm(resid(m3))
hist(resid(m1))
hist(resid(m2))
hist(resid(m3))

summary(m1)
anova(m1)
anova(m2)
anova(m3)


right

m1 <- glmmTMB(
  Righting_time ~ Treatment * Week +
    (1 | UrchinID),
  family = Gamma(link = "log"),
  data = right
)

m2 <- glmmTMB(
  Righting_time ~ Treatment + Week +
    (1 | UrchinID),
  family = Gamma(link = "log"),
  data = right
)

AIC(m1, m2)
summary(m2)
car::Anova(m1)
