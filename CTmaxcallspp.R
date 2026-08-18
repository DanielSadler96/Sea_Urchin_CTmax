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
library(emmeans)
library(performance)
library(MuMIn)
library(rptR)
# readfiles ---------------------------------------------------------------

setwd("~/Documents/Postdoc/CTmaxUrchin")

CTmaxpurp2<-read.csv("~/Documents/Postdoc/CTmaxUrchin/Purpleurchins/CTmaxurchin.csv")
CTmaxgreen<-read.csv("~/Documents/Postdoc/CTmaxUrchin/Greenurchins/CTmaxGreens.csv")
CTmaxvar<-read.csv("~/Documents/Postdoc/CTmaxUrchin/Lytechinus/CTmaxVar.csv")

CTmaxvar$Treatment<-as.factor(CTmaxvar$Treatment)
CTmaxpurp2$Treatment<-as.factor(CTmaxpurp$Treatment)
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
p1<-ggplot(CTmaxpurp2, aes(x = Treatment, y = LossOfAdhesion, color = Treatment , fill =Treatment)) +
  geom_boxplot(aes(fill = Treatment),
               width = 0.2, 
               outlier.shape = NA,
               alpha=0.15,
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
  scale_y_continuous(
    limits = c(24, 32),
    breaks = c(24, 26, 28, 30, 32)
  )

p1  
###add phylopic image
p1<-ggpubr::ggarrange(p1)+
  add_phylopic(uuid = "a6125e0d-c627-40e3-9966-5ad288277545", 
               x=0.85, 
               y=0.25, 
               alpha=0.6,
               ysize=0.2, 
               fill = "purple")


p1
CTmaxgreen
p2<-ggplot(CTmaxgreen, aes(x = Treatment, y = LossofAdhesion, color = Treatment)) +
  geom_boxplot(aes(fill = Treatment),
               width = 0.2, 
               outlier.shape = NA,
               alpha=0.15,
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
  scale_y_continuous(
    limits = c(17, 30),
    breaks = c(18, 20, 22, 24, 26, 28, 30)
  )


p2
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
  ) +
  scale_y_continuous(
    limits = c(30, 38),
    breaks = c(30, 32, 34, 36, 38)
  )

p3<-ggpubr::ggarrange(p3)+
  add_phylopic(uuid = "a6125e0d-c627-40e3-9966-5ad288277545", 
               x=0.85, 
               y=0.25, 
               alpha=0.6,
               ysize=0.2, 
               fill = "tan2")


p3

p4<-p1+p2+p3

###combine using patchwork
p4<-(p2 | p1) /
  (p3 | plot_spacer())
ggsave(p4, file = "panelctax.png", width = 14, height = 12, dpi = 500)

# TSM ---------------------------------------------------------------------
CTmaxgreen$Treatmentnum<-as.numeric(as.character(CTmaxgreen$Treatment))
CTmaxgreen %>% mutate(TSM = LossofAdhesion - Treatmentnum ) ->CTmaxgreen
p1<-ggplot(CTmaxgreen, aes(x = Treatment, y = TSM, color = Treatment , fill =Treatment)) +
  geom_boxplot(aes(fill = Treatment),
               width = 0.2, 
               outlier.shape = NA,
               alpha=0.15,
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
    x = "Acclimation Temperature (°C)",
    y = "TSM"
  ) +
  theme(
    legend.position = "none"
  )
p1  

# validation --------------------------------------------------------------
###Probability of phenotype occuring, note loss of adhesion occurs 100% of the time as the end point of the assay.

CTmaxgreen %>% summarise(probtube=sum(TubeProb, na.rm=T)/n(),
                         probspine=sum(SpineProb, na.rm=T)/n())
#probtube probspine
#  0.875    0.3875
CTmaxvar %>% summarise(probtube=sum(TubeProb, na.rm=T)/n(),
                         probspine=sum(SpineProb, na.rm=T)/n())

CTmaxpurp2 %>% summarise(probtube=sum(TubeProb, na.rm=T)/n(),
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


###Greens
rpt_adj <- rpt(LossofAdhesion ~ Treatment + (1 | ID), 
               grname = "ID", 
               data = CTmaxgreen, 
               datatype = "Gaussian", 
               nboot = 1000, 
               npermut = 1000) #0.399

summary(rpt_adj)

rpt_adj <- rpt(Spines ~ Treatment + (1 | ID), 
               grname = "ID", 
               data = CTmaxgreen, 
               datatype = "Gaussian", 
               nboot = 1000, 
               npermut = 1000)

summary(rpt_adj) #0

rpt_adj <- rpt(Tubefeet ~ Treatment + (1 | ID), 
               grname = "ID", 
               data = CTmaxgreen, 
               datatype = "Gaussian", 
               nboot = 1000, 
               npermut = 1000)

summary(rpt_adj)#0.298



rpt_8  <- rpt(LossofAdhesion ~ (1|ID), grname = "ID", 
              data = filter(CTmaxgreen, Treatment == "8"),
              datatype = "Gaussian", nboot = 1000, npermut = 1000) #0.261

rpt_14 <- rpt(LossofAdhesion ~ (1|ID), grname = "ID", 
              data = filter(CTmaxgreen, Treatment == "14"),
              datatype = "Gaussian", nboot = 1000, npermut = 1000) # 0.564

summary(rpt_8); summary(rpt_14)


###Var
rpt_adj <- rpt(LossofAdhesion ~ Treatment + (1 | ID), 
               grname = "ID", 
               data = CTmaxvar, 
               datatype = "Gaussian", 
               nboot = 1000, 
               npermut = 1000) #0.399

summary(rpt_adj) #0.446

rpt_adj <- rpt(Spines ~ Treatment + (1 | ID), 
               grname = "ID", 
               data = CTmaxvar, 
               datatype = "Gaussian", 
               nboot = 1000, 
               npermut = 1000)

summary(rpt_adj) #0.575

rpt_adj <- rpt(Tubefeet ~ Treatment + (1 | ID), 
               grname = "ID", 
               data = CTmaxvar, 
               datatype = "Gaussian", 
               nboot = 1000, 
               npermut = 1000)

summary(rpt_adj)#0.0758


###plot####

CTmaxgreen <- CTmaxgreen %>%
  mutate(
    ID = interaction(Colour, Tank, drop = TRUE)   # unique ID per individual
  )
CTmaxgreen$Trial<-as.factor(CTmaxgreen$Trial)

CTmaxvar <- CTmaxvar %>%
  mutate(
    ID = interaction(TagColour, Tank, drop = TRUE)   # unique ID per individual
  )
CTmaxvar$Trial<-as.factor(CTmaxvar$Trial)

# Repeatability
R_green <- c(
  "Loss of adhesion" = 0.466,
  "Tube feet" = 0.298,
  "Spine compression" = 0
)

R_var <- c(
  "Loss of adhesion" = 0.337,
  "Tube feet" = 0.070,
  "Spine compression" = 0.575
)

# Plotting function
plot_trait <- function(data, trait, ylab, temp_labs, R, colours) {
  
  data$Trial <- as.factor(data$Trial)
  
  ggplot(data, aes(x = Trial, y = .data[[trait]], colour = Treatment)) +
    geom_line(aes(group = ID), alpha = 0.2, linewidth = 1) +
    geom_jitter(
      aes(fill = Treatment),
      width = 0,
      height = 0,
      size = 3,
      alpha = 0.3,
      shape = 21,
      colour = "black",
      stroke = 1.5
    ) +
    geom_boxplot(
      aes(group = Trial, fill = Treatment),
      width = 0.2,
      alpha = 0.15,
      outlier.shape = NA,
      linewidth = 1.5
    ) +
    scale_fill_manual(values = colours) +
    scale_colour_manual(values = colours) +
    facet_grid(
      . ~ Treatment,
      labeller = as_labeller(temp_labs)
    ) +
    labs(
      # bold() wraps everything, italic(R) keeps R slanted, and .() injects variables
      title = bquote(bold(.(ylab)) ~ "(" * italic(R) ~  "=" ~  .(R) * ")"),
      x = "Trial no.",
      y = expression(bold(CT[max] ~' (°C)'))
    )+
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(colour = "grey50"),
      axis.ticks = element_line(colour = "grey50"),
      axis.text = element_text(size = 18, face = "bold", colour = "black"),
      axis.title = element_text(size = 20, face = "bold"),
      strip.text = element_text(size = 18, face = "bold"),
      plot.title = element_text(size = 18, face = "bold"),
      legend.position = "none"
    )
}


p1 <- plot_trait(
  CTmaxgreen,
  "LossofAdhesion",
  "Loss of adhesion",
  c(`8` = "8°C", `14` = "14°C"),
  R_green["Loss of adhesion"],
  c("8" = "lightblue1", "14" = "firebrick3")
)
p1
p2 <- plot_trait(
  CTmaxgreen,
  "Tubefeet",
  "Tube feet",
  c(`8` = "8°C", `14` = "14°C"),
  R_green["Tube feet"],
  c("8" = "lightblue1", "14" = "firebrick3")
)

p3 <- plot_trait(
  CTmaxgreen,
  "Spines",
  "Spine compression",
  c(`8` = "8°C", `14` = "14°C"),
  R_green["Spine compression"],
  c("8" = "lightblue1", "14" = "firebrick3")
)


p4 <- plot_trait(
  CTmaxvar,
  "LossofAdhesion",
  "Loss of adhesion",
  c(`21` = "21°C", `27` = "27°C"),
  R_var["Loss of adhesion"],
  c("21" = "lightblue1", "27" = "firebrick3")
)

p5 <- plot_trait(
  CTmaxvar,
  "Tubefeet",
  "Tube feet",
  c(`21` = "21°C", `27` = "27°C"),
  R_var["Tube feet"],
  c("21" = "lightblue1", "27" = "firebrick3")
)

p6 <- plot_trait(
  CTmaxvar,
  "Spines",
  "Spine compression",
  c(`21` = "21°C", `27` = "27°C"),
  R_var["Spine compression"],
  c("21" = "lightblue1", "27" = "firebrick3")
)


pall <- (p2 | p5) /
  (p3 | p6) /
  (p1 | p4)

pall

ggsave(pall, file = "ctmaxpanel.png", width = 12, height =15)



p1 <- ggplot(CTmaxpurp2, aes(x = Treatment, y = LossOfAdhesion,
                             color = Treatment, fill = Treatment)) +
  geom_boxplot(
    width = 0.2, outlier.shape = NA, alpha = 0.15, size = 2
  ) +
  geom_jitter(
    size = 3, alpha = 0.5, shape = 21,
    colour = "black", stroke = 1.5, width = 0
  ) +
  geom_segment(aes(xend = Treatment, y = as.numeric(as.character(Treatment))-0.5, yend = as.numeric(as.character(Treatment))), 
               color = "purple2", linewidth = 20)+
  scale_fill_manual(values = c("lightblue1", "firebrick3", "blue3", "firebrick")) +
  scale_color_manual(values = c("lightblue3", "red4", "blue", "red")) +
  scale_y_continuous(limits = c(6, 38)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "grey50"),
    axis.ticks = element_line(colour = "grey50"),
    axis.text = element_text(size = 20, face = "bold", colour = "black"),
    axis.title.x = element_text(margin = margin(t = 8), face = "bold", size = 22),
    axis.title.y = element_text(margin = margin(r = 8), face = "bold", size = 22),
    title = element_text(size = 20, face = "bold"),
    legend.position = "none"
  ) +
  labs(
    title = expression(italic("Strongylocentrotus purpuratus")),
    x = "Acclimation Temperature (°C)",
    y = "CTmax (°C)"
  )

p2 <- ggplot(CTmaxgreen, aes(x = Treatment, y = LossofAdhesion,
                             color = Treatment, fill = Treatment)) +
  geom_boxplot(
    width = 0.2, outlier.shape = NA, alpha = 0.15, size = 2
  ) +
  geom_jitter(
    size = 3, alpha = 0.5, shape = 21,
    colour = "black", stroke = 1.5, width = 0
  ) +
  geom_segment(aes(xend = Treatment, y = as.numeric(as.character(Treatment))-0.5, yend = as.numeric(as.character(Treatment))), 
               color = "green4", linewidth = 20)+
  scale_fill_manual(values = c("8" = "lightblue1", "14" = "firebrick3")) +
  scale_color_manual(values = c("8" = "lightblue3", "14" = "red4")) +
  scale_y_continuous(limits = c(6, 38)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "grey50"),
    axis.ticks = element_line(colour = "grey50"),
    axis.text = element_text(size = 20, face = "bold", colour = "black"),
    axis.title.x = element_text(margin = margin(t = 8), face = "bold", size = 22),
    axis.title.y = element_text(margin = margin(r = 8), face = "bold", size = 22),
    title = element_text(size = 20, face = "bold"),
    legend.position = "none"
  ) +
  labs(
    title = expression(italic("Strongylocentrotus droebachiensis")),
    x = "Acclimation Temperature (°C)",
    y = "CTmax (°C)"
  )

p3 <- ggplot(CTmaxvar, aes(x = Treatment, y = LossofAdhesion,
                           color = Treatment, fill = Treatment)) +
  geom_boxplot(
    width = 0.2, outlier.shape = NA, alpha = 0.15, size = 2
  ) +
  geom_jitter(
    size = 3, alpha = 0.5, shape = 21,
    colour = "black", stroke = 1.5, width = 0
  ) +
  geom_segment(aes(xend = Treatment, y = as.numeric(as.character(Treatment))-0.5, yend = as.numeric(as.character(Treatment))), 
               color = "tan2", linewidth = 20)+
  
  scale_fill_manual(values = c("lightblue1", "firebrick3", "blue3", "firebrick")) +
  scale_color_manual(values = c("lightblue3", "red4", "blue", "red")) +
  scale_y_continuous(limits = c(6, 38)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "grey50"),
    axis.ticks = element_line(colour = "grey50"),
    axis.text = element_text(size = 20, face = "bold", colour = "black"),
    axis.title.x = element_text(margin = margin(t = 8), face = "bold", size = 22),
    axis.title.y = element_text(margin = margin(r = 8), face = "bold", size = 22),
    title = element_text(size = 20, face = "bold"),
    legend.position = "none"
  ) +
  labs(
    title = expression(italic("Lytechinus variegatus")),
    x = "Acclimation Temperature (°C)",
    y = "CTmax (°C)"
  )
p3
# Add phylopic images
p1 <- ggpubr::ggarrange(p1) +
  add_phylopic(
    uuid = "a6125e0d-c627-40e3-9966-5ad288277545",
    x = 0.85, y = 0.25, alpha = 0.6,
    ysize = 0.2, fill = "purple"
  )

p2 <- ggpubr::ggarrange(p2) +
  add_phylopic(
    uuid = "a6125e0d-c627-40e3-9966-5ad288277545",
    x = 0.9, y = 0.25, alpha = 0.6,
    ysize = 0.2, fill = "green4"
  )

p3 <- ggpubr::ggarrange(p3) +
  add_phylopic(
    uuid = "a6125e0d-c627-40e3-9966-5ad288277545",
    x = 0.85, y = 0.25, alpha = 0.6,
    ysize = 0.2, fill = "tan2"
  )

p4 <- p1 + p2 + p3
p4


# Ind correlations --------------------------------------------------------

CTmaxvar_clean <- CTmaxvar %>% filter(!is.na(ID))

# confirm no more duplicates
CTmaxvar_clean %>%
  summarise(n = n(), .by = c(ID, Treatment, Trial)) %>%
  filter(n > 1)

la_wide <- CTmaxvar_clean %>%
  select(ID, Treatment, Trial, LossofAdhesion) %>%
  pivot_wider(names_from = Trial, values_from = LossofAdhesion, names_prefix = "Trial")

cor.test(la_wide$Trial1, la_wide$Trial2, method = "pearson")
cor.test(la_wide$Trial1, la_wide$Trial2, method = "spearman")

la_wide %>%
  group_by(Treatment) %>%
  summarise(
    n = sum(!is.na(Trial1) & !is.na(Trial2)),
    pearson_r = cor(Trial1, Trial2, use = "complete.obs"),
    spearman_rho = cor(Trial1, Trial2, method = "spearman", use = "complete.obs")
  )


ggplot(CTmaxvar, aes(x = Trial, y = LossofAdhesion, group = ID)) +
  geom_line(alpha = 0.4) +
  geom_point(size = 1.5) +
  facet_wrap(~ Treatment) +
  labs(title = "Individual loss-of-adhesion time across trials",
       x = "Trial", y = "Loss of adhesion (time)") +
  theme_minimal()


###green

CTmaxgreen_clean <- CTmaxgreen %>% filter(!is.na(ID))

# confirm no more duplicates
CTmaxgreen_clean %>%
  summarise(n = n(), .by = c(ID, Treatment, Trial)) %>%
  filter(n > 1)

la_wide <- CTmaxgreen_clean %>%
  select(ID, Treatment, Trial, LossofAdhesion) %>%
  pivot_wider(names_from = Trial, values_from = LossofAdhesion, names_prefix = "Trial")

cor.test(la_wide$Trial1, la_wide$Trial2, method = "pearson")
cor.test(la_wide$Trial1, la_wide$Trial2, method = "spearman")

la_wide %>%
  group_by(Treatment) %>%
  summarise(
    n = sum(!is.na(Trial1) & !is.na(Trial2)),
    pearson_r = cor(Trial1, Trial2, use = "complete.obs"),
    spearman_rho = cor(Trial1, Trial2, method = "spearman", use = "complete.obs")
  )


ggplot(CTmaxgreen, aes(x = Trial, y = LossofAdhesion, group = ID)) +
  geom_line(alpha = 0.4) +
  geom_point(size = 1.5) +
  facet_wrap(~ Treatment) +
  labs(title = "Individual loss-of-adhesion time across trials",
       x = "Trial", y = "Loss of adhesion (time)") +
  theme_minimal()


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

p2<-ggplot(CTmaxpurp2, aes(x = Weight, y = LossOfAdhesion, fill = Treatment))+
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

str(CTmaxgreen)
str(CTmaxpurp)
str(CTmaxvar)

cor.test(CTmaxgreen$Weight,CTmaxgreen$LossofAdhesion) #0.03
cor.test(CTmaxpurp2$Weight,CTmaxpurp2$LossOfAdhesion) #0.04
cor.test(CTmaxvar$Weight,CTmaxvar$LossofAdhesion) #0.07

cor.test(CTmaxgreen$Weight,CTmaxgreen$Tubefeet) #0.13
cor.test(CTmaxpurp2$Weight,CTmaxpurp2$TubeFeet) #-0.25
cor.test(CTmaxvar$Weight,CTmaxvar$Tubefeet) #-0.02

cor.test(CTmaxgreen$Weight,CTmaxgreen$Spines) #0.05
cor.test(CTmaxpurp2$Weight,CTmaxpurp2$SpineDec) #-0.15
cor.test(CTmaxvar$Weight,CTmaxvar$Spines) #-0.08

CTmaxgreen %>% group_by(Trial, Treatment) %>% summarise(meanweight = mean(Weight, na.rm = TRUE))
CTmaxvar %>% group_by(Trial, Treatment) %>% summarise(meanweight = mean(Weight, na.rm = TRUE))
CTmaxpurp2 %>% group_by(Treatment) %>% summarise(meanweight = mean(LossOfAdhesion, na.rm = TRUE))


CTmaxgreen %>% filter(Trial == 2) %>% select(Weight,Treatment) -> weightCT
righting %>%  filter(Week == 1) %>% select(Weight_g,Treatment) %>% rename(Weight = Weight_g)-> weightright
weightCT$phase<-"Start"
weightright$phase<-"End"

weightgreen<-rbind(weightright,weightCT)
weightgreen %>% group_by(Treatment, phase) %>% summarise(meanweight =mean(Weight, na.rm =T))
CTmaxvar
right
CTmaxvar %>% filter(Trial == 2) %>% select(Weight,Treatment) -> weightCTvar
right %>%  filter(Week == 1) %>% select(Weight_g,Treatment) %>% rename(Weight = Weight_g) -> weightrightvar
weightCTvar$phase<-"Start"
weightrightvar$phase<-"End"

weightvar<-rbind(weightrightvar,weightCTvar)
weightvar %>% group_by(phase) %>% summarise(meanweight =mean(Weight, na.rm =T))



# Photoperiod -------------------------------------------------------------

m1<-lm(LossofAdhesion~Time, data = CTmaxgreen)
m2<-lm(LossOfAdhesion~Time, data = CTmaxpurp2)
m3<-lm(LossofAdhesion~Time, data = CTmaxvar)
anova(m1)
anova(m2)
anova(m3)

m1<-lm(Spines~Time, data = CTmaxgreen)
m2<-lm(SpineDec~Time, data = CTmaxpurp2)
m3<-lm(Spines~Time, data = CTmaxvar)
anova(m1)
anova(m2)
anova(m3)

m1<-lm(Tubefeet~Time, data = CTmaxgreen)
m2<-lm(TubeFeet~Time, data = CTmaxpurp2)
m3<-lm(Tubefeet~Time, data = CTmaxvar)
anova(m1)
anova(m2)
anova(m3)

###Rotations

m1<-lm(LossofAdhesion~Cage.Rotations, data = CTmaxgreen)
m2<-lm(LossOfAdhesion~Cage.rotations, data = CTmaxpurp2)
m3<-lm(LossofAdhesion~Cage.Rotations, data = CTmaxvar)
anova(m1)
anova(m2)
anova(m3)

m1<-lm(Spines~Cage.Rotations, data = CTmaxgreen)
m2<-lm(SpineDec~Cage.rotations, data = CTmaxpurp2) 
m3<-lm(Spines~Cage.Rotations, data = CTmaxvar)
anova(m1)
anova(m2) 
anova(m3)

m1<-lm(Tubefeet~Cage.Rotations, data = CTmaxgreen)
m2<-lm(TubeFeet~Cage.rotations, data = CTmaxpurp2)
m3<-lm(Tubefeet~Cage.Rotations, data = CTmaxvar)
anova(m1)
anova(m2)
anova(m3)

# variance ----------------------------------------------------------------


CTmaxgreen %>%
  drop_na() %>%
  summarise(CVAdhesion=cv(LossofAdhesion),
            CVSpine = cv(Spines),
            CVtube = cv(Tubefeet))
CTmaxpurp2 %>%
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

righting<-read.csv("~/Documents/Postdoc/CTmaxUrchin/Greenurchins/rightingrespGreens.csv")
righting$Treatment<-as.factor(righting$Treatment)
right<-read.csv("~/Documents/Postdoc/CTmaxUrchin/Lytechinus/rightingrespLyt.csv")
right %>% group_by(UrchinID) %>% drop_na(UrchinID) %>% ungroup() -> right
right$Week<-as.factor(right$Week)
right$Treatment<-as.factor(right$Treatment)
str(righting)
str(right)

right <- right %>%
  mutate(
    Activity = pmax(0, (1000 / Righting_time) - (1000 / 600))
  )

righting <- righting %>%
  mutate(
    Activity = pmax(0, (1000 / Righting_time_s) - (1000 / 400))
  )


righting %>% arrange(desc(Righting_time_s))

rightingplot<-righting %>% 
  dplyr::select(Treatment,Week,Activity) %>% ##selects only columns we are interesed in plotting
  drop_na() ### removes NAs (deaths)
rightingplot %>% dim() ##checking size of the dataframe to make sure it didnt remove everything

rightingplot
rightingplot<-rightingplot %>%
  mutate(treatmentweek = paste(Treatment, Week, sep="_"))
###plot for Righting time by week
righting_summary <- rightingplot %>%
  dplyr::summarise(
    mean_rt = mean(Activity, na.rm = TRUE),
    sd_rt   = sd(Activity, na.rm = TRUE),
    n       = length(Activity),
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
  geom_rect(aes(xmin = 5, xmax = 5.1, ymin = 0, ymax = Inf),
            inherit.aes = FALSE, fill = "red", alpha = 0.01)+
  geom_rect(aes(xmin = 6, xmax = 6.1, ymin = 0, ymax = Inf),
            inherit.aes = FALSE, fill = "red", alpha = 0.01)+
  # geom_text(
  #   aes(x = 5.5, y = 50, label = "CTmax"),
  #   inherit.aes = FALSE,
  #   size = 6, face = "bold"
  # ) +  
  geom_errorbar(aes(ymin = mean_rt - se_rt, ymax = mean_rt + se_rt),
                width = 0.2, linewidth = 0.8) +
  theme_minimal(base_size = 14) +
  ylab("Activity coefficient") +
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

p1
rightingplot<-right %>% 
  dplyr::select(Treatment,Week,Activity) %>% ##selects only columns we are interesed in plotting
  drop_na() ### removes NAs (deaths)
rightingplot %>% dim() ##checking size of the dataframe to make sure it didnt remove everything
tail(rightingplot)

rightingplot %>% filter(Righting_time < 590) ->rightingplot
rightingplot

rightingplot<-rightingplot %>%
  mutate(treatmentweek = paste(Activity, Week, sep="_"))
###plot for Righting time by week
righting_summary <- rightingplot %>%
  dplyr::summarise(
    mean_rt = mean(Activity, na.rm = TRUE),
    sd_rt   = sd(Activity, na.rm = TRUE),
    n       = length(Activity),
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
  geom_rect(aes(xmin = 5, xmax = 5.1, ymin = 0, ymax = Inf),
            inherit.aes = FALSE, fill = "red", alpha = 0.01)+
  geom_rect(aes(xmin = 6, xmax = 6.1, ymin = 0, ymax = Inf),
            inherit.aes = FALSE, fill = "red", alpha = 0.01)+
  #  geom_text(
  #   aes(x = 5.5, y = 7.7, label = "CTmax"),
  #   inherit.aes = FALSE,
  #   size = 6, face = "bold"
  # ) +
  theme_minimal(base_size = 14) +
  ylab("Activity coefficient") +
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
p2

p1+p2


# Stats -------------------------------------------------------------------

###mean CTmax
CTmaxgreen %>% select(Treatment,TestSize, Weight, Tubefeet, Spines, LossofAdhesion,TubeProb,SpineProb) ->greenfilt
CTmaxpurp %>% select(Treatment,Size, Weight, TubeFeet, SpineDec, LossOfAdhesion,TubeProb,SpineProb) ->purpfilt
CTmaxvar %>% select(Treatment, Weight, Tubefeet, Spines, LossofAdhesion,Spines) ->varfilt


purpfilt %>% group_by(Treatment) %>% summarise(meanct = mean(LossOfAdhesion))
greenfilt %>% select(Treatment, LossofAdhesion) %>% group_by(Treatment) %>% drop_na() %>% summarise(meanct = mean(LossofAdhesion))
varfilt %>% select(Treatment, LossofAdhesion) %>% group_by(Treatment) %>% drop_na() %>% summarise(meanct = mean(LossofAdhesion))


####Greens ####
m1<-lmer(LossofAdhesion~Treatment + Trial +(1|ID) +(1|Tank), data = CTmaxgreen,
         REML = FALSE)
qqnorm(resid(m1))
hist(resid(m1))
shapiro.test(resid(m1))
VarCorr(m1) ### Tank has no variance causing singularity 

m2<-lmer(LossofAdhesion~Treatment * Trial +(1|ID), data = CTmaxgreen,
         REML = FALSE)
m3<-lmer(LossofAdhesion~Treatment + Trial +(1|ID), data = CTmaxgreen,REML = FALSE)

AIC(m2) ##319.55
AIC(m3) ##320.5949
anova(m2, m3) ##keep interaction
anova(m2)
r2_nakagawa(m2)
qqnorm(resid(m2))
hist(resid(m2))
shapiro.test(resid(m2))
anova(m3)
summary(m3)

m1<-lmer(Spines~Treatment + Trial + (1|ID) + (1|Tank), data = CTmaxgreen,
         REML = FALSE)
qqnorm(resid(m1))
hist(resid(m1))
shapiro.test(resid(m1))
VarCorr(m1) ##Tank zero vairance drop 
AIC(m1)
m2<-lmer(Spines~Treatment + Trial + (1|ID), data = CTmaxgreen,
         REML = FALSE)
VarCorr(m2) ##ID zero vairance drop 
AIC(m2)
anova(m1,m2)
m3<-lm(Spines~Treatment+Trial, data = CTmaxgreen)
AIC(m3)
m4<-lm(Spines~Treatment, data = CTmaxgreen)
AIC(m4)
anova(m4,m3)


qqnorm(resid(m3))
hist(resid(m3))
shapiro.test(resid(m3))
anova(m3)


m1<-lmer(Tubefeet~Treatment + Trial + (1|ID)+ (1|Tank),  data = CTmaxgreen,REML = FALSE)
VarCorr(m1) ##Tank zero vairance
m2<-lmer(Tubefeet~Treatment + Trial + (1|ID),  data = CTmaxgreen,REML = FALSE)
VarCorr(m3)
AIC(m1)
AIC(m2)
anova(m1,m2)

qqnorm(resid(m2))
hist(resid(m2))
shapiro.test(resid(m2))
anova(m2)

###Purples####
CTmaxpurp2 ##no repeats

m1<-lm(LossOfAdhesion~Treatment, data = CTmaxpurp2)
m2<-lmer(SpineDec~Treatment, data = CTmaxpurp2)
m3<-lmer(TubeFeet~Treatment , data = CTmaxpurp2)

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

###Variegated ####

m1<-lmer(LossofAdhesion~Treatment * Trial +(1|ID) +(1|Tank), data = CTmaxvar,
         REML = FALSE)
qqnorm(resid(m1))
hist(resid(m1))
shapiro.test(resid(m1))
VarCorr(m1) ##Tank zero vairance drop 
AIC(m1)
m2<-lmer(LossofAdhesion~Treatment * Trial +(1|ID), data = CTmaxvar,
         REML = FALSE)

AIC(m2)
anova(m1,m2)
m3<-lmer(LossofAdhesion~Treatment + Trial +(1|ID), data = CTmaxvar,
         REML = FALSE)
AIC(m3)
anova(m2,m3)

qqnorm(resid(m3))
hist(resid(m3))
shapiro.test(resid(m3))
anova(m3)

###spines
m1<-lmer(Spines~Treatment * Trial +(1|ID) +(1|Tank), data = CTmaxvar,
         REML = FALSE)
qqnorm(resid(m1))
hist(resid(m1))
shapiro.test(resid(m1))
VarCorr(m1) ##Tank zero vairance drop 
AIC(m1)
m2<-lmer(Spines~Treatment + Trial +(1|ID), data = CTmaxvar,
         REML = FALSE)
AIC(m2)
anova(m1,m2)
anova(m2)

qqnorm(resid(m2))
hist(resid(m2))
shapiro.test(resid(m2))
anova(m2)

###Tubefeet

m1<-lmer(Tubefeet~Treatment * Trial +(1|ID) +(1|Tank), data = CTmaxvar,
         REML = FALSE)
qqnorm(resid(m1))
hist(resid(m1))
shapiro.test(resid(m1))
VarCorr(m1) ##Tank zero vairance drop 
AIC(m1)
m2<-lmer(Tubefeet~Treatment * Trial +(1|ID), data = CTmaxvar,
         REML = FALSE)
AIC(m2)
anova(m1,m2)
m3<-lmer(Tubefeet~Treatment + Trial +(1|ID), data = CTmaxvar,
         REML = FALSE)
AIC(m3)
anova(m2,m3)

qqnorm(resid(m3))
hist(resid(m3))
shapiro.test(resid(m3))
anova(m3)

# ARR ---------------------------------------------------------------------


ARR_green <- CTmaxgreen %>% group_by(Trial) %>%
  summarise(
    Species = "S. droebachiensis",
    CTmax_low = mean(LossofAdhesion[Treatment == "8"], na.rm = TRUE),
    CTmax_high = mean(LossofAdhesion[Treatment == "14"], na.rm = TRUE)
  ) %>%
  mutate(
    ARR = (CTmax_high - CTmax_low) / 6
  )
ARR_green

ARR_purp <- CTmaxpurp2 %>% 
  summarise(
    Species = "S. purpuratus",
    CTmax_low = mean(LossOfAdhesion[Treatment == "12"], na.rm = TRUE),
    CTmax_high = mean(LossOfAdhesion[Treatment == "18"], na.rm = TRUE)
  ) %>%
  mutate(
    ARR = (CTmax_high - CTmax_low) / 6
  )

ARR_var <- CTmaxvar %>% group_by(Trial) %>%
  summarise(
    Species = "L. variegatus",
    CTmax_low = mean(LossofAdhesion[Treatment == "21"], na.rm = TRUE),
    CTmax_high = mean(LossofAdhesion[Treatment == "27"], na.rm = TRUE)
  ) %>%
  mutate(
    ARR = (CTmax_high - CTmax_low) / 6
  )

ARR_all <- bind_rows(
  ARR_green,
  ARR_purp,
  ARR_var
)

ARR_all

ARR_plot <- ARR_all %>%
  group_by(Species) %>%
  summarise(
    ARR_mean = mean(ARR, na.rm = TRUE),
    ARR_sd = sd(ARR, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(ARR_plot, aes(x = Species, y = ARR_mean, fill = Species)) +
  geom_errorbar(
    aes(ymin = ARR_mean - ARR_sd,
        ymax = ARR_mean + ARR_sd),
    width = 0.25,
    linewidth = 1.5
  ) +
  geom_point(
    size = 9,
    shape = 21,
    stroke = 1.5
  ) +
  labs(
    x = NULL,
    y = "Acclimation response ratio (ARR)"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 16, colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", face = "italic"),
    axis.title = element_text(size = 18, colour = "black"),
    legend.position = "none"
  ) +
  scale_fill_manual(
    values = c("tan2", "green4", "purple2")
  )+
  ylim(c(0,1))

# Stats-Righting ----------------------------------------------------------------

righting$Week<-as.factor(righting$Week)
hist(righting$Activity)

m1 <- glmmTMB(
  Activity ~ Treatment * Week +
    (1 | Tank),
  family = tweedie(link = "log"),
  data = righting
)

m2 <- glmmTMB(
  Activity ~ Treatment + Week +
    (1 | Tank),
  family = tweedie(link = "log"),
  data = righting
)

anova(m1, m2)
AIC(m1)
AIC(m2)
car::Anova(m2)

summary(m1)
car::Anova(m1, type = "III")
emmeans(m1, ~ Treatment)
emmeans(m1, ~ Week)


m1 <- glmmTMB(
  Righting_time_s ~ Treatment * Week +
    (1 | Tank),
  family = Gamma(link = "log"),
  data = righting
)

m2 <- glmmTMB(
  Righting_time_s ~ Treatment + Week +
    (1 | Tank),
  family = Gamma(link = "log"),
  data = righting
)

car::Anova(m2)
AIC(m1, m2)
anova(m1, m2)

summary(m1)
car::Anova(m2, type = "III")
emmeans(m2, ~ Treatment)
emmeans(m2, ~ Week)

m1 <- glmmTMB( 
  Righting_time ~ Treatment * Week +
    (1 | Tank),
  family = Gamma(link = "log"),
  data = rightfilt
)

m2 <- glmmTMB(
  Righting_time ~ Treatment + Week +
    (1 | Tank),
  family = Gamma(link = "log"),
  data = rightfilt
)

AIC(m1, m2)
anova(m1,m2)
summary(m1)
car::Anova(m1)
emmeans(m1, ~ Treatment | Week)
pairs(emmeans(m1, ~ Treatment | Week), adjust = "Tukey")
