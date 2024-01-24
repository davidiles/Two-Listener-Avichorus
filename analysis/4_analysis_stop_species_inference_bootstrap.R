# Required packages
my.packs <- c(
  
  'tidyverse','reshape2','cowplot','dplyr','ggrepel','readxl',
  'knitr', 'kableExtra','cowplot',
  'pscl','jagsUI','ggstance',"viridis")

# if any of them are not installed, install them
if (any(!my.packs %in% installed.packages()[, 'Package'])) {install.packages(my.packs[which(!my.packs %in% installed.packages()[, 'Package'])],dependencies = TRUE)}
lapply(my.packs, require, character.only = TRUE)

setwd("C:/Users/IlesD/OneDrive - EC-EC/Iles/Projects/Landbirds/Two-Listener-Avichorus/analysis")
rm(list=ls())

#*********************************************************
# Analysis 1: comparison of 2-listener data
#*********************************************************

#---------------------------------------------------------
# Load BBS Data
#---------------------------------------------------------
#Load relevant data (prepared in script 1_format_data.R)
load(file = "./summary_data/Part1.RData")
friis_bbs <- Part1$friis_bbs
colnames(friis_bbs)[5] <- "Count.Field"

#---------------------------------------------------------
# Load "2 reviewer data" from Avichorus (called "avi2" in this file)
#---------------------------------------------------------
load(file = "./summary_data/Part2.RData")

#---------------------------------------------------
# Ensure both datasets use same RouteYears
#---------------------------------------------------
avi2 <- subset(avi2, RouteStopYear %in% friis_bbs$RouteStopYear)
friis_bbs <- subset(friis_bbs, RouteStopYear %in% avi2$RouteStopYear)

#-------------------------------------------------------
# Check stops included in both datasets
#-------------------------------------------------------

# Remove cases in which both listeners could not identify a bird
u = which( (avi2$spec_1 == "Unidentified" | is.na(avi2$spec_1)) & (avi2$spec_2 == "Unidentified" | is.na(avi2$spec_2)))
avi2 <- avi2[-u,]

stops_avi <- aggregate(BBSStop ~ BBSRoute + RecordingYear, data = avi2, FUN = function(x) length(unique(x)))
colnames(stops_avi) <- c("BBSRoute","Year","Number.Stops")

stops_bbs <- aggregate(BBSStop ~ BBSRoute + Year, data = friis_bbs, FUN = function(x) length(unique(x)))
colnames(stops_bbs) <- c("BBSRoute","Year","Number.Stops")

sum(stops_avi$Number.Stops)
sum(stops_bbs$Number.Stops)

#-------------------------------------------------------
# Aggregate species counts by first reviewer at each stop
#-------------------------------------------------------

l1_aggregated <- subset(avi2, detect_1 == 1 & spec_1 != "Unidentified") %>% 
  aggregate(TagID ~ spec_1 + RouteStopYear, data = ., FUN = length)
colnames(l1_aggregated) <- c("CName","RouteStopYear","Count.L1")
summary(l1_aggregated)

#-------------------------------------------------------
# Aggregate species counts by second reviewer at each stop
#-------------------------------------------------------

l2_aggregated <- subset(avi2, detect_2 == 1 & spec_2 != "Unidentified" & !is.na(detect_2) & !is.na(spec_2)) %>% 
  aggregate(TagID ~ spec_2 + RouteStopYear, data = ., FUN = length)
colnames(l2_aggregated) <- c("CName","RouteStopYear","Count.L2")
summary(l2_aggregated)

#---------------------------------------------------------
# Merge 2-reviewer Avichorus data with friis data
#---------------------------------------------------------

dat_merge <- full_join(friis_bbs, l1_aggregated) %>%
  full_join(l2_aggregated)

allspecies_allroutes <- expand.grid(CName = unique(dat_merge$CName), RouteStopYear = unique(dat_merge$RouteStopYear))
dat_merge = full_join(dat_merge, allspecies_allroutes) %>%
  dplyr::select(RouteStopYear,CName,Count.Field,Count.L1,Count.L2)

dat_merge[is.na(dat_merge)] <- 0

#Remove Unidentified
dat_merge <- subset(dat_merge, CName != "Unidentified")

#Remove species that were never detected in any dataset
dat_merge$present.Field <- dat_merge$Count.Field > 0
dat_merge$present.L1 <- dat_merge$Count.L1 > 0
dat_merge$present.L2 <- dat_merge$Count.L2 > 0

dat_merge$present.any <- (dat_merge$present.Field + dat_merge$present.L1 + dat_merge$present.L2) > 0

species_counts = aggregate(present.any~CName, dat_merge, FUN = sum)
species_absent = subset(species_counts, present.any == 0)
dat_merge = subset(dat_merge, !(CName %in% species_absent$CName))

table(dat_merge$CName) #All species now have counts at all 690 stops

dat_merge_full <- dat_merge

# Remove species that were not observed in any dataset
dat_merge <- subset(dat_merge, present.any == TRUE)

#--------------------------------------------------
# Species in each dataset
#--------------------------------------------------
dat_Field <- subset(dat_merge, present.Field == 1) %>% add_column(method = "Field")
dat_L1 <- subset(dat_merge, present.L1 == 1)
dat_L2 <- subset(dat_merge, present.L2 == 1)

#--------------------------------------------------
# Stop-level summaries
#--------------------------------------------------
stop_summary <- dat_merge %>%
  group_by(RouteStopYear) %>%
  summarize(Richness.Field = sum(present.Field),
            Richness.L1 = sum(present.L1),
            Richness.L2 = sum(present.L2),
            
            Count.Field = sum(Count.Field),
            Count.L1 = sum(Count.L1),
            Count.L2 = sum(Count.L2))

stop_summary$Richness.Field_minus_Richness.L1 <- stop_summary$Richness.Field - stop_summary$Richness.L1
stop_summary$Richness.Field_minus_Richness.L2 <- stop_summary$Richness.Field - stop_summary$Richness.L2
stop_summary$Richness.L2_minus_Richness.L1 <- stop_summary$Richness.L2 - stop_summary$Richness.L1
stop_summary$Count.Field_minus_Count.L1 <- stop_summary$Count.Field - stop_summary$Count.L1
stop_summary$Count.Field_minus_Count.L2 <- stop_summary$Count.Field - stop_summary$Count.L2
stop_summary$Count.L2_minus_Count.L1 <- stop_summary$Count.L2 - stop_summary$Count.L1

#write.csv(stop_summary,"output/Stop_Summary_690stops.csv",row.names=FALSE)

stop_summary_table <- rbind(data.frame(n.stops = nrow(stop_summary),
                                       Method = "Field",
                                       mean.Richness = mean(stop_summary$Richness.Field),
                                       sd.Richness = sd(stop_summary$Richness.Field),
                                       se.Richness = sd(stop_summary$Richness.Field)/sqrt(nrow(stop_summary)),
                                       min.Richness = min(stop_summary$Richness.Field),
                                       max.Richness = max(stop_summary$Richness.Field),
                                       mean.Count = mean(stop_summary$Count.Field),
                                       sd.Count = sd(stop_summary$Count.Field),
                                       se.Count = sd(stop_summary$Count.Field)/sqrt(nrow(stop_summary)),
                                       min.Count = min(stop_summary$Count.Field),
                                       max.Count = max(stop_summary$Count.Field)),
                            
                            data.frame(n.stops = nrow(stop_summary),
                                       Method = "Single Listener",
                                       mean.Richness = mean(stop_summary$Richness.L1),
                                       sd.Richness = sd(stop_summary$Richness.L1),
                                       se.Richness = sd(stop_summary$Richness.L1)/sqrt(nrow(stop_summary)),
                                       min.Richness = min(stop_summary$Richness.L1),
                                       max.Richness = max(stop_summary$Richness.L1),
                                       mean.Count = mean(stop_summary$Count.L1),
                                       sd.Count = sd(stop_summary$Count.L1),
                                       se.Count = sd(stop_summary$Count.L1)/sqrt(nrow(stop_summary)),
                                       min.Count = min(stop_summary$Count.L1),
                                       max.Count = max(stop_summary$Count.L1)),
                            
                            data.frame(n.stops = nrow(stop_summary),
                                       Method = "Double Listener",
                                       mean.Richness = mean(stop_summary$Richness.L2),
                                       sd.Richness = sd(stop_summary$Richness.L2),
                                       se.Richness = sd(stop_summary$Richness.L2)/sqrt(nrow(stop_summary)),
                                       min.Richness = min(stop_summary$Richness.L2),
                                       max.Richness = max(stop_summary$Richness.L2),
                                       mean.Count = mean(stop_summary$Count.L2),
                                       sd.Count = sd(stop_summary$Count.L2),
                                       se.Count = sd(stop_summary$Count.L2)/sqrt(nrow(stop_summary)),
                                       min.Count = min(stop_summary$Count.L2),
                                       max.Count = max(stop_summary$Count.L2))
)

#write.csv(stop_summary_table,"output/Stop_Summary_Table_690stops.csv",row.names=FALSE)

# Differences between survey methods
stop_summary_difference_table <- rbind(data.frame(n.stops = nrow(stop_summary),
                                                  Comparison = "Field - Single Listener",
                                                  mean.diff.Richness = mean(stop_summary$Richness.Field_minus_Richness.L1),
                                                  sd.diff.Richness = sd(stop_summary$Richness.Field_minus_Richness.L1),
                                                  se.diff.Richness = sd(stop_summary$Richness.Field_minus_Richness.L1)/sqrt(nrow(stop_summary)),
                                                  min.diff.Richness = min(stop_summary$Richness.Field_minus_Richness.L1),
                                                  max.diff.Richness = max(stop_summary$Richness.Field_minus_Richness.L1),
                                                  
                                                  mean.diff.Count = mean(stop_summary$Count.Field_minus_Count.L1),
                                                  sd.diff.Count = sd(stop_summary$Count.Field_minus_Count.L1),
                                                  se.diff.Count = sd(stop_summary$Count.Field_minus_Count.L1)/sqrt(nrow(stop_summary)),
                                                  min.diff.Count = min(stop_summary$Count.Field_minus_Count.L1),
                                                  max.diff.Count = max(stop_summary$Count.Field_minus_Count.L1)),
                                       
                                       data.frame(n.stops = nrow(stop_summary),
                                                  Comparison = "Field - Double Listener",
                                                  mean.diff.Richness = mean(stop_summary$Richness.Field_minus_Richness.L2),
                                                  sd.diff.Richness = sd(stop_summary$Richness.Field_minus_Richness.L2),
                                                  se.diff.Richness = sd(stop_summary$Richness.Field_minus_Richness.L2)/sqrt(nrow(stop_summary)),
                                                  min.diff.Richness = min(stop_summary$Richness.Field_minus_Richness.L2),
                                                  max.diff.Richness = max(stop_summary$Richness.Field_minus_Richness.L2),
                                                  
                                                  mean.diff.Count = mean(stop_summary$Count.Field_minus_Count.L2),
                                                  sd.diff.Count = sd(stop_summary$Count.Field_minus_Count.L2),
                                                  se.diff.Count = sd(stop_summary$Count.Field_minus_Count.L2)/sqrt(nrow(stop_summary)),
                                                  min.diff.Count = min(stop_summary$Count.Field_minus_Count.L2),
                                                  max.diff.Count = max(stop_summary$Count.Field_minus_Count.L2)),
                                       
                                       data.frame(n.stops = nrow(stop_summary),
                                                  Comparison = "Double Listener - Single Listener",
                                                  mean.diff.Richness = mean(stop_summary$Richness.L2_minus_Richness.L1),
                                                  sd.diff.Richness = sd(stop_summary$Richness.L2_minus_Richness.L1),
                                                  se.diff.Richness = sd(stop_summary$Richness.L2_minus_Richness.L1)/sqrt(nrow(stop_summary)),
                                                  min.diff.Richness = min(stop_summary$Richness.L2_minus_Richness.L1),
                                                  max.diff.Richness = max(stop_summary$Richness.L2_minus_Richness.L1),
                                                  
                                                  mean.diff.Count = mean(stop_summary$Count.L2_minus_Count.L1),
                                                  sd.diff.Count = sd(stop_summary$Count.L2_minus_Count.L1),
                                                  se.diff.Count = sd(stop_summary$Count.L2_minus_Count.L1)/sqrt(nrow(stop_summary)),
                                                  min.diff.Count = min(stop_summary$Count.L2_minus_Count.L1),
                                                  max.diff.Count = max(stop_summary$Count.L2_minus_Count.L1)))

#write.csv(stop_summary_difference_table,"output/Stop_Summary_DIFFERENCE_Table_690stops.csv",row.names=FALSE)

# ------------------------------------------------------------------
# SPECIES-LEVEL ESTIMATES OF OCCURRENCE / MEAN COUNTS
# ------------------------------------------------------------------

## Boostrapping to evaluate uncertainty
# nboot <- 5000
# unique_stops <- unique(dat_merge_full$RouteStopYear)
# 
# species_estimates_bootstrapped <- data.frame()
# for (i in 1:nboot){
#   
#   # Create resampled dataset
#   boot_routes <- sample(unique_stops, length(unique_stops), replace = TRUE)
#   boot_dat <- data.frame()
#   
#   for (j in 1:length(boot_routes)){
#     # Store row indices of original dataset
#     boot_dat <- data.frame(row.indices = which(dat_merge_full$RouteStopYear == boot_routes[j]),
#                            boot_route = j) %>%
#       rbind(boot_dat,.)
#   }
#   boot_dat <- dat_merge_full[boot_dat$row.indices,] %>%
#     add_column(boot_route = boot_dat$boot_route)
#   
#   # Calculate species-level estimates for this bootstrap sample
#   # (Mean counts, probability of presence)
#   boot_est <- boot_dat %>%
#     
#     # Estimates of difference between methods  
#     mutate(
#       
#       # Difference in counts
#       diff.Count.Field.L1 = Count.Field - Count.L1,
#       diff.Count.Field.L2 = Count.Field - Count.L2,
#       diff.Count.L2.L1 = Count.L2 - Count.L1,
#       
#       # Difference in probability of presence
#       diff.present.Field.L1 = present.Field - present.L1,
#       diff.present.Field.L2 = present.Field - present.L2,
#       diff.present.L2.L1 = present.L2 - present.L1) %>%
#     group_by(CName) %>%
#     summarize_all(mean) %>%
#     select(-one_of(c("RouteStopYear","boot_stop"))) %>%
#     add_column(boot = i)
#   
#   # Append to total results dataframe
#   species_estimates_bootstrapped <- species_estimates_bootstrapped %>%
#     rbind(boot_est)
#   print(i)
# }
# 
# write.csv(species_estimates_bootstrapped,"output/species_estimates_bootstrapped.csv",row.names=FALSE)

species_estimates_bootstrapped <- read.csv("output/species_estimates_bootstrapped.csv")

# # Visualize correlations in bootstrap estimates
# occ_boot_plot <- ggplot(subset(species_estimates_bootstrapped, boot <= 1000), aes(x = present.L1, y = present.L2)) +
#   geom_jitter(alpha = 0.1)+
#   geom_abline(intercept = 0, slope = 1)+
#   facet_wrap(CName~., scales = "free")
# #pdf("output/figures/bootstrap_occurrence.pdf",width = 50, height = 50)
# #print(occ_boot_plot)
# #dev.off()
# 
# count_boot_plot <- ggplot(subset(species_estimates_bootstrapped, boot <= 1000), aes(x = Count.L1, y = Count.L2)) +
#   geom_jitter(alpha = 0.1)+
#   geom_abline(intercept = 0, slope = 1)+
#   facet_wrap(CName~., scales = "free")
# #pdf("output/figures/bootstrap_count.pdf",width = 50, height = 50)
# #print(count_boot_plot)
# #dev.off()

# Functions
q025.fn <- function(x) quantile(x,0.025)
q975.fn <- function(x) quantile(x,0.975)

species_estimates <- species_estimates_bootstrapped %>%
  group_by(CName) %>%
  summarise_all(list(mean = mean,
                     se = sd,
                     q025 = q025.fn,
                     q975 = q975.fn))

# ------------------------------------------------------------------
# FIGURES/RESULTS
# ------------------------------------------------------------------

#-----------
# Figure 3 
#-----------

# Stop-level richness
pear.corr.a <- cor(stop_summary$Richness.L1,stop_summary$Richness.L2, method = "pearson") %>% round(3)
spear.corr.a <- cor(stop_summary$Richness.L1,stop_summary$Richness.L2, method = "spearman") %>% round(3)
lims.a <- c(0,max(stop_summary[,c("Richness.L1","Richness.L2")]))
Fig3A <- ggplot(stop_summary)+
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = jitter(Richness.L1, amount = 0.2), y = jitter(Richness.L2, amount = 0.2)), alpha = 0.3, size = 1)+
  coord_cartesian(xlim = lims.a,
                  ylim = lims.a)+
  xlab("# species detected at stop\n(first listener)")+
  ylab("# species detected at stop\n(second listener)")+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.a)*0.01, y = max(lims.a)*0.95), label = paste0("Pearson Corr = ",pear.corr.a,"\nSpearman Corr = ",spear.corr.a), hjust = 0, size = 3)

# Stop-level abundance
pear.corr.b <- cor(stop_summary$Count.L1,stop_summary$Count.L2, method = "pearson") %>% round(3)
spear.corr.b  <- cor(stop_summary$Count.L1,stop_summary$Count.L2, method = "spearman") %>% round(3)
lims.b  <- c(0,max(stop_summary[,c("Count.L1","Count.L2")]))
Fig3B <- ggplot(stop_summary)+
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = jitter(Count.L1, amount = 0.1), y = jitter(Count.L2, amount = 0.1)), alpha = 0.3, size = 1)+
  
  coord_cartesian(xlim = lims.b ,
                  ylim = lims.b )+
  xlab("# birds detected at stop\n(first listener)")+
  ylab("# birds detected at stop\n(second listener)")+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.b )*0.01, y = max(lims.b )*0.95), label = paste0("Pearson Corr = ",pear.corr.b ,"\nSpearman Corr = ",spear.corr.b ), hjust = 0, size = 3)

# Species occurrence probabilities
pear.corr.c <- cor(species_estimates$present.L1_mean,species_estimates$present.L2_mean, method = "pearson") %>% round(3)
spear.corr.c <- cor(species_estimates$present.L1_mean,species_estimates$present.L2_mean, method = "spearman") %>% round(3)
lims.c <- c(0,1)
Fig3C <- ggplot(species_estimates) +
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = present.L1_mean, y = present.L2_mean))+
  geom_errorbar(aes(x = present.L1_mean, ymin = present.L2_q025, ymax = present.L2_q975), width = 0)+
  geom_errorbarh(aes(xmin = present.L1_q025, xmax = present.L1_q975, y = present.L2_mean), height = 0)+
  coord_cartesian(xlim = lims.c, ylim = lims.c)+
  xlab(paste0("Species occurrence probability\n(first listener)"))+
  ylab(paste0("Species occurrence probability\n(second listener)"))+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.c)*0.01, y = max(lims.c)*0.95), label = paste0("Pearson Corr = ",pear.corr.c,"\nSpearman Corr = ",spear.corr.c), hjust = 0, size = 3)

# Species mean abundance per stop
pear.corr.d <- cor(species_estimates$Count.L1_mean,species_estimates$Count.L2_mean, method = "pearson") %>% round(3)
spear.corr.d <- cor(species_estimates$Count.L1_mean,species_estimates$Count.L2_mean, method = "spearman") %>% round(3)
lims.d <- c(0,max(species_estimates[,c("Count.L1_q975","Count.L2_q975")]))

Fig3D <- ggplot(species_estimates) +
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = Count.L1_mean, y = Count.L2_mean))+
  geom_errorbar(aes(x = Count.L1_mean, ymin = Count.L2_q025, ymax = Count.L2_q975), width = 0)+
  geom_errorbarh(aes(xmin = Count.L1_q025, xmax = Count.L1_q975, y = Count.L2_mean), height = 0)+
  coord_cartesian(xlim = lims.d, ylim = lims.d)+
  xlab(paste0("Species mean count\n(first listener)"))+
  ylab(paste0("Species mean count\n(second listener)"))+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.d)*0.01, y = max(lims.d)*0.95), label = paste0("Pearson Corr = ",pear.corr.d,"\nSpearman Corr = ",spear.corr.d), hjust = 0, size = 3)
print(Fig3D)

Fig3 <- plot_grid(Fig3A,Fig3B,Fig3C,Fig3D,nrow = 2, align = "hv",
                  labels = c("A","B","C","D"))

png("output/figures/Fig_3.png",width = 7, height = 7, units = "in", res = 600)
print(Fig3)
dev.off()

#-----------
# Figure 4 - left column
#-----------

# Stop-level richness
pear.corr.a <- cor(stop_summary$Richness.Field,stop_summary$Richness.L1, method = "pearson") %>% round(3)
spear.corr.a <- cor(stop_summary$Richness.Field,stop_summary$Richness.L1, method = "spearman") %>% round(3)
lims.a <- c(0,max(stop_summary[,c("Richness.Field","Richness.L1","Richness.L2")]))
Fig4A <- ggplot(stop_summary)+
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = jitter(Richness.Field, amount = 0.2), y = jitter(Richness.L1, amount = 0.2)), alpha = 0.3, size = 1)+
  coord_cartesian(xlim = lims.a,
                  ylim = lims.a)+
  xlab("# species detected at stop\n(field)")+
  ylab("# species detected at stop\n(first listener)")+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.a)*0.01, y = max(lims.a)*0.95), label = paste0("Pearson Corr = ",pear.corr.a,"\nSpearman Corr = ",spear.corr.a), hjust = 0, size = 3)

# Stop-level abundance
pear.corr.b <- cor(stop_summary$Count.Field,stop_summary$Count.L1, method = "pearson") %>% round(3)
spear.corr.b  <- cor(stop_summary$Count.Field,stop_summary$Count.L1, method = "spearman") %>% round(3)
lims.b  <- c(0,max(stop_summary[,c("Count.Field","Count.L1","Count.L2")]))
Fig4C <- ggplot(stop_summary)+
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = jitter(Count.Field, amount = 0.1), y = jitter(Count.L1, amount = 0.1)), alpha = 0.3, size = 1)+
  
  coord_cartesian(xlim = lims.b ,
                  ylim = lims.b )+
  xlab("# birds detected at stop\n(field)")+
  ylab("# birds detected at stop\n(first listener)")+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.b )*0.01, y = max(lims.b )*0.95), label = paste0("Pearson Corr = ",pear.corr.b ,"\nSpearman Corr = ",spear.corr.b ), hjust = 0, size = 3)

# Species occurrence probabilities
pear.corr.c <- cor(species_estimates$present.Field_mean,species_estimates$present.L1_mean, method = "pearson") %>% round(3)
spear.corr.c <- cor(species_estimates$present.Field_mean,species_estimates$present.L1_mean, method = "spearman") %>% round(3)
lims.c <- c(0,1)
Fig4E <- ggplot(species_estimates) +
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = present.Field_mean, y = present.L1_mean))+
  geom_errorbar(aes(x = present.Field_mean, ymin = present.L1_q025, ymax = present.L1_q975), width = 0)+
  geom_errorbarh(aes(xmin = present.Field_q025, xmax = present.Field_q975, y = present.L1_mean), height = 0)+
  coord_cartesian(xlim = lims.c, ylim = lims.c)+
  xlab(paste0("Species occurrence probability\n(field)"))+
  ylab(paste0("Species occurrence probability\n(first listener)"))+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.c)*0.01, y = max(lims.c)*0.95), label = paste0("Pearson Corr = ",pear.corr.c,"\nSpearman Corr = ",spear.corr.c), hjust = 0, size = 3)

# Species mean abundance per stop
pear.corr.d <- cor(species_estimates$Count.Field_mean,species_estimates$Count.L1_mean, method = "pearson") %>% round(3)
spear.corr.d <- cor(species_estimates$Count.Field_mean,species_estimates$Count.L1_mean, method = "spearman") %>% round(3)
lims.d <- c(0,max(species_estimates[,c("Count.Field_q975","Count.L1_q975")]))

Fig4G <- ggplot(species_estimates) +
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = Count.Field_mean, y = Count.L1_mean))+
  geom_errorbar(aes(x = Count.Field_mean, ymin = Count.L1_q025, ymax = Count.L1_q975), width = 0)+
  geom_errorbarh(aes(xmin = Count.Field_q025, xmax = Count.Field_q975, y = Count.L1_mean), height = 0)+
  coord_cartesian(xlim = lims.d, ylim = lims.d)+
  xlab(paste0("Species mean count\n(field)"))+
  ylab(paste0("Species mean count\n(first listener)"))+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.d)*0.01, y = max(lims.d)*0.95), label = paste0("Pearson Corr = ",pear.corr.d,"\nSpearman Corr = ",spear.corr.d), hjust = 0, size = 3)

Fig4_Left <- plot_grid(Fig4A,Fig4C,Fig4E,Fig4G,nrow = 4, align = "hv",
                       labels = c("A","C","E","G"))

#-----------
# Figure 4 - right column
#-----------

# Stop-level richness
pear.corr.a <- cor(stop_summary$Richness.Field,stop_summary$Richness.L2, method = "pearson") %>% round(3)
spear.corr.a <- cor(stop_summary$Richness.Field,stop_summary$Richness.L2, method = "spearman") %>% round(3)
lims.a <- c(0,max(stop_summary[,c("Richness.Field","Richness.L2","Richness.L2")]))
Fig4B <- ggplot(stop_summary)+
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = jitter(Richness.Field, amount = 0.2), y = jitter(Richness.L2, amount = 0.2)), alpha = 0.3, size = 1)+
  coord_cartesian(xlim = lims.a,
                  ylim = lims.a)+
  xlab("# species detected at stop\n(field)")+
  ylab("# species detected at stop\n(second listener)")+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.a)*0.01, y = max(lims.a)*0.95), label = paste0("Pearson Corr = ",pear.corr.a,"\nSpearman Corr = ",spear.corr.a), hjust = 0, size = 3)

# Stop-level abundance
pear.corr.b <- cor(stop_summary$Count.Field,stop_summary$Count.L2, method = "pearson") %>% round(3)
spear.corr.b  <- cor(stop_summary$Count.Field,stop_summary$Count.L2, method = "spearman") %>% round(3)
lims.b  <- c(0,max(stop_summary[,c("Count.Field","Count.L2","Count.L2")]))
Fig4D <- ggplot(stop_summary)+
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = jitter(Count.Field, amount = 0.1), y = jitter(Count.L2, amount = 0.1)), alpha = 0.3, size = 1)+
  
  coord_cartesian(xlim = lims.b ,
                  ylim = lims.b )+
  xlab("# birds detected at stop\n(field)")+
  ylab("# birds detected at stop\n(second listener)")+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.b )*0.01, y = max(lims.b )*0.95), label = paste0("Pearson Corr = ",pear.corr.b ,"\nSpearman Corr = ",spear.corr.b ), hjust = 0, size = 3)

# Species occurrence probabilities
pear.corr.c <- cor(species_estimates$present.Field_mean,species_estimates$present.L2_mean, method = "pearson") %>% round(3)
spear.corr.c <- cor(species_estimates$present.Field_mean,species_estimates$present.L2_mean, method = "spearman") %>% round(3)
lims.c <- c(0,1)
Fig4F <- ggplot(species_estimates) +
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = present.Field_mean, y = present.L2_mean))+
  geom_errorbar(aes(x = present.Field_mean, ymin = present.L2_q025, ymax = present.L2_q975), width = 0)+
  geom_errorbarh(aes(xmin = present.Field_q025, xmax = present.Field_q975, y = present.L2_mean), height = 0)+
  coord_cartesian(xlim = lims.c, ylim = lims.c)+
  xlab(paste0("Species occurrence probability\n(field)"))+
  ylab(paste0("Species occurrence probability\n(second listener)"))+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.c)*0.01, y = max(lims.c)*0.95), label = paste0("Pearson Corr = ",pear.corr.c,"\nSpearman Corr = ",spear.corr.c), hjust = 0, size = 3)

# Species mean abundance per stop
pear.corr.d <- cor(species_estimates$Count.Field_mean,species_estimates$Count.L2_mean, method = "pearson") %>% round(3)
spear.corr.d <- cor(species_estimates$Count.Field_mean,species_estimates$Count.L2_mean, method = "spearman") %>% round(3)
lims.d <- c(0,max(species_estimates[,c("Count.Field_q975","Count.L2_q975")]))

Fig4H <- ggplot(species_estimates) +
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = Count.Field_mean, y = Count.L2_mean))+
  geom_errorbar(aes(x = Count.Field_mean, ymin = Count.L2_q025, ymax = Count.L2_q975), width = 0)+
  geom_errorbarh(aes(xmin = Count.Field_q025, xmax = Count.Field_q975, y = Count.L2_mean), height = 0)+
  coord_cartesian(xlim = lims.d, ylim = lims.d)+
  xlab(paste0("Species mean count\n(field)"))+
  ylab(paste0("Species mean count\n(second listener)"))+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.d)*0.01, y = max(lims.d)*0.95), label = paste0("Pearson Corr = ",pear.corr.d,"\nSpearman Corr = ",spear.corr.d), hjust = 0, size = 3)

Fig4_Right <- plot_grid(Fig4B,Fig4D,Fig4F,Fig4H,nrow = 4, align = "hv",
                        labels = c("B","D","F","H"))

Fig4 <- plot_grid(Fig4_Left,Fig4_Right)

png("output/figures/Fig_4_tmp.png",width = 7, height = 3.5*4, units = "in", res = 600)
print(Fig4)
dev.off()

#------------------------
# Add column titles
#------------------------
library(png)
img<-readPNG("output/figures/Fig_4_tmp.png")

#get size
h<-dim(img)[1]
w<-dim(img)[2]

#open new file for output
png("output/figures/Fig_4.png", width=w, height=h)
par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,20,0), ann=F)
plot.new()
plot.window(0:1, 0:1)

#fill plot with image
usr<-par("usr")
rasterImage(img, usr[1], usr[3], usr[2], usr[4])

#add text
text(0.06,1.05, "Field vs First Listener", adj = c(0,0.5), cex = 12)
text(0.6,1.05, "Field vs Second Listener", adj = c(0,0.5), cex = 12)

#close image
dev.off()

#--------------------------------------------------
# Table 1 in paper:
# Stop-level differences in counts and species abundance
# Species added/removed per stop
#--------------------------------------------------

L1_vs_L2_comparison <- dat_merge %>%
  
  # Calculate per stop
  group_by(RouteStopYear) %>%
  summarize(Richness.method1 = sum(present.L1),
            species.added.method2 = sum(present.L2 & !present.L1),
            species.removed.method2 = sum(present.L1 & !present.L2),
            change.Richness.method2 = sum(present.L2) - sum(present.L1),
            
            Count.method1 = sum(Count.L1),
            change.Count.method2 = sum(Count.L2) - sum(Count.L1)
  ) %>%
  
  # Means, SEs, ranges
  summarize(mean.Richness.method1 = mean(Richness.method1),
            se.Richness.method1 = sd(Richness.method1)/sqrt(length(Richness.method1)),
            min.Richness.method1 = min(Richness.method1),
            max.Richness.method1 = max(Richness.method1),
            
            mean.species.added.method2 = mean(species.added.method2),
            se.species.added.method2 = sd(species.added.method2)/sqrt(length(species.added.method2)),
            min.species.added.method2 = min(species.added.method2),
            max.species.added.method2 = max(species.added.method2),
            
            mean.species.removed.method2 = mean(species.removed.method2),
            se.species.removed.method2 = sd(species.removed.method2)/sqrt(length(species.removed.method2)),
            min.species.removed.method2 = min(species.removed.method2),
            max.species.removed.method2 = max(species.removed.method2),
            
            mean.change.Richness.method2 = mean(change.Richness.method2),
            se.change.Richness.method2 = sd(change.Richness.method2)/sqrt(length(change.Richness.method2)),
            min.change.Richness.method2 = min(change.Richness.method2),
            max.change.Richness.method2 = max(change.Richness.method2),
            
            mean.Count.method1 = mean(Count.method1),
            se.Count.method1 = sd(Count.method1)/sqrt(length(Count.method1)),
            min.Count.method1 = min(Count.method1),
            max.Count.method1 = max(Count.method1),
            
            mean.change.Count.method2 = mean(change.Count.method2),
            se.change.Count.method2 = sd(change.Count.method2)/sqrt(length(change.Count.method2)),
            min.change.Count.method2 = min(change.Count.method2),
            max.change.Count.method2 = max(change.Count.method2),
            
            prop.stops.Richness.change = sum(species.added.method2 != 0 | species.removed.method2 != 0)/length(change.Richness.method2),
            prop.stops.Count.change = sum(change.Count.method2 != 0)/length(change.Count.method2),
            n.stops = length(change.Richness.method2)
            
  ) 

L1_vs_Field_comparison <- dat_merge %>%
  
  # Calculate per stop
  group_by(RouteStopYear) %>%
  summarize(Richness.method1 = sum(present.L1),
            species.added.method2 = sum(present.Field & !present.L1),
            species.removed.method2 = sum(present.L1 & !present.Field),
            change.Richness.method2 = sum(present.Field) - sum(present.L1),
            
            Count.method1 = sum(Count.L1),
            change.Count.method2 = sum(Count.Field) - sum(Count.L1)
  ) %>%
  
  # Means, SEs, ranges
  summarize(mean.Richness.method1 = mean(Richness.method1),
            se.Richness.method1 = sd(Richness.method1)/sqrt(length(Richness.method1)),
            min.Richness.method1 = min(Richness.method1),
            max.Richness.method1 = max(Richness.method1),
            
            mean.species.added.method2 = mean(species.added.method2),
            se.species.added.method2 = sd(species.added.method2)/sqrt(length(species.added.method2)),
            min.species.added.method2 = min(species.added.method2),
            max.species.added.method2 = max(species.added.method2),
            
            mean.species.removed.method2 = mean(species.removed.method2),
            se.species.removed.method2 = sd(species.removed.method2)/sqrt(length(species.removed.method2)),
            min.species.removed.method2 = min(species.removed.method2),
            max.species.removed.method2 = max(species.removed.method2),
            
            mean.change.Richness.method2 = mean(change.Richness.method2),
            se.change.Richness.method2 = sd(change.Richness.method2)/sqrt(length(change.Richness.method2)),
            min.change.Richness.method2 = min(change.Richness.method2),
            max.change.Richness.method2 = max(change.Richness.method2),
            
            mean.Count.method1 = mean(Count.method1),
            se.Count.method1 = sd(Count.method1)/sqrt(length(Count.method1)),
            min.Count.method1 = min(Count.method1),
            max.Count.method1 = max(Count.method1),
            
            mean.change.Count.method2 = mean(change.Count.method2),
            se.change.Count.method2 = sd(change.Count.method2)/sqrt(length(change.Count.method2)),
            min.change.Count.method2 = min(change.Count.method2),
            max.change.Count.method2 = max(change.Count.method2),
            
            prop.stops.Richness.change = sum(species.added.method2 != 0 | species.removed.method2 != 0)/length(change.Richness.method2),
            prop.stops.Count.change = sum(change.Count.method2 != 0)/length(change.Count.method2),
            n.stops = length(change.Richness.method2)
            
  ) 

L2_vs_Field_comparison <- dat_merge %>%
  
  # Calculate per stop
  group_by(RouteStopYear) %>%
  summarize(Richness.method1 = sum(present.L2),
            species.added.method2 = sum(present.Field & !present.L2),
            species.removed.method2 = sum(present.L2 & !present.Field),
            change.Richness.method2 = sum(present.Field) - sum(present.L2),
            
            Count.method1 = sum(Count.L2),
            change.Count.method2 = sum(Count.Field) - sum(Count.L2)
  ) %>%
  
  # Means, SEs, ranges
  summarize(mean.Richness.method1 = mean(Richness.method1),
            se.Richness.method1 = sd(Richness.method1)/sqrt(length(Richness.method1)),
            min.Richness.method1 = min(Richness.method1),
            max.Richness.method1 = max(Richness.method1),
            
            mean.species.added.method2 = mean(species.added.method2),
            se.species.added.method2 = sd(species.added.method2)/sqrt(length(species.added.method2)),
            min.species.added.method2 = min(species.added.method2),
            max.species.added.method2 = max(species.added.method2),
            
            mean.species.removed.method2 = mean(species.removed.method2),
            se.species.removed.method2 = sd(species.removed.method2)/sqrt(length(species.removed.method2)),
            min.species.removed.method2 = min(species.removed.method2),
            max.species.removed.method2 = max(species.removed.method2),
            
            mean.change.Richness.method2 = mean(change.Richness.method2),
            se.change.Richness.method2 = sd(change.Richness.method2)/sqrt(length(change.Richness.method2)),
            min.change.Richness.method2 = min(change.Richness.method2),
            max.change.Richness.method2 = max(change.Richness.method2),
            
            mean.Count.method1 = mean(Count.method1),
            se.Count.method1 = sd(Count.method1)/sqrt(length(Count.method1)),
            min.Count.method1 = min(Count.method1),
            max.Count.method1 = max(Count.method1),
            
            mean.change.Count.method2 = mean(change.Count.method2),
            se.change.Count.method2 = sd(change.Count.method2)/sqrt(length(change.Count.method2)),
            min.change.Count.method2 = min(change.Count.method2),
            max.change.Count.method2 = max(change.Count.method2),
            
            prop.stops.Richness.change = sum(species.added.method2 != 0 | species.removed.method2 != 0)/length(change.Richness.method2),
            prop.stops.Count.change = sum(change.Count.method2 != 0)/length(change.Count.method2),
            n.stops = length(change.Richness.method2)
            
  ) 

Stop_level_comparison_table <- rbind(L1_vs_L2_comparison,
                                     L1_vs_Field_comparison,
                                     L2_vs_Field_comparison)%>%
  add_column(comparison = c("L1 vs L2","L1 vs Field","L2 vs Field"), .before = 1)

write.csv(Stop_level_comparison_table,"output/Stop_level_comparison_table.csv", row.names = FALSE)

#--------------------------------------------------
# SECONDARY ANALYSIS:
# REMOVE FLOCKING SPECIES AND EVALUATE EFFECT ON FIELD VS LISTENER DATA
#--------------------------------------------------

dat_merge_NOFLOCK <- subset(dat_merge, !(CName %in% c("White-winged Crossbill","Pine Siskin","Canada Goose")))

L1_vs_L2_comparison <- dat_merge_NOFLOCK %>%
  
  # Calculate per stop
  group_by(RouteStopYear) %>%
  summarize(Richness.method1 = sum(present.L1),
            species.added.method2 = sum(present.L2 & !present.L1),
            species.removed.method2 = sum(present.L1 & !present.L2),
            change.Richness.method2 = sum(present.L2) - sum(present.L1),
            
            Count.method1 = sum(Count.L1),
            change.Count.method2 = sum(Count.L2) - sum(Count.L1)
  ) %>%
  
  # Means, SEs, ranges
  summarize(mean.Richness.method1 = mean(Richness.method1),
            se.Richness.method1 = sd(Richness.method1)/sqrt(length(Richness.method1)),
            min.Richness.method1 = min(Richness.method1),
            max.Richness.method1 = max(Richness.method1),
            
            mean.species.added.method2 = mean(species.added.method2),
            se.species.added.method2 = sd(species.added.method2)/sqrt(length(species.added.method2)),
            min.species.added.method2 = min(species.added.method2),
            max.species.added.method2 = max(species.added.method2),
            
            mean.species.removed.method2 = mean(species.removed.method2),
            se.species.removed.method2 = sd(species.removed.method2)/sqrt(length(species.removed.method2)),
            min.species.removed.method2 = min(species.removed.method2),
            max.species.removed.method2 = max(species.removed.method2),
            
            mean.change.Richness.method2 = mean(change.Richness.method2),
            se.change.Richness.method2 = sd(change.Richness.method2)/sqrt(length(change.Richness.method2)),
            min.change.Richness.method2 = min(change.Richness.method2),
            max.change.Richness.method2 = max(change.Richness.method2),
            
            mean.Count.method1 = mean(Count.method1),
            se.Count.method1 = sd(Count.method1)/sqrt(length(Count.method1)),
            min.Count.method1 = min(Count.method1),
            max.Count.method1 = max(Count.method1),
            
            mean.change.Count.method2 = mean(change.Count.method2),
            se.change.Count.method2 = sd(change.Count.method2)/sqrt(length(change.Count.method2)),
            min.change.Count.method2 = min(change.Count.method2),
            max.change.Count.method2 = max(change.Count.method2),
            
            prop.stops.Richness.change = sum(species.added.method2 != 0 | species.removed.method2 != 0)/length(change.Richness.method2),
            prop.stops.Count.change = sum(change.Count.method2 != 0)/length(change.Count.method2),
            n.stops = length(change.Richness.method2)
            
  ) 

L1_vs_Field_comparison <- dat_merge_NOFLOCK %>%
  
  # Calculate per stop
  group_by(RouteStopYear) %>%
  summarize(Richness.method1 = sum(present.L1),
            species.added.method2 = sum(present.Field & !present.L1),
            species.removed.method2 = sum(present.L1 & !present.Field),
            change.Richness.method2 = sum(present.Field) - sum(present.L1),
            
            Count.method1 = sum(Count.L1),
            change.Count.method2 = sum(Count.Field) - sum(Count.L1)
  ) %>%
  
  # Means, SEs, ranges
  summarize(mean.Richness.method1 = mean(Richness.method1),
            se.Richness.method1 = sd(Richness.method1)/sqrt(length(Richness.method1)),
            min.Richness.method1 = min(Richness.method1),
            max.Richness.method1 = max(Richness.method1),
            
            mean.species.added.method2 = mean(species.added.method2),
            se.species.added.method2 = sd(species.added.method2)/sqrt(length(species.added.method2)),
            min.species.added.method2 = min(species.added.method2),
            max.species.added.method2 = max(species.added.method2),
            
            mean.species.removed.method2 = mean(species.removed.method2),
            se.species.removed.method2 = sd(species.removed.method2)/sqrt(length(species.removed.method2)),
            min.species.removed.method2 = min(species.removed.method2),
            max.species.removed.method2 = max(species.removed.method2),
            
            mean.change.Richness.method2 = mean(change.Richness.method2),
            se.change.Richness.method2 = sd(change.Richness.method2)/sqrt(length(change.Richness.method2)),
            min.change.Richness.method2 = min(change.Richness.method2),
            max.change.Richness.method2 = max(change.Richness.method2),
            
            mean.Count.method1 = mean(Count.method1),
            se.Count.method1 = sd(Count.method1)/sqrt(length(Count.method1)),
            min.Count.method1 = min(Count.method1),
            max.Count.method1 = max(Count.method1),
            
            mean.change.Count.method2 = mean(change.Count.method2),
            se.change.Count.method2 = sd(change.Count.method2)/sqrt(length(change.Count.method2)),
            min.change.Count.method2 = min(change.Count.method2),
            max.change.Count.method2 = max(change.Count.method2),
            
            prop.stops.Richness.change = sum(species.added.method2 != 0 | species.removed.method2 != 0)/length(change.Richness.method2),
            prop.stops.Count.change = sum(change.Count.method2 != 0)/length(change.Count.method2),
            n.stops = length(change.Richness.method2)
            
  ) 

L2_vs_Field_comparison <- dat_merge_NOFLOCK %>%
  
  # Calculate per stop
  group_by(RouteStopYear) %>%
  summarize(Richness.method1 = sum(present.L2),
            species.added.method2 = sum(present.Field & !present.L2),
            species.removed.method2 = sum(present.L2 & !present.Field),
            change.Richness.method2 = sum(present.Field) - sum(present.L2),
            
            Count.method1 = sum(Count.L2),
            change.Count.method2 = sum(Count.Field) - sum(Count.L2)
  ) %>%
  
  # Means, SEs, ranges
  summarize(mean.Richness.method1 = mean(Richness.method1),
            se.Richness.method1 = sd(Richness.method1)/sqrt(length(Richness.method1)),
            min.Richness.method1 = min(Richness.method1),
            max.Richness.method1 = max(Richness.method1),
            
            mean.species.added.method2 = mean(species.added.method2),
            se.species.added.method2 = sd(species.added.method2)/sqrt(length(species.added.method2)),
            min.species.added.method2 = min(species.added.method2),
            max.species.added.method2 = max(species.added.method2),
            
            mean.species.removed.method2 = mean(species.removed.method2),
            se.species.removed.method2 = sd(species.removed.method2)/sqrt(length(species.removed.method2)),
            min.species.removed.method2 = min(species.removed.method2),
            max.species.removed.method2 = max(species.removed.method2),
            
            mean.change.Richness.method2 = mean(change.Richness.method2),
            se.change.Richness.method2 = sd(change.Richness.method2)/sqrt(length(change.Richness.method2)),
            min.change.Richness.method2 = min(change.Richness.method2),
            max.change.Richness.method2 = max(change.Richness.method2),
            
            mean.Count.method1 = mean(Count.method1),
            se.Count.method1 = sd(Count.method1)/sqrt(length(Count.method1)),
            min.Count.method1 = min(Count.method1),
            max.Count.method1 = max(Count.method1),
            
            mean.change.Count.method2 = mean(change.Count.method2),
            se.change.Count.method2 = sd(change.Count.method2)/sqrt(length(change.Count.method2)),
            min.change.Count.method2 = min(change.Count.method2),
            max.change.Count.method2 = max(change.Count.method2),
            
            prop.stops.Richness.change = sum(species.added.method2 != 0 | species.removed.method2 != 0)/length(change.Richness.method2),
            prop.stops.Count.change = sum(change.Count.method2 != 0)/length(change.Count.method2),
            n.stops = length(change.Richness.method2)
            
  ) 

Stop_level_comparison_table_NOFLOCK <- rbind(L1_vs_L2_comparison,
                                             L1_vs_Field_comparison,
                                             L2_vs_Field_comparison) %>%
  add_column(comparison = c("L1 vs L2","L1 vs Field","L2 vs Field"), .before = 1)

write.csv(Stop_level_comparison_table_NOFLOCK,"output/Stop_level_comparison_table_NOFLOCK.csv", row.names = FALSE)

#-----------
# Appendix Figures S5 and S6 - comparison of species occurrences and mean counts
#-----------

Field_estimates <- species_estimates %>%
  select(CName,
         Count.Field_mean, Count.Field_q025,Count.Field_q975,
         present.Field_mean, present.Field_q025,present.Field_q975) %>%
  rename(CName = 1, count_mean_boot = 2, count_lcl = 3, count_ucl = 4, occ_mean_boot = 5, occ_lcl = 6, occ_ucl = 7) %>%
  add_column(Type = "Field Observer")

L1_estimates <- species_estimates %>%
  select(CName,
         Count.L1_mean, Count.L1_q025,Count.L1_q975,
         present.L1_mean, present.L1_q025,present.L1_q975) %>%
  rename(CName = 1, count_mean_boot = 2, count_lcl = 3, count_ucl = 4, occ_mean_boot = 5, occ_lcl = 6, occ_ucl = 7) %>%
  add_column(Type = "First Listener")

L2_estimates <- species_estimates %>%
  select(CName,
         Count.L2_mean, Count.L2_q025,Count.L2_q975,
         present.L2_mean, present.L2_q025,present.L2_q975)%>%
  rename(CName = 1, count_mean_boot = 2, count_lcl = 3, count_ucl = 4, occ_mean_boot = 5, occ_lcl = 6, occ_ucl = 7)  %>%
  add_column(Type = "Second Listener")

estimates_combined <- full_join(Field_estimates,L1_estimates) %>% full_join(L2_estimates)
estimates_combined$Type <- factor(estimates_combined$Type, levels = c("Field Observer","Second Listener","First Listener"))

#------------
# Plot occurrence probability
#------------

# Arrange in order of field_p
Field_estimates <- Field_estimates %>% arrange(occ_mean_boot)
estimates_combined$CName <- factor(estimates_combined$CName, levels = Field_estimates$CName)

# Identify species with "significant differences" for labeling
# Differences between listeners
L_labels <- subset(species_estimates,diff.present.L2.L1_q975 < 0 | diff.present.L2.L1_q025 > 0) 
L_labels <- subset(estimates_combined,CName %in% L_labels$CName & Type == "First Listener")

# Differences between field and L2
F_labels <- subset(species_estimates,diff.present.Field.L2_q975 < 0 | diff.present.Field.L2_q025 > 0) 
F_labels <- subset(estimates_combined,CName %in% F_labels$CName & Type == "Field Observer")

occ.species.plot <- ggplot(estimates_combined)+
  geom_point(aes(x = CName, y = occ_mean_boot, col = Type), position = position_dodge(width = 0.5))+
  geom_errorbar(aes(x = CName, ymin = occ_lcl, ymax = occ_ucl, col = Type), position = position_dodge(width = 0.5), width = 0)+
  
  geom_text(data = L_labels, aes(y = -0.04, x = CName),
            label = "*L*",
            hjust = 0,
            vjust = -0.3,
            size = 2,
            position = position_dodge(width = 0.5))+
  
  geom_text(data = F_labels, aes(y = -0.04, x = CName),
            label = "*F*",
            hjust = 0,
            vjust = 1.2,
            size = 2,
            position = position_dodge(width = 0.5))+
  
  scale_color_manual(values = RColorBrewer::brewer.pal(3,"Dark2")[c(1,3,2)], name = "Survey type")+
  ylab("Occurrence probability per stop")+
  xlab("Species")+
  ggtitle("Estimates of species occurrence")+
  coord_flip(ylim = c(0,1))+
  theme_bw()+
  theme(legend.position="top")

png("output/figures/Fig_S5.png",width = 8, height = 50, units = "in", res = 600)
print(occ.species.plot)
dev.off()

# number of species with differences between listeners
tmp1 <- subset(species_estimates,diff.present.L2.L1_q025 > 0)
tmp2 <- subset(species_estimates,diff.present.L2.L1_q975 < 0)

nrow(tmp1) # Number of species with "significantly" higher occurrence in L2 data than L1
nrow(tmp2) # Number of species with "significantly" higher occurrence in L1 data than L2

# number of species with differences between field and L2
tmp3 <- subset(species_estimates,diff.present.Field.L2_q025 > 0)
tmp4 <- subset(species_estimates,diff.present.Field.L2_q975 < 0)

nrow(tmp3) # Number of species with "significantly" higher occurrence in Field data than L2
nrow(tmp4) # Number of species with "significantly" higher occurrence in L2 data than Field

#------------
# Plot mean counts
#------------

# Arrange in order of field_p
Field_estimates <- Field_estimates %>% arrange(count_mean_boot)
estimates_combined$CName <- factor(estimates_combined$CName, levels = Field_estimates$CName)

# Identify species with "significant differences" for labeling
# Differences between listeners
L_labels <- subset(species_estimates,diff.Count.L2.L1_q975 < 0 | diff.Count.L2.L1_q025 > 0) 
L_labels <- subset(estimates_combined,CName %in% L_labels$CName & Type == "First Listener")

# Differences between field
F_labels <- subset(species_estimates,diff.Count.Field.L2_q975 < 0 | diff.Count.Field.L2_q025 > 0) 
F_labels <- subset(estimates_combined,CName %in% F_labels$CName & Type == "Field Observer")

count.species.plot <- ggplot(estimates_combined)+
  geom_point(aes(x = CName, y = count_mean_boot, col = Type), position = position_dodge(width = 0.5))+
  geom_errorbar(aes(x = CName, ymin = count_lcl, ymax = count_ucl, col = Type), position = position_dodge(width = 0.5), width = 0)+
  
  geom_text(data = L_labels, aes(y = -0.06, x = CName),
            label = "*L*",
            hjust = 0,
            vjust = -0.3,
            size = 2,
            position = position_dodge(width = 0.5))+
  
  geom_text(data = F_labels, aes(y = -0.06, x = CName),
            label = "*F*",
            hjust = 0,
            vjust = 1.2,
            size = 2,
            position = position_dodge(width = 0.5))+
  
  scale_color_manual(values = RColorBrewer::brewer.pal(3,"Dark2")[c(1,3,2)], name = "Survey type")+
  ylab("Mean count per stop")+
  xlab("Species")+
  ggtitle("Estimates of mean counts")+
  coord_flip(ylim = c(0,1.6))+
  theme_bw()+
  theme(legend.position="top")
count.species.plot 

png("output/figures/Fig_S6.png",width = 8, height = 50, units = "in", res = 600)
print(count.species.plot)
dev.off()

# number of species with differences between listeners
tmp1 <- subset(species_estimates,diff.Count.L2.L1_q025 > 0)
tmp2 <- subset(species_estimates,diff.Count.L2.L1_q975 < 0)

nrow(tmp1) # Number of species with "significantly" higher occurrence in L2 data than L1
nrow(tmp2) # Number of species with "significantly" higher occurrence in L1 data than L2

# number of species with differences between field and L2
tmp3 <- subset(species_estimates,diff.Count.Field.L2_q025 > 0)
tmp4 <- subset(species_estimates,diff.Count.Field.L2_q975 < 0)

nrow(tmp3) # Number of species with "significantly" higher occurrence in Field data than L2
nrow(tmp4) # Number of species with "significantly" higher occurrence in L2 data than Field

# Total counts based on L2 data
greater_L2 <- dat_merge %>%
  group_by(CName) %>%
  summarize(sum_L2 = sum(Count.L2),
            sum_L1 = sum(Count.L1),
            greater_L2 = sum(Count.L2) > sum(Count.L1))

mean(greater_L2$greater_L2)

#--------------------------------------------------
# Appendix Table in Paper - which species are missing at each stop, in each dataset
#--------------------------------------------------

head(dat_merge_full)
table(dat_merge_full$CName)
species_occurrence_table <- dat_merge_full %>%
  group_by(CName) %>%
  summarize(all = sum(present.L1 & present.L2 & present.Field),
            any = sum(present.any),
            none = sum(!present.L1 & !present.L2 & !present.Field),
            L1 = sum(present.L1),
            L2 = sum(present.L2),
            Field = sum(present.Field),
            L1_not_L2 = sum(present.L1 & !(present.L2)),
            L2_not_L1 = sum(present.L2 & !(present.L1)),
            Field_not_L = sum(present.Field & (!(present.L1) & !(present.L2))),
            L_not_Field = sum(!(present.Field) & (present.L1 | present.L2)))

write.csv(species_occurrence_table,"output/species_occurrence_table.csv",row.names=FALSE)

