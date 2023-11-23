# Required packages
my.packs <- c(
  
  'tidyverse','reshape2','cowplot','dplyr','ggrepel','readxl',
  'knitr', 'kableExtra','cowplot',
  'pscl','jagsUI','ggstance',"viridis")

# if any of them are not installed, install them
if (any(!my.packs %in% installed.packages()[, 'Package'])) {install.packages(my.packs[which(!my.packs %in% installed.packages()[, 'Package'])],dependencies = TRUE)}
lapply(my.packs, require, character.only = TRUE)

setwd("~/1_Work/ECCC/BBS_avichorus/analysis/script")
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
load(file = "./summary_data/avi2.RData")

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

dat_merge <- full_join(friis_bbs, l1_aggregated, all = TRUE) %>%
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

# ************************************************************
# ************************************************************
# CAUSES OF DISCREPANCIES BETWEEN LISTENERS
# ************************************************************
# ************************************************************

avi2$index <- 1:nrow(avi2) # Row identifier

# First listener data
L1 <- subset(avi2, detect_1 == 1 & spec_1 != "Unidentified")

# Second listener data
L2 <- subset(avi2, detect_2 == 1 & spec_2 != "Unidentified" & !is.na(detect_2) & !is.na(spec_2))
new_detections <- subset(L2, detect_1 == 0 | spec_1 == "Unidentified")

#nrow(confirmed) + nrow(false_positives) + nrow(id_challenges) == nrow(L1) #This accounts for all birds

#-------------------------------------
# Calculate summary totals from a review of Listener 1's data
#-------------------------------------
false_positives <- subset(L1, is.na(detect_2) | detect_2 == 0 | is.na(spec_2) | spec_2 == "Unidentified")
id_challenges <- subset(L1, !is.na(detect_2) & detect_2 == 1 & spec_2 != "Unidentified" & spec_2 != spec_1)
confirmed <- subset(L2, detect_1 == 1 & spec_2 == spec_1)
L1_true_positives <- subset(L1, index %in% false_positives == FALSE)

species.totals.L1 <- aggregate(detect_1~spec_1, data = L1, FUN = sum) %>% rename(Count = detect_1) %>% add_column(Type = "Total")
species.totals.confirmed <- aggregate(detect_1~spec_1, data = confirmed, FUN = sum) %>% rename(Count = detect_1) %>% add_column(Type = "Confirmed by L2")
species.totals.false_positives <- aggregate(detect_1~spec_1, data = false_positives, FUN = sum) %>% rename(Count = detect_1) %>% add_column(Type = "Detection challenged by L2")
species.totals.id_challenges <- aggregate(detect_1~spec_1, data = id_challenges, FUN = sum) %>% rename(Count = detect_1) %>% add_column(Type = "Species ID challenged by L2")

species.totals <- rbind(species.totals.confirmed, species.totals.false_positives,species.totals.id_challenges) %>% rename(CName = spec_1)
species.totals$Type = factor(species.totals$Type, levels = c("Detection challenged by L2", "Species ID challenged by L2","Confirmed by L2"))

total.counts <- species.totals %>% group_by(CName) %>% summarize(total = sum(Count)) %>% arrange(desc(total))
species.totals$CName = factor(species.totals$CName, levels = rev(total.counts$CName))

review.plot.1 <- ggplot(species.totals, aes(fill = Type)) +
  geom_bar(aes(x = CName, y = Count), stat = "identity")+
  scale_fill_manual(values=c("#d95f0e","gray35","#99d8c9"))+
  xlab("Species")+
  ylab("Number of birds")+
  coord_flip()+
  theme_bw()+
  ylim(c(0,1000))+
  theme(legend.position="top")
review.plot.1

jpeg("./analysis_output/figures/Fig_S1.jpg",width = 10, height = 12, units = "in", res = 1000)
print(review.plot.1)
dev.off()

# Proportion confirmed
tmp1 <- species.totals.confirmed[,c(1,2)] %>% rename(Species = 1, Confirmed = 2)
tmp2 <- species.totals.false_positives[,c(1,2)] %>% rename(Species = 1, False_Detect = 2)
tmp3 <- species.totals.id_challenges[,c(1,2)] %>% rename(Species = 1, ID_Challenge = 2)

joined_records <- full_join(tmp1,tmp2) %>% full_join(tmp3)
joined_records[is.na(joined_records)] <- 0
joined_records$Total <- joined_records$Confirmed + joined_records$False_Detect + joined_records$ID_Challenge
joined_records$Not_Confirmed <- joined_records$Total - joined_records$Confirmed 

joined_records$prop_confirmed <-  joined_records$Confirmed / joined_records$Total        

for (i in 1:nrow(joined_records)){
  m1 <- glm(cbind(joined_records$Confirmed[i],joined_records$Not_Confirmed[i])~1,family = binomial(link="logit"))

  ci <- confint(m1)
  joined_records$p_mean[i] <- plogis(m1$coef)[1]
  joined_records$p_lcl[i] <- plogis(ci)[1]
  joined_records$p_ucl[i] <- plogis(ci)[2]
  
  
}    

joined_records <- joined_records %>% arrange(Total)
joined_records$Species <- factor(joined_records$Species, levels = joined_records$Species)

tmp.plot <- ggplot(joined_records)+
  geom_point(aes(x = Species, y = p_mean), position = position_dodge(width = 0.5))+
  geom_errorbar(aes(x = Species, ymin = p_lcl, ymax = p_ucl), position = position_dodge(width = 0.5), width = 0)+
  ylab(expression(italic(p)))+
  xlab("Species")+
  coord_flip(ylim = c(0,1))+
  theme_bw()+
  theme(legend.position="top")
tmp.plot

#----------------------------------
# Numbers added by Reviewer 2
#----------------------------------
confirmed <- subset(L2, detect_1 == 1 & spec_2 == spec_1)
id_challenges <- subset(L1, !is.na(detect_2) & detect_2 == 1 & spec_2 != "Unidentified" & spec_2 != spec_1)
false_negatives <- subset(L2, is.na(detect_1) | detect_1 == 0 | is.na(spec_1) | spec_1 == "Unidentified") %>%
  subset(!is.na(spec_2))

L1_true_positives <- subset(L1, index %in% false_positives == FALSE)

species.totals.L2 <- aggregate(detect_2~spec_2, data = L2, FUN = sum) %>% rename(Count = detect_2) %>% add_column(Type = "Total")

species.totals.confirmed <- aggregate(detect_2~spec_2, data = confirmed, FUN = sum) %>% rename(Count = detect_2) %>% add_column(Type = "Confirmed by L2")

species.totals.false_negatives <- aggregate(detect_2~spec_2, data = false_negatives, FUN = sum) %>% rename(Count = detect_2) %>% add_column(Type = "New detections by L2")

# Species as classified by second reviewer
species.totals.id_challenges2 <- aggregate(detect_1~spec_2, data = id_challenges, FUN = sum) %>% rename(Count = detect_1) %>% add_column(Type = "New species ID assigned by L2")
species.totals2 <- rbind(species.totals.confirmed, species.totals.false_negatives,species.totals.id_challenges2) %>% rename(CName = spec_2)

new_species <- avi2$spec_2[which(!(avi2$spec_2 %in% avi2$spec_1))]
species.totals2$CName = factor(species.totals2$CName, levels = c(new_species,rev(total.counts$CName)))
species.totals2$Type = factor(species.totals2$Type, levels = c("New detections by L2", "New species ID assigned by L2","Confirmed by L2"))

review.plot.2 <- ggplot(species.totals2, aes(fill = Type)) +
  geom_bar(aes(x = CName, y = Count), stat = "identity")+
  scale_fill_manual(values=c("#8ab7de","gray35","#99d8c9"))+
  xlab("Species")+
  ylab("Number of birds")+
  coord_flip()+
  theme_bw()+
  ylim(c(0,1000))+
  theme(legend.position="top")#axis.text.y = element_text(size = 7))

jpeg("./analysis_output/figures/Fig_S2.jpg",width = 10, height = 12, units = "in", res = 1000)
print(review.plot.2)
dev.off()

