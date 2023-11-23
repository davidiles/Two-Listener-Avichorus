# Required packages
my.packs <- c(
  
  'tidyverse','reshape2','cowplot','dplyr','ggrepel','readxl',
  'knitr', 'kableExtra','cowplot',
  'pscl','jagsUI','ggstance',"viridis")

# if any of them are not installed, install them
if (any(!my.packs %in% installed.packages()[, 'Package'])) {install.packages(my.packs[which(!my.packs %in% installed.packages()[, 'Package'])],dependencies = TRUE)}
lapply(my.packs, require, character.only = TRUE)

setwd("C:\\R work\\analysis_LD_avichorus\\script")
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

range(stops_avi$Number.Stops)

#-------------------------------------------------------
# Assign first and second listeners to each stop
#-------------------------------------------------------
avi2$UserID_1 <- factor(avi2$UserID_1, 
                        levels = c("15813", "17904", "1232"),
                        labels = c("A","B","C")) 
avi2$UserID_2 <- factor(avi2$UserID_2, 
                        levels = c("15813", "17904", "1232"),
                        labels = c("A","B","C")) 

avi2$Listener_Combination <- paste0(avi2$UserID_1,avi2$UserID_2) %>% as.factor()

#-------------------------------------------------------
# Aggregate species counts by first reviewer at each stop
#-------------------------------------------------------

l1_aggregated <- subset(avi2, detect_1 == 1 & spec_1 != "Unidentified") %>% 
  aggregate(TagID ~ spec_1 + RouteStopYear + Listener_Combination, data = ., FUN = length)
colnames(l1_aggregated) <- c("CName","RouteStopYear","Listener_Combination","Count.L1")
summary(l1_aggregated)

#-------------------------------------------------------
# Aggregate species counts by second reviewer at each stop
#-------------------------------------------------------

l2_aggregated <- subset(avi2, detect_2 == 1 & spec_2 != "Unidentified" & !is.na(detect_2) & !is.na(spec_2)) %>% 
  aggregate(TagID ~ spec_2 + RouteStopYear + Listener_Combination, data = ., FUN = length)
colnames(l2_aggregated) <- c("CName","RouteStopYear","Listener_Combination","Count.L2")
summary(l2_aggregated)

#---------------------------------------------------------
# Merge 2-reviewer Avichorus data
#---------------------------------------------------------

dat_merge <- full_join(l1_aggregated,l2_aggregated)

allspecies_allroutes <- expand.grid(CName = unique(dat_merge$CName),
                                    Listener_Combination = unique(dat_merge$Listener_Combination),
                                    RouteStopYear = unique(dat_merge$RouteStopYear))

dat_merge = full_join(dat_merge, allspecies_allroutes) %>%
  dplyr::select(RouteStopYear,CName,Listener_Combination,Count.L1,Count.L2)

dat_merge[is.na(dat_merge)] <- 0

#Remove Unidentified
dat_merge <- subset(dat_merge, CName != "Unidentified")

#Remove species that were never detected in any dataset
dat_merge$present.L1 <- dat_merge$Count.L1 > 0
dat_merge$present.L2 <- dat_merge$Count.L2 > 0

dat_merge$present.any <- (dat_merge$present.L1 + dat_merge$present.L2) > 0

species_counts = aggregate(present.any~CName, dat_merge, FUN = sum)
species_absent = subset(species_counts, present.any == 0)
dat_merge = subset(dat_merge, !(CName %in% species_absent$CName))

table(dat_merge$CName) #All species now have counts at all 650 stops

dat_merge_full <- dat_merge

# Remove species that were not observed in any dataset
dat_merge <- subset(dat_merge, present.any == TRUE)

summary(dat_merge)

#--------------------------------------------------
# Species in each dataset
#--------------------------------------------------
dat_L1 <- subset(dat_merge, present.L1 == 1)
dat_L2 <- subset(dat_merge, present.L2 == 1)

#--------------------------------------------------
# Additions / subtractions
#--------------------------------------------------
dat_merge$species_added <- !(dat_merge$present.L1) & dat_merge$present.L2
dat_merge$species_removed <- dat_merge$present.L1 & !(dat_merge$present.L2)

#--------------------------------------------------
# Stop-level summaries
#--------------------------------------------------

stop_summary <- dat_merge %>%
  group_by(RouteStopYear,Listener_Combination) %>%
  summarize(Richness.L1 = sum(present.L1),
            Richness.L2 = sum(present.L2),
            
            Count.L1 = sum(Count.L1),
            Count.L2 = sum(Count.L2),
            
            Additions = sum(species_added),
            Subtractions = sum(species_removed))

# Stop-level differences
stop_summary$Richness.diff <- stop_summary$Richness.L2 - stop_summary$Richness.L1
stop_summary$Count.diff <- stop_summary$Count.L2 - stop_summary$Count.L1

head(stop_summary)
summary(stop_summary)

x <- subset(stop_summary, Listener_Combination == "CA")

#--------------------------------------------------
# Listener summaries
#--------------------------------------------------
pairwise_listener_summary <- stop_summary %>% 
  group_by(Listener_Combination) %>%
  summarize(n = length(Listener_Combination),
            mean.Richness.L1 = mean(Richness.L1),
            se.Richness.L1 = sd(Richness.L1)/sqrt(length(Listener_Combination)),
            
            mean.Species.Additions = mean(Additions),
            se.Species.Additions = sd(Additions)/sqrt(length(Listener_Combination)),
            
            mean.Species.Subtractions = mean(Subtractions),
            se.Species.Subtractions = sd(Subtractions)/sqrt(length(Listener_Combination)),
            
            mean.diff.Richness = mean(Richness.L2 - Richness.L1),
            se.diff.Richness = sd(Richness.L2 - Richness.L1)/sqrt(length(Listener_Combination)),
            
            mean.Count.L1 = mean(Count.L1),
            se.Count.L1 = sd(Count.L1)/sqrt(length(Listener_Combination)),
            
            mean.diff.Count = mean(Count.L2 - Count.L1),
            se.diff.Count = sd(Count.L2 - Count.L1)/sqrt(length(Listener_Combination))
  )

#write.csv(pairwise_listener_summary, "./analysis_output/pairwise_listener_summary.csv",row.names = FALSE)
#
#--------------------------------------------------
# Differences
#--------------------------------------------------

p1 <- ggplot(stop_summary, aes(Listener_Combination, Richness.diff)) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_violin(alpha = 0.3, 
              fill = "dodgerblue",
              adjust = 0.5)  + 
  scale_y_continuous(breaks = seq(-6,6,2), limits = c(-6,6))+
  theme_bw() +
  xlab("Listener Order") + ylab("Difference in Richness\n(L2 - L1)")
p1
#jpeg("./analysis_output/figures/Fig_6A_Listener_Combinations_Violin.jpg",width = 8, height = 4, units = "in", res = 1000)
#print(p1)
#dev.off()

p2 <- ggplot(stop_summary, aes(Listener_Combination, Count.diff)) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_violin(alpha = 0.3, fill = "dodgerblue", adjust = 0.5)  + 
  theme_bw() +
  xlab("Listener Order") + ylab("Difference in # Birds\n(L2 - L1)")+
  scale_y_continuous(breaks = seq(-8,8,2), limits = c(-7,7))
p2
#jpeg("./analysis_output/figures/Fig_6B_Listener_Combinations_Violin.jpg",width = 8, height = 4, units = "in", res = 1000)
#print(p2)
#dev.off()

#-------------------------------------------------
# CAUSES OF DISCREPANCIES BETWEEN LISTENERS
#-------------------------------------------------

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

species.totals.L1 <- aggregate(detect_1~spec_1 + Listener_Combination, data = L1, FUN = sum) %>% rename(Count = detect_1) %>% add_column(Type = "Total")
species.totals.confirmed <- aggregate(detect_1~spec_1 + Listener_Combination, data = confirmed, FUN = sum) %>% rename(Count = detect_1) %>% add_column(Type = "Confirmed by L2")
species.totals.false_positives <- aggregate(detect_1~spec_1 + Listener_Combination, data = false_positives, FUN = sum) %>% rename(Count = detect_1) %>% add_column(Type = "Detection challenged by L2")
species.totals.id_challenges <- aggregate(detect_1~spec_1 + Listener_Combination, data = id_challenges, FUN = sum) %>% rename(Count = detect_1) %>% add_column(Type = "Species ID challenged by L2")

species.totals <- rbind(species.totals.confirmed, species.totals.false_positives,species.totals.id_challenges) %>% rename(CName = spec_1)
species.totals$Type = factor(species.totals$Type, levels = c("Detection challenged by L2", "Species ID challenged by L2","Confirmed by L2"))

total.counts <- species.totals %>% group_by(CName) %>% summarize(total = sum(Count)) %>% arrange(desc(total))
species.totals$CName = factor(species.totals$CName, levels = rev(total.counts$CName))

review.plot.1A <- ggplot(subset(species.totals, CName %in% head(total.counts,50)$CName), aes(fill = Type)) +
  geom_bar(aes(x = CName, y = Count), stat = "identity")+
  scale_fill_manual(values=c("#d95f0e","gray35","#99d8c9"))+
  xlab("Species")+
  ylab("Number of birds")+
  coord_flip()+
  theme_bw()+
  theme(legend.position="top") +
  facet_grid(.~Listener_Combination, scales = "free")
review.plot.1A

#jpeg("./analysis_output/figures/Fig_X_Listener_Combinations.jpg",width = 12, height = 4, units = "in", res = 1000)
#print(review.plot.1A)
#dev.off()

# Not broken down by listener combination
review.plot.1B <- ggplot(species.totals, aes(fill = Type)) +
  geom_bar(aes(x = CName, y = Count), stat = "identity")+
  scale_fill_manual(values=c("#d95f0e","gray35","#99d8c9"))+
  xlab("Species")+
  ylab("Number of birds")+
  coord_flip()+
  theme_bw()+
  theme(legend.position="top") 

# Add detection challenges and ID challenges to dat_merge summary
dat_merge$detect_challenge <- 0
dat_merge$id_challenge <- 0
for (i in 1:nrow(false_positives)){
  
  dat_merge_row <- which(dat_merge$CName == false_positives$spec_1[i] & dat_merge$RouteStopYear == false_positives$RouteStopYear[i])
  dat_merge$detect_challenge[dat_merge_row] <- 1
}

for (i in 1:nrow(id_challenges)){
  
  dat_merge_row <- which(dat_merge$CName == id_challenges$spec_1[i] & dat_merge$RouteStopYear == id_challenges$RouteStopYear[i])
  dat_merge$id_challenge[dat_merge_row] <- 1
}

# Which outcome is most common for each species
SH1 <- species.totals %>% group_by(CName) %>%
  summarize(Confirm = sum(Count[Type == "Confirmed by L2"]),
            Challenge.Detection = sum(Count[Type == "Detection challenged by L2"]),
            Challenge.ID = sum(Count[Type == "Species ID challenged by L2"]),
            Total = sum(Count)) %>%
  arrange(desc(Total))

# 50 most common species
SH1.50 <- subset(SH1, CName %in% SH1$CName[1:50])
mean(SH1.50$Confirm > (SH1.50$Challenge.Detection + SH1.50$Challenge.ID)) # Second listener confirms more birds than they challenge in 100% of cases

#----------------------------------
# Numbers added by Reviewer 2
#----------------------------------

confirmed <- subset(L2, detect_1 == 1 & spec_2 == spec_1)
id_challenges <- subset(L1, !is.na(detect_2) & detect_2 == 1 & spec_2 != "Unidentified" & spec_2 != spec_1)
false_negatives <- subset(L2, is.na(detect_1) | detect_1 == 0 | is.na(spec_1) | spec_1 == "Unidentified")

L1_true_positives <- subset(L1, index %in% false_positives == FALSE)

species.totals.L2 <- aggregate(detect_2~spec_2 + Listener_Combination, data = L2, FUN = sum) %>% rename(Count = detect_2) %>% add_column(Type = "Total")

species.totals.confirmed <- aggregate(detect_2~spec_2 + Listener_Combination, data = confirmed, FUN = sum) %>% rename(Count = detect_2) %>% add_column(Type = "Confirmed by L2")

species.totals.false_negatives <- aggregate(detect_2~spec_2 + Listener_Combination, data = false_negatives, FUN = sum) %>% rename(Count = detect_2) %>% add_column(Type = "New detections by L2")

# Species as classified by second reviewer
species.totals.id_challenges2 <- aggregate(detect_1~spec_2  + Listener_Combination, data = id_challenges, FUN = sum) %>% rename(Count = detect_1) %>% add_column(Type = "New species ID assigned by L2")
species.totals2 <- rbind(species.totals.confirmed, species.totals.false_negatives,species.totals.id_challenges2) %>% rename(CName = spec_2)

species.totals2$CName = factor(species.totals2$CName, levels = rev(total.counts$CName))
species.totals2$Type = factor(species.totals2$Type, levels = c("New detections by L2", "New species ID assigned by L2","Confirmed by L2"))

review.plot.2A <- ggplot(subset(species.totals2, CName %in% head(total.counts,20)$CName), aes(fill = Type)) +
  geom_bar(aes(x = CName, y = Count), stat = "identity")+
  scale_fill_manual(values=c("#8ab7de","gray35","#99d8c9"))+
  xlab("Species")+
  ylab("Number of birds")+
  coord_flip()+
  theme_bw()+
  facet_grid(.~Listener_Combination, scales = "free")+
  theme(legend.position="top")

# Not broken down by listener combination
review.plot.2B <- ggplot(species.totals2, aes(fill = Type)) +
  geom_bar(aes(x = CName, y = Count), stat = "identity")+
  scale_fill_manual(values=c("#8ab7de","gray35","#99d8c9"))+
  xlab("Species")+
  ylab("Number of birds")+
  coord_flip()+
  theme_bw()+
  theme(legend.position="top")
review.plot.2B

# Add new detections and new IDs to dat_merge summary
dat_merge$new_detect <- 0
dat_merge$new_id <- 0
for (i in 1:nrow(false_negatives)){
  
  dat_merge_row <- which(dat_merge$CName == false_negatives$spec_2[i] & dat_merge$RouteStopYear == false_negatives$RouteStopYear[i])
  dat_merge$new_detect[dat_merge_row] <- 1
}

for (i in 1:nrow(id_challenges)){
  
  dat_merge_row <- which(dat_merge$CName == id_challenges$spec_2[i] & dat_merge$RouteStopYear == id_challenges$RouteStopYear[i])
  dat_merge$new_id[dat_merge_row] <- 1
}

# Which outcome is most common for each species
SH2 <- species.totals2 %>% group_by(CName) %>%
  summarize(Confirm = sum(Count[Type == "Confirmed by L2"]),
            New.Detect = sum(Count[Type == "New detections by L2"]),
            New.ID = sum(Count[Type == "New species ID assigned by L2"]),
            Total = sum(Count)) %>%
  arrange(desc(Total))

# 50 most common species
SH2.50 <- subset(SH2, CName %in% SH2$CName[1:50])

sum(SH2.50$Confirm > (SH2.50$New.Detect + SH2.50$New.ID))
sum(SH2.50$New.Detect > SH2.50$New.ID)


# --------------------------------------------
# Summary statistics - challenges / confirmations / etc
# --------------------------------------------

nrow(L1) # Total birds detected on first listen
mean(L1$detect_2 == 0 | is.na(L1$detect_2)) # Proportion considered absent by L2
mean(L1$detect_2 == 1 & L1$spec_2 != L1$spec_1 & !(is.na(L1$spec_2))) # Proportion challenged on ID


subset(L1, is.na(detect_2))

nrow(L2) # Total birds detected on second listen
(nrow(L2) - nrow(L1))/nrow(L1)


# On second listen
sum(subset(dat_merge, present.L2)$Count.L2)

# How many stops did second listener alter the species list
changes <- dat_merge %>% group_by(RouteStopYear, CName) %>%
  summarize(count.change = abs(Count.L2 - Count.L1) > 0,
            species.change = abs(present.L2 - present.L1) > 0) %>%
  group_by(RouteStopYear) %>%
  summarize(count.change = sum(count.change) > 0,
            species.change = sum(species.change) > 0)

mean(changes$count.change)
mean(changes$species.change)

# 754 cases where a new species was added by second listener
sum(dat_merge$species_added) 

# 455 cases where a species was removed by second listener
sum(dat_merge$species_removed) 

# 176 cases where a new species was added as a result of ID change
sum(dat_merge$new_id == 1 & dat_merge$species_added) 

sum(dat_merge$new_detect == 1 & dat_merge$species_added) 
sum(dat_merge$new_detect == 1 & dat_merge$new_id == 1 & dat_merge$species_added)

sum(dat_merge$id_challenge == 1 & dat_merge$species_removed)
sum(dat_merge$id_challenge == 0 & dat_merge$species_removed)

#---------------------
# Supplementary plots (all species)
#---------------------

review.plot.1.full <- ggplot(species.totals, aes(fill = Type)) +
  geom_bar(aes(x = CName, y = Count), stat = "identity")+
  scale_fill_manual(values=c("#d95f0e","gray35","#99d8c9"))+
  xlab("Species")+
  ylab("Number of birds")+
  coord_flip()+
  theme_bw()+
  ylim(c(0,250))+
  facet_grid(.~Listener_Combination)+
  theme(legend.position="top")

#jpeg("./analysis_output/figures/Fig_S5_Listener_Combinations.jpg",width = 10, height = 20, units = "in", res = 1000)
#print(review.plot.1.full)
#dev.off()

review.plot.2.full <- ggplot(species.totals2, aes(fill = Type)) +
  geom_bar(aes(x = CName, y = Count), stat = "identity")+
  scale_fill_manual(values=c("#8ab7de","gray35","#99d8c9"))+
  xlab("Species")+
  ylab("Number of birds")+
  coord_flip()+
  theme_bw()+
  ylim(c(0,250))+
  facet_grid(.~Listener_Combination)+
  theme(legend.position="top")#axis.text.y = element_text(size = 7))
review.plot.2.full
#jpeg("./analysis_output/figures/Fig_S6_Listener_Combinations.jpg",width = 10, height = 20, units = "in", res = 1000)
#print(review.plot.2.full)
#dev.off()

# #---------------------
# # Rarefaction comparison (1 vs 2 listener)
# #---------------------
# library(vegan)
# library(iNEXT)
# 
# species.vec <- unique(dat_merge$CName) %>% sort()
# site.vec <- unique(dat_merge$RouteStopYear)
# LC.vec <- c()
# 
# L1.matrix <- L2.matrix <- matrix(0,nrow = length(site.vec), ncol = length(species.vec))
# 
# for (i in 1:length(site.vec)){
#   
#   site.dat <- subset(dat_merge, RouteStopYear == site.vec[i])
#   LC.vec[i] <- as.character(site.dat$Listener_Combination)[1]
#   
#   for (j in 1:nrow(site.dat)){
#     
#     if (site.dat$present.L1[j]) L1.matrix[i,which(species.vec == site.dat$CName[j])] <- site.dat$Count.L1[j]
#     if (site.dat$present.L2[j]) L2.matrix[i,which(species.vec == site.dat$CName[j])] <- site.dat$Count.L2[j]
#     
#   }
#   
# }
# 
# colnames(L1.matrix) <- colnames(L2.matrix) <- species.vec
# rownames(L1.matrix) <- rownames(L2.matrix) <- site.vec
# 
# L1.list <- list(AB = t(L1.matrix[which(LC.vec == "AB"),]) > 0,
#                 AC = t(L1.matrix[which(LC.vec == "AC"),])> 0,
#                 BA = t(L1.matrix[which(LC.vec == "BA"),])> 0,
#                 BC = t(L1.matrix[which(LC.vec == "BC"),])> 0,
#                 CA = t(L1.matrix[which(LC.vec == "CA"),])> 0,
#                 CB = t(L1.matrix[which(LC.vec == "CB"),])> 0
# )
# 
# 
# out.L1 <- iNEXT(L1.list, q = c(0,1),datatype="incidence_raw", endpoint=200)
# 
# L2.list <- list(AB = t(L2.matrix[which(LC.vec == "AB"),]) > 0,
#                 AC = t(L2.matrix[which(LC.vec == "AC"),])> 0,
#                 BA = t(L2.matrix[which(LC.vec == "BA"),])> 0,
#                 BC = t(L2.matrix[which(LC.vec == "BC"),])> 0,
#                 CA = t(L2.matrix[which(LC.vec == "CA"),])> 0,
#                 CB = t(L2.matrix[which(LC.vec == "CB"),])> 0
# )
# 
# 
# out.L2 <- iNEXT(L2.list, q = c(0,1), datatype="incidence_raw", endpoint=200)
# 
# L.results <- rbind(out.L1$iNextEst$AB %>% add_column(LC = "AB", nL = 1),
#                    out.L1$iNextEst$AC %>% add_column(LC = "AC", nL = 1),
#                    out.L1$iNextEst$BA %>% add_column(LC = "BA", nL = 1),
#                    out.L1$iNextEst$BC %>% add_column(LC = "BC", nL = 1),
#                    out.L1$iNextEst$CA %>% add_column(LC = "CA", nL = 1),
#                    out.L1$iNextEst$CB %>% add_column(LC = "CB", nL = 1),
#                    
#                    out.L2$iNextEst$AB %>% add_column(LC = "AB", nL = 2),
#                    out.L2$iNextEst$AC %>% add_column(LC = "AC", nL = 2),
#                    out.L2$iNextEst$BA %>% add_column(LC = "BA", nL = 2),
#                    out.L2$iNextEst$BC %>% add_column(LC = "BC", nL = 2),
#                    out.L2$iNextEst$CA %>% add_column(LC = "CA", nL = 2),
#                    out.L2$iNextEst$CB %>% add_column(LC = "CB", nL = 2)
# )
# 
# plot1 <- ggplot(subset(L.results, order == 0),aes(x = t, y = qD, ymin = qD.LCL, ymax = qD.UCL, col = factor(nL), fill = factor(nL))) +
#   geom_ribbon(col = "transparent", alpha = 0.2)+
#   geom_line(data = subset(L.results, method == "interpolated" & order == 0),size = 1)+
#   geom_line(data = subset(L.results, method == "extrapolated" & order == 0), linetype = 3, size = 1)+
#   
#   theme_bw()+
#   facet_grid(.~LC)+
#   ylab("Number of Species")+
#   xlab("Number of BBS stops")+
#   scale_color_manual(values = c("orangered","dodgerblue"), name = "# listeners")+
#   scale_fill_manual(values = c("orangered","dodgerblue"), name = "# listeners")+
#   geom_hline(yintercept = 63)
#   
# 
# plot1
# 
# jpeg("./analysis_output/figures/Rarefaction.jpg",width = 10, height = 3, units = "in", res = 1000)
# print(plot1)
# dev.off()
# 
# sum(colSums(L1.matrix[which(LC.vec == "AC"),]) > 0)
# sum(colSums(L2.matrix[which(LC.vec == "AC"),]) > 0)

