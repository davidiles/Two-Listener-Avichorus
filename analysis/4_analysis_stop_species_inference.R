# Required packages
my.packs <- c(
  
  'tidyverse','reshape2','cowplot','dplyr','ggrepel','readxl',
  'knitr', 'kableExtra','cowplot',
  'pscl','jagsUI','ggstance',"viridis")

# if any of them are not installed, install them
if (any(!my.packs %in% installed.packages()[, 'Package'])) {install.packages(my.packs[which(!my.packs %in% installed.packages()[, 'Package'])],dependencies = TRUE)}
lapply(my.packs, require, character.only = TRUE)

setwd("C:\\R work\\analysis_LD_avichorus\\script")

#*********************************************************
# Analysis 1: comparison of 2-listener data
#*********************************************************

#---------------------------------------------------------
# Load BBS Data
#---------------------------------------------------------
#Load relevant data (prepared in script 1_format_data.R)
load(file = "summary_data/Part1.RData")
friis_bbs <- Part1$friis_bbs
colnames(friis_bbs)[5] <- "Count.Field"

#---------------------------------------------------------
# Load "2 reviewer data" from Avichorus (called "avi2" in this file)
#---------------------------------------------------------
load(file = "summary_data/avi2.RData")

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
#write.csv(dat_merge,"analysis_output/dat_merge.csv")
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

#write.csv(stop_summary,"./analysis_output/Stop_Summary_690stops.csv",row.names=FALSE)

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

#write.csv(stop_summary_table,"./analysis_output/Stop_Summary_Table_690stops.csv",row.names=FALSE)

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

#write.csv(stop_summary_difference_table,"./analysis_output/Stop_Summary_DIFFERENCE_Table_690stops.csv",row.names=FALSE)

#-------------------------------------------------------------------------
# Species-level estimates of occurrence probability
#-------------------------------------------------------------------------

# # Model script
# sink("./analysis_output/bin.jags")
# cat("
# 
#     model {
# 
#       #---------------------------------------------
#       # Zero-inflation term priors
#       #---------------------------------------------
# 
#       p.Field ~ dunif(0,1)
#       logit.p.Field <- logit(p.Field)
# 
#       p.L1 ~ dunif(0,1)
#       logit.p.L1 <- logit(p.L1)
# 
#       p.L2 ~ dunif(0,1)
#       logit.p.L2 <- logit(p.L2)
# 
#       #---------------------------------------------
#       # Likelihood
#       #---------------------------------------------
# 
#       # Each i represents a stop
#       for (i in 1:n.obs){
#         z.Field[i] ~ dbern(p.Field)
#         z.L1[i] ~ dbern(p.L1)
#         z.L2[i] ~ dbern(p.L2)
#       }
# 
#     }
#     ",fill = TRUE)
# sink()
# 
# bin.results <- data.frame()
# all.species <- unique(dat_merge$CName)
# for (s in sort(all.species)){
# 
#   print(s)
#   dat_s <- subset(dat_merge_full, CName == s)
# 
#   jags.data <- list(z.Field = as.numeric(dat_s$Count.Field > 0),
#                     z.L1 = as.numeric(dat_s$Count.L1 > 0),
#                     z.L2 = as.numeric(dat_s$Count.L2 > 0),
#                     n.obs = nrow(dat_s))
# 
#   inits <- NULL
# 
#   parameters.to.save = c("p.Field", "p.L1","p.L2")
# 
#   nc <- 3; nt <- 5; ni <- 8000; nb <- 4000
# 
#   # Fit the model
#   out <- jags(data = jags.data,
#               model.file = "./analysis_output/bin.jags",
#               parameters.to.save = parameters.to.save,
#               inits = inits,n.chains = nc,n.thin = nt,n.iter = ni,n.burnin = nb)
# 
#   pd <- out$sims.list
# 
#   species.results <- data.frame(CName = s,
#                                 max.Rhat = max(unlist(out$Rhat), na.rm = TRUE),
#                                 p.Field.500 = quantile(pd$p.Field,0.5),
#                                 p.Field.025 = quantile(pd$p.Field,0.025),
#                                 p.Field.975 = quantile(pd$p.Field,0.975),
# 
#                                 p.L1.500 = quantile(pd$p.L1,0.5),
#                                 p.L1.025 = quantile(pd$p.L1,0.025),
#                                 p.L1.975 = quantile(pd$p.L1,0.975),
# 
#                                 p.L2.500 = quantile(pd$p.L2,0.5),
#                                 p.L2.025 = quantile(pd$p.L2,0.025),
#                                 p.L2.975 = quantile(pd$p.L2,0.975),
#                                 
#                                 # Differences
#                                 rel.change.q500.L2.vs.L1 = quantile((pd$p.L2 - pd$p.L1)/pd$p.L1, 0.5),
#                                 rel.change.q025.L2.vs.L1 = quantile((pd$p.L2 - pd$p.L1)/pd$p.L1, 0.025),
#                                 rel.change.q975.L2.vs.L1 = quantile((pd$p.L2 - pd$p.L1)/pd$p.L1, 0.975),
#                                 
#                                 rel.change.q500.Field.vs.L1 = quantile((pd$p.Field - pd$p.L1)/pd$p.L1, 0.5),
#                                 rel.change.q025.Field.vs.L1 = quantile((pd$p.Field - pd$p.L1)/pd$p.L1, 0.025),
#                                 rel.change.q975.Field.vs.L1 = quantile((pd$p.Field - pd$p.L1)/pd$p.L1, 0.975),
#                                 
#                                 rel.change.q500.Field.vs.L2 = quantile((pd$p.Field - pd$p.L2)/pd$p.L2, 0.5),
#                                 rel.change.q025.Field.vs.L2 = quantile((pd$p.Field - pd$p.L2)/pd$p.L2, 0.025),
#                                 rel.change.q975.Field.vs.L2 = quantile((pd$p.Field - pd$p.L2)/pd$p.L2, 0.975),
#                                 
#                                 prob.Field.greater.L2 = mean(pd$p.Field > pd$p.L2),
#                                 prob.Field.greater.L1 = mean(pd$p.Field > pd$p.L1),
#                                 prob.L2.greater.L1 = mean(pd$p.L2 > pd$p.L1))
# 
#   bin.results <- rbind(bin.results, species.results)
#   write.csv(bin.results, file = "./analysis_output/bin_results.csv", row.names = FALSE)
# }
# 
# #-------------------------------------------------------------------------
# # Species-level estimates of mean count
# #-------------------------------------------------------------------------
# 
# # Jags code for ZIP
# # Model script
# sink("./analysis_output/zip.jags")
# cat("
# 
#     model {
# 
#       #---------------------------------------------
#       # Zero-inflation term priors
#       #---------------------------------------------
# 
#       p.Field ~ dunif(0,1)
#       logit.p.Field <- logit(p.Field)
# 
#       p.L1 ~ dunif(0,1)
#       logit.p.L1 <- logit(p.L1)
# 
#       p.L2 ~ dunif(0,1)
#       logit.p.L2 <- logit(p.L2)
# 
#       #-------------------------------------
#       # Count term priors
#       #-------------------------------------
# 
#       log.lambda.Field ~ dnorm(0,0.25)
#       log.lambda.L1 ~ dnorm(0,0.25)
#       log.lambda.L2 ~ dnorm(0,0.25)
# 
#       lambda.Field <- exp(log.lambda.Field)
#       lambda.L1 <- exp(log.lambda.L1)
#       lambda.L2 <- exp(log.lambda.L2)
# 
#       #---------------------------------------------
#       # Likelihood
#       #---------------------------------------------
# 
#       # Each i represents a stop
#       for (i in 1:n.obs){
# 
#         z.Field[i] ~ dbern(p.Field)
#         y.Field[i] ~ dpois(lambda.Field*z.Field[i])
# 
#         z.L1[i] ~ dbern(p.L1)
#         y.L1[i] ~ dpois(lambda.L1*z.L1[i])
# 
#         z.L2[i] ~ dbern(p.L2)
#         y.L2[i] ~ dpois(lambda.L2*z.L2[i])
# 
#       }
# 
#       mean.Field <- p.Field * lambda.Field
#       mean.L1 <- p.L1 * lambda.L1
#       mean.L2 <- p.L2 * lambda.L2
# 
#     }
#     ",fill = TRUE)
# sink()
# 
# zip.results <- data.frame()
# 
# all.species <- unique(dat_merge$CName)
# for (s in sort(all.species)){
# 
#   print(s)
#   dat_s <- subset(dat_merge_full, CName == s)
# 
#   jags.data <- list(y.Field = dat_s$Count.Field,
#                     y.L1 = dat_s$Count.L1,
#                     y.L2 = dat_s$Count.L2,
#                     n.obs = nrow(dat_s))
# 
#   inits <- function(){ list(z.Field = rep(1,nrow(dat_s)),
#                             z.L1 = rep(1,nrow(dat_s)),
#                             z.L2 = rep(1,nrow(dat_s)),
# 
#                             log.lambda.Field = log(0.5),
#                             log.lambda.L1 = log(0.5),
#                             log.lambda.L2 = log(0.5))}
# 
#   parameters.to.save = c("mean.Field","mean.L1",'mean.L2',"p.Field","lambda.Field", "p.L1","lambda.L1", "p.L2","lambda.L2")
# 
#   nc <- 3; nt <- 5; ni <- 8000; nb <- 4000
# 
#   # Fit the model
#   out <- jags(data = jags.data,
#               model.file = "./analysis_output/zip.jags",
#               parameters.to.save = parameters.to.save,
#               inits = inits,n.chains = nc,n.thin = nt,n.iter = ni,n.burnin = nb,
#               parallel = TRUE)
# 
#   pd <- out$sims.list
# 
#   species.results <- data.frame(CName = s,
#                                 max.Rhat = max(unlist(out$Rhat)[1:3], na.rm = TRUE),
#                                 p.Field.500 = quantile(pd$p.Field,0.5),
#                                 p.Field.025 = quantile(pd$p.Field,0.025),
#                                 p.Field.975 = quantile(pd$p.Field,0.975),
#                                 lambda.Field.500 = quantile(pd$lambda.Field,0.5),
#                                 lambda.Field.025 = quantile(pd$lambda.Field,0.025),
#                                 lambda.Field.975 = quantile(pd$lambda.Field,0.975),
#                                 mean.Field.500 = quantile(pd$mean.Field,0.5),
#                                 mean.Field.025 = quantile(pd$mean.Field,0.025),
#                                 mean.Field.975 = quantile(pd$mean.Field,0.975),
# 
# 
#                                 p.L1.500 = quantile(pd$p.L1,0.5),
#                                 p.L1.025 = quantile(pd$p.L1,0.025),
#                                 p.L1.975 = quantile(pd$p.L1,0.975),
#                                 lambda.L1.500 = quantile(pd$lambda.L1,0.5),
#                                 lambda.L1.025 = quantile(pd$lambda.L1,0.025),
#                                 lambda.L1.975 = quantile(pd$lambda.L1,0.975),
#                                 mean.L1.500 = quantile(pd$mean.L1,0.5),
#                                 mean.L1.025 = quantile(pd$mean.L1,0.025),
#                                 mean.L1.975 = quantile(pd$mean.L1,0.975),
# 
#                                 p.L2.500 = quantile(pd$p.L2,0.5),
#                                 p.L2.025 = quantile(pd$p.L2,0.025),
#                                 p.L2.975 = quantile(pd$p.L2,0.975),
#                                 lambda.L2.500 = quantile(pd$lambda.L2,0.5),
#                                 lambda.L2.025 = quantile(pd$lambda.L2,0.025),
#                                 lambda.L2.975 = quantile(pd$lambda.L2,0.975),
#                                 mean.L2.500 = quantile(pd$mean.L2,0.5),
#                                 mean.L2.025 = quantile(pd$mean.L2,0.025),
#                                 mean.L2.975 = quantile(pd$mean.L2,0.975),
#                                 
#                                 # Differences in mean counts
#                                 rel.change.q500.L2.vs.L1 = quantile((pd$mean.L2 - pd$mean.L1)/pd$mean.L1, 0.5),
#                                 rel.change.q025.L2.vs.L1 = quantile((pd$mean.L2 - pd$mean.L1)/pd$mean.L1, 0.025),
#                                 rel.change.q975.L2.vs.L1 = quantile((pd$mean.L2 - pd$mean.L1)/pd$mean.L1, 0.975),
#                                 
#                                 rel.change.q500.Field.vs.L1 = quantile((pd$mean.Field - pd$mean.L1)/pd$mean.L1, 0.5),
#                                 rel.change.q025.Field.vs.L1 = quantile((pd$mean.Field - pd$mean.L1)/pd$mean.L1, 0.025),
#                                 rel.change.q975.Field.vs.L1 = quantile((pd$mean.Field - pd$mean.L1)/pd$mean.L1, 0.975),
#                                 
#                                 rel.change.q500.Field.vs.L2 = quantile((pd$mean.Field - pd$mean.L2)/pd$mean.L2, 0.5),
#                                 rel.change.q025.Field.vs.L2 = quantile((pd$mean.Field - pd$mean.L2)/pd$mean.L2, 0.025),
#                                 rel.change.q975.Field.vs.L2 = quantile((pd$mean.Field - pd$mean.L2)/pd$mean.L2, 0.975),
#                                 
#                                 prob.Field.greater.L2 = mean(pd$mean.Field > pd$mean.L2),
#                                 prob.Field.greater.L1 = mean(pd$mean.Field > pd$mean.L1),
#                                 prob.L2.greater.L1 = mean(pd$mean.L2 > pd$mean.L1))
# 
#   zip.results <- rbind(zip.results, species.results)
#   write.csv(zip.results, file = "./analysis_output/zip_results.csv", row.names = FALSE)
# }
# 
bin.results <- read.csv(file = "./analysis_output/bin_results.csv")
bin.results.converge <- subset(bin.results, max.Rhat < 1.1)
zip.results <- read.csv(file = "./analysis_output/zip_results.csv")
zip.results.converge <- subset(zip.results, max.Rhat < 1.1)

# ------------------------------------------------------------------
# FIGURES/RESULTS
# ------------------------------------------------------------------

#-----------
# Figure 1
#-----------

# Stop-level richness
pear.corr.a <- cor(stop_summary$Richness.L1,stop_summary$Richness.L2, method = "pearson") %>% round(3)
spear.corr.a <- cor(stop_summary$Richness.L1,stop_summary$Richness.L2, method = "spearman") %>% round(3)
lims.a <- c(0,max(stop_summary[,c("Richness.L1","Richness.L2")]))
Fig1A <- ggplot(stop_summary)+
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = jitter(Richness.L1, amount = 0.2), y = jitter(Richness.L2, amount = 0.2)), alpha = 0.3, size = 1)+
  coord_cartesian(xlim = lims.a,
                  ylim = lims.a)+
  xlab("# species detected\n(first listener)")+
  ylab("# species detected\n(second listener)")+
  theme_bw()+
  theme(plot.margin = unit(c(3,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.a)*0.01, y = max(lims.a)*0.95), label = paste0("Pearson Corr = ",pear.corr.a,"\nSpearman Corr = ",spear.corr.a), hjust = 0, size = 2)
Fig1A

#jpeg("./analysis_output/figures/Fig_1A.jpg",width = 4, height = 4, units = "in", res = 500)
print(Fig1A)
#dev.off()

# Stop-level abundance
pear.corr.b <- cor(stop_summary$Count.L1,stop_summary$Count.L2, method = "pearson") %>% round(3)
spear.corr.b  <- cor(stop_summary$Count.L1,stop_summary$Count.L2, method = "spearman") %>% round(3)
lims.b  <- c(0,max(stop_summary[,c("Count.L1","Count.L2")]))
Fig1B <- ggplot(stop_summary)+
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = jitter(Count.L1, amount = 0.1), y = jitter(Count.L2, amount = 0.1)), alpha = 0.3, size = 1)+
  
  coord_cartesian(xlim = lims.b ,
                  ylim = lims.b )+
  xlab("# birds detected\n(first listener)")+
  ylab("# birds detected\n(second listener)")+
  theme_bw()+
  theme(plot.margin = unit(c(3,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.b )*0.01, y = max(lims.b )*0.95), label = paste0("Pearson Corr = ",pear.corr.b ,"\nSpearman Corr = ",spear.corr.b ), hjust = 0, size = 2)

#jpeg("./analysis_output/figures/Fig_1B.jpg",width = 4, height = 4, units = "in", res = 500)
print(Fig1B)
#dev.off()

# Species occurrence probabilities
pear.corr.c <- cor(bin.results.converge$p.L1.500,bin.results.converge$p.L2.500, method = "pearson") %>% round(3)
spear.corr.c <- cor(bin.results.converge$p.L1.500,bin.results.converge$p.L2.500, method = "spearman") %>% round(3)
lims.c <- c(0,1)
Fig1C <- ggplot(bin.results.converge) +
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = p.L1.500, y = p.L2.500))+
  geom_errorbar(aes(x = p.L1.500, ymin = p.L2.025, ymax = p.L2.975), width = 0)+
  geom_errorbarh(aes(xmin = p.L1.025, xmax = p.L1.975, y = p.L2.500), height = 0)+
  coord_cartesian(xlim = lims.c, ylim = lims.c)+
  xlab(expression(atop("Species occurrence probability"~italic(p),"(first listener)")))+
  ylab(expression(atop("Species occurrence probability"~italic(p),"(second listener)")))+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,3), "lines"))+
  geom_text(aes(x = max(lims.c)*0.01, y = max(lims.c)*0.95), label = paste0("Pearson Corr = ",pear.corr.c,"\nSpearman Corr = ",spear.corr.c), hjust = 0, size = 2)

#jpeg("./analysis_output/figures/Fig_1C.jpg",width = 4, height = 4, units = "in", res = 500)
print(Fig1C)
#dev.off()

# Species mean abundance per stop
pear.corr.d <- cor(zip.results.converge$mean.L1.500,zip.results.converge$mean.L2.500, method = "pearson") %>% round(3)
spear.corr.d <- cor(zip.results.converge$mean.L1.500,zip.results.converge$mean.L2.500, method = "spearman") %>% round(3)
lims.d <- c(0,max(zip.results.converge[,c("mean.L1.975","mean.L2.975")]))
Fig1D <- ggplot(zip.results.converge) +
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = mean.L1.500, y = mean.L2.500))+
  geom_errorbar(aes(x = mean.L1.500, ymin = mean.L2.025, ymax = mean.L2.975), width = 0)+
  geom_errorbarh(aes(xmin = mean.L1.025, xmax = mean.L1.975, y = mean.L2.500), height = 0)+
  coord_cartesian(xlim = lims.d, ylim = lims.d)+
  xlab(expression(atop("Species mean abundance"~italic(mu),"(first listener)")))+
  ylab(expression(atop("Species mean abundance"~italic(mu),"(second listener)")))+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,3), "lines"))+
  geom_text(aes(x = max(lims.d)*0.01, y = max(lims.d)*0.95), label = paste0("Pearson Corr = ",pear.corr.d,"\nSpearman Corr = ",spear.corr.d), hjust = 0, size = 2)

#jpeg("./analysis_output/figures/Fig_1D.jpg",width = 4, height = 4, units = "in", res = 500)
print(Fig1D)
#dev.off()

Fig1 <- plot_grid(Fig1A,Fig1B,Fig1C,Fig1D,nrow = 2, align = "hv",
                  labels = c("A","B","C","D"))

#jpeg("./analysis_output/figures/Fig_1.jpg",width = 7, height = 7, units = "in", res = 600)
print(Fig1)
#dev.off()

#-----------
# Figure 2 - left column
#-----------

# Stop-level richness
pear.corr.a <- cor(stop_summary$Richness.Field,stop_summary$Richness.L1, method = "pearson") %>% round(3)
spear.corr.a <- cor(stop_summary$Richness.Field,stop_summary$Richness.L1, method = "spearman") %>% round(3)
lims.a <- c(0,max(stop_summary[,c("Richness.Field","Richness.L1","Richness.L2")]))
Fig2LA <- ggplot(stop_summary)+
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = jitter(Richness.Field, amount = 0.2), y = jitter(Richness.L1, amount = 0.2)), alpha = 0.3, size = 1)+
  coord_cartesian(xlim = lims.a,
                  ylim = lims.a)+
  xlab("# species detected\n(field)")+
  ylab("# species detected\n(first listener)")+
  theme_bw()+
  theme(plot.margin = unit(c(3,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.a)*0.01, y = max(lims.a)*0.95), label = paste0("Pearson Corr = ",pear.corr.a,"\nSpearman Corr = ",spear.corr.a), hjust = 0, size = 2)
Fig2LA

#jpeg("./analysis_output/figures/Fig_2LA.jpg",width = 4, height = 4, units = "in", res = 500)
# print(Fig2LA)
# dev.off()

# Stop-level abundance
pear.corr.b <- cor(stop_summary$Count.Field,stop_summary$Count.L1, method = "pearson") %>% round(3)
spear.corr.b  <- cor(stop_summary$Count.Field,stop_summary$Count.L1, method = "spearman") %>% round(3)
lims.b  <- c(0,max(stop_summary[,c("Count.Field","Count.L1","Count.L2")]))
Fig2LB <- ggplot(stop_summary)+
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = jitter(Count.Field, amount = 0.1), y = jitter(Count.L1, amount = 0.1)), alpha = 0.3, size = 1)+
  
  coord_cartesian(xlim = lims.b ,
                  ylim = lims.b )+
  xlab("# birds detected\n(field)")+
  ylab("# birds detected\n(first listener)")+
  theme_bw()+
  theme(plot.margin = unit(c(3,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.b )*0.01, y = max(lims.b )*0.95), label = paste0("Pearson Corr = ",pear.corr.b ,"\nSpearman Corr = ",spear.corr.b ), hjust = 0, size = 2)

#jpeg("./analysis_output/figures/Fig_2LB.jpg",width = 4, height = 4, units = "in", res = 500)
print(Fig2LB)
#dev.off()

# Species occurrence probabilities
pear.corr.c <- cor(bin.results.converge$p.Field.500,bin.results.converge$p.L1.500, method = "pearson") %>% round(3)
spear.corr.c <- cor(bin.results.converge$p.Field.500,bin.results.converge$p.L1.500, method = "spearman") %>% round(3)
lims.c <- c(0,1)
Fig2LC <- ggplot(bin.results.converge) +
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = p.Field.500, y = p.L1.500))+
  geom_errorbar(aes(x = p.Field.500, ymin = p.L1.025, ymax = p.L1.975), width = 0)+
  geom_errorbarh(aes(xmin = p.Field.025, xmax = p.Field.975, y = p.L1.500), height = 0)+
  coord_cartesian(xlim = lims.c, ylim = lims.c)+
  xlab(expression(atop("Species occurrence probability"~italic(p),"(field)")))+
  ylab(expression(atop("Species occurrence probability"~italic(p),"(first listener)")))+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,3), "lines"))+
  geom_text(aes(x = max(lims.c)*0.01, y = max(lims.c)*0.95), label = paste0("Pearson Corr = ",pear.corr.c,"\nSpearman Corr = ",spear.corr.c), hjust = 0, size = 2)

#jpeg("./analysis_output/figures/Fig_2LC.jpg",width = 4, height = 4, units = "in", res = 500)
print(Fig2LC)
#dev.off()

# Species mean abundance per stop
pear.corr.d <- cor(zip.results.converge$mean.Field.500,zip.results.converge$mean.L1.500, method = "pearson") %>% round(3)
spear.corr.d <- cor(zip.results.converge$mean.Field.500,zip.results.converge$mean.L1.500, method = "spearman") %>% round(3)
lims.d <- c(0,max(zip.results.converge[,c("mean.Field.975","mean.L1.975","mean.L2.975")]))
Fig2LD <- ggplot(zip.results.converge) +
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = mean.Field.500, y = mean.L1.500))+
  geom_errorbar(aes(x = mean.Field.500, ymin = mean.L1.025, ymax = mean.L1.975), width = 0)+
  geom_errorbarh(aes(xmin = mean.Field.025, xmax = mean.Field.975, y = mean.L1.500), height = 0)+
  coord_cartesian(xlim = lims.d, ylim = lims.d)+
  xlab(expression(atop("Species mean abundance"~italic(mu),"(field)")))+
  ylab(expression(atop("Species mean abundance"~italic(mu),"(first listener)")))+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,3), "lines"))+
  geom_text(aes(x = max(lims.d)*0.01, y = max(lims.d)*0.95), label = paste0("Pearson Corr = ",pear.corr.d,"\nSpearman Corr = ",spear.corr.d), hjust = 0, size = 2)

#jpeg("./analysis_output/figures/Fig_2LD.jpg",width = 4, height = 4, units = "in", res = 500)
print(Fig2LD)
#dev.off()

Fig2L <- plot_grid(Fig2LA,Fig2LB,Fig2LC,Fig2LD,nrow = 4, align = "hv",
                   labels = c("A","C","E","G"))

#jpeg("./analysis_output/figures/Fig_2L.jpg",width = 4, height = 3.5*4, units = "in", res = 600)
print(Fig2L)
#dev.off()

#-----------
# Figure 2 - right column
#-----------

# Stop-level richness
pear.corr.a <- cor(stop_summary$Richness.Field,stop_summary$Richness.L2, method = "pearson") %>% round(3)
spear.corr.a <- cor(stop_summary$Richness.Field,stop_summary$Richness.L2, method = "spearman") %>% round(3)
lims.a <- c(0,max(stop_summary[,c("Richness.Field","Richness.L1","Richness.L2")]))
Fig2RA <- ggplot(stop_summary)+
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = jitter(Richness.Field, amount = 0.2), y = jitter(Richness.L2, amount = 0.2)), alpha = 0.3, size = 1)+
  coord_cartesian(xlim = lims.a,
                  ylim = lims.a)+
  xlab("# species detected\n(field)")+
  ylab("# species detected\n(second listener)")+
  theme_bw()+
  theme(plot.margin = unit(c(3,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.a)*0.01, y = max(lims.a)*0.95), label = paste0("Pearson Corr = ",pear.corr.a,"\nSpearman Corr = ",spear.corr.a), hjust = 0, size = 2)
Fig2RA

#jpeg("./analysis_output/figures/Fig_2RA.jpg",width = 4, height = 4, units = "in", res = 500)
# print(Fig2RA)
# dev.off()

# Stop-level abundance
pear.corr.b <- cor(stop_summary$Count.Field,stop_summary$Count.L2, method = "pearson") %>% round(3)
spear.corr.b  <- cor(stop_summary$Count.Field,stop_summary$Count.L2, method = "spearman") %>% round(3)
lims.b  <- c(0,max(stop_summary[,c("Count.Field","Count.L1","Count.L2")]))
Fig2RB <- ggplot(stop_summary)+
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = jitter(Count.Field, amount = 0.1), y = jitter(Count.L2, amount = 0.1)), alpha = 0.3, size = 1)+
  
  coord_cartesian(xlim = lims.b ,
                  ylim = lims.b )+
  xlab("# birds detected\n(field)")+
  ylab("# birds detected\n(second listener)")+
  theme_bw()+
  theme(plot.margin = unit(c(3,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.b )*0.01, y = max(lims.b )*0.95), label = paste0("Pearson Corr = ",pear.corr.b ,"\nSpearman Corr = ",spear.corr.b ), hjust = 0, size = 2)

#jpeg("./analysis_output/figures/Fig_2RB.jpg",width = 4, height = 4, units = "in", res = 500)
print(Fig2RB)
#dev.off()

# Species occurrence probabilities
pear.corr.c <- cor(bin.results.converge$p.Field.500,bin.results.converge$p.L2.500, method = "pearson") %>% round(3)
spear.corr.c <- cor(bin.results.converge$p.Field.500,bin.results.converge$p.L2.500, method = "spearman") %>% round(3)
lims.c <- c(0,1)
Fig2RC <- ggplot(bin.results.converge) +
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = p.Field.500, y = p.L2.500))+
  geom_errorbar(aes(x = p.Field.500, ymin = p.L2.025, ymax = p.L2.975), width = 0)+
  geom_errorbarh(aes(xmin = p.Field.025, xmax = p.Field.975, y = p.L2.500), height = 0)+
  coord_cartesian(xlim = lims.c, ylim = lims.c)+
  xlab(expression(atop("Species occurrence probability"~italic(p),"(field)")))+
  ylab(expression(atop("Species occurrence probability"~italic(p),"(second listener)")))+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,3), "lines"))+
  geom_text(aes(x = max(lims.c)*0.01, y = max(lims.c)*0.95), label = paste0("Pearson Corr = ",pear.corr.c,"\nSpearman Corr = ",spear.corr.c), hjust = 0, size = 2)

#jpeg("./analysis_output/figures/Fig_2RC.jpg",width = 4, height = 4, units = "in", res = 500)
print(Fig2RC)
#dev.off()

# Species mean abundance per stop
pear.corr.d <- cor(zip.results.converge$mean.Field.500,zip.results.converge$mean.L2.500, method = "pearson") %>% round(3)
spear.corr.d <- cor(zip.results.converge$mean.Field.500,zip.results.converge$mean.L2.500, method = "spearman") %>% round(3)
lims.d <- c(0,max(zip.results.converge[,c("mean.Field.975","mean.L1.975","mean.L2.975")]))
Fig2RD <- ggplot(zip.results.converge) +
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(x = mean.Field.500, y = mean.L2.500))+
  geom_errorbar(aes(x = mean.Field.500, ymin = mean.L2.025, ymax = mean.L2.975), width = 0)+
  geom_errorbarh(aes(xmin = mean.Field.025, xmax = mean.Field.975, y = mean.L2.500), height = 0)+
  coord_cartesian(xlim = lims.d, ylim = lims.d)+
  xlab(expression(atop("Species mean abundance"~italic(mu),"(field)")))+
  ylab(expression(atop("Species mean abundance"~italic(mu),"(second listener)")))+
  theme_bw()+
  theme(plot.margin = unit(c(1,1,1,3), "lines"))+
  geom_text(aes(x = max(lims.d)*0.01, y = max(lims.d)*0.95), label = paste0("Pearson Corr = ",pear.corr.d,"\nSpearman Corr = ",spear.corr.d), hjust = 0, size = 2)

#jpeg("./analysis_output/figures/Fig_2RD.jpg",width = 4, height = 4, units = "in", res = 500)
print(Fig2RD)
#dev.off()

Fig2R <- plot_grid(Fig2RA,Fig2RB,Fig2RC,Fig2RD,nrow = 4, align = "hv",
                   labels = c("B","D","F","H"))

#jpeg("./analysis_output/figures/Fig_2R.jpg",width = 4, height = 3.5*4, units = "in", res = 600)
print(Fig2R)
#dev.off()

Fig2 <- plot_grid(Fig2L,Fig2R)

#jpeg("./analysis_output/figures/Fig_2.jpg",width = 8, height = 3.5*4, units = "in", res = 600)
print(Fig2)
#dev.off()

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

#write.csv(Stop_level_comparison_table,"./analysis_output/Stop_level_comparison_table.csv", row.names = FALSE)

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

#write.csv(Stop_level_comparison_table_NOFLOCK,"./analysis_output/Stop_level_comparison_table_NOFLOCK.csv", row.names = FALSE)

#-----------
# Appendix Figures - comparison of species occurrences and mean counts
#-----------

# Species occurrence
bin.results.Field <- bin.results[,c("CName","p.Field.500","p.Field.025","p.Field.975")] %>% add_column(Type = "Field Observer")
bin.results.L1 <- bin.results[,c("CName","p.L1.500","p.L1.025","p.L1.975")] %>% add_column(Type = "One Listener")
bin.results.L2 <- bin.results[,c("CName","p.L2.500","p.L2.025","p.L2.975")] %>% add_column(Type = "Two Listeners")
colnames(bin.results.Field)[2:4] = colnames(bin.results.L1)[2:4] = colnames(bin.results.L2)[2:4] = c("q500","q025","q975")
bin.compare <- rbind(bin.results.Field,bin.results.L1,bin.results.L2)

bin.species.order <- bin.results.Field %>% arrange(q500)
bin.compare$CName <- factor(bin.compare$CName, levels = bin.species.order$CName)

# 95% credible intervals on diff(field,L2) don't overlap zero
bin.signif.1 <- subset(bin.results,rel.change.q975.Field.vs.L2 < 0 | rel.change.q025.Field.vs.L2 > 0) %>% arrange(p.Field.500)
nrow(bin.signif.1)
nrow(bin.signif.1)/nrow(bin.results)
mean(bin.results$p.Field.500 > bin.results$p.L2.500)

# 95% credible intervals on diff(L2,L1) don't overlap zero
bin.signif.2 <- subset(bin.results,rel.change.q975.L2.vs.L1 < 0 | rel.change.q025.L2.vs.L1 > 0) %>% arrange(p.Field.500)
nrow(bin.signif.2)
nrow(bin.signif.2)/nrow(bin.results)

bin.compare$Type <- factor(bin.compare$Type, levels = c("Field Observer","Two Listeners","One Listener"))
bin.species.plot <- ggplot(bin.compare)+
  geom_point(data = bin.compare, aes(x = CName, y = q500, col = Type), position = position_dodge(width = 0.5))+
  geom_text(data = bin.signif.1, aes(y = -0.04, x = CName),
            label = "*F*",
            hjust = 0,
            vjust = 1.75,
            size = 2,
            position = position_dodge(width = 0.5))+
  
  geom_text(data = bin.signif.2, aes(y = -0.04, x = CName),
            label = "*L*",
            hjust = 0,
            vjust = -0.25,
            size = 2,
            position = position_dodge(width = 0.5))+
  
  geom_errorbar(data = bin.compare, aes(x = CName, y = q500, ymin = q025, ymax = q975, col = Type), position = position_dodge(width = 0.5), width = 0)+
  
  scale_color_manual(values = RColorBrewer::brewer.pal(3,"Dark2")[c(1,3,2)], name = "Survey type")+
  ylab(expression(italic(p)))+
  xlab("Species")+
  ggtitle("Estimates of species occurrence")+
  coord_flip(ylim = c(0,1))+
  theme_bw()+
  theme(legend.position="top")
bin.species.plot

#jpeg("./analysis_output/figures/Fig_S3.jpg",width = 8, height = 50, units = "in", res = 600)
print(bin.species.plot)
#dev.off()

# Species mean count
zip.results.Field <- zip.results[,c("CName","mean.Field.500","mean.Field.025","mean.Field.975")] %>% add_column(Type = "Field Observer")
zip.results.L1 <- zip.results[,c("CName","mean.L1.500","mean.L1.025","mean.L1.975")] %>% add_column(Type = "One Listener")
zip.results.L2 <- zip.results[,c("CName","mean.L2.500","mean.L2.025","mean.L2.975")] %>% add_column(Type = "Two Listeners")
colnames(zip.results.Field)[2:4] = colnames(zip.results.L1)[2:4] = colnames(zip.results.L2)[2:4] = c("q500","q025","q975")
zip.compare <- rbind(zip.results.Field,zip.results.L1,zip.results.L2)

zip.species.order <- zip.results.Field %>% arrange(q500)
zip.compare$CName <- factor(zip.compare$CName, levels = zip.species.order$CName)

# 95% credible intervals on diff(field,L2) don't overlap zero
zip.signif.1 <- subset(zip.results,rel.change.q975.Field.vs.L2 < 0 | rel.change.q025.Field.vs.L2 > 0) %>% arrange(mean.Field.500)
nrow(zip.signif.1)
nrow(zip.signif.1)/nrow(zip.results)
mean(zip.results$mean.Field.500 > zip.results$mean.L2.500)

# 95% credible intervals on diff(L2,L1) don't overlap zero
zip.signif.2 <- subset(zip.results,rel.change.q975.L2.vs.L1 < 0 | rel.change.q025.L2.vs.L1 > 0) %>% arrange(mean.Field.500)
nrow(zip.signif.2)
nrow(zip.signif.2)/nrow(zip.results)

zip.compare$Type <- factor(zip.compare$Type, levels = c("Field Observer","Two Listeners","One Listener"))
zip.species.plot <- ggplot(zip.compare)+
  geom_point(data = zip.compare, aes(x = CName, y = q500, col = Type), position = position_dodge(width = 0.5))+
  geom_text(data = zip.signif.1, aes(y = -0.06, x = CName),
            label = "*F*",
            hjust = 0,
            vjust = 1.75,
            size = 2,
            position = position_dodge(width = 0.5))+
  
  geom_text(data = zip.signif.2, aes(y = -0.06, x = CName),
            label = "*L*",
            hjust = 0,
            vjust = -0.25,
            size = 2,
            position = position_dodge(width = 0.5))+
  
  geom_errorbar(data = zip.compare, aes(x = CName, y = q500, ymin = q025, ymax = q975, col = Type), position = position_dodge(width = 0.5), width = 0)+
  
  scale_color_manual(values = RColorBrewer::brewer.pal(3,"Dark2")[c(1,3,2)], name = "Survey type")+
  ylab(expression(italic(mu)))+
  xlab("Species")+
  ggtitle("Estimates of species mean counts")+
  coord_flip(ylim = c(0,1.5))+
  theme_bw()+
  theme(legend.position="top")
zip.species.plot

#jpeg("./analysis_output/figures/Fig_S4.jpg",width = 8, height = 50, units = "in", res = 600)
print(zip.species.plot)
#dev.off()

mean(zip.results$lambda.L2.500 > zip.results$lambda.L1.500)

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

#write.csv(species_occurrence_table,"./analysis_output/species_occurrence_table.csv",row.names=FALSE)

#--------------------------------------------------
# CHRISTIAN FRIIS AUDIO AND RECONCILE
#--------------------------------------------------

# -------------------------------------------------------------
# Load the stops that were initially listened to by Christian
# -------------------------------------------------------------

f_audio <- Part1$friis_avi

# Aggregate species counts at each stop
f_audio <- f_audio %>% aggregate(TagID ~ spec_1 + RouteStopYear, data = ., FUN = length)
colnames(f_audio) <- c("CName","RouteStopYear","Count.F.audio")

# Remove one major outlier stop
f_audio = subset(f_audio, RouteStopYear != "68032302011")

summary(f_audio)
dat_merge_2 <- dat_merge %>% 
  dplyr::select(-present.any,-present.Field,-present.L1,-present.L2) %>%
  subset(RouteStopYear %in% f_audio$RouteStopYear) %>%
  full_join(., f_audio) %>%
  arrange(RouteStopYear,CName)

length(unique(dat_merge_2$RouteStopYear)) # 200 stops initially reviewed by Christian

dat_merge_2[is.na(dat_merge_2)] <- 0

dat_merge_2$present.Field <- dat_merge_2$Count.Field > 0
dat_merge_2$present.L1 <- dat_merge_2$Count.L1 > 0
dat_merge_2$present.L2 <- dat_merge_2$Count.L2 > 0
dat_merge_2$present.F.audio <- dat_merge_2$Count.F.audio > 0

stop_summary_2 <- dat_merge_2 %>%
  group_by(RouteStopYear) %>%
  summarize(Richness.Field = sum(present.Field),
            Richness.L1 = sum(present.L1),
            Richness.L2 = sum(present.L2),
            Richness.F.audio = sum(present.F.audio),
            
            Count.Field = sum(Count.Field),
            Count.L1 = sum(Count.L1),
            Count.L2 = sum(Count.L2),
            Count.F.audio = sum(Count.F.audio)) %>%
  as.data.frame()

summary(stop_summary_2)

# Differences between Christian's audio, Field, and Audio listeners
stop_summary_2$Richness.Field_minus_Richness.L1 <- stop_summary_2$Richness.Field - stop_summary_2$Richness.L1
stop_summary_2$Richness.Field_minus_Richness.L2 <- stop_summary_2$Richness.Field - stop_summary_2$Richness.L2
stop_summary_2$Richness.L2_minus_Richness.L1 <- stop_summary_2$Richness.L2 - stop_summary_2$Richness.L1
stop_summary_2$Count.Field_minus_Count.L1 <- stop_summary_2$Count.Field - stop_summary_2$Count.L1
stop_summary_2$Count.Field_minus_Count.L2 <- stop_summary_2$Count.Field - stop_summary_2$Count.L2
stop_summary_2$Count.L2_minus_Count.L1 <- stop_summary_2$Count.L2 - stop_summary_2$Count.L1

stop_summary_2$Richness.F.audio_minus_Richness.L1 <- stop_summary_2$Richness.F.audio - stop_summary_2$Richness.L1
stop_summary_2$Count.F.audio_minus_Count.L1 <- stop_summary_2$Count.F.audio - stop_summary_2$Count.L1

stop_summary_2$Richness.F.audio_minus_Richness.L2 <- stop_summary_2$Richness.F.audio - stop_summary_2$Richness.L2
stop_summary_2$Count.F.audio_minus_Count.L2 <- stop_summary_2$Count.F.audio - stop_summary_2$Count.L2

stop_summary_2$Richness.F.audio_minus_Richness.Field <- stop_summary_2$Richness.F.audio - stop_summary_2$Richness.Field
stop_summary_2$Count.F.audio_minus_Count.Field <- stop_summary_2$Count.F.audio - stop_summary_2$Count.Field

stop_summary_table_2 <- rbind(data.frame(n.stops = nrow(stop_summary_2),
                                         Method = "Field",
                                         mean.Richness = mean(stop_summary_2$Richness.Field),
                                         sd.Richness = sd(stop_summary_2$Richness.Field),
                                         se.Richness = sd(stop_summary_2$Richness.Field)/sqrt(nrow(stop_summary_2)),
                                         min.Richness = min(stop_summary_2$Richness.Field),
                                         max.Richness = max(stop_summary_2$Richness.Field),
                                         mean.Count = mean(stop_summary_2$Count.Field),
                                         sd.Count = sd(stop_summary_2$Count.Field),
                                         se.Count = sd(stop_summary_2$Count.Field)/sqrt(nrow(stop_summary_2)),
                                         min.Count = min(stop_summary_2$Count.Field),
                                         max.Count = max(stop_summary_2$Count.Field)),
                              
                              data.frame(n.stops = nrow(stop_summary_2),
                                         Method = "Single Listener",
                                         mean.Richness = mean(stop_summary_2$Richness.L1),
                                         sd.Richness = sd(stop_summary_2$Richness.L1),
                                         se.Richness = sd(stop_summary_2$Richness.L1)/sqrt(nrow(stop_summary_2)),
                                         min.Richness = min(stop_summary_2$Richness.L1),
                                         max.Richness = max(stop_summary_2$Richness.L1),
                                         mean.Count = mean(stop_summary_2$Count.L1),
                                         sd.Count = sd(stop_summary_2$Count.L1),
                                         se.Count = sd(stop_summary_2$Count.L1)/sqrt(nrow(stop_summary_2)),
                                         min.Count = min(stop_summary_2$Count.L1),
                                         max.Count = max(stop_summary_2$Count.L1)),
                              
                              data.frame(n.stops = nrow(stop_summary_2),
                                         Method = "Double Listener",
                                         mean.Richness = mean(stop_summary_2$Richness.L2),
                                         sd.Richness = sd(stop_summary_2$Richness.L2),
                                         se.Richness = sd(stop_summary_2$Richness.L2)/sqrt(nrow(stop_summary_2)),
                                         min.Richness = min(stop_summary_2$Richness.L2),
                                         max.Richness = max(stop_summary_2$Richness.L2),
                                         mean.Count = mean(stop_summary_2$Count.L2),
                                         sd.Count = sd(stop_summary_2$Count.L2),
                                         se.Count = sd(stop_summary_2$Count.L2)/sqrt(nrow(stop_summary_2)),
                                         min.Count = min(stop_summary_2$Count.L2),
                                         max.Count = max(stop_summary_2$Count.L2)),
                              
                              data.frame(n.stops = nrow(stop_summary_2),
                                         Method = "Field Observer's Review of Audio",
                                         mean.Richness = mean(stop_summary_2$Richness.F.audio),
                                         sd.Richness = sd(stop_summary_2$Richness.F.audio),
                                         se.Richness = sd(stop_summary_2$Richness.F.audio)/sqrt(nrow(stop_summary_2)),
                                         min.Richness = min(stop_summary_2$Richness.F.audio),
                                         max.Richness = max(stop_summary_2$Richness.F.audio),
                                         mean.Count = mean(stop_summary_2$Count.F.audio),
                                         sd.Count = sd(stop_summary_2$Count.F.audio),
                                         se.Count = sd(stop_summary_2$Count.F.audio)/sqrt(nrow(stop_summary_2)),
                                         min.Count = min(stop_summary_2$Count.F.audio),
                                         max.Count = max(stop_summary_2$Count.F.audio))
                              
)

#write.csv(stop_summary_table_2,"./analysis_output/Stop_Summary_Table_Faudio_200stops.csv",row.names=FALSE)

# Differences between survey methods
stop_summary_difference_table_2 <- rbind(data.frame(n.stops = nrow(stop_summary_2),
                                                    Comparison = "Field - Single Listener",
                                                    mean.diff.Richness = mean(stop_summary_2$Richness.Field_minus_Richness.L1),
                                                    sd.diff.Richness = sd(stop_summary_2$Richness.Field_minus_Richness.L1),
                                                    se.diff.Richness = sd(stop_summary_2$Richness.Field_minus_Richness.L1)/sqrt(nrow(stop_summary_2)),
                                                    min.diff.Richness = min(stop_summary_2$Richness.Field_minus_Richness.L1),
                                                    max.diff.Richness = max(stop_summary_2$Richness.Field_minus_Richness.L1),
                                                    
                                                    mean.diff.Count = mean(stop_summary_2$Count.Field_minus_Count.L1),
                                                    sd.diff.Count = sd(stop_summary_2$Count.Field_minus_Count.L1),
                                                    se.diff.Count = sd(stop_summary_2$Count.Field_minus_Count.L1)/sqrt(nrow(stop_summary_2)),
                                                    min.diff.Count = min(stop_summary_2$Count.Field_minus_Count.L1),
                                                    max.diff.Count = max(stop_summary_2$Count.Field_minus_Count.L1)),
                                         
                                         data.frame(n.stops = nrow(stop_summary_2),
                                                    Comparison = "Field - Double Listener",
                                                    mean.diff.Richness = mean(stop_summary_2$Richness.Field_minus_Richness.L2),
                                                    sd.diff.Richness = sd(stop_summary_2$Richness.Field_minus_Richness.L2),
                                                    se.diff.Richness = sd(stop_summary_2$Richness.Field_minus_Richness.L2)/sqrt(nrow(stop_summary_2)),
                                                    min.diff.Richness = min(stop_summary_2$Richness.Field_minus_Richness.L2),
                                                    max.diff.Richness = max(stop_summary_2$Richness.Field_minus_Richness.L2),
                                                    
                                                    mean.diff.Count = mean(stop_summary_2$Count.Field_minus_Count.L2),
                                                    sd.diff.Count = sd(stop_summary_2$Count.Field_minus_Count.L2),
                                                    se.diff.Count = sd(stop_summary_2$Count.Field_minus_Count.L2)/sqrt(nrow(stop_summary_2)),
                                                    min.diff.Count = min(stop_summary_2$Count.Field_minus_Count.L2),
                                                    max.diff.Count = max(stop_summary_2$Count.Field_minus_Count.L2)),
                                         
                                         data.frame(n.stops = nrow(stop_summary_2),
                                                    Comparison = "Double Listener - Single Listener",
                                                    mean.diff.Richness = mean(stop_summary_2$Richness.L2_minus_Richness.L1),
                                                    sd.diff.Richness = sd(stop_summary_2$Richness.L2_minus_Richness.L1),
                                                    se.diff.Richness = sd(stop_summary_2$Richness.L2_minus_Richness.L1)/sqrt(nrow(stop_summary_2)),
                                                    min.diff.Richness = min(stop_summary_2$Richness.L2_minus_Richness.L1),
                                                    max.diff.Richness = max(stop_summary_2$Richness.L2_minus_Richness.L1),
                                                    
                                                    mean.diff.Count = mean(stop_summary_2$Count.L2_minus_Count.L1),
                                                    sd.diff.Count = sd(stop_summary_2$Count.L2_minus_Count.L1),
                                                    se.diff.Count = sd(stop_summary_2$Count.L2_minus_Count.L1)/sqrt(nrow(stop_summary_2)),
                                                    min.diff.Count = min(stop_summary_2$Count.L2_minus_Count.L1),
                                                    max.diff.Count = max(stop_summary_2$Count.L2_minus_Count.L1)),
                                         
                                         data.frame(n.stops = nrow(stop_summary_2),
                                                    Comparison = "Field Observer (audio review) - Single Listener",
                                                    mean.diff.Richness = mean(stop_summary_2$Richness.F.audio_minus_Richness.L1),
                                                    sd.diff.Richness = sd(stop_summary_2$Richness.F.audio_minus_Richness.L1),
                                                    se.diff.Richness = sd(stop_summary_2$Richness.F.audio_minus_Richness.L1)/sqrt(nrow(stop_summary_2)),
                                                    min.diff.Richness = min(stop_summary_2$Richness.F.audio_minus_Richness.L1),
                                                    max.diff.Richness = max(stop_summary_2$Richness.F.audio_minus_Richness.L1),
                                                    
                                                    mean.diff.Count = mean(stop_summary_2$Count.F.audio_minus_Count.L1),
                                                    sd.diff.Count = sd(stop_summary_2$Count.F.audio_minus_Count.L1),
                                                    se.diff.Count = sd(stop_summary_2$Count.F.audio_minus_Count.L1)/sqrt(nrow(stop_summary_2)),
                                                    min.diff.Count = min(stop_summary_2$Count.F.audio_minus_Count.L1),
                                                    max.diff.Count = max(stop_summary_2$Count.F.audio_minus_Count.L1)),
                                         
                                         data.frame(n.stops = nrow(stop_summary_2),
                                                    Comparison = "Field Observer (audio review) - Double Listener",
                                                    mean.diff.Richness = mean(stop_summary_2$Richness.F.audio_minus_Richness.L2),
                                                    sd.diff.Richness = sd(stop_summary_2$Richness.F.audio_minus_Richness.L2),
                                                    se.diff.Richness = sd(stop_summary_2$Richness.F.audio_minus_Richness.L2)/sqrt(nrow(stop_summary_2)),
                                                    min.diff.Richness = min(stop_summary_2$Richness.F.audio_minus_Richness.L2),
                                                    max.diff.Richness = max(stop_summary_2$Richness.F.audio_minus_Richness.L2),
                                                    
                                                    mean.diff.Count = mean(stop_summary_2$Count.F.audio_minus_Count.L2),
                                                    sd.diff.Count = sd(stop_summary_2$Count.F.audio_minus_Count.L2),
                                                    se.diff.Count = sd(stop_summary_2$Count.F.audio_minus_Count.L2)/sqrt(nrow(stop_summary_2)),
                                                    min.diff.Count = min(stop_summary_2$Count.F.audio_minus_Count.L2),
                                                    max.diff.Count = max(stop_summary_2$Count.F.audio_minus_Count.L2)),
                                         
                                         data.frame(n.stops = nrow(stop_summary_2),
                                                    Comparison = "Field Observer (audio review) - Field",
                                                    mean.diff.Richness = mean(stop_summary_2$Richness.F.audio_minus_Richness.Field),
                                                    sd.diff.Richness = sd(stop_summary_2$Richness.F.audio_minus_Richness.Field),
                                                    se.diff.Richness = sd(stop_summary_2$Richness.F.audio_minus_Richness.Field)/sqrt(nrow(stop_summary_2)),
                                                    min.diff.Richness = min(stop_summary_2$Richness.F.audio_minus_Richness.Field),
                                                    max.diff.Richness = max(stop_summary_2$Richness.F.audio_minus_Richness.Field),
                                                    
                                                    mean.diff.Count = mean(stop_summary_2$Count.F.audio_minus_Count.Field),
                                                    sd.diff.Count = sd(stop_summary_2$Count.F.audio_minus_Count.Field),
                                                    se.diff.Count = sd(stop_summary_2$Count.F.audio_minus_Count.Field)/sqrt(nrow(stop_summary_2)),
                                                    min.diff.Count = min(stop_summary_2$Count.F.audio_minus_Count.Field),
                                                    max.diff.Count = max(stop_summary_2$Count.F.audio_minus_Count.Field))
)

#write.csv(stop_summary_difference_table_2,"./analysis_output/Stop_Summary_DIFFERENCE_Table_Faudio_200stops.csv",row.names=FALSE)

#-----------
# Figure S5
#-----------

# Stop-level richness
pear.corr.a <- cor(stop_summary_2$Richness.F.audio,stop_summary_2$Richness.L2, method = "pearson") %>% round(3)
spear.corr.a <- cor(stop_summary_2$Richness.F.audio,stop_summary_2$Richness.L2, method = "spearman") %>% round(3)
lims.a <- c(0,max(stop_summary_2[,c("Richness.F.audio","Richness.L2")]))
FigS5A <- ggplot(stop_summary_2)+
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(y = jitter(Richness.F.audio, amount = 0.2), x = jitter(Richness.L2, amount = 0.2)), alpha = 0.3, size = 1)+
  coord_cartesian(xlim = lims.a,
                  ylim = lims.a)+
  ylab("# species detected by field observer\n(acoustic review)")+
  xlab("# species detected by second listener")+
  theme_bw()+
  theme(plot.margin = unit(c(3,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.a)*0.01, y = max(lims.a)*0.95), label = paste0("Pearson Corr = ",pear.corr.a,"\nSpearman Corr = ",spear.corr.a), hjust = 0, size = 2)
FigS5A

# Stop-level abundance
pear.corr.b <- cor(stop_summary_2$Count.F.audio,stop_summary_2$Count.L2, method = "pearson") %>% round(3)
spear.corr.b  <- cor(stop_summary_2$Count.F.audio,stop_summary_2$Count.L2, method = "spearman") %>% round(3)
lims.b  <- c(0,max(stop_summary_2[,c("Count.F.audio","Count.L2")]))
FigS5B <- ggplot(stop_summary_2)+
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(y = jitter(Count.F.audio, amount = 0.1), x = jitter(Count.L2, amount = 0.1)), alpha = 0.3, size = 1)+
  
  coord_cartesian(xlim = lims.b ,
                  ylim = lims.b )+
  ylab("# species detected by field observer\n(acoustic review)")+
  xlab("# birds detected by second listener")+
  theme_bw()+
  theme(plot.margin = unit(c(3,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.b )*0.01, y = max(lims.b )*0.95), label = paste0("Pearson Corr = ",pear.corr.b ,"\nSpearman Corr = ",spear.corr.b ), hjust = 0, size = 2)



FigS5 <- plot_grid(FigS5A,FigS5B,nrow = 2, align = "hv",
                   labels = c("A","B"))

#jpeg("./analysis_output/figures/Fig_S5.jpg",width = 4, height = 7, units = "in", res = 600)
print(FigS5)
#dev.off()

#-----------
# Figure S6 - comparison with field data
#-----------

# Stop-level richness
pear.corr.a <- cor(stop_summary_2$Richness.F.audio,stop_summary_2$Richness.Field, method = "pearson") %>% round(3)
spear.corr.a <- cor(stop_summary_2$Richness.F.audio,stop_summary_2$Richness.Field, method = "spearman") %>% round(3)
lims.a <- c(0,max(stop_summary_2[,c("Richness.F.audio","Richness.Field")]))
FigS6A <- ggplot(stop_summary_2)+
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(y = jitter(Richness.F.audio, amount = 0.2), x = jitter(Richness.Field, amount = 0.2)), alpha = 0.3, size = 1)+
  coord_cartesian(xlim = lims.a,
                  ylim = lims.a)+
  ylab("# species detected by field observer\n(acoustic review)")+
  xlab("# species detected by field observer\n(in field)")+
  theme_bw()+
  theme(plot.margin = unit(c(3,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.a)*0.01, y = max(lims.a)*0.95), label = paste0("Pearson Corr = ",pear.corr.a,"\nSpearman Corr = ",spear.corr.a), hjust = 0, size = 2)
FigS6A

# Stop-level abundance
pear.corr.b <- cor(stop_summary_2$Count.F.audio,stop_summary_2$Count.Field, method = "pearson") %>% round(3)
spear.corr.b  <- cor(stop_summary_2$Count.F.audio,stop_summary_2$Count.Field, method = "spearman") %>% round(3)
lims.b  <- c(0,max(stop_summary_2[,c("Count.F.audio","Count.Field")]))
FigS6B <- ggplot(stop_summary_2)+
  geom_abline(intercept = 0, slope = 1, col = "gray85")+
  geom_point(aes(y = jitter(Count.F.audio, amount = 0.1), x = jitter(Count.Field, amount = 0.1)), alpha = 0.3, size = 1)+
  
  coord_cartesian(xlim = lims.b ,
                  ylim = lims.b )+
  ylab("# birds detected by field observer\n(acoustic review)")+
  xlab("# birds detected by field observer\n(in field)")+
  theme_bw()+
  theme(plot.margin = unit(c(3,1,1,1), "lines"))+
  geom_text(aes(x = max(lims.b )*0.01, y = max(lims.b )*0.95), label = paste0("Pearson Corr = ",pear.corr.b ,"\nSpearman Corr = ",spear.corr.b ), hjust = 0, size = 2)



FigS6 <- plot_grid(FigS6A,FigS6B,nrow = 2, align = "hv",
                   labels = c("A","B"))

#jpeg("./analysis_output/figures/Fig_S6.jpg",width = 4, height = 7, units = "in", res = 600)
print(FigS6)
#dev.off()

# -------------------------------------------------------------
# Load Christian's reconciled data
# -------------------------------------------------------------

# --------------
# Determine species that are being mixed up with each other
# --------------
# Read species names
species_list <- read_xlsx(path = "../data/species_list.xlsx")

# Read discrepancy dataframe
friis_reconcile <- read_xlsx("../data/friis_reconcile_merged.xlsx")

# Pull out species mistaken for one another
mistaken <- subset(friis_reconcile, Revised_discrepancy == "Mistaken")

colnames(species_list) <- c("CName_disrepancy", "Correct_species_abbreviation")
mistaken <- full_join(mistaken,species_list)

mistaken$Species_Pair <- NA

# Organize species pairs in alphabetical order
for (i in 1:nrow(mistaken)){
  spec1 <- as.character(mistaken$CName[i])
  spec2 <- as.character(mistaken$CName_disrepancy[i])
  specpair <- c(spec1,spec2) %>% sort()
  specpair <- paste(specpair, collapse = " x ")
  mistaken$Species_Pair[i] <- specpair
}


# Field-audio mixups
mistaken_1 <- subset(mistaken, (Field == 0 & F_audio != 0) | (Field != 0 & F_audio == 0))
mistaken_2 <- subset(mistaken, (Field == 0 & D_audio != 0) | (Field != 0 & D_audio == 0))

# Friis audio vs D_audio mixups
mistaken_3 <- subset(mistaken, (F_audio == 0 & D_audio != 0) | (D_audio != 0 & D_audio == 0))

# --------------
# Create full species list
# --------------
friis_reconcile$Correct_CName <- friis_reconcile$CName
for (i in 1:nrow(friis_reconcile)){
  if (!is.na(friis_reconcile$Correct_species_abbreviation[i]) &
      friis_reconcile$Correct_species_abbreviation[i] %in% species_list$Abbreviation)friis_reconcile$Correct_CName[i] <- species_list$CName[which(species_list$Abbreviation == friis_reconcile$Correct_species_abbreviation[i])]
}

# Limit to only the species Christian determined were present
fr_dat <- friis_reconcile %>%
  select(RouteStopYear, Correct_CName) %>%
  unique() %>%
  rename(CName = Correct_CName)
fr_dat$present.Reconcile <- TRUE
fr_dat$RouteStopYear = as.character(fr_dat$RouteStopYear)

# --------------
# Compare to single/double/field 
# --------------
dat_merge_3 <- dat_merge_2 %>% 
  select(RouteStopYear, CName, present.Field, present.L1, present.L2, present.F.audio) %>%
  subset(RouteStopYear %in% fr_dat$RouteStopYear) %>%
  full_join(., fr_dat) %>%
  arrange(RouteStopYear,CName)

dat_merge_3[is.na(dat_merge_3)] <- FALSE

# Summary of species richness at each stop, as determined by Christian
fr_stop_summary <- fr_dat %>%
  group_by(RouteStopYear) %>%
  summarize(Richness.Reconcile = length(CName))

stop_summary_3 <- dat_merge_3 %>%
  group_by(RouteStopYear) %>%
  summarize(Richness.Field = sum(present.Field),
            Richness.L1 = sum(present.L1),
            Richness.L2 = sum(present.L2),
            Richness.F.audio = sum(present.F.audio),
            Richness.Reconcile = sum(present.Reconcile)) %>%
  as.data.frame()

stop_summary_table_3 <- rbind(data.frame(n.stops = nrow(stop_summary_3),
                                         Method = "Field",
                                         mean.Richness = mean(stop_summary_3$Richness.Field),
                                         sd.Richness = sd(stop_summary_3$Richness.Field),
                                         se.Richness = sd(stop_summary_3$Richness.Field)/sqrt(nrow(stop_summary_3)),
                                         min.Richness = min(stop_summary_3$Richness.Field),
                                         max.Richness = max(stop_summary_3$Richness.Field)),
                              
                              data.frame(n.stops = nrow(stop_summary_3),
                                         Method = "Single Listener",
                                         mean.Richness = mean(stop_summary_3$Richness.L1),
                                         sd.Richness = sd(stop_summary_3$Richness.L1),
                                         se.Richness = sd(stop_summary_3$Richness.L1)/sqrt(nrow(stop_summary_3)),
                                         min.Richness = min(stop_summary_3$Richness.L1),
                                         max.Richness = max(stop_summary_3$Richness.L1)),
                              
                              data.frame(n.stops = nrow(stop_summary_3),
                                         Method = "Double Listener",
                                         mean.Richness = mean(stop_summary_3$Richness.L2),
                                         sd.Richness = sd(stop_summary_3$Richness.L2),
                                         se.Richness = sd(stop_summary_3$Richness.L2)/sqrt(nrow(stop_summary_3)),
                                         min.Richness = min(stop_summary_3$Richness.L2),
                                         max.Richness = max(stop_summary_3$Richness.L2)),
                              
                              data.frame(n.stops = nrow(stop_summary_3),
                                         Method = "Field Observer's Initial Review of Audio",
                                         mean.Richness = mean(stop_summary_3$Richness.F.audio),
                                         sd.Richness = sd(stop_summary_3$Richness.F.audio),
                                         se.Richness = sd(stop_summary_3$Richness.F.audio)/sqrt(nrow(stop_summary_3)),
                                         min.Richness = min(stop_summary_3$Richness.F.audio),
                                         max.Richness = max(stop_summary_3$Richness.F.audio)),
                              
                              data.frame(n.stops = nrow(stop_summary_3),
                                         Method = "Field Observer's Final Review of All Data",
                                         mean.Richness = mean(stop_summary_3$Richness.Reconcile),
                                         sd.Richness = sd(stop_summary_3$Richness.Reconcile),
                                         se.Richness = sd(stop_summary_3$Richness.Reconcile)/sqrt(nrow(stop_summary_3)),
                                         min.Richness = min(stop_summary_3$Richness.Reconcile),
                                         max.Richness = max(stop_summary_3$Richness.Reconcile)))


#write.csv(stop_summary_table_3,"./analysis_output/Stop_Summary_Table_Reconcile_40stops.csv",row.names=FALSE)

# Differences between survey methods
stop_summary_3$Richness.Reconcile_minus_Richness.Field <- stop_summary_3$Richness.Reconcile - stop_summary_3$Richness.Field
stop_summary_3$Richness.Reconcile_minus_Richness.L1 <- stop_summary_3$Richness.Reconcile - stop_summary_3$Richness.L1
stop_summary_3$Richness.Reconcile_minus_Richness.L2 <- stop_summary_3$Richness.Reconcile - stop_summary_3$Richness.L2
stop_summary_3$Richness.Reconcile_minus_Richness.F.audio <- stop_summary_3$Richness.Reconcile - stop_summary_3$Richness.F.audio

stop_summary_difference_table_3 <- rbind(data.frame(n.stops = nrow(stop_summary_3),
                                                    Comparison = "Reconcile - Field",
                                                    mean.diff.Richness = mean(stop_summary_3$Richness.Reconcile_minus_Richness.Field),
                                                    sd.diff.Richness = sd(stop_summary_3$Richness.Reconcile_minus_Richness.Field),
                                                    se.diff.Richness = sd(stop_summary_3$Richness.Reconcile_minus_Richness.Field)/sqrt(nrow(stop_summary_3)),
                                                    min.diff.Richness = min(stop_summary_3$Richness.Reconcile_minus_Richness.Field),
                                                    max.diff.Richness = max(stop_summary_3$Richness.Reconcile_minus_Richness.Field)),
                                         
                                         data.frame(n.stops = nrow(stop_summary_3),
                                                    Comparison = "Reconcile - Single Listener",
                                                    mean.diff.Richness = mean(stop_summary_3$Richness.Reconcile_minus_Richness.L1),
                                                    sd.diff.Richness = sd(stop_summary_3$Richness.Reconcile_minus_Richness.L1),
                                                    se.diff.Richness = sd(stop_summary_3$Richness.Reconcile_minus_Richness.L1)/sqrt(nrow(stop_summary_3)),
                                                    min.diff.Richness = min(stop_summary_3$Richness.Reconcile_minus_Richness.L1),
                                                    max.diff.Richness = max(stop_summary_3$Richness.Reconcile_minus_Richness.L1)),
                                         
                                         data.frame(n.stops = nrow(stop_summary_3),
                                                    Comparison = "Reconcile - Double Listener",
                                                    mean.diff.Richness = mean(stop_summary_3$Richness.Reconcile_minus_Richness.L2),
                                                    sd.diff.Richness = sd(stop_summary_3$Richness.Reconcile_minus_Richness.L2),
                                                    se.diff.Richness = sd(stop_summary_3$Richness.Reconcile_minus_Richness.L2)/sqrt(nrow(stop_summary_3)),
                                                    min.diff.Richness = min(stop_summary_3$Richness.Reconcile_minus_Richness.L2),
                                                    max.diff.Richness = max(stop_summary_3$Richness.Reconcile_minus_Richness.L2)),
                                         
                                         data.frame(n.stops = nrow(stop_summary_3),
                                                    Comparison = "Reconcile - Initial review of audio",
                                                    mean.diff.Richness = mean(stop_summary_3$Richness.Reconcile_minus_Richness.F.audio),
                                                    sd.diff.Richness = sd(stop_summary_3$Richness.Reconcile_minus_Richness.F.audio),
                                                    se.diff.Richness = sd(stop_summary_3$Richness.Reconcile_minus_Richness.F.audio)/sqrt(nrow(stop_summary_3)),
                                                    min.diff.Richness = min(stop_summary_3$Richness.Reconcile_minus_Richness.F.audio),
                                                    max.diff.Richness = max(stop_summary_3$Richness.Reconcile_minus_Richness.F.audio)))

#write.csv(stop_summary_difference_table_3,"./analysis_output/Stop_Summary_DIFFERENCE_Table_Reconcile_40stops.csv",row.names=FALSE)

# # Plot of Christian's reconciled dataset (40 stops)
# Fig_S2A <- ggplot(stop_summary_3)+
#   geom_abline(intercept = 0, slope = 1, col = "gray85")+
#   geom_point(aes(x = jitter(Richness.Field, amount = 0.2), y = jitter(Richness.Reconcile, amount = 0.2)), alpha = 0.3, size = 1)+
#   coord_cartesian(xlim = range(stop_summary_3[,2:6]),
#                   ylim = range(stop_summary_3[,2:6]))+
#   ylab("# species detected\n(comprehensive review)")+
#   xlab("# species detected\n(field survey)")+
#   theme_bw()+
#   theme(plot.margin = unit(c(3,1,1,1), "lines"))
# Fig_S2A
# 
# Fig_S2B <- ggplot(stop_summary_3)+
#   geom_abline(intercept = 0, slope = 1, col = "gray85")+
#   geom_point(aes(x = jitter(Richness.F.audio, amount = 0.2), y = jitter(Richness.Reconcile, amount = 0.2)), alpha = 0.3, size = 1)+
#   coord_cartesian(xlim = range(stop_summary_3[,2:6]),
#                   ylim = range(stop_summary_3[,2:6]))+
#   ylab("# species detected\n(comprehensive review)")+
#   xlab("# species detected\n(acoustic review by field observer)")+
#   theme_bw()+
#   theme(plot.margin = unit(c(3,1,1,1), "lines"))
# Fig_S2B
# 
# Fig_S2C <- ggplot(stop_summary_3)+
#   geom_abline(intercept = 0, slope = 1, col = "gray85")+
#   geom_point(aes(x = jitter(Richness.L1, amount = 0.2), y = jitter(Richness.Reconcile, amount = 0.2)), alpha = 0.3, size = 1)+
#   coord_cartesian(xlim = range(stop_summary_3[,2:6]),
#                   ylim = range(stop_summary_3[,2:6]))+
#   ylab("# species detected\n(comprehensive review)")+
#   xlab("# species detected\n(single listener)")+
#   theme_bw()+
#   theme(plot.margin = unit(c(3,1,1,1), "lines"))
# Fig_S2C
# 
# Fig_S2D <- ggplot(stop_summary_3)+
#   geom_abline(intercept = 0, slope = 1, col = "gray85")+
#   geom_point(aes(x = jitter(Richness.L2, amount = 0.2), y = jitter(Richness.Reconcile, amount = 0.2)), alpha = 0.3, size = 1)+
#   coord_cartesian(xlim = range(stop_summary_3[,2:6]),
#                   ylim = range(stop_summary_3[,2:6]))+
#   ylab("# species detected\n(comprehensive review)")+
#   xlab("# species detected\n(double listener)")+
#   theme_bw()+
#   theme(plot.margin = unit(c(3,1,1,1), "lines"))
# Fig_S2D
# 
# Fig_S2 <- plot_grid(Fig_S2A,Fig_S2B,Fig_S2C,Fig_S2D,
#                     align = "hv", labels = c("(A)","(B)","(C)","(D)"),nrow = 2)
# 
# jpeg("./analysis_output/figures/Fig_S2.jpg",width = 10*2/3, height = 7.5, units = "in", res = 1000)
# print(Fig_S2)
# dev.off()