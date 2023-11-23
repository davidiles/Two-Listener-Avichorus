library(readxl)  #for reading excel files
library(dplyr)   #for code formatting
library(reshape2)
library(tidyverse)

rm(list=ls())
setwd("~/1_Work/ECCC/BBS_avichorus/analysis/script")

#------------------------------------------------------------------------------------
# Part 1: Comparison of field observer to their own recording data
#------------------------------------------------------------------------------------

#------------
# Read in avichorus data from Christian Friis
#------------

friis_avi <- read_excel("../data/Avichorus BBS - project tags 2019-05-23.xlsx") %>% 
  as.data.frame() %>% 
  subset(. , TaggingUserID == 2223)

# Split FileName column to extract BBS Route and Year information (equivalent to "text to columns" functionality in Excel)
friis_avi = separate(friis_avi, "OriginalFileName", sep = "_", remove = TRUE, into = c("mmdd","tosplit","BBSStop","ddmmyyyy")) %>%
  separate( . , "tosplit", sep = "-", remove = TRUE, into = c("unkwn","BBSRoute")) %>%
  separate( . , "ddmmyyyy", sep = 8, remove = TRUE, into = c("ddmmyyyy","wav")) %>%
  separate( . , "mmdd", sep = 2, remove = TRUE, into = c("Month","Day")) %>%
  separate( . , "ddmmyyyy", sep = c(2,4), remove = TRUE, into = c("dd2","mm2","Year")) %>%  
  subset( . , select = -c(dd2,mm2,wav)) # Drop irrelevant columns

#Create new column to merge CName and AltTaxa
friis_avi$spec_1 = friis_avi$CName
friis_avi$spec_1[which(is.na(friis_avi$spec_1))] = friis_avi$AltTaxa[which(is.na(friis_avi$spec_1))]

#Fix incorrect Route assignment
friis_avi$BBSRoute[which(friis_avi$BBSRoute == 68055 & friis_avi$Year == 2012)] = 68092
friis_avi$RouteStopYear = paste0(friis_avi$BBSRoute,friis_avi$BBSStop, friis_avi$Year)

friis_avi_uncleaned = friis_avi

#Remove non-target taxa
friis_avi = subset(friis_avi, spec_1 %in% c("Green Frog","Red Squirrel","Northern Leopard Frog","Spring Peeper", "Striped Chorus Frog") == FALSE)

#Replace problematic species names
friis_avi$spec_1[which(friis_avi$spec_1 == "Red-eyed Vireo")] = "Red-eyed / Philadelphia Vireo"
friis_avi$spec_1[which(friis_avi$spec_1 == "Philadelphia Vireo")] = "Red-eyed / Philadelphia Vireo"
friis_avi$spec_1[which(friis_avi$spec_1 == "Nashville / Tennessee Warbler")] = "Unidentified"
friis_avi$spec_1[which(friis_avi$spec_1 == "Unidentified bird")] = "Unidentified"
friis_avi$spec_1[which(friis_avi$spec_1 == "Unidentified duck")] = "Unidentified"
friis_avi$spec_1[which(friis_avi$spec_1 == "Unidentified warbler")] = "Unidentified"
friis_avi$spec_1[which(friis_avi$spec_1 == "Unidentified woodpecker")] = "Unidentified"

friis_avi$spec_1[which(friis_avi$spec_1 == "American Three-toed Woodpecker")] = "Black-backed / American Three-toed Woodpecker"
friis_avi$spec_1[which(friis_avi$spec_1 == "Black-backed Woodpecker")] = "Black-backed / American Three-toed Woodpecker"

#------------
# Read in and format BBS data
#------------
friis_bbs <- read_excel("../data/BBS Routes - Christian Friis - BirdData.xlsx") %>% as.data.frame()
friis_bbs = subset(friis_bbs, select = -c(Country,Scientific_Name,Aou))
colnames(friis_bbs)[5:54] = sprintf("%02d", 1:50)

#*****************************************
#friis_bbs$RYS <- paste0(friis_bbs$Route,friis_bbs$Year,friis_bbs$English_Name)
# No duplicates yet
#*****************************************

friis_bbs = melt(friis_bbs, id=c("State","Route","Year","English_Name"))
colnames(friis_bbs) = c("State","BBSRoute","Year","CName","BBSStop","field")
friis_bbs$BBSRoute = friis_bbs$State * 1000 + friis_bbs$BBSRoute
friis_bbs = subset(friis_bbs, select = -c(State))

#Replace inconsistent species names in bbs data
friis_bbs$CName[which(friis_bbs$CName=="Myrtle Warbler")] = "Yellow-rumped Warbler"
friis_bbs$CName[which(friis_bbs$CName=="Slate-colored Junco")] = "Dark-eyed Junco"
friis_bbs$CName[which(friis_bbs$CName=="Northern Flicker yellow-shafted form")] = "Northern Flicker"
friis_bbs$CName[which(friis_bbs$CName=="Red-eyed Vireo")] = "Red-eyed / Philadelphia Vireo"
friis_bbs$CName[which(friis_bbs$CName=="Philadelphia Vireo")] = "Red-eyed / Philadelphia Vireo"
friis_bbs$CName[which(friis_bbs$CName=="Traill's Flycatcher (Alder/Willow)")] = "Alder Flycatcher"

friis_bbs$CName[which(friis_bbs$CName=="American Three-toed Woodpecker")] = "Black-backed / American Three-toed Woodpecker"
friis_bbs$CName[which(friis_bbs$CName=="Black-backed Woodpecker")] = "Black-backed / American Three-toed Woodpecker"

friis_bbs <- aggregate(field ~ BBSRoute + Year + CName + BBSStop, data = friis_bbs, FUN = sum) #Collect counts

# Create RouteStopYear column
friis_bbs$RouteStopYear <- paste0(friis_bbs$BBSRoute,friis_bbs$BBSStop,friis_bbs$Year)
friis_bbs$RouteYear <- paste0(friis_bbs$BBSRoute,friis_bbs$Year)

#Rename friis_avi$spec_1 column to friis_avi$CName (for merging)
friis_avi$CName = friis_avi$spec_1

colnames(friis_bbs)[5] <- "Count.Field"

Part1 = list(friis_avi = friis_avi,
             friis_avi_uncleaned = friis_avi_uncleaned,
             friis_bbs = friis_bbs)

save(Part1, file = "./summary_data/Part1.RData")



#------------------------------------------------------------------------------------
# Part 2: Comparison of multiple listeners to each other
#------------------------------------------------------------------------------------

rm(list=ls())

# Removal of irrelevant user IDs, Routes, and missing data

# Read in data directly from excel spreadsheet
avi <- readxl::read_excel("../data/Avichorus BBS Bulk Tags report 2019-05-23 correctedStops.xlsx")
avi = as.data.frame(avi) # 10176 rows x 25 columns
avi$BBSStop = sprintf("%02d", avi$BBSStop)

# Select only relevant user IDs
UserID_keep = c("1232","17904","15813",NA)
avi = subset(avi, TaggingUserID %in% UserID_keep) # 9245 rows remaining
avi = subset(avi, CommentingUserID %in% UserID_keep) # 9194 rows remaining

# Rows where Tagger and Commenter are both listed as NA
missing_UserID_rows = which(is.na(avi$TaggingUserID) & is.na(avi$CommentingUserID)) # 1213 rows
avi = avi[-missing_UserID_rows,] # 7981 remaining rows where at least tagger or commenter is present

subset(avi, BBSRoute == "68032" & BBSStop == "21" & RecordingYear == "2013")

#-----------
# Examples of data in need of fixing
#-----------
review_columns = c("TagID","TagBird","TaggingUserID","TagSpecID","TagAltTaxa","TagPublished","TagComment",
                   "CommentingUserID","CommentStatus","CommentSpecID","CommentAltTaxa","CommentComment")

#1. Original tagger examines reviewer ID (and disagrees)
ex1 = subset(avi, BBSRoute == 68285 & BBSStop == 31 & RecordingYear == 2011 )
ex1 = ex1[order(ex1$TagID),review_columns]
#ex1
#Notes: remove cases where original tagger "reviews" commenter's calls

#2. Bird appears 3 times in database (TagID == 38908)
ex2 = subset(avi, BBSRoute == 68286 & BBSStop == 23 & RecordingYear == 2013)
ex2 = ex2[order(ex2$TagID),review_columns]
#ex2
#Notes: this example demonstrates that some birds get erroneously "duplicated"
#       need to remove cases where this happens


#--------
# Remove cases where the bird ID appears more than once in the dataset
# Note: only keep *first* row with bird information
#       "first" is defined by earliest tagging date
#--------
duplicated_birds = unique(avi$TagID[duplicated(avi$TagID)])

for (d in duplicated_birds){

  # Rows in full dataset in which this bird appears
  d_rows = which(avi$TagID == d)
  d_dat = subset(avi, TagID == d)

  # Confirm that TagPublished and CommentPublished are both present
  if (NA %in% d_dat[,c("TagPublished","CommentPublished")]) print(paste("Tag/Comment date missing for bird",d))

  # Confirm TagPublished is earlier than CommentPublished
  if (sum(d_dat$TagPublished >= d_dat$CommentPublished) > 0) print(paste("Tag/Comment date error for bird",d))

  # Identify earliest comment date
  first_comment = min(d_dat$CommentPublished)

  #Only keep first comment
  d_rows_keep = which(avi$TagID == d & avi$CommentPublished == first_comment)[1]
  d_rows_remove = d_rows[which(d_rows != d_rows_keep)]

  avi = avi[-d_rows_remove,] # Remove subsequent comments for this bird (and thus remove additional erroneous rows of data)
}

#-------
# Remove non-target species (squirrels, frogs, etc)
#-------

#Remove nontarget (non avian) species
nontarget = c("Red Squirrel","Spring Peeper","Gray Treefrog","American Toad","Bullfrog","Great Plains Toad", "Striped Chorus Frog")
nontarget_rows = which(avi$TagSpecID %in% nontarget | avi$CommentSpecID %in% nontarget |avi$TagAltTaxa %in% nontarget | avi$CommentAltTaxa %in% nontarget)
avi = avi[-nontarget_rows,]

#--------
# Merge AltTaxa columns with SpecID columns (separately for Tagger and Commenter)
#--------

# Merge the primary and alternate taxa columns (species named are stored in two columns)
avi$TagTaxaMerge = avi$TagSpecID #new column to store the "merged" species ID
avi$TagTaxaMerge[which(is.na(avi$TagSpecID))] = avi$TagAltTaxa[which(is.na(avi$TagSpecID))]

#When CommentSpecID == 0 or CommentSpecID == NA, replace with CommentAltTaxa
avi$CommentTaxaMerge = avi$CommentSpecID
avi$CommentTaxaMerge[which(is.na(avi$CommentTaxaMerge))] = avi$CommentAltTaxa[which(is.na(avi$CommentTaxaMerge))] #Replace NAs
avi$CommentTaxaMerge[which(avi$CommentTaxaMerge == "0")] = avi$CommentAltTaxa[which(avi$CommentTaxaMerge == "0")] #Replace 0s

#--------------
# Isolate the initial tagger and first commenter to be used at each BBS stop
# Only choose ONE Tagger/Commenter pair to be used at each stop
# (subsequent taggers or commenters will be ignored)
# Assign the earliest tagger/commenter pair to "observers 1 and 2".
# Generate detection histories for each bird
#--------------

TaggerCommenter.pairs = data.frame()

# empty dataframe that will store a subset of the full "avi" dataset; only
# information from the initial tagger at a site and the first commenter
avi2 = data.frame()

#Good examples:    r = 68032; s = 22;y = 2011       r = 68032; s = 21; y = 2011

for (r in unique(avi$BBSRoute)){
  for (s in unique(avi$BBSStop)){
    for (y in unique(avi$RecordingYear)){

      #Extract data for this Route/Stop/Year
      rsy_dat = subset(avi, BBSRoute == r & BBSStop == s & RecordingYear == y)
      if (nrow(rsy_dat) == 0) next #skip if no data exists for this combination

      #Extract R/S/Y data where an initial tagger and commenter were both present (otherwise skip this R/S/Y)
      rsy_dat2 = subset(rsy_dat, !is.na(TaggingUserID) & !is.na(CommentingUserID))
      if (nrow(rsy_dat2) == 0) next

      #Relevant timestamps for determining who the tagger/commenter pair will be
      first_tagging_date = min(rsy_dat2$TagPublished) # Earliest tag
      first_commenting_date = min(rsy_dat2$CommentPublished[which(rsy_dat2$TagPublished == first_tagging_date)]) # Earliest comment
      first_tagging_UserID = subset(rsy_dat2, TagPublished == first_tagging_date)$TaggingUserID[1] # UserID of earliest tagger
      first_commenting_UserID = subset(rsy_dat2, TagPublished == first_tagging_date)$CommentingUserID[1] #UserID of earliest commenter

      #Store relevant info in a vector
      rsy_info = data.frame(BBSRoute = r,BBSStop = s,RecordingYear = y,
                            first_tagging_UserID = first_tagging_UserID, first_tagging_date = first_tagging_date,
                            first_commenting_UserID = first_commenting_UserID,first_commenting_date = first_commenting_date)

      #Add to the larger dataframe that stores this information
      TaggerCommenter.pairs = rbind(TaggerCommenter.pairs,rsy_info)

      #Limits the data to *only* birds that were initially identified by either the initial tagger or the commenter
      use_data = subset(rsy_dat,

                        #Initial tagger detections (but before the earliest commenter)
                        (TaggingUserID == first_tagging_UserID & TagPublished <= first_commenting_date) |

                          #Commenter detections (beyond those tagged by initial tagger)
                          (TaggingUserID == first_commenting_UserID & TagPublished >= first_tagging_date)
      )

      # Order the data in a sensible manner
      use_data = use_data[order(use_data$TagTaxaMerge, use_data$TagPublished, use_data$TagID),]

      #------
      # Construct encounter histories for each bird
      #------

      # Columns to store the identity of reviewer 1 and 2
      use_data$UserID_1 = first_tagging_UserID
      use_data$UserID_2 = first_commenting_UserID

      # Columns to store detections (by reviewer 1 and 2)
      use_data$detect_1 = NA
      use_data$detect_2 = NA

      # Columns to store species identities (by reviewer 1 and 2)
      use_data$spec_1 = NA
      use_data$spec_2 = NA

      #
      for (i in 1:nrow(use_data)){

        #Detection History

        #initial tag was by user 1
        if (use_data$TaggingUserID[i] == use_data$UserID_1[i]){
          use_data$detect_1[i] = 1
          use_data$spec_1[i] = use_data$TagTaxaMerge[i]
        }

        if (use_data$TaggingUserID[i] != use_data$UserID_1[i]){
          use_data$detect_1[i] = 0 #not initially detected by user 1
        }

        if (use_data$TaggingUserID[i] == use_data$UserID_2[i]){
          use_data$detect_2[i] = 1 #initially detected by user 2
          use_data$spec_2[i] = use_data$TagTaxaMerge[i]
        }

        #Comment Status will also be used to adjust detection histories
        if (!is.na(use_data$CommentStatus[i])){

          #Presence and identity confirmed by user 2
          if (use_data$CommentStatus[i] == "Confirmed" & use_data$CommentingUserID[i] == use_data$UserID_2[i]){
            use_data$detect_2[i] = 1
            use_data$spec_2[i] = use_data$spec_1[i]
          }

          # Removed by user 2 (thus spec_2 should remain NA)
          if (use_data$CommentStatus[i] == "Removed" & use_data$CommentingUserID[i] == use_data$UserID_2[i]){
            use_data$detect_2[i] = 0
          }

          # Detected by user 2 but identity is challenged
          if (use_data$CommentStatus[i] == "Challenged" & use_data$CommentingUserID[i] == use_data$UserID_2[i]){
            use_data$detect_2[i] = 1
            use_data$spec_2[i] = use_data$CommentTaxaMerge[i]
          }

        }
      }

      # Add to "useable" dataset
      avi2 = rbind(avi2, use_data)
    }
  }
}

#TaggerCommenter.pairs # This dataframe contains the identity of the first and second reviewer of audio files at each BBS stop
#avi2 # This dataframe contains the data that will be used in analysis

#------
# Replace various species names
#------

#Convert indistinguishable vireos
avi2$spec_1[which(avi2$spec_1 == "Red-eyed Vireo")] = "Red-eyed / Philadelphia Vireo"
avi2$spec_1[which(avi2$spec_1 == "Philadelphia Vireo")] = "Red-eyed / Philadelphia Vireo"
avi2$spec_2[which(avi2$spec_2 == "Red-eyed Vireo")] = "Red-eyed / Philadelphia Vireo"
avi2$spec_2[which(avi2$spec_2 == "Philadelphia Vireo")] = "Red-eyed / Philadelphia Vireo"

#Convert indistinguishable flycatchers
#avi2$spec_1[which(avi2$spec_1 == "Least Flycatcher")] = "Least / Yellow-bellied Flycatcher"
#avi2$spec_1[which(avi2$spec_1 == "Yellow-bellied Flycatcher")] = "Least / Yellow-bellied Flycatcher"
#avi2$spec_2[which(avi2$spec_2 == "Least Flycatcher")] = "Least / Yellow-bellied Flycatcher"
#avi2$spec_2[which(avi2$spec_2 == "Yellow-bellied Flycatcher")] = "Least / Yellow-bellied Flycatcher"

#Convert indistinguishable woodpeckers
avi2$spec_1[which(avi2$spec_1 == "Black-backed Woodpecker")] = "Black-backed / American Three-toed Woodpecker"
avi2$spec_1[which(avi2$spec_1 == "American Three-toed Woodpecker")] = "Black-backed / American Three-toed Woodpecker"
avi2$spec_2[which(avi2$spec_2 == "Black-backed Woodpecker")] = "Black-backed / American Three-toed Woodpecker"
avi2$spec_2[which(avi2$spec_2 == "American Three-toed Woodpecker")] = "Black-backed / American Three-toed Woodpecker"

#Assume unidentified Empidonax are Least/Yellow-bellied
avi2$spec_1[which(avi2$spec_1 == "Unidentified Empidonax flycatcher")] = "Unidentified" #"Least / Yellow-bellied Flycatcher"
avi2$spec_2[which(avi2$spec_2 == "Unidentified Empidonax flycatcher")] = "Unidentified" #"Least / Yellow-bellied Flycatcher"

#Ensure all unidentified birds are classified using the same word
avi2$spec_1[which(avi2$spec_1 == "Unidentified bird")] = "Unidentified"
avi2$spec_1[which(avi2$spec_1 == "Unknown species")] = "Unidentified"
avi2$spec_2[which(avi2$spec_2 == "Unidentified bird")] = "Unidentified"
avi2$spec_2[which(avi2$spec_2 == "Unknown species")] = "Unidentified"

#Unidentified species -> convert "Unidentified warbler" to generic "Unidentified"?
avi2$spec_1[which(avi2$spec_1 == "Red-winged / Rusty Blackbird")] = "Unidentified"
avi2$spec_2[which(avi2$spec_2 == "Red-winged / Rusty Blackbird")] = "Unidentified"

avi2$spec_1[which(avi2$spec_1 == "Unidentified warbler")] = "Unidentified"
avi2$spec_2[which(avi2$spec_2 == "Unidentified warbler")] = "Unidentified"
avi2$spec_1[which(avi2$spec_1 == "Unidentified woodpecker")] = "Unidentified"
avi2$spec_2[which(avi2$spec_2 == "Unidentified woodpecker")] = "Unidentified"
avi2$spec_1[which(avi2$spec_1 == "Unidentified duck")] = "Unidentified"
avi2$spec_2[which(avi2$spec_2 == "Unidentified duck")] = "Unidentified"
avi2$spec_1[which(avi2$spec_1 == "Unidentified vireo")] = "Unidentified"
avi2$spec_2[which(avi2$spec_2 == "Unidentified vireo")] = "Unidentified"
avi2$spec_1[which(avi2$spec_1 == "Nashville / Tennessee Warbler")] = "Unidentified"
avi2$spec_2[which(avi2$spec_2 == "Nashville / Tennessee Warbler")] = "Unidentified"

#Examine all rows where species identification was challenged
mis_ident = subset(avi2, spec_1 != spec_2)[,c("spec_1","spec_2")]

#Remove temporary columns
avi2 = avi2[,-which(colnames(avi2) %in% c("TagTaxaMerge","CommentTaxaMerge"))]

# Create RouteStopYear and RouteYear columns
avi2$RouteStopYear <- paste0(avi2$BBSRoute,avi2$BBSStop,avi2$RecordingYear)
avi2$RouteYear <- paste0(avi2$BBSRoute,avi2$RecordingYear)

#Remove one major outlier stop (incomplete data entry)
avi2 = avi2[-which(avi2$RouteStopYear == "68032302011"),]

save(avi2, file = "./summary_data/avi2.RData")

