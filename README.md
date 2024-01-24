# Overview

This repo contains code to evaluate the effect of having multiple listeners review acoustic recordings for estimating bird population sizes and species richness, compared to a single listener.

The scripts in the analysis folder achieve the following:

`0_route_map.R` creates a map of the study area and BBS route locations.

`1_prepare_data.R` processes a large file of user tags, extracts tags created by the specific listeners who were contracted for this study, and formats them for analysis.  Also performs some data cleaning.

`2_analysis_listener_discrepancies.R` evaluates discrepancies between first and second listeners.  This script counts the number of previously detected birds that were confirmed, removed, and modified by second listeners, as well as the number of new birds detected by the second listeners.

`3_analysis_listener_combinations.R` conducts the same analysis as script 2, but breaks the analysis down into individual listener combinations to evaluate if particular listeners were more/less likely to add, delete, or modify records of other listeners.  This is meant to examine individual observer differences.

`4_analysis_stop_species_inference_bootstrap.R` examines the overall effect on species estimates across 690 BBS stops (in terms of species occurrence and relative abundance).  Does a second listener appreciably change the resultant estimates for any species? How similar are listener estimates to those from field surveys?

