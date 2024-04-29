# WTF_CH4_CWD

Data for: The problem with estimating global termite emissions

[![DOI](https://zenodo.org/badge/790478324.svg)](https://zenodo.org/doi/10.5281/zenodo.11089743)



## This repository contains the data and analyses used for the manuscript 'The problem with estimating global termite emissions'.

There are 11 .csv files in total, including 'woodblock_termite_presence.csv', 'dead_wood_survey.csv', 'mesocosm_gas_data_raw.csv', 'mesocosm.offsets.csv', 'mesocosm_weights.csv', 'mesocosm_wood_traits.csv', 'mesocosm_processed.csv', 'mesocosm_Q10_raw.csv', 'mesocosm_Q10_processed.csv', 'mound_CH4_emissions.csv' and 'individual_CH4_emissions.csv', and all R-scripts used to run the analyses and plot figures for case studies presented in box 1 and box 2 of the manuscript and to produce figure 3 and figures S4.

## DESCRIPTION OF THE DATA AND FILE STRUCTURE



### Box one: Methane production from deadwood



(1) The dataset 'woodblock_termite_presence.csv' contains identification information on wood blocks which measurements of gas production were taken in Duan et al., 2023 (https://doi.org/10.5194/egusphere-2023-1952), specifically this dataset identifies if termites were visibly present in the wood block when gas emissions were taken. Wood blocks in this dataset and in Duan et al., 2023 can be identified via the same SampleID code. There are 1078 rows of data and 5 columns, each row contains data for a single wood block. NA values indicate no data was collected, any blanks should be interpreted as NA. Descriptions of data for each column are as follows:

"SampleID" = a unique number that identifies each wood block, corresponds with the Sample ID in Duan et al., 2023;

"site" = a unique identification code for each site; 

"ter_in_block" = binary score for presence of termites in wood block (0 = absent; 1 = present); 

"Species.Code" = a unique identification code for the type of wood (ALSC = Alstonia_scholaris; ARPE = Argyrodendron peralatum; CAAU = Castanospermum_australe; CASU = Cardwelia sublimis; CLOB = Cleistanthus oblongifoloius; DYPA = Dysoxylum papuanum; EUCU = Eucalyptus cullenii; EULE = Eucalyptus chlorophylla; MEST = Melaleuca stenostachya; MEVI = Melaleuca viridiflora; MYGL = Myristica globosa; NONO = Normanbya normanbyi; PEBA = Petalostigma banksii ; PIRA = Pinus radiata; ROAN = Rockinghamia angustifolia; SYSA = Syzygium sayeri; TEAR = Terminalia aridicola subsp. chillagoensis); 

"experiment" = description if the decomposition experiment used native wood or non native pine (either Pine gradient or Native gradient);

(2) The dataset 'dead_wood_survey.csv' contains data from deadwood surveys completed at five different sites in Queensland, Australia in 2018. In each site deadwood surveys were completed along a 50 m line transect. For each piece of deadwood, with a diameter >2cm that intersected the transect, length and diameter at opposite ends were measured. There are 347 rows of data and 12 columns, each row contains data for a single piece of deadwood collected from a survey. NA values indicate no data was collected, any blanks should be interpreted as NA. Descriptions of data for each column are as follows:

"site" = a unique identification code for each site;

"year" = year the data was collected (yyyy);

"individual" = unique number identifying each piece of wood measured;

"x_coordinate" = location of deadwood described, point along x-axis of a 50m x 50m grid; 

"y_coordinate" = location of deadwood described, point along y-axis of a 50m x 50m grid;

"diameter_1" = measured diameter at one end of deadwood (cm);

"diameter_2" = measured diameter at opposite end of deadwood (cm);

"length" = length of deadwood (cm);

"snag_fallen" = describes if deadwood was downed (i.e. fallen to the ground) or suspended ( d = downed and s = suspended);

"notes" = additional information provided on the deadwood (string);

"decomposition_class" = score describing th estate of decay of the deadwood ();

"taxonomic_id" = identification code of species of deadwood if known, same codes as used for "Species.Code" in dataset 'termite_presence.csv'.

### Box two: Gas production from termite mesocosms



Ten termite mounds from the same termite species of the genus Microcerotermes were excavated from tropical rainforest in Queensland, Australia, and maintained as mesocosms in an outdoor enclosure. Each termite mound was provided with one of five species of native wood. Mound height and weight were recorded alongside gaseous emissions from each mound at 34, 72, 106, 134, and 237 days since the start of the experiment. Mound weight was estimated from mound volume on each of these days by using regression parameters. Gaseous emissions (CO2 and CH4) were measured with an infrared gas analyser (IRGA) (Los Gatos Ultraportable Greenhouse Gas Analyzer with a LI-COR Long Term Chamber, Model 8100 -104). For each mesocosm, a single termite mound was kept inside a 25-litre plastic, lidded bucket containing between 12-13 kg of sterilised sand and 50g of vermiculite clay. Food and water were separated from the termites to prevent the growth of fungi and instead were connected to the mound via separate clear, plastic tubing.

A single additional Microcerotermes mound was excavated and installed as a laboratory mesocosm (following the above method) and used to investigate the thermal response of termite gaseous emissions. Using a plant growth chamber, relative humidity was kept constant to ca. 62%. The mound was subjected to differing temperatures by altering the ambient temperature. First the mesocosm was cooled to 12.5oC and then heated to 17.5oC, 22.5oC, 27.5oC, 32.5oC, 37.5oC, 42.5oC, 47.5oC and 50oC. The termite mound was left to equilibrate at each temperature for 30 minutes before gaseous emission were measured using the same IRGA. 

(3) The dataset 'mesocosm_gas_data_raw.csv' contains raw IRGA data for measurements of carbon dioxide and methane production from mesocosms. There are 90153 rows of data and 10 columns, each row represents a gas reading from a single mesocosm. NA values indicate no data collected. Data descriptors for each column are as follows:

"Time" = date and time gas measurement was taken (yyyy-mm-dd hh-mm-ss);

"CH4d_ppm" = methane production (in ppm);

"CO2d_ppm" = carbon dioxide production (in ppm);

"GasP_torr" = gas pressure (in torr);

"GasT_C" = gas temperature (in degrees Celsius);

"AmbT_C" = ambient temperature (in degrees Celsius);

"site" = unique string identifying when gas measurement was taken;

"tag" = unique code identifying each mesocosm;

"Etime" = Duration of IRGA measurements (in seconds, counts down -60 to 0 as a calibration period and then up from 0 to 300)

"Tcham" = temperature of chamber (in degrees Celsius).

(4) The dataset 'mesocosm_offsets.csv' contains offset data for each mesocosm, used alongside the datafile 'mesocosm_gas_data_raw.csv to calculate gas production rates for each mound. There are 12 rows of data and three columns, each row is data for a single mesocosm. Data descriptors for each column are as follows:

"genus" = genus of termites in the mesocosm;

"mound_id" = unique code identifying each mesocosm;

"offset" = denotes differences in the chamber volume of the termite mesocosms.

(5) The dataset 'mesocosm_weights.csv' contains data on termite mound dimensions (height and weight) and mesocosm characteristics. There are 90 rows of data and 12 columns, each row is data for a single mesocosm. NA values indicate no data was collected, any blanks should be interpreted as NA. Data descriptors for each column are as follows:

"genus" = genus of termites in the mesocosm;

"mound_id" = unique code identifying each mesocosm;

"total_ht" = height of termite mound from top to bottom (in cm);

"first_half_ht" = height of termite mound from bottom to maximum diameter (in cm);

"second_half_ht" = height of termite mound from maximum diameter to top (in cm);

"maximum_diameter" = diameter of mound at the thickest part of the mound (in cm);

"mound_wt" = weight of the mound (in kg);

"sand_wt" = weight of the sand (in kg);

"bucket_wt" = weight of the bucket without lid (in kg);

"total_weight" = total weight of bucket containing the termite mound (in kg);

"vermiculite_wt" = weight of vermiculite added around termite mound (in kg);

"measurement_date" = date measurements were taken (dd/mm/yyyy).

(6) The dataset 'mesocosm_wood_traits.csv' contains data on the wood fed to each mesocosm. There are 10 rows of data and 12 columns, each row is data for a single mesocosm. Trait values (density,  % C, % N, S.G ratio, pH and C.N ratio) for each species of wood were taken from Law et al., 2023 (https://doi.org/10.1111/1365-2745.14090). Data descriptors for each column are as follows:

"mound_id" = unique code identifying each mesocosm;

"wood_code" =  unique code identifying each wood;

"species_code" =  a unique identification code for the type of wood (ALSC = Alstonia_scholaris; ARPE = Argyrodendron peralatum; CAAU = Castanospermum_australe; CASU = Cardwelia sublimis; CLOB = Cleistanthus oblongifoloius; DYPA = Dysoxylum papuanum; EUCU = Eucalyptus cullenii; EULE = Eucalyptus chlorophylla; MEST = Melaleuca stenostachya; MEVI = Melaleuca viridiflora; MYGL = Myristica globosa; NONO = Normanbya normanbyi; PEBA = Petalostigma banksii ; PIRA = Pinus radiata; ROAN = Rockinghamia angustifolia; SYSA = Syzygium sayeri; TEAR = Terminalia aridicola subsp. chillagoensis);

"initial_weight" = initial weight of wood supp;ied to the mesocosm (in grams);

"final_weight" = final weight of wood left at end of mesocsom experiment (in grams);

"wood_density" = mean wood density (in g per cm3);

"mean_C_%" = mean carbon content (in %);

"mean_N_%" = mean nitrogen content (in %);

"mean_S.G" = ratio of syringyl : guaiacyl;

"mean_pH" = mean pH;

"mean_C.N"= ratio of carbon to nitrogen content;

"wood_deployment" = date wood was given to mesocosm (dd/mm/yyyy).

(7) The dataset 'mesocosm_processed.csv' contains the processed data set with CO2 and CH4 production rates calculated from the raw IRGA measurements given in the datafile 'mesocosm_gas_data.csv'. There are 112 rows of data and 20 columns, each row contains data for a single mesocosm. Data descriptos for each column are as follows:

"site" = unique string identifying when gas measurement was taken;

"tag" = unique code identifying each mesocosm;

"date" = date that measurements were taken (dd/mm/yyyy);

"Tcham" = temperature of the chamber (in degrees Celsius);

"wt_calculated_kg" = calculated weight of mound, estimated from mound volume and using regression parameters from known mound weights and volumes (in kg);

"volume_m3" = volume of mound calculated from diameter and height measurements assuming a paraboloid (in m3);

"resp" = respiration rate (CO2 production) (in ug CO2 g-1(mound) s-1);

"date_since_start" = number of days since the start of the mesocosm experiment;

"methane" = methane production rate (in ug CH4 g-1(mound) s-1);

"avg_wt" = average weight of mound over the duration of the mesocosm experiment (in kg);

"species_code" =  a unique identification code for the type of wood (ALSC = Alstonia_scholaris; ARPE = Argyrodendron peralatum; CAAU = Castanospermum_australe; CASU = Cardwelia sublimis; CLOB = Cleistanthus oblongifoloius; DYPA = Dysoxylum papuanum; EUCU = Eucalyptus cullenii; EULE = Eucalyptus chlorophylla; MEST = Melaleuca stenostachya; MEVI = Melaleuca viridiflora; MYGL = Myristica globosa; NONO = Normanbya normanbyi; PEBA = Petalostigma banksii ; PIRA = Pinus radiata; ROAN = Rockinghamia angustifolia; SYSA = Syzygium sayeri; TEAR = Terminalia aridicola subsp. chillagoensis);

"initial weight of wood supp;ied to the mesocosm (in grams);

"final_weight" = final weight of wood left at end of mesocsom experiment (in grams);

"wood_density" = mean wood density (in g per cm3);

"mean_C" = mean carbon content (in %);

"mean_N" = mean nitrogen content (in %);

"mean_S.G" = ratio of syringyl : guaiacyl;

"mean_pH" = mean pH;

"mean_C.N"= ratio of carbon to nitrogen conten;

"season" = season that measurments were taken (WET or DRY).

(8) The dataset 'mesocosm_Q10_raw.csv' contain raw IRGA data for measurements of carbon dioxide and methane production from a single termite mound exposed to different temperatures. There are 51992 rows of data and 10 columns, each row represents a gas reading from the same mound. NA values indicate no data collected. Data descriptors for each column are as follows:

"Time" = date and time gas measurement was taken (yyyy-mm-dd hh-mm-ss)

"CH4d_ppm" = methane production (in ppm);

"CO2d_ppm" = carbon dioxide production (in ppm);

"GasP_torr" = gas pressure (in torr);

"GasT_C" = gas temperature (in degrees Celsius)

"AmbT_C" = ambient temperature (in degrees Celsius);

"site" = unique code identifying the date when gas measurement was taken;

"tag" = unique code identifying the air temperature when gas measurement was taken;

"Etime" =  Duration of IRGA measurements (in seconds, counts down -60 to 0 as a calibration period and then up from 0 to 300);

"Tcham" = temperature of chamber (in degrees Celsius).

(9) The dataset 'mesocosm_Q10_processed.csv' contains the processed data set with CO2 and CH4 production rates calculated from the raw IRGA measurements given in the datafile 'mesocosm_Q10_raw.csv'. There are 11 rows of data and 5 columns, each row contains data for gas emissions taken from the same mound at a certain temperature. Data descriptors for columns are as follows:

"date" = date measurement was taken (dd/mm/yyyy);

"temp_id" = air temperature when measurments were taken (in degrees Celsius);

"Tcham" = temperature of the chamber (iun degrees Celsius):

"resp" = respiration rate (CO2 production)(in ug CO2 g-1(mound) s-1);

"methane" = methane production rate (in ug CH4 g-1(mound) s-1).

Data from published studies



(10) The dataset 'mound_CH4_emissions.csv' contains data from 14 published studies (and one unpublished) that have measured methane emissions from termite mounds. There are 91 rows of data adn 27 columns, each row contains data for a given estimate of methane emissions from a termite mound in a single study. Multiple estimates are provided by the same study for different termite species or at different times (e.g. different seasons). NA values indicate no data was collected, any blanks should be interpreted as NA. Data descriptors for columns are as follows:

"ID" = unique number identifying for each row of data;

"study" = string identifying the study;

"year" = year the study was carried out (yyyy);

"location" = string describing the study location;

"latitude" = latitude of the study site in decimal degrees (WGS84);

"longitude" = longitude of the study site in decimal degrees (WGS84);

"site" = site name of the study

"biome" = description of the biome of the study site;

"vegetation" = description of the vegetation given of the study site;

"family" = termite family;

"sub_family" = termite subfamily;

"genus" = termite genus;

"species" = termite species;

"food" = feeding substrate of termite species (either wood, wood/grass, wood/soil, soil, grass, fungus, debris);

"method" = description of methosd used to take methane emissions from mound if given;

"nest_type" = description of nest type (either epigeal, subterranean or arboreal);

"number" = number of mounds/nest methane emissions were measured from;

"unit" = unit of methane emissions;

"scale" = scale of methane emissions (either by mound, nest, area (of mound), or hectare);

"methane_emissions" = mean methane emissions;

"MEse" = standard error of methane emisssions if provided; 

"mound_density" = number of mounds per hectare (mounds ha-1);

"mound_biomass_kg" = biomass of termites in mound (g(termite) per kg of mound)

"mound_biomass_md" = biomass of termites in mound (g(termite) per mound)

"mound_SA" = surface area of termite mounds (m2 per hectare);

"note" = information on how mean methane emission was found;

"full_reference" = full reference of the study.

(11) The dataset 'individual_CH4_emissions.csv' contains additional information on data provided on methane emissions from individual termites provided by Zhou et al., 2023 (https://doi.org/10.1002/ecy.3905) available at https://doi.org/10.5061/dryad.vt4b8gtvk. Each study referenced by Zhou et al 2023 was examined for further information. There are 250 rows of data and 8 columns. NA values indicate no data was collected, any blanks should be interpreted as NA. Data descriptors are as follows:

"ID" = unique number identifying for each row of data, corresponds with ID given in Zhou et al., 2023;

"species" = termite species;

"location" = location of where termite species was collected from (minor differences with location given in Zhou et al., 2023);

"latitude" = latitude of the study site in decimal degrees (WGS84);

"longitude" = longitude of the study site in decimal degrees (WGS84);

"new_subfamily_names" = termite subfamily, significant differences to subfamily given in Zhou et al., 2023;

"new_feeding_groups" = termite feeding group, significant difference to feeding group given in Zhou et al., 2023; 

"reference" = full reference of the study.

### USAGE

The data could be processed with almost any program but we used  R version 4.3.1 (2023-06-16) (R Core Team, 2023. R: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing, Vienna, Austria) and the following packages: and the following packages:

"ggplot2", "tidyverse", "gridExtra", "ggpubr", "png", "parameters", "readxl", "viridis", "ggrepel", "lme4", "lmerTest", "ggeffects", "MuMIn", "corrplot", "car", "broom", "lubridate", "rTOC", "nls.multstart", "raster", "sf", "rnaturalearth", "rnaturalearthdata"

There are five R scripts in total, including:

(1) "deadwood_methane.R" is the R script that contains code for all data analyses on methane emissions from deadwood (box one of the main manuscript and supplementary materials S2). Includes code for figure S2 and figure S2.1. This R script uses the provided datasets 'woodblock_termite_presence.csv', 'dead_wood_survey.csv' and downloads an existing dataset 'wood_respiration_rates.csv' by Duan et al., 2023, (https://doi.org/10.5194/egusphere-2023-1952) available at https://github.com/Zanne-Lab/WTF-Climate-Flux/blob/main/weather_flux/data/processed/wood_respiration/wood_respiration_rates.csv.

(2) "process_mesocosm_data.R" is the R script that produces a processed dataset named 'mesocosm_processed.csv' that is used in all subsequent analyses and includes code to produce figure S3.1. The script uses the raw IRGA data from the file 'mesocosm_gas_data_raw.csv' to calulate respiration rates (CO2 production) and methane production rates from each mesocosm. The script also uses additional mesocosm data from the files 'mesocosm_offsets.csv', 'mesocosm_weights.csv' and 'mesocosm_wood_traits.csv'.

(3) "mesocosm_methane.R" is the R script that contains code for all data analyses on gas (CO2 and CH4) production rates from mesocosms (box two of the main manuscript and supplementary materials S3). Includes code to produce figure 4, figure S3.2, figure S3.3, figure S3.4. The script uses the provided dataset 'mesocosm.processed.csv' which is generated from the R.script 'process_mesocosm_data.R'.

(4) "process_Q10_data.R" is the R script that produces a processed dataset named 'mesocosm_Q10_processed.csv'. The scripts uses raw IRGA data from the file 'mesocosm_Q10_raw.csv' to calculate respiration rates (CO2 production) and methane production rates from a single termite mound exposed to different temperatures.

(5) "mesocosm_Q10_est.R" is the R script that contains code to fit thermal response curves to a termite mound and estimate Q10 values. Includes code to produce figure 5.

(6) "figure_3.R" is the R script that contains code to produce figure 3: a map of showing the geographic distribution of studies with estimates of termite methane emissions at the termite mound level and at the individual termtie level. This script uses the provided datasets 'mound_CH4_emissions.csv' and 'individual_CH4_emissions.csv'.

(7) "figure_S4.R" is the R scrip that contains code to produce figures S4.1, S4.2, S4.3 and S4.4 in the supplementary material. The R script uses the provided datasets 'mound_CH4_emissions.csv' and 'individual_CH4_emissions.csv' and the published dataset by Zhou et al., 2023 (https://doi.org/10.1002/ecy.3905) available at https://doi.org/10.5061/dryad.vt4b8gtvk

### FUNDING


Collection of data provided in this repository was supported by awards from the Natural  Environment  Research  Council (NERC) (grant number NE/K01613X/1) and the National Science Foundation (grant number DEB-1655759, DEB-2149151 and DEB-1655340).
