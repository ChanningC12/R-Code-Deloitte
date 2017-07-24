######################################################################################################################################
###                                                Digital Bag Prioritization Algorithm                                            ###
######################################################################################################################################

## ** 1. Kill both inclusion and exclusion rules ##

# Clear up the space for the job
rm(list=ls()) # remove all objects in the memory
gc() # garbage collection

# Set up working directory
setwd("D:\\")

# Read in library
library(dplyr) # data manipulation
library(ggplot2) # visualization
library(data.table) # fast data table format and file loading

######################################################## 1. Data Loading #############################################################
# Note: At this step, load in the raw 360 data with necessary columns

# Create header names for ads_360 data
header = c("pfz_cust_id",
           "brand",
           "year",
           "emails_dlvd",
           "hqem_dlvd",
           "brand_trx",
           "brand_trx_dol",
           "mrkt_trx",
           "mrkt_trx_dol",
           "overall_details")

# Read in HCP_brand_year level 360 dataset with necessary columns selected (can be adjusted based on the dataset structure)
ads <- fread("Chi Cheng/Master Digital Bag/Data Loading/ads_360_year_1516.csv",header=F,select=c("V1",  # pfz_cust_id
                                                                                                 "V2",  # brand
                                                                                                 "V4",  # year
                                                                                                 "V22", # emails_dlvd
                                                                                                 "V24", # HQ emails_dlvd
                                                                                                 "V66", # brand_trx
                                                                                                 "V67", # brand_trx_dol
                                                                                                 "V58", # mrkt_trx
                                                                                                 "V60", # mrkt_trx_dol
                                                                                                 "V5"  # overall_details
))

# Assign the header accordingly to the 360 dataset
names(ads) = header
summary(ads)

######################################################## 2. Data Preparation #############################################################
### (1) Keep 2016 360 dataset to start with digital bag
digital_bag <- ads[ads$year==2016,]

### (2) Rename the columns properly
names(digital_bag)[names(digital_bag)=="emails_dlvd"] <- "emails_dlvd_2016"

### (3) Drop missing pfz_cust_id (28 records out of 28M)
digital_bag <- digital_bag[!(is.na(digital_bag$pfz_cust_id)),]

### (4) Update the PREVNAR 13 and TRUMENBA brand_trx ad mrkt_trx based on Eric's updated vaccine data (vaccine_doses_output.csv)
# Read in vaccine_doses_output.csv
vaccine <- fread("raw/xfer_May1_2017/vaccine_doses_output.csv")
# Drop additional pfz_cust_id that not appear in the year_360 data for 2016, ~110k unique pfz_cust_id were dropped in this step
vaccine <- vaccine[vaccine$pfz_cust_id %in% unique(digital_bag$pfz_cust_id),] # 618k out of 754k remain
# Outer join the vaccine data with digital_bag dataset, note that new combos of pfz_cust_id/brand came in
digital_bag <- merge(digital_bag,vaccine[,c("pfz_cust_id","brand","brand_trx","brand_trx_dol","mrkt_trx","mrkt_trx_dol")],
                     by=c("pfz_cust_id","brand"),all=T,suffixes = c("","_new"))

# For new combos, update the brand_trx, brand_trx_dol, mrkt_trx and mrkt_trx_dol data with the vaccine data
digital_bag[!(is.na(digital_bag$brand_trx_new)),]$brand_trx <- digital_bag[!(is.na(digital_bag$brand_trx_new)),]$brand_trx_new
digital_bag[!(is.na(digital_bag$brand_trx_dol_new)),]$brand_trx_dol <- digital_bag[!(is.na(digital_bag$brand_trx_dol_new)),]$brand_trx_dol_new
digital_bag[!(is.na(digital_bag$mrkt_trx_new)),]$mrkt_trx <- digital_bag[!(is.na(digital_bag$mrkt_trx_new)),]$mrkt_trx_new
digital_bag[!(is.na(digital_bag$mrkt_trx_dol_new)),]$mrkt_trx_dol <- digital_bag[!(is.na(digital_bag$mrkt_trx_dol_new)),]$mrkt_trx_dol_new

digital_bag[is.na(digital_bag$year),]$year = 2016
digital_bag[is.na(digital_bag$emails_dlvd_2016),]$emails_dlvd_2016 = 0
digital_bag[is.na(digital_bag$hqem_dlvd),]$hqem_dlvd = 0
colSums(is.na(digital_bag))

### (5) Remove the UNKNOWN and UNBRANDED brands from digital bag
digital_bag <- digital_bag[!(digital_bag$brand %in% c("UNBRANDED","UNKNOWN PRODUCT")),-c("brand_trx_new",
                                                                                         "brand_trx_dol_new",
                                                                                         "mrkt_trx_new",
                                                                                         "mrkt_trx_dol_new")]

### (6) Consumer brand additional data put in below based on pch_targetlist_v1_hdfs.csv
# Add an additional column of pch_value_score to for the constrain after the scoring process
pch_targetlist <- fread("Chi Cheng/Master Digital Bag/Data Loading/pch_targetlist_v1_hdfs.csv")
# Drop additional pfz_cust_id that not appear in the year_360 data for 2016
pch_targetlist <- pch_targetlist[!(is.na(pch_targetlist$pfz_cust_id)) & pch_targetlist$pfz_cust_id %in% unique(digital_bag$pfz_cust_id),]
# Calculate total consumer value score by brand
pch_targetlist <- pch_targetlist %>% group_by(brand) %>%
  mutate(pch_Value_Score_brand = sum(pch_Value_Score))
# Percentage of total value score was used to calculate mock brand_trx_dol along with US revenue numbers for ADVIL, NEXIUM 24HR (OTC) and PREPARATION H
pch_targetlist$percent <- pch_targetlist$pch_Value_Score/pch_targetlist$pch_Value_Score_brand
# Revise NEXIUM 24HR (OTC) to NEXIUM
pch_targetlist[pch_targetlist$brand == "NEXIUM 24HR (OTC)",]$brand <- "NEXIUM"
# Calculate mock brand_trx_dol, total PCH brand revenue for ADVIL $539,000,000, NEXIUM 24HR (OTC) $333,000,000, PREPARATION H $107,000,000
pch_targetlist$brand_trx_dol_add <- ifelse(pch_targetlist$brand == "ADVIL", 539000000*pch_targetlist$percent,
                                           ifelse(pch_targetlist$brand == "NEXIUM", 333000000*pch_targetlist$percent,
                                                  ifelse(pch_targetlist$brand == "PREPARATION H", 107000000*pch_targetlist$percent,NA)))
# Merge the additional HCPs/brand combos into the digital bag dataset
digital_bag <- merge(digital_bag, pch_targetlist[,c("pfz_cust_id","brand","brand_trx_dol_add")],
                     by=c("pfz_cust_id","brand"),all = T)
# Impute the missing columns as 0 since we are not using the revenue for PCH brands in scoring
digital_bag[is.na(digital_bag$brand_trx),]$brand_trx <- 0
digital_bag[is.na(digital_bag$brand_trx_dol),]$brand_trx_dol <- 0
digital_bag[is.na(digital_bag$mrkt_trx),]$mrkt_trx <- 0
digital_bag[is.na(digital_bag$mrkt_trx_dol),]$mrkt_trx_dol <- 0
digital_bag[is.na(digital_bag$emails_dlvd_2016),]$emails_dlvd_2016 <- 0
digital_bag[is.na(digital_bag$hqem_dlvd),]$hqem_dlvd <- 0

digital_bag[is.na(digital_bag$year),]$year = 2016
digital_bag[is.na(digital_bag$overall_details),]$overall_details <- 0
colSums(is.na(digital_bag))

### (7) Incorporate onco_brnd_pull_v2.csv
onco_brnd <- fread("Chi Cheng/Master Digital Bag/Data Loading/onco_brnd_pull_v2.csv")
# Drop additional pfz_cust_id that not appear in the year_360 data for 2016
onco_brnd <- onco_brnd[onco_brnd$pfz_cust_id %in% unique(digital_bag$pfz_cust_id),]
# For IBRANCE only, replace mrkt_trx_dol with old mrkt_trx_dol + brand_trx_dol
onco_brnd[onco_brnd$brand == "IBRANCE",]$mrkt_trx_dol <- 
  onco_brnd[onco_brnd$brand == "IBRANCE",]$mrkt_trx_dol + onco_brnd[onco_brnd$brand == "IBRANCE",]$brand_trx_dol
# Outer join the digital bag data with the updated onco_brnd data
digital_bag <- merge(digital_bag, onco_brnd,by=c("pfz_cust_id","brand"),all=T,suffixes = c("","_onco"))
# Impute new brand_trx, mrkt_trx data to replace the raw columns
digital_bag[!(is.na(digital_bag$brand_trx_onco)),]$brand_trx <- digital_bag[!(is.na(digital_bag$brand_trx_onco)),]$brand_trx_onco
digital_bag[!(is.na(digital_bag$brand_trx_dol_onco)),]$brand_trx_dol <- digital_bag[!(is.na(digital_bag$brand_trx_dol_onco)),]$brand_trx_dol_onco
digital_bag[!(is.na(digital_bag$mrkt_trx_onco)),]$mrkt_trx <- digital_bag[!(is.na(digital_bag$mrkt_trx_onco)),]$mrkt_trx_onco
digital_bag[!(is.na(digital_bag$mrkt_trx_dol_onco)),]$mrkt_trx_dol <- digital_bag[!(is.na(digital_bag$mrkt_trx_dol_onco)),]$mrkt_trx_dol_onco

# Impute missing values
digital_bag[is.na(digital_bag$year),]$year <- 2016
digital_bag[is.na(digital_bag$emails_dlvd_2016),]$emails_dlvd_2016 <- 0
digital_bag[is.na(digital_bag$hqem_dlvd),]$hqem_dlvd <- 0
digital_bag[is.na(digital_bag$overall_details),]$overall_details <- 0

# Remove new columns
digital_bag <- digital_bag[,-c("brand_trx_onco","brand_trx_dol_onco","mrkt_trx_onco","mrkt_trx_dol_onco","year_onco")]
colSums(is.na(digital_bag))


### (8) Market Relevance calculation: For each HCP, Market relevance = mrkt_trx_dol / sum(mrkt_trx_dol)
# Aggregate mrkt_trx_dol to hcp level for the calculation of market relevance
digital_bag <- digital_bag %>% 
  group_by(pfz_cust_id) %>%
  dplyr::mutate(
    mrkt_trx_dol_hcp = sum(mrkt_trx_dol,na.rm=T))
# Negative mrkt_trx_dol as 0
digital_bag[digital_bag$mrkt_trx_dol<0,]$mrkt_trx_dol = 0
digital_bag[digital_bag$mrkt_trx_dol_hcp<0,]$mrkt_trx_dol_hcp = 0
# Re-calculate mrkt_rel to avoid NA value and negatives
digital_bag$mrkt_rel <- ifelse(digital_bag$mrkt_trx_dol_hcp==0,0,
                               digital_bag$mrkt_trx_dol / digital_bag$mrkt_trx_dol_hcp)

### (9) Append headroom analysis result to the 360 dataset
# Load headroom dataset
headroom <- fread("Chi Cheng/Master Digital Bag/Data Loading/Headroom_with_updated_onco_bands_0510.csv")

# Digital_bag has ~1.9M HCPs, headroom has ~700k HCPs that wrote at least 1 script
# Merge the two datasets to append the headroom towards the end
digital_bag <- merge(digital_bag,headroom[,c("pfz_cust_id","brand","headroom")],by=c("pfz_cust_id","brand"),all.x=T)
# Impute missing headroom as 0
digital_bag[is.na(digital_bag$headroom),]$headroom <- 0
# Absolute value for headroom
digital_bag$headroom_abs <- abs(digital_bag$headroom)

### (10) Add column Email Saturation as 100
digital_bag$emails_saturation = 100

### (11) Append the persuadability score from persuasion modeling
# Read in persuadability result file
persuasion <- fread("Chi Cheng/Master Digital Bag/Data Loading/HCP_Brand_Persuasion.csv")
names(persuasion)[names(persuasion)=="predicted"] <- "persuadability"
# Merge the persuasion score to digital bag dataset
digital_bag <- merge(digital_bag,persuasion,by=c("pfz_cust_id","brand"),all.x=T)
# Persuadability, impute NA as 0
digital_bag[is.na(digital_bag$persuadability),]$persuadability = 0

### (12) Bring in ama_no_contact and do_not_target as email distribution rule/constrains, also bring in specialty and opt_out_flg
year_360 <- fread("Chi Cheng/Master Digital Bag/Data Loading/year_360_0329.csv",
                  select = c("pfz_cust_id","year","ama_no_contact_max","do_not_target_max","spec_grp_max","opt_out_flg_max"))
year_360 <- year_360[year_360$year==2016,c("pfz_cust_id","ama_no_contact_max","do_not_target_max","spec_grp_max","opt_out_flg_max")]
# Merge the demographic columns with the digital bag data
digital_bag <- merge(digital_bag,year_360,by="pfz_cust_id",all.x=T)
colSums(is.na(digital_bag))

# Add indicator columns for ama_no_contact_ind and do_not_target_ind
digital_bag <- digital_bag %>%
  mutate(
    ama_no_contact_ind = ifelse(ama_no_contact_max=="true",0,1),
    do_not_target_ind = ifelse(do_not_target_max=="true",0,1)
  )

### (13) Adjust Field Force visit, to penalize higher value, take the negative value of overall details so lower FF visits get higher score
digital_bag$overall_details_adj <- -digital_bag$overall_details

### (14) Append target list (result from ccom_rule_out.csv) as an additional attribute in scoring
targetList <- fread("Chi Cheng/Master Digital Bag/Data Loading/targetList_0418.csv")
# Outer join digital bag and targetList, no additional combo
digital_bag <- merge(digital_bag,targetList,by=c("pfz_cust_id","brand"),all=T) # use all=T to include all possible combos
# Assign non-match hcp and brand combo to 1
digital_bag[is.na(digital_bag$approved_target),]$approved_target <- 0

### (15) Email indicator: if hqem_dlvd_hcp>0 in 2016 & opt_out_flg != 1, then email_ind = 1
# Impute missing opt_out_flg_max as 0
digital_bag[is.na(digital_bag$opt_out_flg_max),]$opt_out_flg_max <- 0
# Calculate HCP level hqem_dlvd
digital_bag <- digital_bag %>% group_by(pfz_cust_id) %>%
  dplyr::mutate(
    hqem_dlvd_hcp = sum(hqem_dlvd,na.rm=T)
  )
digital_bag$email_ind <- ifelse(digital_bag$opt_out_flg_max==0 & digital_bag$hqem_dlvd_hcp>0,1,0)
table(digital_bag$email_ind)

### (16) Add Business Unit
# XTANDI is missing in this BU data, XTANDI is PIH
bu <- fread("Chi Cheng/Master Digital Bag/Data Loading/Brand_Bu.csv")
# merge the digital bag data with busienss unit data
digital_bag <- merge(digital_bag, bu, by.x="brand", by.y="Brand", all.x=T)
digital_bag[digital_bag$brand=="XTANDI",]$Bu <- "PIH"

### (17) Finally, assign 0 to negative brand_trx, brand_trx_dol, mrkt_trx, mrkt_trx_dol
digital_bag[digital_bag$brand_trx<0,]$brand_trx <- 0
digital_bag[digital_bag$brand_trx_dol<0,]$brand_trx_dol <- 0
digital_bag[digital_bag$mrkt_trx<0,]$mrkt_trx <- 0
digital_bag[digital_bag$mrkt_trx_dol<0,]$mrkt_trx_dol <- 0

### (18) Add segment information, keep only the 735K HCPs
segment <- fread("Chi Cheng/Master Digital Bag/Data Loading/segment_all.csv")
names(segment)[1] <- "pfz_cust_id"
digital_bag <- merge(digital_bag,segment,by="pfz_cust_id",all.x=T)
digital_bag <- digital_bag[!(is.na(digital_bag$Segment)),]

# Save the copy of dataset
colSums(is.na(digital_bag))
summary(digital_bag)
fwrite(digital_bag,"Chi Cheng/Master Digital Bag/Processed Data/digital_bag_pre_scoring_0525.csv",row.names = F)

######################################################## 3. Scoring ################################################################
# Normalize the inputs (fix scale issue for different attributes) used to calculate scores
# Adjust the distribution before normalization for scoring
digital_bag$brand_trx_dol_pre_score = with(digital_bag,ifelse(brand_trx_dol>5000,5000,brand_trx_dol))
digital_bag$mrkt_rel_pre_score = with(digital_bag,ifelse(mrkt_rel>0.25,0.25,mrkt_rel))
digital_bag$headroom_abs_pre_score = with(digital_bag,ifelse(headroom_abs>10,10,headroom_abs))
digital_bag$persuadability_pre_score = with(digital_bag,ifelse(persuadability>0.10,0.10,persuadability))
digital_bag$overall_details_adj_pre_score = with(digital_bag,ifelse(overall_details_adj<=-10,-10,overall_details_adj))
summary(digital_bag[,c(31:35)])


# Normalization: (Actual - min) / (max() - min())
digital_bag <- digital_bag %>%
  dplyr::mutate(
    brand_trx_dol_nor = (brand_trx_dol_pre_score - min(brand_trx_dol_pre_score)) / (max(brand_trx_dol_pre_score) - min(brand_trx_dol_pre_score)),
    mrkt_rel_nor = (mrkt_rel_pre_score - min(mrkt_rel_pre_score)) / (max(mrkt_rel_pre_score) - min(mrkt_rel_pre_score)),
    headroom_abs_nor = (headroom_abs_pre_score - min(headroom_abs_pre_score)) / (max(headroom_abs_pre_score) - min(headroom_abs_pre_score)),
    persuadability_nor = (persuadability_pre_score - min(persuadability_pre_score)) / (max(persuadability_pre_score) - min(persuadability_pre_score)),
    overall_details_adj_nor = (overall_details_adj_pre_score - min(overall_details_adj_pre_score)) / (max(overall_details_adj_pre_score) - min(overall_details_adj_pre_score))
  )
summary(digital_bag[,c(36:40)])

# Check the number of missing values
colSums(is.na(digital_bag))

# Assign weights: Weights can be adjusted as required
# Weights order: 1.brand_trx_dol, 2.market relevance, 3.headroom (absolute value), 4.persuadabiity, 5.overall_details (adjusted), 6. brand plan (TargetList)

weights_ad <- c(0.3, # 1.brand_trx_dol
             0.2, # 2.market relevance
             0.15, # 3.headroom (absolute value)
             0.2, # 4.persuadabiity
             0.15) # 5.overall_details (adjusted)

weights_ef <- c(0.45, # 1.brand_trx_dol
                 0.25, # 2.market relevance
                 0.1, # 3.headroom (absolute value)
                 0.1, # 4.persuadabiity
                 0.1) # 5.overall_details (adjusted)

# Scoring
digital_bag <- digital_bag %>% 
  mutate(
    # If market_trx_dol at HCP level is 0, then market relevance is not meaningful, use mrkt_trx_dol instead
    # If brand_trx_dol==0 & persuadability==0 & headroom_abs==0 then score is 0
    overall_score_nor = ifelse(brand_trx_dol==0 & persuadability==0 & headroom_abs==0,0,
                               ifelse(Segment %in% c("A","B","C","D"),
                                      weights_ad[1]* brand_trx_dol_nor + weights_ad[2]*mrkt_rel_nor 
                                      + weights_ad[3]*headroom_abs_nor + weights_ad[4]*persuadability_nor + weights_ad[5]*overall_details_adj_nor
                                      ,
                                      ifelse(Segment %in% c("E","F"),
                                             weights_ef[1]* brand_trx_dol_nor + weights_ef[2]*mrkt_rel_nor 
                                             + weights_ef[3]*headroom_abs_nor + weights_ef[4]*persuadability_nor + weights_ef[5]*overall_details_adj_nor
                                                    ,NA))))

# Get summary of the scored dataset
summary(digital_bag)
# Save a copy of output
fwrite(digital_bag,"Chi Cheng/Master Digital Bag/Processed Data/digital_bag_scored_0525.csv",row.names = F)

######################################## 4. Email Distribution based on Score and Rules ###########################################
# Only keep relevant columns in the data
digital_bag_email <- digital_bag[,c("pfz_cust_id","brand","overall_score_nor",
                                    "brand_trx_dol_nor","mrkt_rel_nor","headroom_abs_nor","persuadability_nor","overall_details_adj_nor",
                                    "emails_dlvd_2016","hqem_dlvd","hqem_dlvd_hcp",
                                    "emails_saturation","brand_trx","brand_trx_dol","brand_trx_dol_add",
                                    "mrkt_rel","mrkt_trx","mrkt_trx_dol",
                                    "headroom_abs","overall_details_adj",
                                    "persuadability","ama_no_contact_max","ama_no_contact_ind",
                                    "do_not_target_max","do_not_target_ind","approved_target","email_ind",
                                    "spec_grp_max","Segment","Segment_minor")]

# Business Rules Application
# 1. For a specific brand, if <=100 email were sent in 2016, then assign score should be 0 (XTANDI will have emails)
# Emails delivered by brand in 2016
digital_bag_email <- digital_bag_email %>%
  group_by(brand) %>%
  dplyr::mutate(
    emails_dlvd_2016_brand = sum(emails_dlvd_2016))

# Add an indicator on whether the sum of emails is greater than 0 or not
digital_bag_email$emails_dlvd_ind <- ifelse(digital_bag_email$emails_dlvd_2016_brand>100 | digital_bag_email$brand=="XTANDI",1,0)
n_distinct(digital_bag_email[digital_bag_email$emails_dlvd_ind==1,]$brand) # 56 brands have sent an email in 2016


# 2. For a HCP, if no email was sent in 2016, then assign score should be 0
digital_bag_email <- digital_bag_email %>% 
  group_by(pfz_cust_id) %>%
  dplyr::mutate(emails_dlvd_2016_hcp = sum(emails_dlvd_2016))

# Add an indicator whether a HCP received any email in 2016
digital_bag_email$emails_dlvd_2016_hcp_ind <- ifelse(digital_bag_email$emails_dlvd_2016_hcp>0,1,0)

# 3. Oncology in-scope brand rule
# 10 brands: Ibrance, Torisel, Protonix, Sutent, Xalkori, Xtandi Bosulif, Inlyta, Aromasin, Estring
digital_bag_email$onco_ind <- with(digital_bag_email, ifelse(spec_grp_max == "ONCO" 
                                                       & !(brand %in% c("IBRANCE","TORISEL","PROTONIX","SUTENT",
                                                                        "XALKORI","XTANDI","BOSULIF","INLYTA","AROMASIN",
                                                                        "ESTRING")),0,1))
table(digital_bag_email$onco_ind)

# 4. Rare Disease in-scope specialties
# CARD, ENDO, IND, NEPH, NEUR, NP/PA, OBG, ORTH, PAIN, PCP, PEDs, PULM
digital_bag_email$rd_ind <- with(digital_bag_email,ifelse(brand=="RARE DISEASES" & !(spec_grp_max %in% c("CARD","ENDO","IND","NEPH","NEUR","NP/PA",
                                                                                                   "OBG","ORTH","PAIN","PCP","PEDS","PULM")),
                                                    0,1))
table(digital_bag_email$rd_ind)

# 5. Rheums in-scope brand rule
# Lyrica, Xeljanz, Celebrex, Pristiq, Relpax, Arthrotec, Embeda, Zoloft, Chantix
digital_bag_email$rhu_ind <- with(digital_bag_email,ifelse(spec_grp_max == "RHU" & !(brand %in% c("LYRICA","XELJANZ","CELEBREX","PRISTIQ",
                                                                                            "RELPAX","ARTHROTEC","EMBEDA","ZOLOFT",
                                                                                            "CHANTIX")),0,1))
table(digital_bag_email$rhu_ind)

# Apply business rule to adjust the score before distributing emails
digital_bag_email$overall_score_nor_adj <- with(digital_bag_email,
                                                 overall_score_nor*              # original score
                                                   ama_no_contact_ind*            # ama_no_contact
                                                   do_not_target_ind*             # do_not_target_ind
                                                   email_ind*                     # email address indicator
                                                   emails_dlvd_ind*               # brand emails delivered indicator
#                                                  emails_dlvd_2016_ind*          # HCP brand level emails delivered indicator
                                                   emails_dlvd_2016_hcp_ind*      # HCP level email deliver indicator
                                                   onco_ind*
                                                   rd_ind*
                                                   rhu_ind
)

# Add digital bag indicator
digital_bag_email$digital_bag_ind <- with(digital_bag_email,
                                          ama_no_contact_ind*do_not_target_ind*email_ind*emails_dlvd_ind*
#                                           emails_dlvd_2016_ind*
                                            emails_dlvd_2016_hcp_ind*onco_ind*rd_ind*rhu_ind)
table(digital_bag_email$digital_bag_ind) # 7M fall into digital bag out of 20M combos
with(digital_bag_email,tapply(overall_score_nor_adj,digital_bag_ind,mean))

# 6. For 8th, 9th and 10th brand by each HCP, assign the score to 0 if the score is less than bottom 30% percentile
# Additionally, for ONCO and RHU, 8th, 9th, 10th brand score will be 0 since the bag maximum brand is 7
# Rank the scores by brand and get Top N brands for each HCP
# Rank the digital bag dataset by overall score
digital_bag_email <- digital_bag_email %>%
  dplyr::arrange(pfz_cust_id,-overall_score_nor_adj) %>%
  dplyr::group_by(pfz_cust_id) %>%
  dplyr::mutate(
    rank = rank(-overall_score_nor_adj,ties.method = "min")
  )

# Determine the cutoff for 8th, 9th, 10th brand
digital_bag_email_brand_cutoff <- digital_bag_email %>%
  filter(rank>=8,rank<=10) %>%
  group_by(rank) %>%
  dplyr::summarize(
    score_cutoff = quantile(overall_score_nor_adj,probs=0.3) # bottom 30 percentile cutoff
  )

# Assign cutoff for top 8th, 9th and 10th brand in the dataset
digital_bag_email$cutoff_top_8 = ifelse(digital_bag_email$rank==8,digital_bag_email_brand_cutoff[digital_bag_email_brand_cutoff$rank==8,]$score_cutoff,NA)
digital_bag_email$cutoff_top_9 = ifelse(digital_bag_email$rank==9,digital_bag_email_brand_cutoff[digital_bag_email_brand_cutoff$rank==9,]$score_cutoff,NA)
digital_bag_email$cutoff_top_10 = ifelse(digital_bag_email$rank==10,digital_bag_email_brand_cutoff[digital_bag_email_brand_cutoff$rank==10,]$score_cutoff,NA)

# Add an indicator whether the score for top 8th, 9th and 10th brands are less than the cutoffs
# In addition, force 8th, 9th, 10th brand score for RHU to be 0
digital_bag_email$cutoff_top_8_ind = with(digital_bag_email,ifelse(rank==8 
                                                                   & (overall_score_nor_adj<cutoff_top_8 |
                                                                        spec_grp_max %in% c("RHU")),0,1))
digital_bag_email$cutoff_top_9_ind = with(digital_bag_email,ifelse(rank==9  
                                                                   & (overall_score_nor_adj<cutoff_top_9 |
                                                                        spec_grp_max %in% c("RHU")),0,1))
digital_bag_email$cutoff_top_10_ind = with(digital_bag_email,ifelse(rank==10  
                                                                    & (overall_score_nor_adj<cutoff_top_10 |
                                                                         spec_grp_max %in% c("RHU")),0,1))

# Adjust the score based on 8th, 9th and 10th brand indicator
digital_bag_email$overall_score_nor_adj_final = with(digital_bag_email,
                                                      overall_score_nor_adj*cutoff_top_8_ind*cutoff_top_9_ind*cutoff_top_10_ind)
summary(digital_bag_email$overall_score_nor_adj_final)

# Percentage of overall score by hcp for top 10 brands by HCP
# Add an indicator on top 10 brand for each HCP
digital_bag_email_10 <- digital_bag_email %>%
  group_by(pfz_cust_id) %>%
  dplyr::mutate(top_10_ind = ifelse(rank<=10,1,0))

# Calculate the total overall scores for each HCP by top_10_ind
digital_bag_email_10 <- digital_bag_email_10 %>%
  group_by_(.dots=c("pfz_cust_id","top_10_ind")) %>%
  dplyr::mutate(overall_score_nor_adj_hcp_topTen = sum(overall_score_nor_adj_final,na.rm=T))

summary(digital_bag_email_10$overall_score_nor_adj_hcp_topTen)

# Adjusted percentage for top 10 brands by each HCP
digital_bag_email_10 <- digital_bag_email_10 %>%
  dplyr::mutate(
    percentage_10 = ifelse(top_10_ind==1,overall_score_nor_adj_final/overall_score_nor_adj_hcp_topTen,0))

summary(digital_bag_email_10$percentage_10)

# Determine the distribution of emails
digital_bag_email_10 <- digital_bag_email_10 %>% 
  dplyr::mutate(
    emails_pred_10 = ifelse(is.na(percentage_10),0,round(emails_saturation*percentage_10,0)))

summary(digital_bag_email_10)

# Save intermediate output
fwrite(digital_bag_email_10,"Chi Cheng/Master Digital Bag/Processed Data/digital_bag_output_business_rule_0525.csv",row.names = F)

# 7. Headroom and Brand_TRx Constrain
# 1) If predicted email is less than 5 & (brand_trx + headroom) by brand is above top 25% percentile, then assign 5 emails
# 2) If for PCH brands, the HCP is above top 25 percentile threshold then assign 5 emails

# Consolidate brand_trx_dol and brand_trx_dol_adj for percentile calculation
digital_bag_email_10$brand_trx_dol_headroom <- ifelse(!(is.na(digital_bag_email_10$brand_trx_dol_add)),
                                                      digital_bag_email_10$brand_trx_dol_add,
                                                      digital_bag_email_10$brand_trx_dol + digital_bag_email_10$headroom_abs)

# By brand, get the cutoff points for top 25% on headroom+brand_trx_dol
digital_bag_email_10 <- digital_bag_email_10 %>% 
  group_by(brand) %>% 
  dplyr::mutate(
    cutoff_brand_trx_dol_headroom = quantile(brand_trx_dol_headroom,probs = 0.75))

# Create an indicator on whether the rule needs to be applied
digital_bag_email_10$brand_trx_dol_headroom_ind = 
  with(digital_bag_email_10,
       ifelse(is.na(digital_bag_email_10$brand_trx_dol_add) & rank<=10 & 
                brand_trx_dol_headroom > cutoff_brand_trx_dol_headroom & emails_pred_10<5 & 
                emails_pred_10>0,1,
              ifelse(!(is.na(digital_bag_email_10$brand_trx_dol_add)) &
                       digital_bag_ind  == 1 & 
                       brand_trx_dol_headroom > cutoff_brand_trx_dol_headroom,1,0)))

table(digital_bag_email_10$brand_trx_dol_headroom_ind) # 172k out of 27M records need to be adjusted
n_distinct(digital_bag_email_10[digital_bag_email_10$brand_trx_dol_headroom_ind==1,]$pfz_cust_id) # will affect 110k unique HCPs

# Reallocate the remaining emails distribution
# Get the count of brands need to be adjusted by HCP
digital_bag_email_10 <- digital_bag_email_10 %>% 
  group_by(pfz_cust_id) %>%
  dplyr::mutate(count = sum(brand_trx_dol_headroom_ind))

# Remaining emails to be distributed for each HCP
digital_bag_email_10 <- digital_bag_email_10 %>% mutate(
  emails_saturation_adj = 100-5*count)
table(digital_bag_email_10$emails_saturation_adj)

# New email distribution for adjusted cases and new top_10_ind
digital_bag_email_10$top_10_ind_adj = with(digital_bag_email_10,ifelse(top_10_ind==1 & brand_trx_dol_headroom_ind==0,1,0))

# Re-calculate the percentage
digital_bag_email_10 <- digital_bag_email_10 %>%
  group_by_(.dots=c("pfz_cust_id","top_10_ind_adj")) %>%
  dplyr::mutate(overall_score_nor_adj_hcp_topTen_adj = sum(overall_score_nor_adj_final,na.rm=T))

# Adjusted percentage for top 10 brands
digital_bag_email_10 <- digital_bag_email_10 %>%
  dplyr::mutate(percentage_10_adj = ifelse(top_10_ind_adj==1,overall_score_nor_adj_final/overall_score_nor_adj_hcp_topTen_adj,0))

summary(digital_bag_email_10$percentage_10_adj)

# Determine the distribution of emails
digital_bag_email_10$emails_pred_10_adj = with(digital_bag_email_10,
                                               ifelse(is.na(percentage_10_adj),0,
                                                      ifelse(brand_trx_dol_headroom_ind==1,5,
                                                             round(emails_saturation_adj*percentage_10_adj,0))))
summary(digital_bag_email_10)

# 8. Apply Distribution Rules
# Four level of emails: 
# 1) Predicted emails 1-4.5 OR email_dlvd_2016 1-4.5 : 3
# 2) Predicted emails 4.5-9 OR email_dlvd_2016 4.5-9: 6
# 3) Predicted emails 9-18 OR email_dlvd_2016 9-18: 12
# 4) Predicted emails 18+ OR email_dlvd_2016 18+: 24
# Constrain: if emails_dlvd_2016_hcp <= 6 OR FF visit by brand <= 100, then apply 3/6/12 rule only
# Constrain: if maximum emails sent by brand < 3, then apply 3/6 rule only
# Constrain: if brand is Rare Disease, apply 3 rule only
# Constrain: if brand is Advil, Nexium, Centrum, Preparation H, then apply 3/6 rule

# Calculate ff visit by brand
digital_bag_email_10 <- digital_bag_email_10 %>% 
  group_by(brand) %>%
  dplyr::mutate(ff_visit = sum(-overall_details_adj))
# Calculate maximum emails sent by brand in 2016
brand_email_max <- digital_bag_email_10 %>% group_by(brand) %>%
  dplyr::summarize(
    max_email_2016 = max(emails_dlvd_2016))

brand_max_list <- unique(brand_email_max[brand_email_max$max_email_2016>0 & brand_email_max$max_email_2016<3,]$brand)

# Re-allocate the emails prediction based on conditions
digital_bag_email_10$emails_pred_final = 
  with(digital_bag_email_10,ifelse(
    emails_pred_10_adj==0,0,ifelse(
      emails_pred_10_adj<4.5 | emails_dlvd_2016<4.5 | brand=="RARE DISEASES",3,ifelse(
        brand %in% brand_max_list | brand %in% c("ADVIL","NEXIUM","CENTRUM","PREPARATION H"),6,ifelse(
          emails_pred_10_adj<9 | emails_dlvd_2016<9,6,ifelse(
            emails_dlvd_2016_hcp<=6 | ff_visit<=100,12,ifelse(
              emails_pred_10_adj<18 | emails_dlvd_2016<18,12,24)))))))

# Adjust for Xtandi allocation
digital_bag_email_10$emails_pred_final = 
  with(digital_bag_email_10,ifelse(
    !(brand %in% c("XTANDI")),emails_pred_final,ifelse(
      emails_pred_10_adj==0,0,ifelse(
        emails_pred_10_adj<4.5,3,ifelse(
          emails_pred_10_adj<9,6,ifelse(
            emails_pred_10_adj<18,12,24))))))

summary(digital_bag_email_10$emails_pred_final)
table(digital_bag_email_10$emails_pred_final)

# Summary by HCP
digital_bag_email_hcp <- digital_bag_email_10 %>% group_by(pfz_cust_id) %>%
  dplyr::summarize(
    email_dlvd_2016 = sum(emails_dlvd_2016,na.rm=T),
    emails_pred_tot = sum(emails_pred_final,na.rm=T))

mean(digital_bag_email_hcp$emails_pred_tot) # average emails predicted by HCP (entire population)
mean(digital_bag_email_hcp[digital_bag_email_hcp$emails_pred_tot>0,]$emails_pred_tot) # average emails predicted by HCP (HCPs who received at 1 email)

# Save output
fwrite(digital_bag_email_10,"Chi Cheng/Master Digital Bag/Output Data/digital_bag_email_distribution_top10_0525.csv",row.names = F)
