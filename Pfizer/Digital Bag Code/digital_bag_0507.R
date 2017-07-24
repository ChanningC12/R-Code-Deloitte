######################################################################################################################################
###                                                Digital Bag Prioritization Algorithm                                            ###
######################################################################################################################################

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
           "brand_trx",
           "brand_trx_dol",
           "mrkt_trx",
           "mrkt_trx_dol",
           "overall_details")

# Read in HCP_brand_year level 360 dataset with necessary columns selected (can be adjusted based on the dataset structure)
ads <- fread("raw/xfer_March21_2017/ads_360_year_1516.csv",header=F,select=c("V1",  # pfz_cust_id
                                                                             "V2",  # brand
                                                                             "V4",  # year
                                                                             "V22", # emails_dlvd
                                                                             "V66", # brand_trx
                                                                             "V67", # brand_trx_dol
                                                                             "V58", # mrkt_trx
                                                                             "V60", # mrkt_trx_dol
                                                                             "V5"   # overall_details
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
colSums(is.na(digital_bag))

### (5) Remove the UNKNOWN and UNBRANDED brands from digital bag
digital_bag <- digital_bag[!(digital_bag$brand %in% c("UNBRANDED","UNKNOWN PRODUCT")),-c("brand_trx_new",
                                                                                         "brand_trx_dol_new",
                                                                                         "mrkt_trx_new",
                                                                                         "mrkt_trx_dol_new")]

### (6) Consumer brand additional data put in below based on pch_targetlist_v1_hdfs.csv
# Add an additional column of pch_value_score to for the constrain after the scoring process
pch_targetlist <- fread("D:/raw/xfer_May1_2017/pch_targetlist_v1_hdfs.csv")
# Drop additional pfz_cust_id that not appear in the year_360 data for 2016
pch_targetlist <- pch_targetlist[!(is.na(pch_targetlist$pfz_cust_id)) & pch_targetlist$pfz_cust_id %in% unique(digital_bag$pfz_cust_id),]
# Calculate total consumer value score by brand
pch_targetlist <- pch_targetlist %>% group_by(brand) %>%
  mutate(pch_Value_Score_brand = sum(pch_Value_Score))
# Percentage of total value score was used to calculate mock brand_trx_dol along with US revenue numbers for ADVIL, NEXIUM 24HR (OTC) and PREPARATION H
pch_targetlist$percent <- pch_targetlist$pch_Value_Score/pch_targetlist$pch_Value_Score_brand
# Calculate mock brand_trx_dol, total PCH brand revenue for ADVIL $539,000,000, NEXIUM 24HR (OTC) $333,000,000, PREPARATION H $107,000,000
pch_targetlist$brand_trx_dol_add <- ifelse(pch_targetlist$brand == "ADVIL", 539000000*pch_targetlist$percent,
                                           ifelse(pch_targetlist$brand == "NEXIUM 24HR (OTC)", 333000000*pch_targetlist$percent,
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

digital_bag[is.na(digital_bag$year),]$year = 2016
digital_bag[is.na(digital_bag$overall_details),]$overall_details <- 0
colSums(is.na(digital_bag))

### (7) Market Relevance calculation: For each HCP, Market relevance = mrkt_trx_dol / sum(mrkt_trx_dol)
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

### (8) Append headroom analysis result to the 360 dataset
# Load headroom dataset
headroom <- fread("Soumyajit/Output/Brand_Reports/Spec_Headroom/hcplist_prevnar_trumenba_0502.csv")

# Digital_bag has ~1.9M HCPs, headroom has ~700k HCPs that wrote at least 1 script
# Merge the two datasets to append the headroom towards the end
digital_bag <- merge(digital_bag,headroom[,c("pfz_cust_id","brand","headroom")],by=c("pfz_cust_id","brand"),all.x=T)
# Impute missing headroom as 0
digital_bag[is.na(digital_bag$headroom),]$headroom <- 0
# Absolute value for headroom
digital_bag$headroom_abs <- abs(digital_bag$headroom)

### (9) Add column Email Saturation as 100
digital_bag$emails_saturation = 100

### (10) Append the persuadability score from persuasion modeling
# Read in persuadability result file
persuasion <- fread("Model Ready Data/Persuasion/Results/HCP_Brand_Persuasion.csv")
names(persuasion)[names(persuasion)=="predicted"] <- "persuadability"
# Merge the persuasion score to digital bag dataset
digital_bag <- merge(digital_bag,persuasion,by=c("pfz_cust_id","brand"),all.x=T)
# Persuadability, impute NA as 0
digital_bag[is.na(digital_bag$persuadability),]$persuadability = 0

### (11) Bring in ama_no_contact and do_not_target as email distribution rule/constrains, also bring in specialty
year_360 <- fread("Chi Cheng/Processed Data/year_360_0329.csv",
                  select = c("pfz_cust_id","year","ama_no_contact_max","do_not_target_max","spec_grp_max"))
year_360 <- year_360[year_360$year==2016,c("pfz_cust_id","ama_no_contact_max","do_not_target_max","spec_grp_max")]
# Merge the demographic columns with the digital bag data
digital_bag <- merge(digital_bag,year_360,by="pfz_cust_id",all.x=T)
colSums(is.na(digital_bag))

# Add indicator columns for ama_no_contact_ind and do_not_target_ind
digital_bag <- digital_bag %>%
  mutate(
    ama_no_contact_ind = ifelse(ama_no_contact_max=="true",0,1),
    do_not_target_ind = ifelse(do_not_target_max=="true",0,1)
  )

### (12) Adjust Field Force visit, to penalize higher value, take the negative value of overall details so lower FF visits get higher score
digital_bag$overall_details_adj <- -digital_bag$overall_details

### (13) Append target list (result from ccom_rule_out.csv) as an additional attribute in scoring
targetList <- fread("Chi Cheng/Processed Data/targetList_0418.csv")
# Outer join digital bag and targetList, no additional combo
digital_bag <- merge(digital_bag,targetList,by=c("pfz_cust_id","brand"),all=T) # use all=T to include all possible combos
# Assign non-match hcp and brand combo to 0
digital_bag[is.na(digital_bag$approved_target),]$approved_target <- 0

### (14) Append Email id appearence indicator as a constrain in email distribution process
email <- fread("Chi Cheng/Processed Data/email_address_extract_ind.csv")
names(email)[names(email)=="ind"]="email_ind"
digital_bag <- merge(digital_bag,email,by="pfz_cust_id",all.x=T)
# Impute non-match cases as 0
digital_bag[is.na(digital_bag$email_ind),]$email_ind = 0
table(digital_bag$email_ind)

### (15) Append BU (Business Unit)
bu <- fread("D:/Processed Data/Brand_Bu.csv")
digital_bag <- merge(digital_bag,bu,by.x="brand",by.y="Brand",all.x = T)

### (16) Finally, assign 0 to negative brand_trx, brand_trx_dol, mrkt_trx, mrkt_trx_dol
digital_bag[digital_bag$brand_trx<0,]$brand_trx <- 0
digital_bag[digital_bag$brand_trx_dol<0,]$brand_trx_dol <- 0
digital_bag[digital_bag$mrkt_trx<0,]$mrkt_trx <- 0
digital_bag[digital_bag$mrkt_trx_dol<0,]$mrkt_trx_dol <- 0

# Save the copy of dataset
colSums(is.na(digital_bag))
summary(digital_bag)
fwrite(digital_bag,"Z:/Derived/Processed Digital Bag Dataset/digital_bag_0502.csv",row.names = F)

######################################################## 3. Scoring ################################################################
# Load in intermediate preProcessed data
digital_bag <- fread("Z:/Derived/Processed Digital Bag Dataset/digital_bag_0502.csv")

# Normalize the inputs (fix scale issue for different attributes) used to calculate scores
# Normalization: (Actual - min) / (max() - min())
digital_bag <- digital_bag %>%
  mutate(
    brand_trx_dol_nor = (brand_trx_dol - min(brand_trx_dol)) / (max(brand_trx_dol) - min(brand_trx_dol)),
    mrkt_rel_nor = (mrkt_rel - min(mrkt_rel)) / (max(mrkt_rel) - min(mrkt_rel)),
    headroom_abs_nor = (headroom_abs - min(headroom_abs)) / (max(headroom_abs) - min(headroom_abs)),
    persuadability_nor = (persuadability - min(persuadability)) / (max(persuadability) - min(persuadability)),
    overall_details_adj_nor = (overall_details_adj - min(overall_details_adj)) / (max(overall_details_adj) - min(overall_details_adj)),
    mrkt_trx_dol_nor = (mrkt_trx_dol - min(mrkt_trx_dol)) / (max(mrkt_trx_dol) - min(mrkt_trx_dol))
  )

# Square root arcsin transformation, transform to smooth the skewness
# First do the square root transformation
digital_bag <- digital_bag %>%
  mutate(
    brand_trx_dol_sqrt = sqrt(brand_trx_dol_nor),
    mrkt_rel_sqrt = sqrt(mrkt_rel_nor),
    headroom_abs_sqrt = sqrt(headroom_abs_nor),
    persuadability_sqrt = sqrt(persuadability_nor),
    overall_details_adj_sqrt = sqrt(overall_details_adj_nor),
    mrkt_trx_dol_sqrt = sqrt(mrkt_trx_dol_nor)
  )

# Then do arcsin transformation
digital_bag <- digital_bag %>%
  mutate(
    brand_trx_dol_asin = asin(brand_trx_dol_sqrt),
    mrkt_rel_asin = asin(mrkt_rel_sqrt),
    headroom_abs_asin = asin(headroom_abs_sqrt),
    persuadability_asin = asin(persuadability_sqrt),
    overall_details_adj_asin = asin(overall_details_adj_sqrt),
    mrkt_trx_dol_asin = asin(mrkt_trx_dol_sqrt)
  )

# Check the number of missing values
colSums(is.na(digital_bag))

# Assign weights: Weights can be adjusted as required
# Apply 0.3 on brand_trx, 0,1 on market relevance, 0.2 on rest
# Weights order: 1.brand_trx_dol, 2.market relevance, 3.headroom (absolute value), 4.persuadabiity, 5.overall_details (adjusted), 6. brand plan (TargetList)

weights <- c(0.3, # 1.brand_trx_dol
             0.1, # 2.market relevance
             0.2, # 3.headroom (absolute value)
             0.2, # 4.persuadabiity
             0.2, # 5.overall_details (adjusted)
             0.2) # 6. brand plan (TargetList)

# Scoring
digital_bag <- digital_bag %>% 
  mutate(
    # If market_trx_dol at HCP level is 0, then market relevance is not meaningful, use mrkt_trx_dol instead
    # If brand_trx_dol==0 & persuadability==0 & headroom_abs==0 then score is 0
    overall_score_asin = ifelse(brand_trx_dol==0 & persuadability==0 & headroom_abs==0,0,
                                ifelse(mrkt_trx_dol_hcp==0,
                                       weights[1]*brand_trx_dol_asin + weights[2]*mrkt_trx_dol_asin + 
                                         weights[3]*headroom_abs_asin + weights[4]*persuadability_asin + weights[5]*overall_details_adj_asin
                                       +weights[6]*approved_target,
                                       weights[1]* brand_trx_dol_asin + weights[2]*mrkt_rel_asin 
                                       + weights[3]*headroom_abs_asin + weights[4]*persuadability_asin + weights[5]*overall_details_adj_asin
                                       +weights[6]*approved_target)))

# Get summary of the scored dataset
summary(digital_bag)
# Save a copy of output
fwrite(digital_bag,"Z:/Derived/Processed Digital Bag Dataset/digital_bag_score_0505_v2.csv",row.names = F)

######################################## 4. Email Distribution based on Score and Rules ###########################################
# Only keep relevant columns in the data
digital_bag_email <- digital_bag[,c("pfz_cust_id","brand","overall_score_asin",
                                    "brand_trx_dol_asin","mrkt_trx_dol_asin","mrkt_rel_asin","headroom_abs_asin",
                                    "persuadability_asin","overall_details_adj_asin",
                                    "emails_dlvd_2016",
                                    "emails_saturation","brand_trx","brand_trx_dol","brand_trx_dol_add",
                                    "mrkt_rel","mrkt_trx","mrkt_trx_dol",
                                    "headroom_abs","overall_details_adj",
                                    "persuadability","ama_no_contact_max","ama_no_contact_ind",
                                    "do_not_target_max","do_not_target_ind","approved_target","email_ind",
                                    "spec_grp_max")]

# Business Rules Application
# 1. For a specific brand, if <=100 email were sent in 2016, then assign score should be 0
# Emails delivered by brand in 2016
digital_bag_email <- digital_bag_email %>%
  group_by(brand) %>%
  dplyr::mutate(
    emails_dlvd_2016_brand = sum(emails_dlvd_2016))

# Add an indicator on whether the sum of emails is greater than 0 or not
digital_bag_email$emails_dlvd_ind <- ifelse(digital_bag_email$emails_dlvd_2016_brand>100,1,0)
table(digital_bag_email$emails_dlvd_ind) # 55 out of 138 brands have sent at least 1 email in 2016

# 2. For a HCP, if no email was sent in 2016 by brand, then assign score should be 0
# Emails delivered by brand in 2016 by HCP
digital_bag_email <- digital_bag_email %>% group_by_(.dots=c("pfz_cust_id","brand")) %>%
  dplyr::mutate(
    emails_dlvd_2016_hcp_brand = sum(emails_dlvd_2016))

# Add an indicator whether a HCP received any email in 2016
digital_bag_email$emails_dlvd_2016_ind <- ifelse(digital_bag_email$emails_dlvd_2016_hcp_brand>0,1,0)

# 3. If mrkt_trx is less than bottom 10% percentile mrkt_trx by brand & mrkt_trx < 10 & brand_trx < 2, then score should be 0
# Get bottom 10th percentile mrkt_trx cutoff by brand
digital_bag_email <- digital_bag_email %>%
  group_by(brand) %>%
  dplyr::mutate(
    cutoff = quantile(mrkt_trx,probs=0.1)
  )

# Merge back the threshold to the digital bag dataset
digital_bag_email$mrkt_cutoff_ind <- ifelse(digital_bag_email$mrkt_trx < digital_bag_email$cutoff &
                                              digital_bag_email$mrkt_trx<10 & digital_bag_email$brand_trx<2,
                                            0,1)

# Apply business rule to adjust the score before distributing emails
digital_bag_email$overall_score_asin_adj <- with(digital_bag_email,
                                                 overall_score_asin*     # original score
                                                   ama_no_contact_ind*   # ama_no_contact
                                                   do_not_target_ind*    # do_not_target_ind
                                                   email_ind*            # email address indicator
                                                   emails_dlvd_ind*      # brand emails delivered indicator
                                                   emails_dlvd_2016_ind* # HCP emails delivered indicator
                                                   mrkt_cutoff_ind       # market trx cutoff
)

# Add digital bag indicator
digital_bag_email$digital_bag_ind <- with(digital_bag_email,
                                          ama_no_contact_ind*do_not_target_ind*email_ind*emails_dlvd_ind*emails_dlvd_2016_ind*mrkt_cutoff_ind)
table(digital_bag_email$digital_bag_ind) # 4.3M fall into digital bag out of 28.6M combos
with(digital_bag_email,tapply(overall_score_asin_adj,digital_bag_ind,mean))

# 4. For 8th, 9th and 10th brand by each HCP, assign the score to 0 if the score is less than bottom 30% percentile
# Rank the scores by brand and get Top N brands for each HCP
# Rank the digital bag dataset by overall score
digital_bag_email <- digital_bag_email %>%
  dplyr::arrange(pfz_cust_id,-overall_score_asin_adj) %>%
  dplyr::group_by(pfz_cust_id) %>%
  dplyr::mutate(
    rank = rank(-overall_score_asin_adj,ties.method = "min")
  )

# Determine the cutoff for 8th, 9th, 10th brand
digital_bag_email_brand_cutoff <- digital_bag_email %>%
  filter(rank>=8,rank<=10) %>%
  group_by(rank) %>%
  dplyr::summarize(
    score_cutoff = quantile(overall_score_asin_adj,probs=0.3) # bottom 30 percentile cutoff
  )

# Assign cutoff for top 8th, 9th and 10th brand in the dataset
digital_bag_email$cutoff_top_8 = ifelse(digital_bag_email$rank==8,digital_bag_email_brand_cutoff[digital_bag_email_brand_cutoff$rank==8,]$score_cutoff,NA)
digital_bag_email$cutoff_top_9 = ifelse(digital_bag_email$rank==9,digital_bag_email_brand_cutoff[digital_bag_email_brand_cutoff$rank==9,]$score_cutoff,NA)
digital_bag_email$cutoff_top_10 = ifelse(digital_bag_email$rank==10,digital_bag_email_brand_cutoff[digital_bag_email_brand_cutoff$rank==10,]$score_cutoff,NA)

# Add an indicator whether the score for top 8th, 9th and 10th brands are less than the cutoffs
digital_bag_email$cutoff_top_8_ind = with(digital_bag_email,ifelse(rank==8 & overall_score_asin_adj<cutoff_top_8,0,1))
digital_bag_email$cutoff_top_9_ind = with(digital_bag_email,ifelse(rank==9 & overall_score_asin_adj<cutoff_top_9,0,1))
digital_bag_email$cutoff_top_10_ind = with(digital_bag_email,ifelse(rank==10 & overall_score_asin_adj<cutoff_top_10,0,1))

# Adjust the score based on 8th, 9th and 10th brand indicator
digital_bag_email$overall_score_asin_adj_final = with(digital_bag_email,
                                                      overall_score_asin_adj*cutoff_top_8_ind*cutoff_top_9_ind*cutoff_top_10_ind)
summary(digital_bag_email$overall_score_asin_adj_final)

# Percentage of overall score by hcp for top 10 brands by HCP
# Add an indicator on top 10 brand for each HCP
digital_bag_email_10 <- digital_bag_email %>%
  group_by(pfz_cust_id) %>%
  dplyr::mutate(top_10_ind = ifelse(rank<=10,1,0))

# Calculate the total overall scores for each HCP by top_10_ind
digital_bag_email_10 <- digital_bag_email_10 %>%
  group_by_(.dots=c("pfz_cust_id","top_10_ind")) %>%
  dplyr::mutate(overall_score_asin_adj_hcp_topTen = sum(overall_score_asin_adj_final,na.rm=T))

summary(digital_bag_email_10$overall_score_asin_adj_hcp_topTen)

# Adjusted percentage for top 10 brands by each HCP
digital_bag_email_10 <- digital_bag_email_10 %>%
  dplyr::mutate(
    percentage_10 = ifelse(top_10_ind==1,overall_score_asin_adj_final/overall_score_asin_adj_hcp_topTen,0))

summary(digital_bag_email_10$percentage_10)

# Determine the distribution of emails
digital_bag_email_10 <- digital_bag_email_10 %>% 
  dplyr::mutate(
    emails_pred_10 = ifelse(is.na(percentage_10),0,round(emails_saturation*percentage_10,0)))

summary(digital_bag_email_10)

# Save intermediate output
fwrite(digital_bag_email_10,"Z:/Derived/Processed Digital Bag Dataset/digital_bag_output_intermediate_0505_v2.csv",row.names = F)

# 5. Headroom and Brand_TRx Constrain
# 1) If predicted email is less than 5 & (brand_trx + headroom) by brand is above top 25% percentile, then assign 5 emails
# 2) If for PCH brands, the HCP is above top 25 percentile threshold then assign 5 emails

# Consolidate brand_trx_dol and brand_trx_dol_adj for percentile calculation
digital_bag_email_10$brand_trx_dol_headroom <- ifelse(!(is.na(digital_bag_email_10$brand_trx_dol_add)),
                                                      digital_bag_email_10$brand_trx_dol_add,
                                                      digital_bag_email_10$brand_trx_dol + digital_bag_email_10$headroom_abs)

# By brand, get the cutoff points for top 25% on headroom+brand_trx_dol
cutoff_brand_trx_dol_headroom <- digital_bag_email_10 %>% 
  group_by(brand) %>% 
  dplyr::mutate(
    cutoff_brand_trx_dol_headroom = quantile(brand_trx_dol_headroom,probs = 0.75))

# Create an indicator on whether the rule needs to be applied
digital_bag_email_10$brand_trx_dol_headroom_ind = 
  with(digital_bag_email_10,
       ifelse(is.na(digital_bag_email_10$brand_trx_dol_add) & rank<=10 & 
                brand_trx_dol_headroom > cutoff_brand_trx_dol_headroom & emails_pred_10<5 & 
                overall_score_asin_adj_final>0,1,
              ifelse(!(is.na(digital_bag_email_10$brand_trx_dol_add)) &
                       digital_bag_ind  == 1 & 
                       brand_trx_dol_headroom > cutoff_brand_trx_dol_headroom,1,0)))

table(digital_bag_email_10$brand_trx_dol_headroom_ind) # 77k out of 27M records need to be adjusted
n_distinct(digital_bag_email_10[digital_bag_email_10$brand_trx_dol_headroom_ind==1,]$pfz_cust_id) # will affect 65k unique HCPs

# Reallocate the remaining emails distribution
# Get the count of brands need to be adjusted by HCP
digital_bag_email_10 <- digital_bag_email_10 %>% 
  group_by(pfz_cust_id) %>%
  dplyr::mutate(count = sum(brand_trx_dol_headroom_ind))

# Remaining emails to be distributed for each HCP
digital_bag_email_10 <- digital_bag_email_10 %>% mutate(
  emails_saturation_adj = 100-5*count)
table(hcp_count_adj$emails_saturation_adj)

# New email distribution for adjusted cases and new top_10_ind
digital_bag_email_10$top_10_ind_adj = with(digital_bag_email_10,ifelse(top_10_ind==1 & brand_trx_dol_headroom_ind==0,1,0))

# Re-calculate the percentage
digital_bag_email_10 <- digital_bag_email_10 %>%
  group_by_(.dots=c("pfz_cust_id","top_10_ind_adj")) %>%
  dplyr::mutate(overall_score_asin_adj_hcp_topTen_adj = sum(overall_score_asin_adj_final,na.rm=T))

# Adjusted percentage for top 10 brands
digital_bag_email_10 <- digital_bag_email_10 %>%
  dplyr::mutate(percentage_10_adj = ifelse(top_10_ind_adj==1,overall_score_asin_adj_final/overall_score_asin_adj_hcp_topTen_adj,0))

summary(digital_bag_email_10$percentage_10_adj)

# Determine the distribution of emails
digital_bag_email_10$emails_pred_10_adj = with(digital_bag_email_10,
                                               ifelse(is.na(percentage_10_adj),0,
                                                      ifelse(brand_trx_dol_headroom_ind==1,5,
                                                             round(emails_saturation_adj*percentage_10_adj,0))))
summary(digital_bag_email_10)

# 5. Apply Distribution Rules
# Four level of emails: 
# 1) Predicted emails 1-5: 3
# 2) Predicted emails 6-11: 6
# 3) Predicted emails 12-23: 12
# 4) Predicted emails 24+: 24
# Constrain: if emails_dlvd_2016_hcp <= 6 OR FF visit by brand <= 100, then apply 3/6/12 rule only
# Constrain: if maximum emails sent by brand < 3, then apply 3/6 rule only
# Calculate ff visit by brand
digital_bag_email_10 <- digital_bag_email_10 %>% 
  group_by(brand) %>%
  dplyr::mutate(ff_visit = sum(-overall_details_adj))
# Calculate maximum emails sent by brand in 2016
brand_email_max <- digital_bag_email_10 %>% group_by(brand) %>%
  dplyr::summarize(
    max_email_2016 = max(emails_dlvd_2016))

brand_max_list <- unique(brand_email_max[brand_email_max$max_email_2016>0 & brand_email_max$max_email_2016<3,]$brand)

# Calculate emails delivered by hcp
digital_bag_email_10 <- digital_bag_email_10 %>% 
  group_by(pfz_cust_id) %>%
  dplyr::mutate(emails_dlvd_2016_hcp = sum(emails_dlvd_2016))

# Re-allocate the emails prediction based on conditions
digital_bag_email_10$emails_pred_final = 
  with(digital_bag_email_10,ifelse(
    emails_pred_10_adj==0,0,ifelse(
      emails_pred_10_adj<6,3,ifelse(
        brand %in% brand_max_list,6,ifelse(
          emails_pred_10_adj<12,6,ifelse(
            emails_dlvd_2016_hcp<=6 | ff_visit<=100,12,ifelse(
              emails_pred_10_adj<24,12,24)))))))

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
fwrite(digital_bag_email_10,"Z:/Derived/Processed Digital Bag Dataset/digital_bag_email_distribution_top10_0505_v2.csv",row.names = F)

######################################## 5. Additional Summary #####################################################################
# digital_bag_email_10 <- fread("Z:/Derived/Processed Digital Bag Dataset/digital_bag_email_distribution_top10_0503_v2.csv")
# PCP Summary
digital_bag_email_PCP <- digital_bag_email_10[digital_bag_email_10$spec_grp_max=="PCP",]

# Add indicator on whether a HCP received an email for a specific brand
digital_bag_email_PCP <- digital_bag_email_PCP %>%
  mutate(
    hcp_count_2016 = ifelse(emails_dlvd_2016>0,1,0),
    hcp_count_pred = ifelse(emails_pred_final>0,1,0))

# Brand Summary 1: count of emails delivered in 2016 and emails prediction by brand
digital_bag_PCP_summary_1 <- digital_bag_email_PCP %>%
  group_by(brand) %>%
  dplyr::summarize(
    emails_dlvd_2016 = sum(emails_dlvd_2016),
    emails_pred = sum(emails_pred_final))

# Add an indicator on brand_trx_dol and market_trx_dol for HCPs who received emails in 2016 / in prediction
digital_bag_email_PCP$brand_trx_dol_2016 = ifelse(digital_bag_email_PCP$hcp_count_2016==1,digital_bag_email_PCP$brand_trx_dol,0)
digital_bag_email_PCP$brand_trx_dol_pred = ifelse(digital_bag_email_PCP$hcp_count_pred==1,digital_bag_email_PCP$brand_trx_dol,0)
digital_bag_email_PCP$mrkt_trx_dol_2016 = ifelse(digital_bag_email_PCP$hcp_count_2016==1,digital_bag_email_PCP$mrkt_trx_dol,0)
digital_bag_email_PCP$mrkt_trx_dol_pred = ifelse(digital_bag_email_PCP$hcp_count_pred==1,digital_bag_email_PCP$mrkt_trx_dol,0)

# Brand Summary 2: count of HCPs received emails in 2016 / in prediction, total brand_trx_dol and market_trx_dol
digital_bag_PCP_summary_2 <- digital_bag_email_PCP %>%
  group_by(brand) %>%
  dplyr::summarize(
    hcp_count_2016 = sum(hcp_count_2016),
    hcp_count_pred = sum(hcp_count_pred),
    brand_trx_dol_2016 = sum(brand_trx_dol_2016),
    brand_trx_dol_pred = sum(brand_trx_dol_pred),
    mrkt_trx_dol_2016 = sum(mrkt_trx_dol_2016),
    mrkt_trx_dol_pred = sum(mrkt_trx_dol_pred))

# Merge the two summaries
digital_bag_PCP_summary <- merge(digital_bag_PCP_summary_1,digital_bag_PCP_summary_2,by="brand")
fwrite(digital_bag_PCP_summary,"D:/Chi Cheng/Output/digital_bag_PCP_summary_0505_v2.csv",row.names = F)


######################################################## Appendix  ##################################################################
# Need to delete before sending the code
# 1. Distribution of emails prediction
ggplot(test[test$brand_trx_dol_headroom_ind==1,],aes(x=emails_pred_final)) + geom_histogram()

# 2. Maximum # emails delivered and predicted by brand / how many HCPs are at maximum by brand
brand_email_max <- digital_bag_email_10 %>% group_by_("brand") %>%
  dplyr::summarize(
    max_email_pred = max(emails_pred_final),
    max_email_2016 = max(emails_dlvd_2016))
fwrite(brand_email_max,"D:/Chi Cheng/Output/brand_email_max_0505_v2.csv",row.names = F)

test <- merge(digital_bag_email_10,brand_email_max,by="brand",all.x = T)
test$max_pred_ind <- ifelse(test$max_email_pred==test$emails_pred_final,1,0)
test$max_2016_ind <- ifelse(test$max_email_2016==test$emails_dlvd_2016,1,0)
table(test$max_pred_ind)
table(test$max_2016_ind)

brand_email_max_summary <- test %>% 
  group_by(brand) %>%
  dplyr::summarize(
    count_hcp_pred = sum(max_pred_ind),
    count_hcp_2016 = sum(max_2016_ind))

test_brand <- test[test$brand=="ADVIL" & test$emails_pred_10_adj==100,]
fwrite(merge(brand_email_max,brand_email_max_summary,by="brand"),"Chi Cheng/Output/brand_summary_0427_v2.csv",row.names = F)


# Distribution chart by HCP
emails_dist <- digital_bag_email_PCP %>% group_by(pfz_cust_id) %>%
  dplyr::summarize(
    emails_dlvd_2016 = sum(emails_dlvd_2016),
    emails_pred_final = sum(emails_pred_final))

hist(emails_dist$emails_dlvd_2016)
hist(emails_dist$emails_pred_final)

emails_dlvd_2016 <- emails_dist %>% group_by(emails_dlvd_2016) %>%
  dplyr::summarize(
    count = n()
  )
fwrite(emails_dlvd_2016,"Chi Cheng/Output/emails_dlvd_2016_distribution_0505_v2.csv",row.names = F)

emails_pred_final <- emails_dist %>% group_by(emails_pred_final) %>%
  dplyr::summarize(
    count = n()
  )
fwrite(emails_pred_final,"Chi Cheng/Output/emails_pred_final_distribution_0505_v2.csv",row.names = F)

# Brand Level Emails
brand_email_summary <- digital_bag_email_10 %>% group_by(brand) %>%
  dplyr::summarize(
    emails_dlvd_2016 = sum(emails_dlvd_2016),
    emails_pred_final = sum(emails_pred_10))
fwrite(brand_email_summary,"Chi Cheng/Output/brand_email_summary_0505_v2.csv",row.names = F)


#################################### Actual vs Predicted Emails ######################################
email_comparison <- digital_bag_email_10[,c("pfz_cust_id","brand","spec_grp_max","emails_dlvd_2016","emails_pred_final")]
names(email_comparison)
# remove emails_dlvd_2016 = 0
email_comparison <- email_comparison[email_comparison$emails_dlvd_2016!=0,]
# Percentage of changes
email_comparison$percent <- with(email_comparison, (emails_pred_final-emails_dlvd_2016)/emails_dlvd_2016)
summary(email_comparison$percent)
email_comparison$cut <- cut(email_comparison$percent,breaks = seq(-1,1,by=0.1))
table(email_comparison$cut)

email_comparison_summary <- email_comparison %>% group_by_(.dots=c("spec_grp_max","cut")) %>%
  dplyr::summarize(
    count = n()
  )
fwrite(email_comparison_summary,"Chi Cheng/Output/digital_bag_email_pred_comparison.csv",row.names = F)

### HCP level
email_comparison <- digital_bag_email_10 %>% group_by(pfz_cust_id) %>%
  dplyr::summarize(
    emails_dlvd_2016 = sum(emails_dlvd_2016),
    emails_pred_final = sum(emails_pred_final)
  )

spec <- digital_bag_email_10[,c("pfz_cust_id","spec_grp_max")] %>% group_by_(.dots=c("pfz_cust_id","spec_grp_max")) %>%
  dplyr::summarise(
    count=n()
  )

email_comparison <- merge(email_comparison,spec[,c("pfz_cust_id","spec_grp_max")],
                          by="pfz_cust_id")

# remove emails_dlvd_2016 = 0
email_comparison <- email_comparison[email_comparison$emails_dlvd_2016!=0 & email_comparison$emails_pred_final!=0,]
# Percentage of changes
email_comparison$percent <- with(email_comparison, (emails_pred_final-emails_dlvd_2016)/emails_dlvd_2016)
summary(email_comparison$percent)
email_comparison$cut <- cut(email_comparison$percent,breaks = c(-Inf,seq(-1,1,by=0.1),Inf))

email_comparison_summary <- email_comparison %>% group_by_(.dots=c("spec_grp_max","cut")) %>%
  dplyr::summarize(
    count = n()
  )

email_comparison_summary_reshape <- cast(email_comparison_summary,spec_grp_max~cut)
email_comparison_summary_reshape[is.na(email_comparison_summary_reshape)] <- 0
fwrite(email_comparison_summary_reshape,"D:/Chi Cheng/Output/email_comparison_summary_0506.csv",row.names = F)

### HCP level absolute number of emails
email_comparison <- digital_bag_email_10 %>% group_by(pfz_cust_id) %>%
  dplyr::summarize(
    emails_dlvd_2016 = sum(emails_dlvd_2016),
    emails_pred_final = sum(emails_pred_final)
  )

email_comparison$emails_diff <- email_comparison$emails_pred_final - email_comparison$emails_dlvd_2016
email_comparison$cut <- cut(email_comparison$emails_diff,breaks = c(-Inf,seq(-50,50,by=10),Inf))
table(email_comparison$cut)
colSums(is.na(email_comparison))

spec <- digital_bag_email_10[,c("pfz_cust_id","spec_grp_max")] %>% group_by_(.dots=c("pfz_cust_id","spec_grp_max")) %>%
  dplyr::summarise(
    count=n()
  )

email_comparison <- merge(email_comparison,spec[,c("pfz_cust_id","spec_grp_max")],
                          by="pfz_cust_id")

email_comparison <- email_comparison[email_comparison$emails_pred_final>0,]

email_comparison_summary <- email_comparison %>% group_by_(.dots=c("spec_grp_max","cut")) %>%
  dplyr::summarize(
    count = n()
  )

email_comparison_summary_reshape <- cast(email_comparison_summary,spec_grp_max~cut)
email_comparison_summary_reshape[is.na(email_comparison_summary_reshape)] <- 0
fwrite(email_comparison_summary_reshape,"D:/Chi Cheng/Output/email_comparison_summary_abs_0506.csv",row.names = F)




