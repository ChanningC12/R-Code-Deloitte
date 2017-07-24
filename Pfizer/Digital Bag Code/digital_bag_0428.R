###################### Digital Bag Prioritization Algorithm ########################
# Clear up the space for the job
rm(list=ls()) # remove all objects in the memory
gc() # garbage collection

# Set up working directory
setwd("D:\\")

# Read in library
library(dplyr) # data manipulation
library(ggplot2) # visualization
library(data.table) # fast data table format and file loading

###################### 1. Data Loading ##############################
### Note: At this step, load in the raw 360 data with necessary columns
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

##################### 2. Data Preparation ########################
# Separate 2015 and 2016 data
ads_16 <- ads[ads$year==2016,] # 2016 360 dataset
ads_15 <- ads[ads$year==2015,] # 2015 360 dataset

# Left join 2016 data with 2015 data to extract emails delivered in 2015, but keep all HCPs in 2016 (~1.9M)
ads_16 <- merge(ads_16,ads_15[,c(1,2,4)],by=c("pfz_cust_id","brand"),all.x=T)

# Rename the columns properly
names(ads_16)[4] <- "emails_dlvd_2016"
names(ads_16)[10] <- "emails_dlvd_2015" 

# Market Relevance calculation: For each HCP, Market relevance = mrkt_trx_dol / sum(mrkt_trx_dol)
digital_bag <- ads_16
# Aggregate mrkt_trx_dol to hcp level for the calculation of market relevance
trx_dol <- digital_bag %>% 
  group_by(pfz_cust_id) %>%
  dplyr::summarize(
    mrkt_trx_dol_hcp = sum(mrkt_trx_dol,na.rm=T))
# Merge back the market_trx_dol total by HCP
digital_bag <- merge(digital_bag,trx_dol,by="pfz_cust_id",all.x = T)
# Negative mrkt_trx_dol as 0
digital_bag[digital_bag$mrkt_trx_dol<0,]$mrkt_trx_dol = 0
digital_bag[digital_bag$mrkt_trx_dol_hcp<0,]$mrkt_trx_dol_hcp = 0
# Re-calculate mrkt_rel to avoid NA value and negatives
digital_bag$mrkt_rel <- ifelse(digital_bag$mrkt_trx_dol_hcp==0,0,
                               digital_bag$mrkt_trx_dol / digital_bag$mrkt_trx_dol_hcp)

# Append headroom analysis result to the 360 dataset
# Load headroom dataset
headroom <- fread("sunil/headroom/data/hcplist_prevnar_0414_v1.csv")

# Digital_bag has ~1.9M HCPs, headroom has ~700k HCPs that wrote at least 1 script
# Merge the two datasets to append the headroom towards the end
digital_bag <- merge(digital_bag,headroom[,c(2:3,13)],by=c("pfz_cust_id","brand"),all.x=T)
colSums(is.na(digital_bag))

# Impute missing headroom as 0s
digital_bag[is.na(digital_bag$headroom),]$headroom <- 0
# Absolute value for headroom
digital_bag$headroom_abs <- abs(digital_bag$headroom)

# For HCPs who are not in the 2016 data, imput emails delivered in 2015 as 0
digital_bag[is.na(digital_bag$emails_dlvd_2015),]$emails_dlvd_2015 <- 0

# Drop missing pfz_cust_id
digital_bag <- digital_bag[!(is.na(digital_bag$pfz_cust_id)),]

# Add column Email Saturation as 100 for now
digital_bag$emails_saturation = 100

# Append the persuadability score
# Read in persuadability result file
persuasion <- fread("Model Ready Data/Persuasion/Results/HCP_Brand_Persuasion.csv")
names(persuasion)[2] <- "brand"
names(persuasion)[3] <- "persuadability"
# Merge the score to digital bag dataset
digital_bag <- merge(digital_bag,persuasion,by=c("pfz_cust_id","brand"),all.x=T)
# Persuadability, impute NA as 0
digital_bag[is.na(digital_bag$persuadability),]$persuadability = 0

# Additionally, bring in ama_no_contact and do_not_target as email distribution rule/constraints
year_360 <- fread("Chi Cheng/Processed Data/year_360_0329.csv")
year_360 <- year_360[year_360$year==2016,c("pfz_cust_id","ama_no_contact_max","do_not_target_max","spec_grp_max")]

digital_bag <- merge(digital_bag,year_360,by="pfz_cust_id",all.x=T)

digital_bag <- digital_bag %>%
  mutate(
    ama_no_contact_ind = ifelse(ama_no_contact_max=="true",0,1),
    do_not_target_ind = ifelse(do_not_target_max=="true",0,1)
  )

# Adjust Field Force visit, to penalize higher value, take the negative value of overall details and then normalize the value
digital_bag$overall_details_adj <- -digital_bag$overall_details

# Append target list (result from ccom_rule_out.csv) as an additional attribute in scoring
targetList <- fread("Chi Cheng/Processed Data/targetList_0418.csv")
# Outer join digital bag and targetList
digital_bag <- merge(digital_bag,targetList,by=c("pfz_cust_id","brand"),all=T) # use all=T to include all possible combos
# Assign non-match hcp and brand combo to 0
digital_bag[is.na(digital_bag$approved_target),]$approved_target <- 0

# Append Email id appearence indicator as a constrain in email distribution process
email <- fread("Chi Cheng/Processed Data/email_address_extract_ind.csv")
names(email)[2]="email_ind"
digital_bag <- merge(digital_bag,email,by="pfz_cust_id",all.x=T)
# Impute non-match cases as 0
digital_bag[is.na(digital_bag$email_ind),]$email_ind = 0
table(digital_bag$email_ind)

# Append BU (Business Unit)
bu <- fread("D:/Processed Data/Brand_Bu.csv")
digital_bag <- merge(digital_bag,bu,by.x="brand",by.y="Brand",all.x = T)

# Save the copy of dataset
summary(digital_bag)


# Normalize the inputs (fix scale) used to calculate scores
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
# Weights order: 1.brand_trx_dol, 2.market relevance, 3.headroom (absolute value), 4.persuadabiity, 5.overall_details (adjusted), 6. brand plan (TargetList)
weights <- c(0.2, # 1.brand_trx_dol
             0.2, # 2.market relevance
             0.2, # 3.headroom (absolute value)
             0.2, # 4.persuadabiity
             0.2, # 5.overall_details (adjusted)
             0.2) # 6. brand plan (TargetList)

# Scoring
digital_bag <- digital_bag %>% 
  mutate(
# If market_trx_dol at HCP level is 0, then market relevance is not meaningful, use mrkt_trx_dol instead
        overall_score_asin = ifelse(mrkt_trx_dol_hcp==0,
                                weights[1]*brand_trx_dol_asin + weights[2]*mrkt_trx_dol_asin + 
                                  weights[3]*headroom_abs_asin + weights[4]*persuadability_asin + weights[5]*overall_details_adj_asin
                                +weights[6]*approved_target,
                                weights[1]* brand_trx_dol_asin + weights[2]*mrkt_rel_asin 
                                + weights[3]*headroom_abs_asin + weights[4]*persuadability_asin + weights[5]*overall_details_adj_asin
                                +weights[6]*approved_target))

# Get summary of the scored dataset
summary(digital_bag)


############## 4. Email Distribution based on Score and Rules ####################

# Only keep relevant columns in the data
digital_bag_email <- digital_bag[,c("pfz_cust_id","brand","overall_score_asin",
                                    "brand_trx_dol_asin","mrkt_trx_dol_asin","mrkt_rel_asin","headroom_abs_asin",
                                    "persuadability_asin","overall_details_adj_asin",
                                    "emails_dlvd_2016","emails_dlvd_2015",
                                    "emails_saturation","brand_trx","brand_trx_dol","mrkt_rel","mrkt_trx","mrkt_trx_dol",
                                    "headroom_abs","overall_details_adj",
                                    "persuadability","ama_no_contact_max","ama_no_contact_ind",
                                    "do_not_target_max","do_not_target_ind","approved_target","email_ind",
                                    "Bu","spec_grp_max")]

# Business Rules Application
# 1. For a specific brand, if no email was sent in 2016, then assign score should be 0
# Emails delivered by brand in 2016
brand_email <- digital_bag_email %>%
  group_by(brand) %>%
  dplyr::summarize(
    emails_dlvd_2016_brand = sum(emails_dlvd_2016)
  )
# Add an indicator on whether the sum of emails is greater than 0 or not
brand_email$emails_dlvd_ind <- ifelse(brand_email$emails_dlvd_2016_brand>0,1,0)
table(brand_email$emails_dlvd_ind) # 61 brands have sent at least 1 email in 2016

# Merge back to the digital bag dataset
digital_bag_email <- merge(digital_bag_email,brand_email,by="brand",all.x=T)

# 2. If mrkt_trx is less than bottom 10% percentile mrkt_trx by brand & mrkt_trx < 10 & brand_trx < 2, then score should be 0
# Get bottom 10th percentile mrkt_trx cutoff by brand
mrkt_trx_percentile <- digital_bag %>%
  group_by(brand) %>%
  dplyr::summarize(
    cutoff = quantile(mrkt_trx,probs=0.1)
  )

# Merge back the threshold to the digital bag dataset
digital_bag_email <- merge(digital_bag_email,mrkt_trx_percentile,by="brand",all.x=T)
digital_bag_email$mrkt_cutoff_ind <- ifelse(digital_bag_email$mrkt_trx < digital_bag_email$cutoff &
                                              digital_bag_email$mrkt_trx<10 & digital_bag_email$brand_trx<2,
                                            0,1)

# Apply business rule to adjust the score before distributing emails
digital_bag_email$overall_score_asin_adj <- with(digital_bag_email,
                                                 overall_score_asin*    # original score
                                                   ama_no_contact_ind*  # ama_no_contact
                                                   do_not_target_ind*   # do_not_target_ind
                                                   email_ind*           # email address indicator
                                                   emails_dlvd_ind*     # emails delivered indicator
                                                   mrkt_cutoff_ind      # market trx cutoff
)

# Add digital bag indicator
digital_bag_email$digital_bag_ind <- with(digital_bag_email,
                                          ama_no_contact_ind*do_not_target_ind*email_ind*emails_dlvd_ind*mrkt_cutoff_ind)
table(digital_bag_email$digital_bag_ind) # 9.2M fall into digital bag out of 27.9M combos
with(digital_bag_email,tapply(overall_score_asin_adj,digital_bag_ind,mean))

# 3. For 8th, 9th and 10th brand by each HCP, assign the score to 0 if the score is less than bottom 30% percentile
# Rank the scores by brand and get Top N brands for each HCP
# Rank the digital bag dataset by overall score
system.time(
  digital_bag_summary <- digital_bag_email %>%
    dplyr::arrange(pfz_cust_id,-overall_score_asin_adj) %>%
    dplyr::group_by(pfz_cust_id) %>%
    dplyr::mutate(
      rank = rank(-overall_score_asin_adj,ties.method = "min")
    ))

# Determine the cutoff for 8th, 9th, 10th brand
digital_bag_summary_brand_cutoff <- digital_bag_summary %>%
  filter(rank>=8,rank<=10) %>%
  group_by(rank) %>%
  dplyr::summarize(
    score_cutoff = quantile(overall_score_asin_adj,probs=0.3) # bottom 30 percentile
  )

# Assign cutoff for top 8th, 9th and 10th brand in the dataset
digital_bag_summary <- digital_bag_summary %>%
  dplyr::mutate(
    cutoff_top_8 = ifelse(rank==8,digital_bag_summary_brand_cutoff[digital_bag_summary_brand_cutoff$rank==8,]$score_cutoff,NA),
    cutoff_top_9 = ifelse(rank==9,digital_bag_summary_brand_cutoff[digital_bag_summary_brand_cutoff$rank==9,]$score_cutoff,NA),
    cutoff_top_10 = ifelse(rank==10,digital_bag_summary_brand_cutoff[digital_bag_summary_brand_cutoff$rank==10,]$score_cutoff,NA)
  )

# Add an indicator whether the score for top 8th, 9th and 10th brands are less than the cutoffs
digital_bag_summary <- digital_bag_summary %>%
  dplyr::mutate(
    cutoff_top_8_ind = ifelse(rank==8 & overall_score_asin_adj<cutoff_top_8,0,1),
    cutoff_top_9_ind = ifelse(rank==9 & overall_score_asin_adj<cutoff_top_9,0,1),
    cutoff_top_10_ind = ifelse(rank==10 & overall_score_asin_adj<cutoff_top_10,0,1)
  )

# Adjust the score based on 8th, 9th and 10th brand indicator
digital_bag_summary <- digital_bag_summary %>%
  dplyr::mutate(
    overall_score_asin_adj_final = overall_score_asin_adj*cutoff_top_8_ind*cutoff_top_9_ind*cutoff_top_10_ind)

summary(digital_bag_summary$overall_score_asin_adj_final)

# Digital bag: 10 bags
# percentage of overall score by hcp
# Add an indicator on top 10 brand for each HCP
digital_bag_email_10 <- digital_bag_summary %>%
  dplyr::group_by(pfz_cust_id) %>%
  dplyr::mutate(top_10_ind = ifelse(rank<=10,1,0))

# Calculate the total overall scores for each HCP by top_10_ind
digital_bag_email_10 <- digital_bag_email_10 %>%
  dplyr::group_by_(.dots=c("pfz_cust_id","top_10_ind")) %>%
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

# 4. Headroom and Brand_TRx Constrain
# If predicted email is less than 5 & (brand_trx + headroom) by brand is above top 25% percentile, then assign 5 emails

# By brand, get the cutoff points for top 25% on headroom+brand_trx_dol
digital_bag_email_10 <- digital_bag_email_10 %>% 
  mutate(brand_trx_dol_headroom = brand_trx_dol + headroom_abs)

cutoff_brand_trx_dol_headroom <- digital_bag_email_10 %>% 
  group_by(brand) %>% 
  dplyr::summarize(
    cutoff_brand_trx_dol_headroom = quantile(brand_trx_dol_headroom,probs = 0.75))

# Merge back the cutoff to the dataset by brand
digital_bag_email_10 <- merge(digital_bag_email_10,cutoff_brand_trx_dol_headroom,by="brand",all.x=T)

# Create an indicator on whether the rule needs to be applied
# The rule is applied when rank<=10 & brand_trx_dol + headroom is above top 25% threshold & emails prediction is less than 5 & score is not 0
digital_bag_email_10 <- digital_bag_email_10 %>% mutate(
  brand_trx_dol_headroom_ind = ifelse(
    rank<=10 & brand_trx_dol_headroom > cutoff_brand_trx_dol_headroom & emails_pred_10<5 & 
      overall_score_asin_adj_final>0,1,0))

table(digital_bag_email_10$brand_trx_dol_headroom_ind) 
n_distinct(digital_bag_email_10[digital_bag_email_10$brand_trx_dol_headroom_ind==1,]$pfz_cust_id) 

# Reallocate the remaining emails distribution
# Get the count of brands need to be adjusted by HCP
hcp_count_adj <- digital_bag_email_10 %>% 
  group_by(pfz_cust_id) %>%
  dplyr::summarize(
    count = sum(brand_trx_dol_headroom_ind))

# Remaining emails to be distributed for each HCP
hcp_count_adj <- hcp_count_adj %>% mutate(
  emails_saturation_adj = 100-5*count)
table(hcp_count_adj$emails_saturation_adj)

# New email distribution for adjusted cases and new top_10_ind
digital_bag_email_10 <- digital_bag_email_10 %>% mutate(
  emails_adj = ifelse(brand_trx_dol_headroom_ind==1,5,emails_pred_10),
  top_10_ind_adj = ifelse(top_10_ind==1 & brand_trx_dol_headroom_ind==0,1,0))

# Re-calculate the percentage
digital_bag_email_10 <- digital_bag_email_10 %>%
  dplyr::group_by_(.dots=c("pfz_cust_id","top_10_ind_adj")) %>%
  dplyr::mutate(overall_score_asin_adj_hcp_topTen_adj = sum(overall_score_asin_adj_final,na.rm=T))
# Adjusted percentage for top 10 brands
digital_bag_email_10 <- digital_bag_email_10 %>%
  dplyr::mutate(
    percentage_10_adj = ifelse(top_10_ind_adj==1,overall_score_asin_adj_final/overall_score_asin_adj_hcp_topTen_adj,0))

summary(digital_bag_email_10$percentage_10_adj)

# Determine the distribution of emails
digital_bag_email_10 <- digital_bag_email_10 %>% 
  dplyr::mutate(
    emails_pred_10_adj = ifelse(is.na(percentage_10_adj),0,round(emails_saturation*percentage_10_adj,0)))

summary(digital_bag_email_10)

# 5. Apply Distribution Rules
# Four level of emails: 
# 1) Predicted emails 1-5: 3
# 2) Predicted emails 6-11: 6
# 3) Predicted emails 12-23: 12
# 4) Predicted emails 24+: 24
summary(digital_bag_email_10$emails_pred_10_adj)

digital_bag_email_10$emails_pred_final = with(digital_bag_email_10,ifelse(
  emails_pred_10_adj==0,0,ifelse(
    emails_pred_10_adj<6,3,ifelse(
      emails_pred_10_adj<12,6,ifelse(
        emails_pred_10_adj<24,12,24)))))

summary(digital_bag_email_10$emails_pred_final)

# Summary by HCP
digital_bag_email_hcp <- digital_bag_email_10 %>% group_by(pfz_cust_id) %>%
  dplyr::summarize(
    emails_pred_tot = sum(emails_pred_final))

mean(digital_bag_email_hcp$emails_pred_tot) # average emails predicted by HCP (entire population)
mean(digital_bag_email_hcp[digital_bag_email_hcp$emails_pred_tot>0,]$emails_pred_tot) # average emails predicted by HCP (HCPs who received at 1 email)

# Save output
fwrite(digital_bag_email_10,"D:/Chi Cheng/Processed Data/digital_bag_email_distribution_top10_0428.csv",row.names = F)

###################### 5. Additional Summary ##########################
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
fwrite(digital_bag_PCP_summary,"Output/digital_bag_PCP_summary_0428.csv",row.names = F)





