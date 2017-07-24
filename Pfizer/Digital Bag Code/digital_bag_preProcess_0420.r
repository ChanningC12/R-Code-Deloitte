# Read in library
library(dplyr)
library(ggplot2)
library(data.table)
# Set up working directory
setwd("D:\\")

# Create header names for ads_360 subset data
header = c("pfz_cust_id",
           "brand",
           "year",
           "emails_dlvd",
           "hqem_click",
           "hqem_dlvd",
           "hqem_open",
           "rte_click",
           "rte_dlvd",
           "rte_open",
           "brand_trx",
           "brand_trx_dol",
           "mrkt_trx",
           "mrkt_trx_unit",
           "mrkt_trx_dol",
           "overall_details",
           "opt_out_flg"
)

################# Data Loading ###################
# Read-in HCP_brand_year level 360 dataset
ads <- fread("raw/xfer_March21_2017/ads_360_year_1516.csv",header=F,select=c(1:2,4,22:28,66:67,58:60,5,149))
# Assign the header
names(ads) = header

############# Data Wrangling ###############
# Separate 2015 and 2016 data
ads_16 <- ads[ads$year==2016,]
ads_15 <- ads[ads$year==2015,]

# Left join 2016 data and 2015 data with emails delivered, keep all HCPs in 2016 
ads_16 <- merge(ads_16,ads_15[,c(1,2,4)],by=c("pfz_cust_id","brand"),all.x=T)

# Rename the columns properly
names(ads_16)[4] <- "emails_dlvd_2016"
names(ads_16)[18] <- "emails_dlvd_2015" 

# Aggregate brand_trx to hcp level for the calculation of brand relevance
ads_trx <- ads_16 %>% 
  group_by(pfz_cust_id) %>%
  dplyr::summarize(
    brand_trx_hcp = sum(brand_trx,na.rm=T))

# Merge back the brand_trx total by HCP
ads_16 <- merge(ads_16,ads_trx,by="pfz_cust_id",all.x = T)
names(ads_16)[19] <- "brand_trx_hcp_2016"

# Create brand_relevance variable
ads_16$brand_rel <- ads_16$brand_trx/ads_16$brand_trx_hcp_2016
# Create emails opened
ads_16$emails_open <- ads_16$hqem_open+ads_16$rte_open
# Create emails clicked
ads_16$emails_click <- ads_16$hqem_click+ads_16$rte_click

# Keep relevant columns for digital prioritization dataset
names(ads_16)
digital_bag <- ads_16[,c(1:3,11,20,16,4,18,19,12:15,17,21:22)]
names(digital_bag)

# Append headroom analysis result
# Read in headroom dataset
headroom <- fread("headroom/data/hcplist_prevnar_0414_v1.csv")

# digital_bag has ~1.9M HCPs, headroom has ~700k HCPs that wrote at least 1 script
# Merge the two datasets to append the headroom towards the end
digital_bag <- merge(digital_bag,headroom[,c(2:3,13:14)],by=c("pfz_cust_id","brand"),all.x=T)
colSums(is.na(digital_bag))

# Impute missing headroom as 0s
digital_bag[is.na(digital_bag$headroom),]$headroom <- 0
digital_bag[is.na(digital_bag$headroom_adj),]$headroom_adj <- 0

# Add emails_dlvd_avg for 2015 and 2016
# For HCPs who are not in the 2016 data, imput emails delivered as 0
digital_bag[is.na(digital_bag$emails_dlvd_2015),]$emails_dlvd_2015 <- 0
# Drop missing pfz_cust_id
digital_bag <- digital_bag[!(is.na(digital_bag$pfz_cust_id)),]
# Add necessary columns
digital_bag <- digital_bag %>%
  mutate(
    emails_dlvd_avg = (emails_dlvd_2016+emails_dlvd_2015)/2,
    persuadability = NA, # assign a placeholder for persuadability for now
    emails_saturation = NA
  )
names(digital_bag)
# Adjust the column position
digital_bag <- digital_bag[,c(1:5,18,20,6,7:8,19,21,9:17)]
# Check the # missing values by column
colSums(is.na(digital_bag))

# Append the persuadability score
# Read in persuadability result file
persuasion <- fread("Model Ready Data/Persuasion/Results/HCP_Brand_Persuasion.csv")
names(persuasion)[2] <- "brand"
names(persuasion)[3] <- "persuadability"
# Merge the score to digital bag dataset
digital_bag <- merge(digital_bag,persuasion,by=c("pfz_cust_id","brand"),all.x=T)

# Additionally, bring in ama_no_contact and do_not_target as email distribution constrains
year_360 <- fread("Processed Data/year_360_0329.csv")
year_360 <- year_360[year_360$year==2016,c("pfz_cust_id","ama_no_contact_max","do_not_target_max")]
digital_bag <- merge(digital_bag,year_360,by="pfz_cust_id",all.x=T)
digital_bag <- digital_bag %>%
  mutate(
    ama_no_contact_ind = ifelse(ama_no_contact_max=="true",0,1),
    do_not_target_ind = ifelse(do_not_target_max=="true",0,1)
  )
table(digital_bag$ama_no_contact_max,digital_bag$ama_no_contact_ind)
table(digital_bag$do_not_target_max,digital_bag$do_not_target_ind)

# Field Force visit, penalize higher value
digital_bag$overall_details_adj <- -digital_bag$overall_details

# Absolute value for headroom
digital_bag$headroom_abs <- abs(digital_bag$headroom)

# Append target list (result from ccom_rule_out.csv) as email distribution constrain
targetList <- fread("Processed Data/targetList_0418.csv")
digital_bag <- merge(digital_bag,targetList,by=c("pfz_cust_id","brand"),all.x=T)

# Email id appearence
email <- fread("Processed Data/email_address_extract_ind.csv")
names(email)[2]="email_ind"
digital_bag <- merge(digital_bag,email,by="pfz_cust_id",all.x=T)
digital_bag[is.na(digital_bag$email_ind),]$email_ind = 0
table(digital_bag$email_ind)

# Append specialty group and BU
bu <- fread("D:/Processed Data/Brand_Bu.csv")
digital_bag <- merge(digital_bag,bu,by.x="brand",by.y="Brand",all.x = T)
digital_bag <- merge(digital_bag,year_360[year_360$year==2016,c("pfz_cust_id","spec_grp_max")])

# Save the copy of dataset
summary(digital_bag)
fwrite(digital_bag,"Processed Data/digital_bag_0420.csv",row.names = F)

##################### Calculate overall score #########################
digital_bag <- fread("Processed Data/digital_bag_0420.csv")
names(digital_bag)
colSums(is.na(digital_bag))

# Can be updated by segments if needed
# Email Saturation, 50
digital_bag$emails_saturation = as.numeric(digital_bag$emails_saturation)
digital_bag$emails_saturation = 50
# Persuadability, NA as 0
digital_bag[is.na(digital_bag$persuadability),]$persuadability = 0
# Negative brand TRx to 0
digital_bag[digital_bag$brand_trx<0,]$brand_trx = 0
digital_bag[digital_bag$brand_trx_hcp_2016<0,]$brand_trx_hcp_2016 = 0
# Re-calculate brand_rel
digital_bag$brand_rel <- ifelse(digital_bag$brand_trx_hcp_2016==0,0,
                                digital_bag$brand_trx / digital_bag$brand_trx_hcp_2016)
summary(digital_bag$brand_rel)
# Negative mrkt_trx as 0
digital_bag[digital_bag$mrkt_trx<0,]$mrkt_trx = 0

# Normalize the inputs used to calculate scores
digital_bag <- digital_bag %>%
  mutate(
    brand_trx_nor = (brand_trx - min(brand_trx)) / (max(brand_trx) - min(brand_trx)),
    brand_rel_nor = (brand_rel - min(brand_rel)) / (max(brand_rel) - min(brand_rel)),
    headroom_abs_nor = (headroom_abs - min(headroom_abs)) / (max(headroom_abs) - min(headroom_abs)),
    persuadability_nor = (persuadability - min(persuadability)) / (max(persuadability) - min(persuadability)),
    overall_details_adj_nor = (overall_details_adj - min(overall_details_adj)) / (max(overall_details_adj) - min(overall_details_adj)),
    mrkt_trx_nor = (mrkt_trx - min(mrkt_trx)) / (max(mrkt_trx) - min(mrkt_trx))
  )

# Square root arcsin transformation
# sqrt transformation
digital_bag <- digital_bag %>%
  mutate(
    brand_trx_sqrt = sqrt(brand_trx_nor),
    brand_rel_sqrt = sqrt(brand_rel_nor),
    headroom_abs_sqrt = sqrt(headroom_abs_nor),
    persuadability_sqrt = sqrt(persuadability_nor),
    overall_details_adj_sqrt = sqrt(overall_details_adj_nor),
    mrkt_trx_sqrt = sqrt(mrkt_trx_nor)
  )

# arcsin transformation
digital_bag <- digital_bag %>%
  mutate(
    brand_trx_asin = asin(brand_trx_sqrt),
    brand_rel_asin = asin(brand_rel_sqrt),
    headroom_abs_asin = asin(headroom_abs_sqrt),
    persuadability_asin = asin(persuadability_sqrt),
    overall_details_adj_asin = asin(overall_details_adj_sqrt),
    mrkt_trx_asin = asin(mrkt_trx_sqrt)
  )

colSums(is.na(digital_bag))

# Weights can be adjusted as required
# Weights order: 1.brand_trx, 2.brand_rel, 3.headroom, 4.persuadabiity, 5.overall_details, 
weights <- c(0.2,0.2,0.2,0.2,0.2)

digital_bag <- digital_bag %>% 
  mutate(
    overall_score_asin = ifelse(brand_trx_hcp_2016==0,
                                0.5*(weights[1] + weights[2])*mrkt_trx_asin + 
                                  weights[3]*headroom_abs_asin + weights[4]*persuadability_asin + weights[5]*overall_details_adj_asin,
                                weights[1]* brand_trx_asin + weights[2]*brand_rel_asin 
                                + weights[3]*headroom_abs_asin + weights[4]*persuadability_asin + weights[5]*overall_details_adj_asin
    ),
    overall_score_nor = ifelse(brand_trx_hcp_2016==0,
                               0.5*(weights[1] + weights[2])*mrkt_trx_nor + 
                                 weights[3]*headroom_abs_nor + weights[4]*persuadability_nor + weights[5]*overall_details_adj_nor,
                               weights[1]* brand_trx_nor + weights[2]*brand_rel_nor 
                               + weights[3]*headroom_abs_nor + weights[4]*persuadability_nor + weights[5]*overall_details_adj_nor
    )
  )

summary(digital_bag)
fwrite(digital_bag,"D:/Processed Data/digital_bag_output_0420.csv",row.names = F)

######## Email Distribution based on Score ###########
digital_bag <- fread("Processed Data/digital_bag_output_0420.csv")
digital_bag_email <- digital_bag[,c("pfz_cust_id","brand","overall_score_asin",
                                    "emails_dlvd_2016","emails_dlvd_2015",
                                    "emails_saturation","brand_trx","brand_rel","mrkt_trx",
                                    "headroom_abs","overall_details_adj",
                                    "persuadability","ama_no_contact_max","ama_no_contact_ind",
                                    "do_not_target_max","do_not_target_ind","approved_target","email_ind",
                                    "Bu","spec_grp_max")]

# Apply business rule to adjust the score before distributing emails
digital_bag_email$overall_score_asin_adj <- with(digital_bag_email,
                                                 overall_score_asin*ama_no_contact_ind*
                                                   do_not_target_ind*approved_target*email_ind)
# percentage of overall score by hcp
digital_bag_email <- digital_bag_email %>%
  dplyr::group_by(pfz_cust_id) %>%
  dplyr::mutate(overall_score_asin_adj_hcp = sum(overall_score_asin_adj,na.rm=T)
                #    percent = overall_score_asin_adj/sum(overall_score_asin_adj,na.rm=T)
  )

digital_bag_email$percentage <- ifelse(is.na(digital_bag_email$overall_score_asin_adj),NA,
                                       ifelse(digital_bag_email$overall_score_asin_adj_hcp==0,0,
                                              with(digital_bag_email,overall_score_asin_adj/overall_score_asin_adj_hcp)))

# Determine the distribution of emails
digital_bag_email <- digital_bag_email %>% 
  dplyr::mutate(
    emails_pred = round(emails_saturation*percentage,0))

summary(digital_bag_email)
# Save output
fwrite(digital_bag_email,"D:/Processed Data/digital_bag_email_distribution_0420.csv",row.names = F)

# Output summary
options(digits = 3)
summary_db <- data.frame(unclass(summary(digital_bag_email)),check.names=F, stringsAsFactors=F)

