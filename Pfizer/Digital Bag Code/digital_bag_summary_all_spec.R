# all_spec Summary
digital_bag_email_all_spec <- digital_bag_email_10

digital_bag_email_all_spec$brand_trx_dol_agg <- with(digital_bag_email_all_spec,
                                                 ifelse(brand %in% c("ADVIL","NEXIUM","PREPARATION H"),brand_trx_dol_add,
                                                        brand_trx_dol))
digital_bag_email_all_spec$overall_details <- - digital_bag_email_all_spec$overall_details_adj

# Add indicator on whether a HCP received an email for a specific brand
digital_bag_email_all_spec <- digital_bag_email_all_spec %>%
  mutate(
    hcp_count_2016 = ifelse(emails_dlvd_2016>0,1,0),
    hcp_count_pred = ifelse(emails_pred_final>0,1,0))

# Brand Summary 1: count of emails delivered in 2016 and emails prediction by brand
digital_bag_all_spec_summary_1 <- digital_bag_email_all_spec %>%
  group_by_(.dots=c("brand","spec_grp_max")) %>%
  dplyr::summarize(
    emails_dlvd_2016 = sum(emails_dlvd_2016,na.rm = T),
    emails_pred = sum(emails_pred_final,na.rm = T))

# Add an indicator on brand_trx_dol and market_trx_dol for HCPs who received emails in 2016 / in prediction
digital_bag_email_all_spec$brand_trx_dol_2016 = ifelse(digital_bag_email_all_spec$hcp_count_2016==1,digital_bag_email_all_spec$brand_trx_dol_agg,0)
digital_bag_email_all_spec$brand_trx_dol_pred = ifelse(digital_bag_email_all_spec$hcp_count_pred==1,digital_bag_email_all_spec$brand_trx_dol_agg,0)
digital_bag_email_all_spec$mrkt_trx_dol_2016 = ifelse(digital_bag_email_all_spec$hcp_count_2016==1,digital_bag_email_all_spec$mrkt_trx_dol,0)
digital_bag_email_all_spec$mrkt_trx_dol_pred = ifelse(digital_bag_email_all_spec$hcp_count_pred==1,digital_bag_email_all_spec$mrkt_trx_dol,0)

# Brand Summary 2: count of HCPs received emails in 2016 / in prediction, total brand_trx_dol and market_trx_dol
digital_bag_all_spec_summary_2 <- digital_bag_email_all_spec %>%
  group_by_(.dots=c("brand","spec_grp_max")) %>%
  dplyr::summarize(
    hcp_count_2016 = sum(hcp_count_2016,na.rm = T),
    hcp_count_pred = sum(hcp_count_pred,na.rm = T),
    brand_trx_dol_2016 = sum(brand_trx_dol_2016,na.rm = T),
    brand_trx_dol_pred = sum(brand_trx_dol_pred,na.rm = T),
    mrkt_trx_dol_2016 = sum(mrkt_trx_dol_2016,na.rm = T),
    mrkt_trx_dol_pred = sum(mrkt_trx_dol_pred,na.rm = T),
    persuadability_avg = mean(persuadability,na.rm = T),
    ff_visit_avg = mean(overall_details,na.rm = T),
    brand_trx_dol_avg = mean(brand_trx_dol_agg,na.rm = T),
    mrkt_rel_avg = mean(mrkt_rel,na.rm = T),
    headroom_abs = mean(headroom_abs,na.rm = T),
    targetlist = mean(approved_target,na.rm = T),
    overall_score_raw = mean(overall_score_nor,na.rm = T),
    ama_no_contact = mean(ama_no_contact_ind,na.rm = T),
    do_not_target = mean(do_not_target_ind,na.rm = T),
    email_ind = mean(email_ind,na.rm = T),
    overall_score_final = mean(overall_score_nor_adj_final,na.rm = T)
    )

# Merge the two summaries
digital_bag_all_spec_summary <- merge(digital_bag_all_spec_summary_1,digital_bag_all_spec_summary_2,by=c("brand","spec_grp_max"))
fwrite(digital_bag_all_spec_summary,paste("D:/Chi Cheng/Output/digital_bag_all_spec_summary_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# Split the data into chunks by spec_grp_max
digital_bag_all_spec_summary_chunks <- split(digital_bag_all_spec_summary,digital_bag_all_spec_summary$spec_grp_max,drop = T)
length(digital_bag_all_spec_summary_chunks)
names(digital_bag_all_spec_summary_chunks)[1] <- "NULL"
names(digital_bag_all_spec_summary_chunks)[14] <- "NPPA"

for (i in 1:length(digital_bag_all_spec_summary_chunks)) {
  write.csv(digital_bag_all_spec_summary_chunks[i],
paste("Chi Cheng/Output/Digital bag/chunks_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),"/",names(digital_bag_all_spec_summary_chunks[i]),".csv",sep=""),row.names = F)
}



# By brand, specialty and segment
# Brand Summary 1: count of emails delivered in 2016 and emails prediction by brand
digital_bag_all_spec_summary_1 <- digital_bag_email_all_spec %>%
  group_by_(.dots=c("brand","spec_grp_max","Segment")) %>%
  dplyr::summarize(
    emails_dlvd_2016 = sum(emails_dlvd_2016,na.rm = T),
    emails_pred = sum(emails_pred_final,na.rm = T))

# Add an indicator on brand_trx_dol and market_trx_dol for HCPs who received emails in 2016 / in prediction
digital_bag_email_all_spec$brand_trx_dol_2016 = ifelse(digital_bag_email_all_spec$hcp_count_2016==1,digital_bag_email_all_spec$brand_trx_dol_agg,0)
digital_bag_email_all_spec$brand_trx_dol_pred = ifelse(digital_bag_email_all_spec$hcp_count_pred==1,digital_bag_email_all_spec$brand_trx_dol_agg,0)
digital_bag_email_all_spec$mrkt_trx_dol_2016 = ifelse(digital_bag_email_all_spec$hcp_count_2016==1,digital_bag_email_all_spec$mrkt_trx_dol,0)
digital_bag_email_all_spec$mrkt_trx_dol_pred = ifelse(digital_bag_email_all_spec$hcp_count_pred==1,digital_bag_email_all_spec$mrkt_trx_dol,0)

# Brand Summary 2: count of HCPs received emails in 2016 / in prediction, total brand_trx_dol and market_trx_dol
digital_bag_all_spec_summary_2 <- digital_bag_email_all_spec %>%
  group_by_(.dots=c("brand","spec_grp_max","Segment")) %>%
  dplyr::summarize(
    hcp_count_2016 = sum(hcp_count_2016,na.rm = T),
    hcp_count_pred = sum(hcp_count_pred,na.rm = T),
    brand_trx_dol_2016 = sum(brand_trx_dol_2016,na.rm = T),
    brand_trx_dol_pred = sum(brand_trx_dol_pred,na.rm = T),
    mrkt_trx_dol_2016 = sum(mrkt_trx_dol_2016,na.rm = T),
    mrkt_trx_dol_pred = sum(mrkt_trx_dol_pred,na.rm = T),
    persuadability_avg = mean(persuadability,na.rm = T),
    ff_visit_avg = mean(overall_details,na.rm = T),
    brand_trx_dol_avg = mean(brand_trx_dol_agg,na.rm = T),
    mrkt_rel_avg = mean(mrkt_rel,na.rm = T),
    headroom_abs = mean(headroom_abs,na.rm = T),
    mrkt_trx = mean(mrkt_trx,na.rm=T),
    mrkt_trx_dol = mean(mrkt_trx_dol,na.rm=T),
    targetlist = mean(approved_target,na.rm = T),
    overall_score_raw = mean(overall_score_nor,na.rm = T),
    ama_no_contact = mean(ama_no_contact_ind,na.rm = T),
    do_not_target = mean(do_not_target_ind,na.rm = T),
    email_ind = mean(email_ind,na.rm = T),
    overall_score_final = mean(overall_score_nor_adj_final,na.rm = T)
  )

# Merge the two summaries
digital_bag_all_spec_summary <- merge(digital_bag_all_spec_summary_1,digital_bag_all_spec_summary_2,by=c("brand","spec_grp_max","Segment"))
fwrite(digital_bag_all_spec_summary,paste("D:/Chi Cheng/Output/digital_bag_all_spec_seg_summary_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


