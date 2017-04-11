getwd()

# list of variables in Tableau
v = c(
  "AGE_BIN",
  "IF_SPOUSE_DISC",
  "SHARED_POLICY",
  "POLICYHOLDER_AGE",
  "CLAIM_COUNT_CONTACT_DYNAMIC",
  "CLAIM_COUNT_CLOSED_DYNAMIC",
  "TOTAL_AMOUNT_PAID",
  "CLAIM_COUNT_DYNAMIC",
  "RATE_INCR","RATE_INCR_AFFORD",
  "IFA_GROUP_STATE_PROD_FA",
  "IFA_GROUP_STATE_PROD_FAWP",
  "IFA_GROUP_STATE_PROD_OTFA",
  "POLICY_AGE_2",
  "ANNL_PREM_BFOR",
  "RESPONSE_PRE",
  "CNT_FPO_PREV1",
  "CNT_RBO_PREV1",
  "BENEFIT_POOL_ASSET_RATIO",
  "DECISION_STAGE",
  "BINARY_CARDIO_TRANS",
  "BINARY_DIAB_TRANS",
  "BINARY_NF_TRANS",
  "BINARY_HYPT_TRANS",
  "HH_INCOME",
  "NETW",
  "BANK",
  "NOC19",
  "BAL_NONAGNFIRSTMTG",
  "HIGHCRD_1STMTGCREDIT",
  "HIGHCRD_TOTALALLCREDIT",
  "BAL_BANKCARD_90TO119DPD",
  "ZAGG_CCW",
  "ZAGG_TUOC_B",
  "ZAGG_ILOR_15",
  "BENE_TRM_BFOR",
  "BIO_BEFORE_NO_BIO",
  "ORIG_DBA_BFOR",
  "FMLY_PRSNC",
  "HHCOMP_A",
  "NUM_DECISIONS",
  "TOTL_COMMUNICATED_INCR_RATE",
  "PREM_PAID_TOT_2",
  "BIO_BEFORE_5_COMPOUND",
  "FREQ_BFOR_M",
  "FREQ_BFOR_Q",
  "RATE_INCR_PRE",
  "BAVGONDL",
  "CA00",
  "FAMP_M",
  "HHMARST",
  "ALL",
  "AALZ",
  "AHRA",
  "AOSA",
  "ARHM",
  "BINARY_CERV_TRANS",
  "INMEDI",
  "IORE",
  "N2029",
  "N3039",
  "N6064",
  "N65P",
  "NAH19",
  "ONLINE_22"
)

# read in the uni data
uni = read.csv("uni_2016-12-14.csv",stringsAsFactors = F)
uni_filter = subset(uni,Variable %in% v)
v %in% unique(uni_filter$Variable)
str(uni_filter)


# impute description for Total_Amount_Paid
uni_filter[uni_filter$Variable=="TOTAL_AMOUNT_PAID","Description"]<-"Total amount paid"
uni_filter[uni_filter$Variable=="TOTAL_AMOUNT_PAID","Source"]<-"Internal - Claim"
uni_filter[uni_filter$Variable=="TOTAL_AMOUNT_PAID","Ranking_NFO"]<-"Medium"
uni_filter[uni_filter$Variable=="TOTAL_AMOUNT_PAID","Ranking_RBO"]<-"Medium"

total_amount_paid = uni_filter[uni_filter$Variable=="TOTAL_AMOUNT_PAID",]

# switch the benefit period lifetime label
uni_filter[uni_filter$Variable=="BENE_TRM_BFOR" & uni_filter$Variable_Values=="(00):0",
           "Variable_Values"]<-"(99):0"

BENE_TRM_BFOR = uni_filter[uni_filter$Variable=="BENE_TRM_BFOR",]

# export to csv
write.csv(uni_filter,paste(paste("uni_filter",Sys.Date(),sep="_"),".csv",sep=""),row.names = F)








