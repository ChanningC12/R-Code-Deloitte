

%LET VARLIST01=
%STR(
ANNL_PREM_BFOR
BENEFIT_POOL_ASSET_RATIO
/*BIO_AFTER_NO_BIO*/
CNT_FPO_PREV1
CNT_RBO_PREV1
DECISION_STAGE
/*NUM_DECISIONS*/
POLICY_AGE_2
RATE_INCR
RATE_INCR_AFFORD
RATE_INCR_PRE
IF_SPOUSE_DISC
ISSUE_AGE
POLICYHOLDER_AGE

SHARED_POLICY
TOTAL_AMOUNT_PAID
CLAIM_COUNT_CLOSED_DYNAMIC
CLAIM_COUNT_CONTACT_DYNAMIC
CLAIM_COUNT_DYNAMIC
CLAIM_COUNT_NP_CLOSED_DYNAMIC
CLAIM_COUNT_OPEN_DYNAMIC
BINARY_CARDIO_TRANS
BINARY_DIAB_TRANS
BINARY_HERNIA_TRANS
BINARY_HYPT_TRANS
BINARY_NF_TRANS
BINARY_NM_TRANS
BINARY_NO_TRANS
BINARY_PREG_TRANS
SEV_DIAB_TRANS
SEV_NT_TRANS
BANK
BNKI
C210BLU
C210BPVT
C210CIP
C210EBI
C210HMI
C210HVA
C210KSES
C210MOB
C210MYS_2
C210MYS_7
C210PDV
C210WHT
HH_INCOME
HHMARST
HOMSTAT_R
HOMSTAT_T
IHCA
INMEDI
IOAPPLE
IORE
ITBI
ITCV
NETW
NOC19
ONLINE_22
PLIPITOR
ZINCDEC
BAL_BANKCARD_90TO119DPD
BAL_HELOAN_SEVEREDEROG
BAL_HELOC
BAL_HELOC_NEW
BAL_NONAGNFIRSTMTG
BAL_TOTALALLCREDIT_90TO119DPD
HH_BANKCARD_SEVEREDEROG
HH_BANKCARDCREDIT_60DPD
ZAGG_C210BLU
ZAGG_C210BPVT
ZAGG_C210CIP
ZAGG_C210EBI
ZAGG_C210HMI
ZAGG_C210HVA
ZAGG_C210MOB
ZAGG_C210PDV
ZAGG_C210WHT
ZAGG_HOMSTAT_R
ZAGG_ILOR_15
ZAGG_TUOC_B
ZAGG_TUOC_C
ZAGG_TUOC_Q
ZAGG_TUOC_S
BENE_TRM_BFOR
BIO_BEFORE_5_COMPOUND
BIO_BEFORE_NO_BIO
FREQ_BFOR_M
FREQ_BFOR_Q
IFA_GROUP_STATE_PROD_FA
IFA_GROUP_STATE_PROD_FAWP
ORIG_DBA_BFOR
PREM_PAID_TOT_2
STATE_AK
STATE_AR
STATE_AZ
STATE_CT
STATE_IA
STATE_IN
STATE_MD
STATE_MI
STATE_MN
STATE_OR
STATE_PARTNERSHIP
STATE_TX
STATE_UT
STATE_VA
STATE_WA
STATE_WI
STATE_WV
STATE_WY
TOTL_COMMUNICATED_INCR_RATE
BINARY_BP_TRANS
BINARY_CERV_TRANS
BINARY_DRUG_TRANS
BINARY_NS_TRANS
BINARY_NT_TRANS
SEV_HYPT_TRANS
SEV_RESP_TRANS
AACN
AALZ
AANG
ABED
ABRC
ACDE
ACNK
AHRA
AINS
AIRR
AOSA
APT_A
APT_C
APT_M
APT_N
APT_P
APT_T
ARHM
ASTK
BAVGONDL
BONDOLLR
BONORDER
C210APVT
C210MYS_3
C210MYS_6
CA00
CCW
CGN1_F
ESTDII30
ESTINC30
FAMP_M
FAMP_O
FMLY_PRSNC
HHCOMP_A
HHCOMP_D
HHCOMP_J
HOMSTAT_P
HOMSTAT_U
ICCO
ICDO
ICPL
ICSP
ICST
IDLOW
IHCR
IHDIY
IHIGHSCL
IHORSE
IMJE
IMPS
INIRA
IODVDPLR
IOMG
IOMO
IOOW
IOR_2
IOR_4
IOR_5
IORV
IOTEACHR
IOTR
IPSR
IRNA
ISSC
ITRV
MEDSUP
N2029
N3039
N6064
N65P
NAH19
NETW30
NXSD
OACHILD
OAPLUS
OATEEN
OCOLLECT
ONLINE_10
ONLINE_27
ONLINE_33
PALLERGY
PANTIDEP
PANXIETY
PARTHRTS
PATHFOOT
PCHODIET
PCHOLMED
PDIAORAL
PHEARTRX
PHORM
PHRTMEDS
PLASERVC
PMENSTRL
PPAIN
PPREMARN
PSIR
PSNORING
PVIS
PVITSUPP
RACHILD
RAUTO
RBEAUTY
RBOOKS
RCOMPHMO
RELECTNC
RFOOD
RFURNITR
RGARDEN
RGIFT
RHEALTH
RHMCARE
RHMFURN
RHOLIDAY
RHSWARE
RJEWELRY
RLINENS
ROTHER
RTRAVEL
SPENDPAT
SSON
TUOC_C
TUOC_D
TUOC_G
TUOC_H
TUOC_K
TUOCS_B
TUOCS_D
TUOCS_E
TUOCS_G
TUOCS_L
TUOCS_M
VAC
VFLAG
BAL_1STMTGCREDIT
BAL_1STMTGCREDIT_NEW
BAL_AGENCYFIRSTMTG
BAL_AUTOFINANCE_60DPD
BAL_HELOC_90TO119DPD
BAL_HOMEEQUITYLOANS_60DPD
BAL_MTGCREDIT
BAL_MTGCREDIT_NEW
BAL_NONAGN1STMORG_60TO89DPD
BAL_NONAGN1STMORG_BANKRUPTC
BAL_NONMTGCREDIT_60DPD
BAL_RETAILCREDIT_60DPD
BAL_TOTALALLCREDIT
BAL_TOTALALLCREDIT_60TO89DPD
HH_AUTOBANK_60DPD
HH_BANKCARD_60TO89DPD
HH_BANKCARD_COLLECTIONS
HH_CONSUMERFINANCE_60DPD
HH_HELOAN_30TO59DPD
HH_HELOAN_COLLECTIONS
HH_HELOAN_SEVEREDEROG
HH_HELOC
HH_HOMEEQUITYLOANS_60DPD
HH_MTG_60TO89DPD
HH_NONAGN1STMORG_60TO89DPD
HH_NONAGN1STMORG_90TO119DPD
HH_NONMTGCREDIT_60DPD
HH_RETAILCREDIT_60DPD
HH_TOTALALLCREDIT_60DPD
HH_TOTALALLCREDIT_60TO89DPD
HH_TOTALALLCREDIT_90TO119DPD
HH_TOTALALLCREDIT_BANKRUPTCY
HH_TOTALALLCREDIT_COLLECTIONS
HH_TOTALALLCREDIT_SEVEREDEROG
HIGHCRD_1STMTGCREDIT
HIGHCRD_1STMTGCREDIT_NEW
HIGHCRD_AGENCYFIRSTMTG
HIGHCRD_AGENCYFIRSTMTG_NEW
HIGHCRD_BANKCARDCREDIT
HIGHCRD_BANKCARDCREDIT_NEW
HIGHCRD_HELOC
HIGHCRD_HELOC_NEW
HIGHCRD_MTGCREDIT
HIGHCRD_MTGCREDIT_NEW
HIGHCRD_NONAGNFIRSTMTG
HIGHCRD_NONMTGCREDIT
HIGHCRD_TOTALALLCREDIT
NUM_1STMTG_60TO89DPD
NUM_1STMTG_90TO119DPD
NUM_1STMTG_BANKRUPTCY
NUM_1STMTG_SEVEREDEROG
NUM_AGENCY1STMORG_30TO59DPD
NUM_AUTOBANK_60DPD
NUM_BANKCARD_90TO119DPD
NUM_BANKCARD_SEVEREDEROG
NUM_HELOC_60DPD
NUM_HELOC_SEVEREDEROG
NUM_HOMEEQUITYLOANS_60DPD
NUM_MTG_60TO89DPD
NUM_NONAGN1STMORG_60TO89DPD
NUM_NONAGNFIRSTMTG_NEW
NUM_NONMTGCREDIT_60DPD
NUM_RETAILCREDIT_60DPD
NUM_STUDENTLOAN_60DPD
NUM_TOTALALLCREDIT_BANKRUPTCY
PRCNT_AUTOLEASEMATURATION
PRCNT_NONMTGCREDIT
ZAGG_AGJOINT
ZAGG_APT_P
ZAGG_BAVGDOLL
ZAGG_BAVGOFDL
ZAGG_BAVGONDL
ZAGG_BC_M
ZAGG_BOFDOLLR
ZAGG_BOFORDER
ZAGG_BONDOLLR
ZAGG_BONORDER
ZAGG_BPAMEXY
ZAGG_BPCREDIT
ZAGG_BPVISA
ZAGG_BTOTLDOL
ZAGG_BTOTLORD
ZAGG_BUYER
ZAGG_C210APVT
ZAGG_C210B200
ZAGG_C210PBL
ZAGG_CCW
ZAGG_COLLEGE
ZAGG_ESTINC30__AVE_A_
ZAGG_ESTINC30_007
ZAGG_ESTINC30_017
ZAGG_ESTINC30_025
ZAGG_ESTINC30_035
ZAGG_ESTINC30_045
ZAGG_ESTINC30_055
ZAGG_ESTINC30_067
ZAGG_ESTINC30_174
ZAGG_ESTINC30_224
ZAGG_ESTINC30_324
ZAGG_ESTINC30_449
ZAGG_ESTINC30_500
ZAGG_FINB
ZAGG_HOMSTAT_T
ZAGG_HOMSTAT_U
ZAGG_HOMSTAT_Y
ZAGG_IDNAT
ZAGG_IHGO
ZAGG_IHGREEN
ZAGG_IHIGHSCL
ZAGG_IHORSE
ZAGG_IMGO
ZAGG_INSHOP
ZAGG_IOLA
ZAGG_IOMO
ZAGG_IOR_6
ZAGG_IORV
ZAGG_IRAS
ZAGG_ISCA
ZAGG_ITRV
ZAGG_MEDSUP_9
ZAGG_MOBPLUS_M
ZAGG_MOBPLUS_P
ZAGG_NETW30__AVE_A_
ZAGG_NETW30_0000
ZAGG_NETW30_0003
ZAGG_NETW30_0019
ZAGG_NETW30_0037
ZAGG_NETW30_0062
ZAGG_NETW30_0087
ZAGG_NETW30_0625
ZAGG_NETW30_0875
ZAGG_NETW30_1000
ZAGG_OACHILD
ZAGG_OAGEN
ZAGG_OAMEN
ZAGG_OANOGEN
ZAGG_OAPETITE
ZAGG_OARTS
ZAGG_OATEEN
ZAGG_OAWOMEN
ZAGG_OCHILDPR
ZAGG_OCOMPHMO
ZAGG_OFOOD
ZAGG_OFURNITR
ZAGG_OGIFT
ZAGG_OHMCARE
ZAGG_OHOLIDAY
ZAGG_OJEWELRY
ZAGG_OSPECFD
ZAGG_OSTATION
ZAGG_PBRONCH
ZAGG_PDIADIET
ZAGG_PHEARTAT
ZAGG_PHRTMEDS
ZAGG_PORALINJ
ZAGG_PPAIN
ZAGG_PRESRI
ZAGG_PRHEUMTS
ZAGG_PSIR
ZAGG_PSKINMED
ZAGG_PVIAGRA
ZAGG_RAGEN
ZAGG_RARTS
ZAGG_RATEEN
ZAGG_RAWOMEN
ZAGG_RBOOKS
ZAGG_RC_M
ZAGG_RCOMPHMO
ZAGG_RCRAFTS
ZAGG_RDOLLARS
ZAGG_RELECTNC
ZAGG_RETAIL
ZAGG_RHEALTH
ZAGG_RHMFURN
ZAGG_RMUSIC
ZAGG_RSTATION
ZAGG_SPENDPAT_00
ZAGG_SPENDPAT_15
ZAGG_SPENDPAT_16
ZAGG_SSON
ZAGG_TRAD
ZAGG_TUOC_H
ZAGG_TUOC_N
ZAGG_VOTE
ZAGG_Z4HMVALU
ZAGG_ZINCDEC_5
);

%LET VARLIST_RF =
%STR(
ANNL_PREM_BFOR
BENEFIT_POOL_ASSET_RATIO
CNT_FPO_PREV1
CNT_RBO_PREV1
DECISION_STAGE
POLICY_AGE_2
RATE_INCR
RATE_INCR_AFFORD
RATE_INCR_PRE
IF_SPOUSE_DISC
ISSUE_AGE
POLICYHOLDER_AGE
SHARED_POLICY
TOTAL_AMOUNT_PAID
CLAIM_COUNT_CLOSED_DYNAMIC
CLAIM_COUNT_CONTACT_DYNAMIC
CLAIM_COUNT_DYNAMIC
CLAIM_COUNT_NP_CLOSED_DYNAMIC
CLAIM_COUNT_OPEN_DYNAMIC
BINARY_CARDIO_TRANS
BINARY_DIAB_TRANS
BINARY_HERNIA_TRANS
BINARY_HYPT_TRANS
BINARY_NF_TRANS
BINARY_NM_TRANS
BINARY_NO_TRANS
BINARY_PREG_TRANS
SEV_DIAB_TRANS
SEV_NT_TRANS
BANK
BNKI
C210BLU
C210BPVT
C210CIP
C210EBI
C210HMI
C210HVA
C210KSES
C210MOB
C210MYS_2
C210MYS_7
C210PDV
C210WHT
HH_INCOME
HHMARST
HOMSTAT_R
HOMSTAT_T
IHCA
INMEDI
IOAPPLE
IORE
ITBI
ITCV
NETW
NOC19
ONLINE_22
PLIPITOR
ZINCDEC
BAL_BANKCARD_90TO119DPD
BAL_HELOAN_SEVEREDEROG
BAL_HELOC
BAL_HELOC_NEW
BAL_NONAGNFIRSTMTG
BAL_TOTALALLCREDIT_90TO119DPD
HH_BANKCARD_SEVEREDEROG
HH_BANKCARDCREDIT_60DPD
ZAGG_C210BLU
ZAGG_C210BPVT
ZAGG_C210CIP
ZAGG_C210EBI
ZAGG_C210HMI
ZAGG_C210HVA
ZAGG_C210MOB
ZAGG_C210PDV
ZAGG_C210WHT
ZAGG_HOMSTAT_R
ZAGG_ILOR_15
ZAGG_TUOC_B
ZAGG_TUOC_C
ZAGG_TUOC_Q
ZAGG_TUOC_S
BENE_TRM_BFOR
BIO_BEFORE_5_COMPOUND
BIO_BEFORE_NO_BIO
FREQ_BFOR_M
FREQ_BFOR_Q
IFA_GROUP_STATE_PROD_FA
IFA_GROUP_STATE_PROD_FAWP
ORIG_DBA_BFOR
PREM_PAID_TOT_2
STATE_AK
STATE_AR
STATE_AZ
STATE_CT
STATE_IA
STATE_IN
STATE_MD
STATE_MI
STATE_MN
STATE_OR
STATE_PARTNERSHIP
STATE_TX
STATE_UT
STATE_VA
STATE_WA
STATE_WI
STATE_WV
STATE_WY
TOTL_COMMUNICATED_INCR_RATE
BINARY_BP_TRANS
BINARY_CERV_TRANS
BINARY_DRUG_TRANS
BINARY_NS_TRANS
BINARY_NT_TRANS
SEV_HYPT_TRANS
SEV_RESP_TRANS
AACN
AALZ
AANG
ABED
ABRC
ACDE
ACNK
AHRA
AINS
AIRR
AOSA
APT_A
APT_C
APT_M
APT_N
APT_P
APT_T
ARHM
ASTK
BAVGONDL
BONDOLLR
BONORDER
C210APVT
C210MYS_3
C210MYS_6
CA00
CCW
CGN1_F
ESTDII30
ESTINC30
FAMP_M
FAMP_O
FMLY_PRSNC
HHCOMP_A
HHCOMP_D
HHCOMP_J
HOMSTAT_P
HOMSTAT_U
ICCO
ICDO
ICPL
ICSP
ICST
IDLOW
IHCR
IHDIY
IHIGHSCL
IHORSE
IMJE
IMPS
INIRA
IODVDPLR
IOMG
IOMO
IOOW
IOR_2
IOR_4
IOR_5
IORV
IOTEACHR
IOTR
IPSR
IRNA
ISSC
ITRV
MEDSUP
N2029
N3039
N6064
N65P
NAH19
NETW30
NXSD
OACHILD
OAPLUS
OATEEN
OCOLLECT
ONLINE_10
ONLINE_27
ONLINE_33
PALLERGY
PANTIDEP
PANXIETY
PARTHRTS
PATHFOOT
PCHODIET
PCHOLMED
PDIAORAL
PHEARTRX
PHORM
PHRTMEDS
PLASERVC
PMENSTRL
PPAIN
PPREMARN
PSIR
PSNORING
PVIS
PVITSUPP
RACHILD
RAUTO
RBEAUTY
RBOOKS
RCOMPHMO
RELECTNC
RFOOD
RFURNITR
RGARDEN
RGIFT
RHEALTH
RHMCARE
RHMFURN
RHOLIDAY
RHSWARE
RJEWELRY
RLINENS
ROTHER
RTRAVEL
SPENDPAT
SSON
TUOC_C
TUOC_D
TUOC_G
TUOC_H
TUOC_K
TUOCS_B
TUOCS_D
TUOCS_E
TUOCS_G
TUOCS_L
TUOCS_M
VAC
VFLAG
BAL_1STMTGCREDIT
BAL_1STMTGCREDIT_NEW
BAL_AGENCYFIRSTMTG
BAL_AUTOFINANCE_60DPD
BAL_HELOC_90TO119DPD
BAL_HOMEEQUITYLOANS_60DPD
BAL_MTGCREDIT
BAL_MTGCREDIT_NEW
BAL_NONAGN1STMORG_60TO89DPD
BAL_NONAGN1STMORG_BANKRUPTC
BAL_NONMTGCREDIT_60DPD
BAL_RETAILCREDIT_60DPD
BAL_TOTALALLCREDIT
BAL_TOTALALLCREDIT_60TO89DPD
HH_AUTOBANK_60DPD
HH_BANKCARD_60TO89DPD
HH_BANKCARD_COLLECTIONS
HH_CONSUMERFINANCE_60DPD
HH_HELOAN_30TO59DPD
HH_HELOAN_COLLECTIONS
HH_HELOAN_SEVEREDEROG
HH_HELOC
HH_HOMEEQUITYLOANS_60DPD
HH_MTG_60TO89DPD
HH_NONAGN1STMORG_60TO89DPD
HH_NONAGN1STMORG_90TO119DPD
HH_NONMTGCREDIT_60DPD
HH_RETAILCREDIT_60DPD
HH_TOTALALLCREDIT_60DPD
HH_TOTALALLCREDIT_60TO89DPD
HH_TOTALALLCREDIT_90TO119DPD
HH_TOTALALLCREDIT_BANKRUPTCY
HH_TOTALALLCREDIT_COLLECTIONS
HH_TOTALALLCREDIT_SEVEREDEROG
HIGHCRD_1STMTGCREDIT
HIGHCRD_1STMTGCREDIT_NEW
HIGHCRD_AGENCYFIRSTMTG
HIGHCRD_AGENCYFIRSTMTG_NEW
HIGHCRD_BANKCARDCREDIT
HIGHCRD_BANKCARDCREDIT_NEW
HIGHCRD_HELOC
HIGHCRD_HELOC_NEW
HIGHCRD_MTGCREDIT
HIGHCRD_MTGCREDIT_NEW
HIGHCRD_NONAGNFIRSTMTG
HIGHCRD_NONMTGCREDIT
HIGHCRD_TOTALALLCREDIT
NUM_1STMTG_60TO89DPD
NUM_1STMTG_90TO119DPD
NUM_1STMTG_BANKRUPTCY
NUM_1STMTG_SEVEREDEROG
NUM_AGENCY1STMORG_30TO59DPD
NUM_AUTOBANK_60DPD
NUM_BANKCARD_90TO119DPD
NUM_BANKCARD_SEVEREDEROG
NUM_HELOC_60DPD
NUM_HELOC_SEVEREDEROG
NUM_HOMEEQUITYLOANS_60DPD
NUM_MTG_60TO89DPD
NUM_NONAGN1STMORG_60TO89DPD
NUM_NONAGNFIRSTMTG_NEW
NUM_NONMTGCREDIT_60DPD
NUM_RETAILCREDIT_60DPD
NUM_STUDENTLOAN_60DPD
NUM_TOTALALLCREDIT_BANKRUPTCY
PRCNT_AUTOLEASEMATURATION
PRCNT_NONMTGCREDIT
ZAGG_AGJOINT
ZAGG_APT_P
ZAGG_BAVGDOLL
ZAGG_BAVGOFDL
ZAGG_BAVGONDL
ZAGG_BC_M
ZAGG_BOFDOLLR
ZAGG_BOFORDER
ZAGG_BONDOLLR
ZAGG_BONORDER
ZAGG_BPAMEXY
ZAGG_BPCREDIT
ZAGG_BPVISA
ZAGG_BTOTLDOL
ZAGG_BTOTLORD
ZAGG_BUYER
ZAGG_C210APVT
ZAGG_C210B200
ZAGG_C210PBL
ZAGG_CCW
ZAGG_COLLEGE
ZAGG_ESTINC30__AVE_A_
ZAGG_ESTINC30_007
ZAGG_ESTINC30_017
ZAGG_ESTINC30_025
ZAGG_ESTINC30_035
ZAGG_ESTINC30_045
ZAGG_ESTINC30_055
ZAGG_ESTINC30_067
ZAGG_ESTINC30_174
ZAGG_ESTINC30_224
ZAGG_ESTINC30_324
ZAGG_ESTINC30_449
ZAGG_ESTINC30_500
ZAGG_FINB
ZAGG_HOMSTAT_T
ZAGG_HOMSTAT_U
ZAGG_HOMSTAT_Y
ZAGG_IDNAT
ZAGG_IHGO
ZAGG_IHGREEN
ZAGG_IHIGHSCL
ZAGG_IHORSE
ZAGG_IMGO
ZAGG_INSHOP
ZAGG_IOLA
ZAGG_IOMO
ZAGG_IOR_6
ZAGG_IORV
ZAGG_IRAS
ZAGG_ISCA
ZAGG_ITRV
ZAGG_MEDSUP_9
ZAGG_MOBPLUS_M
ZAGG_MOBPLUS_P
ZAGG_NETW30__AVE_A_
ZAGG_NETW30_0000
ZAGG_NETW30_0003
ZAGG_NETW30_0019
ZAGG_NETW30_0037
ZAGG_NETW30_0062
ZAGG_NETW30_0087
ZAGG_NETW30_0625
ZAGG_NETW30_0875
ZAGG_NETW30_1000
ZAGG_OACHILD
ZAGG_OAGEN
ZAGG_OAMEN
ZAGG_OANOGEN
ZAGG_OAPETITE
ZAGG_OARTS
ZAGG_OATEEN
ZAGG_OAWOMEN
ZAGG_OCHILDPR
ZAGG_OCOMPHMO
ZAGG_OFOOD
ZAGG_OFURNITR
ZAGG_OGIFT
ZAGG_OHMCARE
ZAGG_OHOLIDAY
ZAGG_OJEWELRY
ZAGG_OSPECFD
ZAGG_OSTATION
ZAGG_PBRONCH
ZAGG_PDIADIET
ZAGG_PHEARTAT
ZAGG_PHRTMEDS
ZAGG_PORALINJ
ZAGG_PPAIN
ZAGG_PRESRI
ZAGG_PRHEUMTS
ZAGG_PSIR
ZAGG_PSKINMED
ZAGG_PVIAGRA
ZAGG_RAGEN
ZAGG_RARTS
ZAGG_RATEEN
ZAGG_RAWOMEN
ZAGG_RBOOKS
ZAGG_RC_M
ZAGG_RCOMPHMO
ZAGG_RCRAFTS
ZAGG_RDOLLARS
ZAGG_RELECTNC
ZAGG_RETAIL
ZAGG_RHEALTH
ZAGG_RHMFURN
ZAGG_RMUSIC
ZAGG_RSTATION
ZAGG_SPENDPAT_00
ZAGG_SPENDPAT_15
ZAGG_SPENDPAT_16
ZAGG_SSON
ZAGG_TRAD
ZAGG_TUOC_H
ZAGG_TUOC_N
ZAGG_VOTE
ZAGG_Z4HMVALU
ZAGG_ZINCDEC_5
);

%LET VARLIST_RF_internal =
%STR(
ANNL_PREM_BFOR
/*BENEFIT_POOL_ASSET_RATIO*/
CNT_FPO_PREV1
CNT_RBO_PREV1
DECISION_STAGE
POLICY_AGE_2
RATE_INCR
/*RATE_INCR_AFFORD*/
RATE_INCR_PRE
IF_SPOUSE_DISC
ISSUE_AGE
POLICYHOLDER_AGE
SHARED_POLICY
TOTAL_AMOUNT_PAID
CLAIM_COUNT_CLOSED_DYNAMIC
CLAIM_COUNT_CONTACT_DYNAMIC
CLAIM_COUNT_DYNAMIC
CLAIM_COUNT_NP_CLOSED_DYNAMIC
CLAIM_COUNT_OPEN_DYNAMIC
/*BINARY_CARDIO_TRANS*/
/*BINARY_DIAB_TRANS*/
/*BINARY_HERNIA_TRANS*/
/*BINARY_HYPT_TRANS*/
/*BINARY_NF_TRANS*/
/*BINARY_NM_TRANS*/
/*BINARY_NO_TRANS*/
/*BINARY_PREG_TRANS*/
/*SEV_DIAB_TRANS*/
/*SEV_NT_TRANS*/
/*BANK*/
/*BNKI*/
/*C210BLU*/
/*C210BPVT*/
/*C210CIP*/
/*C210EBI*/
/*C210HMI*/
/*C210HVA*/
/*C210KSES*/
/*C210MOB*/
/*C210MYS_2*/
/*C210MYS_7*/
/*C210PDV*/
/*C210WHT*/
/*HH_INCOME*/
/*HHMARST*/
/*HOMSTAT_R*/
/*HOMSTAT_T*/
/*IHCA*/
/*INMEDI*/
/*IOAPPLE*/
/*IORE*/
/*ITBI*/
/*ITCV*/
/*NETW*/
/*NOC19*/
/*ONLINE_22*/
/*PLIPITOR*/
/*ZINCDEC*/
/*BAL_BANKCARD_90TO119DPD*/
/*BAL_HELOAN_SEVEREDEROG*/
/*BAL_HELOC*/
/*BAL_HELOC_NEW*/
/*BAL_NONAGNFIRSTMTG*/
/*BAL_TOTALALLCREDIT_90TO119DPD*/
/*HH_BANKCARD_SEVEREDEROG*/
/*HH_BANKCARDCREDIT_60DPD*/
/*ZAGG_C210BLU*/
/*ZAGG_C210BPVT*/
/*ZAGG_C210CIP*/
/*ZAGG_C210EBI*/
/*ZAGG_C210HMI*/
/*ZAGG_C210HVA*/
/*ZAGG_C210MOB*/
/*ZAGG_C210PDV*/
/*ZAGG_C210WHT*/
/*ZAGG_HOMSTAT_R*/
/*ZAGG_ILOR_15*/
/*ZAGG_TUOC_B*/
/*ZAGG_TUOC_C*/
/*ZAGG_TUOC_Q*/
/*ZAGG_TUOC_S*/
BENE_TRM_BFOR
BIO_BEFORE_5_COMPOUND
BIO_BEFORE_NO_BIO
FREQ_BFOR_M
FREQ_BFOR_Q
IFA_GROUP_STATE_PROD_FA
IFA_GROUP_STATE_PROD_FAWP
ORIG_DBA_BFOR
PREM_PAID_TOT_2
STATE_AK
STATE_AR
STATE_AZ
STATE_CT
STATE_IA
STATE_IN
STATE_MD
STATE_MI
STATE_MN
STATE_OR
STATE_PARTNERSHIP
STATE_TX
STATE_UT
STATE_VA
STATE_WA
STATE_WI
STATE_WV
STATE_WY
TOTL_COMMUNICATED_INCR_RATE
/*BINARY_BP_TRANS*/
/*BINARY_CERV_TRANS*/
/*BINARY_DRUG_TRANS*/
/*BINARY_NS_TRANS*/
/*BINARY_NT_TRANS*/
/*SEV_HYPT_TRANS*/
/*SEV_RESP_TRANS*/
/*AACN*/
/*AALZ*/
/*AANG*/
/*ABED*/
/*ABRC*/
/*ACDE*/
/*ACNK*/
/*AHRA*/
/*AINS*/
/*AIRR*/
/*AOSA*/
/*APT_A*/
/*APT_C*/
/*APT_M*/
/*APT_N*/
/*APT_P*/
/*APT_T*/
/*ARHM*/
/*ASTK*/
/*BAVGONDL*/
/*BONDOLLR*/
/*BONORDER*/
/*C210APVT*/
/*C210MYS_3*/
/*C210MYS_6*/
/*CA00*/
/*CCW*/
/*CGN1_F*/
/*ESTDII30*/
/*ESTINC30*/
/*FAMP_M*/
/*FAMP_O*/
/*FMLY_PRSNC*/
/*HHCOMP_A*/
/*HHCOMP_D*/
/*HHCOMP_J*/
/*HOMSTAT_P*/
/*HOMSTAT_U*/
/*ICCO*/
/*ICDO*/
/*ICPL*/
/*ICSP*/
/*ICST*/
/*IDLOW*/
/*IHCR*/
/*IHDIY*/
/*IHIGHSCL*/
/*IHORSE*/
/*IMJE*/
/*IMPS*/
/*INIRA*/
/*IODVDPLR*/
/*IOMG*/
/*IOMO*/
/*IOOW*/
/*IOR_2*/
/*IOR_4*/
/*IOR_5*/
/*IORV*/
/*IOTEACHR*/
/*IOTR*/
/*IPSR*/
/*IRNA*/
/*ISSC*/
/*ITRV*/
/*MEDSUP*/
/*N2029*/
/*N3039*/
/*N6064*/
/*N65P*/
/*NAH19*/
/*NETW30*/
/*NXSD*/
/*OACHILD*/
/*OAPLUS*/
/*OATEEN*/
/*OCOLLECT*/
/*ONLINE_10*/
/*ONLINE_27*/
/*ONLINE_33*/
/*PALLERGY*/
/*PANTIDEP*/
/*PANXIETY*/
/*PARTHRTS*/
/*PATHFOOT*/
/*PCHODIET*/
/*PCHOLMED*/
/*PDIAORAL*/
/*PHEARTRX*/
/*PHORM*/
/*PHRTMEDS*/
/*PLASERVC*/
/*PMENSTRL*/
/*PPAIN*/
/*PPREMARN*/
/*PSIR*/
/*PSNORING*/
/*PVIS*/
/*PVITSUPP*/
/*RACHILD*/
/*RAUTO*/
/*RBEAUTY*/
/*RBOOKS*/
/*RCOMPHMO*/
/*RELECTNC*/
/*RFOOD*/
/*RFURNITR*/
/*RGARDEN*/
/*RGIFT*/
/*RHEALTH*/
/*RHMCARE*/
/*RHMFURN*/
/*RHOLIDAY*/
/*RHSWARE*/
/*RJEWELRY*/
/*RLINENS*/
/*ROTHER*/
/*RTRAVEL*/
/*SPENDPAT*/
/*SSON*/
/*TUOC_C*/
/*TUOC_D*/
/*TUOC_G*/
/*TUOC_H*/
/*TUOC_K*/
/*TUOCS_B*/
/*TUOCS_D*/
/*TUOCS_E*/
/*TUOCS_G*/
/*TUOCS_L*/
/*TUOCS_M*/
/*VAC*/
/*VFLAG*/
/*BAL_1STMTGCREDIT*/
/*BAL_1STMTGCREDIT_NEW*/
/*BAL_AGENCYFIRSTMTG*/
/*BAL_AUTOFINANCE_60DPD*/
/*BAL_HELOC_90TO119DPD*/
/*BAL_HOMEEQUITYLOANS_60DPD*/
/*BAL_MTGCREDIT*/
/*BAL_MTGCREDIT_NEW*/
/*BAL_NONAGN1STMORG_60TO89DPD*/
/*BAL_NONAGN1STMORG_BANKRUPTC*/
/*BAL_NONMTGCREDIT_60DPD*/
/*BAL_RETAILCREDIT_60DPD*/
/*BAL_TOTALALLCREDIT*/
/*BAL_TOTALALLCREDIT_60TO89DPD*/
/*HH_AUTOBANK_60DPD*/
/*HH_BANKCARD_60TO89DPD*/
/*HH_BANKCARD_COLLECTIONS*/
/*HH_CONSUMERFINANCE_60DPD*/
/*HH_HELOAN_30TO59DPD*/
/*HH_HELOAN_COLLECTIONS*/
/*HH_HELOAN_SEVEREDEROG*/
/*HH_HELOC*/
/*HH_HOMEEQUITYLOANS_60DPD*/
/*HH_MTG_60TO89DPD*/
/*HH_NONAGN1STMORG_60TO89DPD*/
/*HH_NONAGN1STMORG_90TO119DPD*/
/*HH_NONMTGCREDIT_60DPD*/
/*HH_RETAILCREDIT_60DPD*/
/*HH_TOTALALLCREDIT_60DPD*/
/*HH_TOTALALLCREDIT_60TO89DPD*/
/*HH_TOTALALLCREDIT_90TO119DPD*/
/*HH_TOTALALLCREDIT_BANKRUPTCY*/
/*HH_TOTALALLCREDIT_COLLECTIONS*/
/*HH_TOTALALLCREDIT_SEVEREDEROG*/
/*HIGHCRD_1STMTGCREDIT*/
/*HIGHCRD_1STMTGCREDIT_NEW*/
/*HIGHCRD_AGENCYFIRSTMTG*/
/*HIGHCRD_AGENCYFIRSTMTG_NEW*/
/*HIGHCRD_BANKCARDCREDIT*/
/*HIGHCRD_BANKCARDCREDIT_NEW*/
/*HIGHCRD_HELOC*/
/*HIGHCRD_HELOC_NEW*/
/*HIGHCRD_MTGCREDIT*/
/*HIGHCRD_MTGCREDIT_NEW*/
/*HIGHCRD_NONAGNFIRSTMTG*/
/*HIGHCRD_NONMTGCREDIT*/
/*HIGHCRD_TOTALALLCREDIT*/
/*NUM_1STMTG_60TO89DPD*/
/*NUM_1STMTG_90TO119DPD*/
/*NUM_1STMTG_BANKRUPTCY*/
/*NUM_1STMTG_SEVEREDEROG*/
/*NUM_AGENCY1STMORG_30TO59DPD*/
/*NUM_AUTOBANK_60DPD*/
/*NUM_BANKCARD_90TO119DPD*/
/*NUM_BANKCARD_SEVEREDEROG*/
/*NUM_HELOC_60DPD*/
/*NUM_HELOC_SEVEREDEROG*/
/*NUM_HOMEEQUITYLOANS_60DPD*/
/*NUM_MTG_60TO89DPD*/
/*NUM_NONAGN1STMORG_60TO89DPD*/
/*NUM_NONAGNFIRSTMTG_NEW*/
/*NUM_NONMTGCREDIT_60DPD*/
/*NUM_RETAILCREDIT_60DPD*/
/*NUM_STUDENTLOAN_60DPD*/
/*NUM_TOTALALLCREDIT_BANKRUPTCY*/
/*PRCNT_AUTOLEASEMATURATION*/
/*PRCNT_NONMTGCREDIT*/
/*ZAGG_AGJOINT*/
/*ZAGG_APT_P*/
/*ZAGG_BAVGDOLL*/
/*ZAGG_BAVGOFDL*/
/*ZAGG_BAVGONDL*/
/*ZAGG_BC_M*/
/*ZAGG_BOFDOLLR*/
/*ZAGG_BOFORDER*/
/*ZAGG_BONDOLLR*/
/*ZAGG_BONORDER*/
/*ZAGG_BPAMEXY*/
/*ZAGG_BPCREDIT*/
/*ZAGG_BPVISA*/
/*ZAGG_BTOTLDOL*/
/*ZAGG_BTOTLORD*/
/*ZAGG_BUYER*/
/*ZAGG_C210APVT*/
/*ZAGG_C210B200*/
/*ZAGG_C210PBL*/
/*ZAGG_CCW*/
/*ZAGG_COLLEGE*/
/*ZAGG_ESTINC30__AVE_A_*/
/*ZAGG_ESTINC30_007*/
/*ZAGG_ESTINC30_017*/
/*ZAGG_ESTINC30_025*/
/*ZAGG_ESTINC30_035*/
/*ZAGG_ESTINC30_045*/
/*ZAGG_ESTINC30_055*/
/*ZAGG_ESTINC30_067*/
/*ZAGG_ESTINC30_174*/
/*ZAGG_ESTINC30_224*/
/*ZAGG_ESTINC30_324*/
/*ZAGG_ESTINC30_449*/
/*ZAGG_ESTINC30_500*/
/*ZAGG_FINB*/
/*ZAGG_HOMSTAT_T*/
/*ZAGG_HOMSTAT_U*/
/*ZAGG_HOMSTAT_Y*/
/*ZAGG_IDNAT*/
/*ZAGG_IHGO*/
/*ZAGG_IHGREEN*/
/*ZAGG_IHIGHSCL*/
/*ZAGG_IHORSE*/
/*ZAGG_IMGO*/
/*ZAGG_INSHOP*/
/*ZAGG_IOLA*/
/*ZAGG_IOMO*/
/*ZAGG_IOR_6*/
/*ZAGG_IORV*/
/*ZAGG_IRAS*/
/*ZAGG_ISCA*/
/*ZAGG_ITRV*/
/*ZAGG_MEDSUP_9*/
/*ZAGG_MOBPLUS_M*/
/*ZAGG_MOBPLUS_P*/
/*ZAGG_NETW30__AVE_A_*/
/*ZAGG_NETW30_0000*/
/*ZAGG_NETW30_0003*/
/*ZAGG_NETW30_0019*/
/*ZAGG_NETW30_0037*/
/*ZAGG_NETW30_0062*/
/*ZAGG_NETW30_0087*/
/*ZAGG_NETW30_0625*/
/*ZAGG_NETW30_0875*/
/*ZAGG_NETW30_1000*/
/*ZAGG_OACHILD*/
/*ZAGG_OAGEN*/
/*ZAGG_OAMEN*/
/*ZAGG_OANOGEN*/
/*ZAGG_OAPETITE*/
/*ZAGG_OARTS*/
/*ZAGG_OATEEN*/
/*ZAGG_OAWOMEN*/
/*ZAGG_OCHILDPR*/
/*ZAGG_OCOMPHMO*/
/*ZAGG_OFOOD*/
/*ZAGG_OFURNITR*/
/*ZAGG_OGIFT*/
/*ZAGG_OHMCARE*/
/*ZAGG_OHOLIDAY*/
/*ZAGG_OJEWELRY*/
/*ZAGG_OSPECFD*/
/*ZAGG_OSTATION*/
/*ZAGG_PBRONCH*/
/*ZAGG_PDIADIET*/
/*ZAGG_PHEARTAT*/
/*ZAGG_PHRTMEDS*/
/*ZAGG_PORALINJ*/
/*ZAGG_PPAIN*/
/*ZAGG_PRESRI*/
/*ZAGG_PRHEUMTS*/
/*ZAGG_PSIR*/
/*ZAGG_PSKINMED*/
/*ZAGG_PVIAGRA*/
/*ZAGG_RAGEN*/
/*ZAGG_RARTS*/
/*ZAGG_RATEEN*/
/*ZAGG_RAWOMEN*/
/*ZAGG_RBOOKS*/
/*ZAGG_RC_M*/
/*ZAGG_RCOMPHMO*/
/*ZAGG_RCRAFTS*/
/*ZAGG_RDOLLARS*/
/*ZAGG_RELECTNC*/
/*ZAGG_RETAIL*/
/*ZAGG_RHEALTH*/
/*ZAGG_RHMFURN*/
/*ZAGG_RMUSIC*/
/*ZAGG_RSTATION*/
/*ZAGG_SPENDPAT_00*/
/*ZAGG_SPENDPAT_15*/
/*ZAGG_SPENDPAT_16*/
/*ZAGG_SSON*/
/*ZAGG_TRAD*/
/*ZAGG_TUOC_H*/
/*ZAGG_TUOC_N*/
/*ZAGG_VOTE*/
/*ZAGG_Z4HMVALU*/
/*ZAGG_ZINCDEC_5*/
);


%let varlist_rf1 = %str(
POST_URS_VAR1
POST_URS_VAR2
POST_URS_VAR3
TREE_NFO1A
TREE_NFO3A
TREE_NFO4
ANNL_PREM_BFOR
BENEFIT_POOL_ASSET_RATIO
/*BIO_AFTER_NO_BIO*/
CNT_FPO_PREV1
CNT_RBO_PREV1
DECISION_STAGE
POLICY_AGE_2
RATE_INCR
RATE_INCR_AFFORD
RATE_INCR_PRE
IF_SPOUSE_DISC
ISSUE_AGE
POLICYHOLDER_AGE
SHARED_POLICY
TOTAL_AMOUNT_PAID
CLAIM_COUNT_CLOSED_DYNAMIC
CLAIM_COUNT_CONTACT_DYNAMIC
CLAIM_COUNT_DYNAMIC
CLAIM_COUNT_NP_CLOSED_DYNAMIC
CLAIM_COUNT_OPEN_DYNAMIC
BINARY_CARDIO_TRANS
BINARY_DIAB_TRANS
BINARY_HERNIA_TRANS
BINARY_HYPT_TRANS
BINARY_NF_TRANS
BINARY_NM_TRANS
BINARY_NO_TRANS
BINARY_PREG_TRANS
SEV_DIAB_TRANS
SEV_NT_TRANS
BANK
BNKI
C210BLU
C210BPVT
C210CIP
C210EBI
C210HMI
C210HVA
C210KSES
C210MOB
C210MYS_2
C210MYS_7
C210PDV
C210WHT
HH_INCOME
HHMARST
HOMSTAT_R
HOMSTAT_T
IHCA
INMEDI
IOAPPLE
IORE
ITBI
ITCV
NETW
NOC19
ONLINE_22
PLIPITOR
ZINCDEC
BAL_BANKCARD_90TO119DPD
BAL_HELOAN_SEVEREDEROG
BAL_HELOC
BAL_HELOC_NEW
BAL_NONAGNFIRSTMTG
BAL_TOTALALLCREDIT_90TO119DPD
HH_BANKCARD_SEVEREDEROG
HH_BANKCARDCREDIT_60DPD
ZAGG_C210BLU
ZAGG_C210BPVT
ZAGG_C210CIP
ZAGG_C210EBI
ZAGG_C210HMI
ZAGG_C210HVA
ZAGG_C210MOB
ZAGG_C210PDV
ZAGG_C210WHT
ZAGG_HOMSTAT_R
ZAGG_ILOR_15
ZAGG_TUOC_B
ZAGG_TUOC_C
ZAGG_TUOC_Q
ZAGG_TUOC_S
BENE_TRM_BFOR
BIO_BEFORE_5_COMPOUND
BIO_BEFORE_NO_BIO
FREQ_BFOR_M
FREQ_BFOR_Q
IFA_GROUP_STATE_PROD_FA
IFA_GROUP_STATE_PROD_FAWP
ORIG_DBA_BFOR
PREM_PAID_TOT_2
STATE_AK
STATE_AR
STATE_AZ
STATE_CT
STATE_IA
STATE_IN
STATE_MD
STATE_MI
STATE_MN
STATE_OR
STATE_PARTNERSHIP
STATE_TX
STATE_UT
STATE_VA
STATE_WA
STATE_WI
STATE_WV
STATE_WY
TOTL_COMMUNICATED_INCR_RATE
BINARY_BP_TRANS
BINARY_CERV_TRANS
BINARY_DRUG_TRANS
BINARY_NS_TRANS
BINARY_NT_TRANS
SEV_HYPT_TRANS
SEV_RESP_TRANS
AACN
AALZ
AANG
ABED
ABRC
ACDE
ACNK
AHRA
AINS
AIRR
AOSA
APT_A
APT_C
APT_M
APT_N
APT_P
APT_T
ARHM
ASTK
BAVGONDL
BONDOLLR
BONORDER
C210APVT
C210MYS_3
C210MYS_6
CA00
CCW
CGN1_F
ESTDII30
ESTINC30
FAMP_M
FAMP_O
FMLY_PRSNC
HHCOMP_A
HHCOMP_D
HHCOMP_J
HOMSTAT_P
HOMSTAT_U
ICCO
ICDO
ICPL
ICSP
ICST
IDLOW
IHCR
IHDIY
IHIGHSCL
IHORSE
IMJE
IMPS
INIRA
IODVDPLR
IOMG
IOMO
IOOW
IOR_2
IOR_4
IOR_5
IORV
IOTEACHR
IOTR
IPSR
IRNA
ISSC
ITRV
MEDSUP
N2029
N3039
N6064
N65P
NAH19
NETW30
NXSD
OACHILD
OAPLUS
OATEEN
OCOLLECT
ONLINE_10
ONLINE_27
ONLINE_33
PALLERGY
PANTIDEP
PANXIETY
PARTHRTS
PATHFOOT
PCHODIET
PCHOLMED
PDIAORAL
PHEARTRX
PHORM
PHRTMEDS
PLASERVC
PMENSTRL
PPAIN
PPREMARN
PSIR
PSNORING
PVIS
PVITSUPP
RACHILD
RAUTO
RBEAUTY
RBOOKS
RCOMPHMO
RELECTNC
RFOOD
RFURNITR
RGARDEN
RGIFT
RHEALTH
RHMCARE
RHMFURN
RHOLIDAY
RHSWARE
RJEWELRY
RLINENS
ROTHER
RTRAVEL
SPENDPAT
SSON
TUOC_C
TUOC_D
TUOC_G
TUOC_H
TUOC_K
TUOCS_B
TUOCS_D
TUOCS_E
TUOCS_G
TUOCS_L
TUOCS_M
VAC
VFLAG
BAL_1STMTGCREDIT
BAL_1STMTGCREDIT_NEW
BAL_AGENCYFIRSTMTG
BAL_AUTOFINANCE_60DPD
BAL_HELOC_90TO119DPD
BAL_HOMEEQUITYLOANS_60DPD
BAL_MTGCREDIT
BAL_MTGCREDIT_NEW
BAL_NONAGN1STMORG_60TO89DPD
BAL_NONAGN1STMORG_BANKRUPTC
BAL_NONMTGCREDIT_60DPD
BAL_RETAILCREDIT_60DPD
BAL_TOTALALLCREDIT
BAL_TOTALALLCREDIT_60TO89DPD
HH_AUTOBANK_60DPD
HH_BANKCARD_60TO89DPD
HH_BANKCARD_COLLECTIONS
HH_CONSUMERFINANCE_60DPD
HH_HELOAN_30TO59DPD
HH_HELOAN_COLLECTIONS
HH_HELOAN_SEVEREDEROG
HH_HELOC
HH_HOMEEQUITYLOANS_60DPD
HH_MTG_60TO89DPD
HH_NONAGN1STMORG_60TO89DPD
HH_NONAGN1STMORG_90TO119DPD
HH_NONMTGCREDIT_60DPD
HH_RETAILCREDIT_60DPD
HH_TOTALALLCREDIT_60DPD
HH_TOTALALLCREDIT_60TO89DPD
HH_TOTALALLCREDIT_90TO119DPD
HH_TOTALALLCREDIT_BANKRUPTCY
HH_TOTALALLCREDIT_COLLECTIONS
HH_TOTALALLCREDIT_SEVEREDEROG
HIGHCRD_1STMTGCREDIT
HIGHCRD_1STMTGCREDIT_NEW
HIGHCRD_AGENCYFIRSTMTG
HIGHCRD_AGENCYFIRSTMTG_NEW
HIGHCRD_BANKCARDCREDIT
HIGHCRD_BANKCARDCREDIT_NEW
HIGHCRD_HELOC
HIGHCRD_HELOC_NEW
HIGHCRD_MTGCREDIT
HIGHCRD_MTGCREDIT_NEW
HIGHCRD_NONAGNFIRSTMTG
HIGHCRD_NONMTGCREDIT
HIGHCRD_TOTALALLCREDIT
NUM_1STMTG_60TO89DPD
NUM_1STMTG_90TO119DPD
NUM_1STMTG_BANKRUPTCY
NUM_1STMTG_SEVEREDEROG
NUM_AGENCY1STMORG_30TO59DPD
NUM_AUTOBANK_60DPD
NUM_BANKCARD_90TO119DPD
NUM_BANKCARD_SEVEREDEROG
NUM_HELOC_60DPD
NUM_HELOC_SEVEREDEROG
NUM_HOMEEQUITYLOANS_60DPD
NUM_MTG_60TO89DPD
NUM_NONAGN1STMORG_60TO89DPD
NUM_NONAGNFIRSTMTG_NEW
NUM_NONMTGCREDIT_60DPD
NUM_RETAILCREDIT_60DPD
NUM_STUDENTLOAN_60DPD
NUM_TOTALALLCREDIT_BANKRUPTCY
PRCNT_AUTOLEASEMATURATION
PRCNT_NONMTGCREDIT
ZAGG_AGJOINT
ZAGG_APT_P
ZAGG_BAVGDOLL
ZAGG_BAVGOFDL
ZAGG_BAVGONDL
ZAGG_BC_M
ZAGG_BOFDOLLR
ZAGG_BOFORDER
ZAGG_BONDOLLR
ZAGG_BONORDER
ZAGG_BPAMEXY
ZAGG_BPCREDIT
ZAGG_BPVISA
ZAGG_BTOTLDOL
ZAGG_BTOTLORD
ZAGG_BUYER
ZAGG_C210APVT
ZAGG_C210B200
ZAGG_C210PBL
ZAGG_CCW
ZAGG_COLLEGE
ZAGG_ESTINC30__AVE_A_
ZAGG_ESTINC30_007
ZAGG_ESTINC30_017
ZAGG_ESTINC30_025
ZAGG_ESTINC30_035
ZAGG_ESTINC30_045
ZAGG_ESTINC30_055
ZAGG_ESTINC30_067
ZAGG_ESTINC30_174
ZAGG_ESTINC30_224
ZAGG_ESTINC30_324
ZAGG_ESTINC30_449
ZAGG_ESTINC30_500
ZAGG_FINB
ZAGG_HOMSTAT_T
ZAGG_HOMSTAT_U
ZAGG_HOMSTAT_Y
ZAGG_IDNAT
ZAGG_IHGO
ZAGG_IHGREEN
ZAGG_IHIGHSCL
ZAGG_IHORSE
ZAGG_IMGO
ZAGG_INSHOP
ZAGG_IOLA
ZAGG_IOMO
ZAGG_IOR_6
ZAGG_IORV
ZAGG_IRAS
ZAGG_ISCA
ZAGG_ITRV
ZAGG_MEDSUP_9
ZAGG_MOBPLUS_M
ZAGG_MOBPLUS_P
ZAGG_NETW30__AVE_A_
ZAGG_NETW30_0000
ZAGG_NETW30_0003
ZAGG_NETW30_0019
ZAGG_NETW30_0037
ZAGG_NETW30_0062
ZAGG_NETW30_0087
ZAGG_NETW30_0625
ZAGG_NETW30_0875
ZAGG_NETW30_1000
ZAGG_OACHILD
ZAGG_OAGEN
ZAGG_OAMEN
ZAGG_OANOGEN
ZAGG_OAPETITE
ZAGG_OARTS
ZAGG_OATEEN
ZAGG_OAWOMEN
ZAGG_OCHILDPR
ZAGG_OCOMPHMO
ZAGG_OFOOD
ZAGG_OFURNITR
ZAGG_OGIFT
ZAGG_OHMCARE
ZAGG_OHOLIDAY
ZAGG_OJEWELRY
ZAGG_OSPECFD
ZAGG_OSTATION
ZAGG_PBRONCH
ZAGG_PDIADIET
ZAGG_PHEARTAT
ZAGG_PHRTMEDS
ZAGG_PORALINJ
ZAGG_PPAIN
ZAGG_PRESRI
ZAGG_PRHEUMTS
ZAGG_PSIR
ZAGG_PSKINMED
ZAGG_PVIAGRA
ZAGG_RAGEN
ZAGG_RARTS
ZAGG_RATEEN
ZAGG_RAWOMEN
ZAGG_RBOOKS
ZAGG_RC_M
ZAGG_RCOMPHMO
ZAGG_RCRAFTS
ZAGG_RDOLLARS
ZAGG_RELECTNC
ZAGG_RETAIL
ZAGG_RHEALTH
ZAGG_RHMFURN
ZAGG_RMUSIC
ZAGG_RSTATION
ZAGG_SPENDPAT_00
ZAGG_SPENDPAT_15
ZAGG_SPENDPAT_16
ZAGG_SSON
ZAGG_TRAD
ZAGG_TUOC_H
ZAGG_TUOC_N
ZAGG_VOTE
ZAGG_Z4HMVALU
ZAGG_ZINCDEC_5
);

%let varlist_rf1_internal = %str(
/*POST_URS_VAR1*/
/*POST_URS_VAR2*/
/*POST_URS_VAR3*/
TREE_NFO1A
TREE_NFO3A
TREE_NFO4
ANNL_PREM_BFOR
/*BENEFIT_POOL_ASSET_RATIO*/
/*BIO_AFTER_NO_BIO*/
CNT_FPO_PREV1
CNT_RBO_PREV1
DECISION_STAGE
POLICY_AGE_2
RATE_INCR
/*RATE_INCR_AFFORD*/
RATE_INCR_PRE
IF_SPOUSE_DISC
ISSUE_AGE
POLICYHOLDER_AGE
SHARED_POLICY
TOTAL_AMOUNT_PAID
CLAIM_COUNT_CLOSED_DYNAMIC
CLAIM_COUNT_CONTACT_DYNAMIC
CLAIM_COUNT_DYNAMIC
CLAIM_COUNT_NP_CLOSED_DYNAMIC
CLAIM_COUNT_OPEN_DYNAMIC
/*BINARY_CARDIO_TRANS*/
/*BINARY_DIAB_TRANS*/
/*BINARY_HERNIA_TRANS*/
/*BINARY_HYPT_TRANS*/
/*BINARY_NF_TRANS*/
/*BINARY_NM_TRANS*/
/*BINARY_NO_TRANS*/
/*BINARY_PREG_TRANS*/
/*SEV_DIAB_TRANS*/
/*SEV_NT_TRANS*/
/*BANK*/
/*BNKI*/
/*C210BLU*/
/*C210BPVT*/
/*C210CIP*/
/*C210EBI*/
/*C210HMI*/
/*C210HVA*/
/*C210KSES*/
/*C210MOB*/
/*C210MYS_2*/
/*C210MYS_7*/
/*C210PDV*/
/*C210WHT*/
/*HH_INCOME*/
/*HHMARST*/
/*HOMSTAT_R*/
/*HOMSTAT_T*/
/*IHCA*/
/*INMEDI*/
/*IOAPPLE*/
/*IORE*/
/*ITBI*/
/*ITCV*/
/*NETW*/
/*NOC19*/
/*ONLINE_22*/
/*PLIPITOR*/
/*ZINCDEC*/
/*BAL_BANKCARD_90TO119DPD*/
/*BAL_HELOAN_SEVEREDEROG*/
/*BAL_HELOC*/
/*BAL_HELOC_NEW*/
/*BAL_NONAGNFIRSTMTG*/
/*BAL_TOTALALLCREDIT_90TO119DPD*/
/*HH_BANKCARD_SEVEREDEROG*/
/*HH_BANKCARDCREDIT_60DPD*/
/*ZAGG_C210BLU*/
/*ZAGG_C210BPVT*/
/*ZAGG_C210CIP*/
/*ZAGG_C210EBI*/
/*ZAGG_C210HMI*/
/*ZAGG_C210HVA*/
/*ZAGG_C210MOB*/
/*ZAGG_C210PDV*/
/*ZAGG_C210WHT*/
/*ZAGG_HOMSTAT_R*/
/*ZAGG_ILOR_15*/
/*ZAGG_TUOC_B*/
/*ZAGG_TUOC_C*/
/*ZAGG_TUOC_Q*/
/*ZAGG_TUOC_S*/
BENE_TRM_BFOR
BIO_BEFORE_5_COMPOUND
BIO_BEFORE_NO_BIO
FREQ_BFOR_M
FREQ_BFOR_Q
IFA_GROUP_STATE_PROD_FA
IFA_GROUP_STATE_PROD_FAWP
ORIG_DBA_BFOR
PREM_PAID_TOT_2
STATE_AK
STATE_AR
STATE_AZ
STATE_CT
STATE_IA
STATE_IN
STATE_MD
STATE_MI
STATE_MN
STATE_OR
STATE_PARTNERSHIP
STATE_TX
STATE_UT
STATE_VA
STATE_WA
STATE_WI
STATE_WV
STATE_WY
TOTL_COMMUNICATED_INCR_RATE
/*BINARY_BP_TRANS*/
/*BINARY_CERV_TRANS*/
/*BINARY_DRUG_TRANS*/
/*BINARY_NS_TRANS*/
/*BINARY_NT_TRANS*/
/*SEV_HYPT_TRANS*/
/*SEV_RESP_TRANS*/
/*AACN*/
/*AALZ*/
/*AANG*/
/*ABED*/
/*ABRC*/
/*ACDE*/
/*ACNK*/
/*AHRA*/
/*AINS*/
/*AIRR*/
/*AOSA*/
/*APT_A*/
/*APT_C*/
/*APT_M*/
/*APT_N*/
/*APT_P*/
/*APT_T*/
/*ARHM*/
/*ASTK*/
/*BAVGONDL*/
/*BONDOLLR*/
/*BONORDER*/
/*C210APVT*/
/*C210MYS_3*/
/*C210MYS_6*/
/*CA00*/
/*CCW*/
/*CGN1_F*/
/*ESTDII30*/
/*ESTINC30*/
/*FAMP_M*/
/*FAMP_O*/
/*FMLY_PRSNC*/
/*HHCOMP_A*/
/*HHCOMP_D*/
/*HHCOMP_J*/
/*HOMSTAT_P*/
/*HOMSTAT_U*/
/*ICCO*/
/*ICDO*/
/*ICPL*/
/*ICSP*/
/*ICST*/
/*IDLOW*/
/*IHCR*/
/*IHDIY*/
/*IHIGHSCL*/
/*IHORSE*/
/*IMJE*/
/*IMPS*/
/*INIRA*/
/*IODVDPLR*/
/*IOMG*/
/*IOMO*/
/*IOOW*/
/*IOR_2*/
/*IOR_4*/
/*IOR_5*/
/*IORV*/
/*IOTEACHR*/
/*IOTR*/
/*IPSR*/
/*IRNA*/
/*ISSC*/
/*ITRV*/
/*MEDSUP*/
/*N2029*/
/*N3039*/
/*N6064*/
/*N65P*/
/*NAH19*/
/*NETW30*/
/*NXSD*/
/*OACHILD*/
/*OAPLUS*/
/*OATEEN*/
/*OCOLLECT*/
/*ONLINE_10*/
/*ONLINE_27*/
/*ONLINE_33*/
/*PALLERGY*/
/*PANTIDEP*/
/*PANXIETY*/
/*PARTHRTS*/
/*PATHFOOT*/
/*PCHODIET*/
/*PCHOLMED*/
/*PDIAORAL*/
/*PHEARTRX*/
/*PHORM*/
/*PHRTMEDS*/
/*PLASERVC*/
/*PMENSTRL*/
/*PPAIN*/
/*PPREMARN*/
/*PSIR*/
/*PSNORING*/
/*PVIS*/
/*PVITSUPP*/
/*RACHILD*/
/*RAUTO*/
/*RBEAUTY*/
/*RBOOKS*/
/*RCOMPHMO*/
/*RELECTNC*/
/*RFOOD*/
/*RFURNITR*/
/*RGARDEN*/
/*RGIFT*/
/*RHEALTH*/
/*RHMCARE*/
/*RHMFURN*/
/*RHOLIDAY*/
/*RHSWARE*/
/*RJEWELRY*/
/*RLINENS*/
/*ROTHER*/
/*RTRAVEL*/
/*SPENDPAT*/
/*SSON*/
/*TUOC_C*/
/*TUOC_D*/
/*TUOC_G*/
/*TUOC_H*/
/*TUOC_K*/
/*TUOCS_B*/
/*TUOCS_D*/
/*TUOCS_E*/
/*TUOCS_G*/
/*TUOCS_L*/
/*TUOCS_M*/
/*VAC*/
/*VFLAG*/
/*BAL_1STMTGCREDIT*/
/*BAL_1STMTGCREDIT_NEW*/
/*BAL_AGENCYFIRSTMTG*/
/*BAL_AUTOFINANCE_60DPD*/
/*BAL_HELOC_90TO119DPD*/
/*BAL_HOMEEQUITYLOANS_60DPD*/
/*BAL_MTGCREDIT*/
/*BAL_MTGCREDIT_NEW*/
/*BAL_NONAGN1STMORG_60TO89DPD*/
/*BAL_NONAGN1STMORG_BANKRUPTC*/
/*BAL_NONMTGCREDIT_60DPD*/
/*BAL_RETAILCREDIT_60DPD*/
/*BAL_TOTALALLCREDIT*/
/*BAL_TOTALALLCREDIT_60TO89DPD*/
/*HH_AUTOBANK_60DPD*/
/*HH_BANKCARD_60TO89DPD*/
/*HH_BANKCARD_COLLECTIONS*/
/*HH_CONSUMERFINANCE_60DPD*/
/*HH_HELOAN_30TO59DPD*/
/*HH_HELOAN_COLLECTIONS*/
/*HH_HELOAN_SEVEREDEROG*/
/*HH_HELOC*/
/*HH_HOMEEQUITYLOANS_60DPD*/
/*HH_MTG_60TO89DPD*/
/*HH_NONAGN1STMORG_60TO89DPD*/
/*HH_NONAGN1STMORG_90TO119DPD*/
/*HH_NONMTGCREDIT_60DPD*/
/*HH_RETAILCREDIT_60DPD*/
/*HH_TOTALALLCREDIT_60DPD*/
/*HH_TOTALALLCREDIT_60TO89DPD*/
/*HH_TOTALALLCREDIT_90TO119DPD*/
/*HH_TOTALALLCREDIT_BANKRUPTCY*/
/*HH_TOTALALLCREDIT_COLLECTIONS*/
/*HH_TOTALALLCREDIT_SEVEREDEROG*/
/*HIGHCRD_1STMTGCREDIT*/
/*HIGHCRD_1STMTGCREDIT_NEW*/
/*HIGHCRD_AGENCYFIRSTMTG*/
/*HIGHCRD_AGENCYFIRSTMTG_NEW*/
/*HIGHCRD_BANKCARDCREDIT*/
/*HIGHCRD_BANKCARDCREDIT_NEW*/
/*HIGHCRD_HELOC*/
/*HIGHCRD_HELOC_NEW*/
/*HIGHCRD_MTGCREDIT*/
/*HIGHCRD_MTGCREDIT_NEW*/
/*HIGHCRD_NONAGNFIRSTMTG*/
/*HIGHCRD_NONMTGCREDIT*/
/*HIGHCRD_TOTALALLCREDIT*/
/*NUM_1STMTG_60TO89DPD*/
/*NUM_1STMTG_90TO119DPD*/
/*NUM_1STMTG_BANKRUPTCY*/
/*NUM_1STMTG_SEVEREDEROG*/
/*NUM_AGENCY1STMORG_30TO59DPD*/
/*NUM_AUTOBANK_60DPD*/
/*NUM_BANKCARD_90TO119DPD*/
/*NUM_BANKCARD_SEVEREDEROG*/
/*NUM_HELOC_60DPD*/
/*NUM_HELOC_SEVEREDEROG*/
/*NUM_HOMEEQUITYLOANS_60DPD*/
/*NUM_MTG_60TO89DPD*/
/*NUM_NONAGN1STMORG_60TO89DPD*/
/*NUM_NONAGNFIRSTMTG_NEW*/
/*NUM_NONMTGCREDIT_60DPD*/
/*NUM_RETAILCREDIT_60DPD*/
/*NUM_STUDENTLOAN_60DPD*/
/*NUM_TOTALALLCREDIT_BANKRUPTCY*/
/*PRCNT_AUTOLEASEMATURATION*/
/*PRCNT_NONMTGCREDIT*/
/*ZAGG_AGJOINT*/
/*ZAGG_APT_P*/
/*ZAGG_BAVGDOLL*/
/*ZAGG_BAVGOFDL*/
/*ZAGG_BAVGONDL*/
/*ZAGG_BC_M*/
/*ZAGG_BOFDOLLR*/
/*ZAGG_BOFORDER*/
/*ZAGG_BONDOLLR*/
/*ZAGG_BONORDER*/
/*ZAGG_BPAMEXY*/
/*ZAGG_BPCREDIT*/
/*ZAGG_BPVISA*/
/*ZAGG_BTOTLDOL*/
/*ZAGG_BTOTLORD*/
/*ZAGG_BUYER*/
/*ZAGG_C210APVT*/
/*ZAGG_C210B200*/
/*ZAGG_C210PBL*/
/*ZAGG_CCW*/
/*ZAGG_COLLEGE*/
/*ZAGG_ESTINC30__AVE_A_*/
/*ZAGG_ESTINC30_007*/
/*ZAGG_ESTINC30_017*/
/*ZAGG_ESTINC30_025*/
/*ZAGG_ESTINC30_035*/
/*ZAGG_ESTINC30_045*/
/*ZAGG_ESTINC30_055*/
/*ZAGG_ESTINC30_067*/
/*ZAGG_ESTINC30_174*/
/*ZAGG_ESTINC30_224*/
/*ZAGG_ESTINC30_324*/
/*ZAGG_ESTINC30_449*/
/*ZAGG_ESTINC30_500*/
/*ZAGG_FINB*/
/*ZAGG_HOMSTAT_T*/
/*ZAGG_HOMSTAT_U*/
/*ZAGG_HOMSTAT_Y*/
/*ZAGG_IDNAT*/
/*ZAGG_IHGO*/
/*ZAGG_IHGREEN*/
/*ZAGG_IHIGHSCL*/
/*ZAGG_IHORSE*/
/*ZAGG_IMGO*/
/*ZAGG_INSHOP*/
/*ZAGG_IOLA*/
/*ZAGG_IOMO*/
/*ZAGG_IOR_6*/
/*ZAGG_IORV*/
/*ZAGG_IRAS*/
/*ZAGG_ISCA*/
/*ZAGG_ITRV*/
/*ZAGG_MEDSUP_9*/
/*ZAGG_MOBPLUS_M*/
/*ZAGG_MOBPLUS_P*/
/*ZAGG_NETW30__AVE_A_*/
/*ZAGG_NETW30_0000*/
/*ZAGG_NETW30_0003*/
/*ZAGG_NETW30_0019*/
/*ZAGG_NETW30_0037*/
/*ZAGG_NETW30_0062*/
/*ZAGG_NETW30_0087*/
/*ZAGG_NETW30_0625*/
/*ZAGG_NETW30_0875*/
/*ZAGG_NETW30_1000*/
/*ZAGG_OACHILD*/
/*ZAGG_OAGEN*/
/*ZAGG_OAMEN*/
/*ZAGG_OANOGEN*/
/*ZAGG_OAPETITE*/
/*ZAGG_OARTS*/
/*ZAGG_OATEEN*/
/*ZAGG_OAWOMEN*/
/*ZAGG_OCHILDPR*/
/*ZAGG_OCOMPHMO*/
/*ZAGG_OFOOD*/
/*ZAGG_OFURNITR*/
/*ZAGG_OGIFT*/
/*ZAGG_OHMCARE*/
/*ZAGG_OHOLIDAY*/
/*ZAGG_OJEWELRY*/
/*ZAGG_OSPECFD*/
/*ZAGG_OSTATION*/
/*ZAGG_PBRONCH*/
/*ZAGG_PDIADIET*/
/*ZAGG_PHEARTAT*/
/*ZAGG_PHRTMEDS*/
/*ZAGG_PORALINJ*/
/*ZAGG_PPAIN*/
/*ZAGG_PRESRI*/
/*ZAGG_PRHEUMTS*/
/*ZAGG_PSIR*/
/*ZAGG_PSKINMED*/
/*ZAGG_PVIAGRA*/
/*ZAGG_RAGEN*/
/*ZAGG_RARTS*/
/*ZAGG_RATEEN*/
/*ZAGG_RAWOMEN*/
/*ZAGG_RBOOKS*/
/*ZAGG_RC_M*/
/*ZAGG_RCOMPHMO*/
/*ZAGG_RCRAFTS*/
/*ZAGG_RDOLLARS*/
/*ZAGG_RELECTNC*/
/*ZAGG_RETAIL*/
/*ZAGG_RHEALTH*/
/*ZAGG_RHMFURN*/
/*ZAGG_RMUSIC*/
/*ZAGG_RSTATION*/
/*ZAGG_SPENDPAT_00*/
/*ZAGG_SPENDPAT_15*/
/*ZAGG_SPENDPAT_16*/
/*ZAGG_SSON*/
/*ZAGG_TRAD*/
/*ZAGG_TUOC_H*/
/*ZAGG_TUOC_N*/
/*ZAGG_VOTE*/
/*ZAGG_Z4HMVALU*/
/*ZAGG_ZINCDEC_5*/
);
