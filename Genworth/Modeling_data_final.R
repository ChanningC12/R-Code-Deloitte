rm(list=ls())
gc()
getwd()
setwd("../Desktop/")

library(caret)
library(rpart)
library(DMwR)
library(ROCR)
library(lift)
library(PRROC)
library(randomForest)
library(kernlab)
library(dplyr)
library(data.table)
library(ggplot2)

# read in data
allvars = read.csv("modeling_data_final.csv")

# add PCs to allvars
attach(allvars)
#### NFO PCs
allvars$PC_FINANCIAL1_NFO <- 
  0.3552711508  * ( allvars$ZAGG_ESTINC30_500  - 1.1986689877  ) / 1.4610820987
+ -0.37463203  * ( allvars$ZAGG_ESTINC30_035  - 5.4394540991  ) / 2.7667326432
+ 0.4185607477  * ( allvars$ZAGG_C210EBI  - 5.527519975  ) / 2.8170229474
+ -0.36312479  * ( allvars$ZAGG_ESTINC30_045  - 5.4383978954  ) / 2.8122356596
+ 0.3999954267  * ( allvars$ZAGG_C210CIP  - 5.5349525197  ) / 2.9416006107
+ -0.377929018  * ( allvars$ZAGG_C210BPVT  - 5.4049123253  ) / 2.8485031073
+ 0.3515022838  * ( allvars$ZAGG_BPAMEXY  - 5.4339041397  ) / 2.8456105216

allvars$PC_FINANCIAL2_NFO =
  0.6019217452  * ( ZAGG_ESTINC30_500  - 1.1986689877  ) / 1.4610820987
+ 0.3292899406  * ( ZAGG_ESTINC30_035  - 5.4394540991  ) / 2.7667326432
+ -0.017326045  * ( ZAGG_C210EBI  - 5.527519975  ) / 2.8170229474
+ 0.292125426  * ( ZAGG_ESTINC30_045  - 5.4383978954  ) / 2.8122356596
+ -0.032914128  * ( ZAGG_C210CIP  - 5.5349525197  ) / 2.9416006107
+ 0.3994615425  * ( ZAGG_C210BPVT  - 5.4049123253  ) / 2.8485031073
+ 0.5319473917  * ( ZAGG_BPAMEXY  - 5.4339041397  ) / 2.8456105216

allvars$PC_FINANCIAL3_NFO =
  0.1934402711  * ( ZAGG_ESTINC30_500  - 1.1986689877  ) / 1.4610820987
+ 0.2202414437  * ( ZAGG_ESTINC30_035  - 5.4394540991  ) / 2.7667326432
+ 0.275325027  * ( ZAGG_C210EBI  - 5.527519975  ) / 2.8170229474
+ 0.6010590699  * ( ZAGG_ESTINC30_045  - 5.4383978954  ) / 2.8122356596
+ 0.4509560358  * ( ZAGG_C210CIP  - 5.5349525197  ) / 2.9416006107
+ -0.256040901  * ( ZAGG_C210BPVT  - 5.4049123253  ) / 2.8485031073
+ -0.456158309  * ( ZAGG_BPAMEXY  - 5.4339041397  ) / 2.8456105216

allvars$PC_FINANCIAL4_NFO =
  0.3637925989  * ( ZAGG_ESTINC30_500  - 1.1986689877  ) / 1.4610820987
+ 0.3048964094  * ( ZAGG_ESTINC30_035  - 5.4394540991  ) / 2.7667326432
+ 0.0045462208  * ( ZAGG_C210EBI  - 5.527519975  ) / 2.8170229474
+ -0.606440471  * ( ZAGG_ESTINC30_045  - 5.4383978954  ) / 2.8122356596
+ 0.176289942  * ( ZAGG_C210CIP  - 5.5349525197  ) / 2.9416006107
+ 0.3389444278  * ( ZAGG_C210BPVT  - 5.4049123253  ) / 2.8485031073
+ -0.510823789  * ( ZAGG_BPAMEXY  - 5.4339041397  ) / 2.8456105216

allvars$PC_BUYER1_NFO = 
  0.3150842376  * ( ZAGG_MOBPLUS_M  - 5.5170606241  ) / 2.7873606874
+ 0.3106414498  * ( ZAGG_OAMEN  - 5.5186253704  ) / 2.8200398763
+ 0.3557096643  * ( ZAGG_OAGEN  - 5.4822401299  ) / 2.7625064263
+ 0.3410398398  * ( ZAGG_OCHILDPR  - 5.5497442618  ) / 2.7756379304
+ 0.3319309144  * ( ZAGG_OSTATION  - 5.5164640646  ) / 2.799528321
+ 0.3071250823  * ( ZAGG_OFURNITR  - 4.942109278  ) / 2.4381422134
+ 0.2715636445  * ( ZAGG_RC_M  - 5.5555533823  ) / 2.7886869553
+ 0.2898235285  * ( ZAGG_OFOOD  - 4.5646875886  ) / 2.0708753512
+ 0.2618888308  * ( ZAGG_RATEEN  - 5.4524561627  ) / 2.7932587552
+ 0.3613199093  * ( ZAGG_BUYER  - 5.5859681379  ) / 2.6531946855

allvars$PC_BUYER2_NFO =
  -0.468808406  * ( ZAGG_MOBPLUS_M  - 5.5170606241  ) / 2.7873606874
+ 0.3533408905  * ( ZAGG_OAMEN  - 5.5186253704  ) / 2.8200398763
+ 0.2277270755  * ( ZAGG_OAGEN  - 5.4822401299  ) / 2.7625064263
+ -0.061127544  * ( ZAGG_OCHILDPR  - 5.5497442618  ) / 2.7756379304
+ -0.125233346  * ( ZAGG_OSTATION  - 5.5164640646  ) / 2.799528321
+ 0.237590176  * ( ZAGG_OFURNITR  - 4.942109278  ) / 2.4381422134
+ -0.504361401  * ( ZAGG_RC_M  - 5.5555533823  ) / 2.7886869553
+ 0.232277917  * ( ZAGG_OFOOD  - 4.5646875886  ) / 2.0708753512
+ 0.3991514881  * ( ZAGG_RATEEN  - 5.4524561627  ) / 2.7932587552
+ -0.244918046  * ( ZAGG_BUYER  - 5.5859681379  ) / 2.6531946855

allvars$PC_BUYER3_NFO =
  -0.178188186  * ( ZAGG_MOBPLUS_M  - 5.5170606241  ) / 2.7873606874
+ -0.319805471  * ( ZAGG_OAMEN  - 5.5186253704  ) / 2.8200398763
+ -0.19119736  * ( ZAGG_OAGEN  - 5.4822401299  ) / 2.7625064263
+ -0.08528861  * ( ZAGG_OCHILDPR  - 5.5497442618  ) / 2.7756379304
+ 0.1921075041  * ( ZAGG_OSTATION  - 5.5164640646  ) / 2.799528321
+ 0.4113869703  * ( ZAGG_OFURNITR  - 4.942109278  ) / 2.4381422134
+ 0.0135616195  * ( ZAGG_RC_M  - 5.5555533823  ) / 2.7886869553
+ 0.6599922638  * ( ZAGG_OFOOD  - 4.5646875886  ) / 2.0708753512
+ -0.419930108  * ( ZAGG_RATEEN  - 5.4524561627  ) / 2.7932587552
+ -0.062316285  * ( ZAGG_BUYER  - 5.5859681379  ) / 2.6531946855

allvars$PC_BUYER4_NFO =
  -0.227478533  * ( ZAGG_MOBPLUS_M  - 5.5170606241  ) / 2.7873606874
+ -0.387909588  * ( ZAGG_OAMEN  - 5.5186253704  ) / 2.8200398763
+ -0.238218812  * ( ZAGG_OAGEN  - 5.4822401299  ) / 2.7625064263
+ -0.047629439  * ( ZAGG_OCHILDPR  - 5.5497442618  ) / 2.7756379304
+ 0.1701095199  * ( ZAGG_OSTATION  - 5.5164640646  ) / 2.799528321
+ -0.053737707  * ( ZAGG_OFURNITR  - 4.942109278  ) / 2.4381422134
+ 0.4387383344  * ( ZAGG_RC_M  - 5.5555533823  ) / 2.7886869553
+ 0.071341891  * ( ZAGG_OFOOD  - 4.5646875886  ) / 2.0708753512
+ 0.6891102213  * ( ZAGG_RATEEN  - 5.4524561627  ) / 2.7932587552
+ -0.185698746  * ( ZAGG_BUYER  - 5.5859681379  ) / 2.6531946855

allvars$PC_BUYER5_NFO = 
  0.0139447554  * ( ZAGG_MOBPLUS_M  - 5.5170606241  ) / 2.7873606874
+ 0.3496376664  * ( ZAGG_OAMEN  - 5.5186253704  ) / 2.8200398763
+ 0.158954171  * ( ZAGG_OAGEN  - 5.4822401299  ) / 2.7625064263
+ -0.467455667  * ( ZAGG_OCHILDPR  - 5.5497442618  ) / 2.7756379304
+ -0.431185865  * ( ZAGG_OSTATION  - 5.5164640646  ) / 2.799528321
+ -0.218496643  * ( ZAGG_OFURNITR  - 4.942109278  ) / 2.4381422134
+ 0.5107173512  * ( ZAGG_RC_M  - 5.5555533823  ) / 2.7886869553
+ 0.360280511  * ( ZAGG_OFOOD  - 4.5646875886  ) / 2.0708753512
+ -0.06782624  * ( ZAGG_RATEEN  - 5.4524561627  ) / 2.7932587552
+ -0.069865054  * ( ZAGG_BUYER  - 5.5859681379  ) / 2.6531946855

allvars$PC_BUYER6_NFO = 
  -0.07510201  * ( ZAGG_MOBPLUS_M  - 5.5170606241  ) / 2.7873606874
+ 0.0405452834  * ( ZAGG_OAMEN  - 5.5186253704  ) / 2.8200398763
+ 0.0005711192  * ( ZAGG_OAGEN  - 5.4822401299  ) / 2.7625064263
+ -0.039486259  * ( ZAGG_OCHILDPR  - 5.5497442618  ) / 2.7756379304
+ -0.27702366  * ( ZAGG_OSTATION  - 5.5164640646  ) / 2.799528321
+ 0.7552329088  * ( ZAGG_OFURNITR  - 4.942109278  ) / 2.4381422134
+ 0.3244836159  * ( ZAGG_RC_M  - 5.5555533823  ) / 2.7886869553
+ -0.462381137  * ( ZAGG_OFOOD  - 4.5646875886  ) / 2.0708753512
+ -0.073605227  * ( ZAGG_RATEEN  - 5.4524561627  ) / 2.7932587552
+ -0.139763229  * ( ZAGG_BUYER  - 5.5859681379  ) / 2.6531946855

allvars$PC_CREDIT1_NFO =
  0.5958707984  * ( HH_TOTALALLCREDIT_60DPD  - 2.2309125405  ) / 2.1288792654
+ 0.5404834014  * ( NUM_BANKCARD_SEVEREDEROG  - 0.8727470099  ) / 1.3652427509
+ 0.5939828991  * ( BAL_NONMTGCREDIT_60DPD  - 2.449243543  ) / 2.4637154625

allvars$PC_CREDIT2_NFO =
  -0.368779987  * ( HH_TOTALALLCREDIT_60DPD  - 2.2309125405  ) / 2.1288792654
+ 0.8411908605  * ( NUM_BANKCARD_SEVEREDEROG  - 0.8727470099  ) / 1.3652427509
+ -0.39547346  * ( BAL_NONMTGCREDIT_60DPD  - 2.449243543  ) / 2.4637154625

allvars$PC_MRTGAGE1_NFO =
  0.7071067812  * ( HIGHCRD_AGENCYFIRSTMTG_NEW  - 0.8352908961  ) / 1.3226505054
+ 0.7071067812  * ( BAL_1STMTGCREDIT_NEW  - 1.3715392213  ) / 1.7796374337

allvars$PC_EQUITY1_NFO =
  0.7071067812  * ( HIGHCRD_HELOC_NEW  - 0.3041133267  ) / 0.6461743433
+ 0.7071067812  * ( BAL_HELOC_NEW  - 0.3177461786  ) / 0.7894673989

allvars$PC_CHILD1_NFO =
  0.7071067812  * ( NOC19  - 0.2241792417  ) / 0.5518351508
+ 0.7071067812  * ( HHCOMP_A  - 0.129820152  ) / 0.3361062816

allvars$PC_AGE_DS1_NFO =
  0.5900453435  * ( BINARY_HYPT_TRANS  - 0.3320281365  ) / 0.1260727836
+ 0.5767940236  * ( BINARY_DIAB_TRANS  - 0.1461374381  ) / 0.0552779975
+ 0.5649381798  * ( POLICYHOLDER_AGE  - 5.3028615297  ) / 2.8918643584

allvars$PC_AGE_DS2_NFO =
  -0.179439423  * ( BINARY_HYPT_TRANS  - 0.3320281365  ) / 0.1260727836
+ -0.588538904  * ( BINARY_DIAB_TRANS  - 0.1461374381  ) / 0.0552779975
+ 0.7883041622  * ( POLICYHOLDER_AGE  - 5.3028615297  ) / 2.8918643584

allvars$PC_SURVEY1_NFO =
  0.7071067812  * ( ZAGG_ISCA  - 5.4490528395  ) / 2.8247385891
+ 0.7071067812  * ( ZAGG_IHGREEN  - 5.5488494225  ) / 2.8545778883

allvars$PC_HOMSTAT1_NFO =
  -0.631820779  * ( ZAGG_HOMSTAT_Y  - 5.4584853256  ) / 2.7731947694
+ 0.5763758551  * ( ZAGG_HOMSTAT_U  - 5.5390061905  ) / 2.704700943
+ 0.5182599507  * ( ZAGG_HOMSTAT_T  - 4.4651892854  ) / 2.8474740163

allvars$PC_HOMSTAT2_NFO =
  0.1277889804  * ( ZAGG_HOMSTAT_Y  - 5.4584853256  ) / 2.7731947694
+ -0.58201729  * ( ZAGG_HOMSTAT_U  - 5.5390061905  ) / 2.704700943
+ 0.8030727552  * ( ZAGG_HOMSTAT_T  - 4.4651892854  ) / 2.8474740163

##### Add RBO PCs
allvars$PC_INCOME_ZAGG1_RBO =
  -0.431759869  * ( ZAGG_NETW30_1000  - 3.6476853728  ) / 2.6383972298
+ 0.4315403912  * ( ZAGG_NETW30_0000  - 5.5043050615  ) / 2.8037259593
+ 0.4082992405  * ( ZAGG_NETW30_0019  - 2.8970287321  ) / 2.2793289431
+ -0.391958873  * ( ZAGG_ESTINC30_500  - 1.1998439071  ) / 1.461424864
+ 0.3857305069  * ( ZAGG_ESTINC30_017  - 2.8110525763  ) / 2.2036353415
+ 0.3977791531  * ( ZAGG_ESTINC30_045  - 5.4364296757  ) / 2.8111072307

allvars$PC_INCOME_ZAGG2_RBO =
  0.4280210298  * ( ZAGG_NETW30_1000  - 3.6476853728  ) / 2.6383972298
+ 0.3705555779  * ( ZAGG_NETW30_0000  - 5.5043050615  ) / 2.8037259593
+ -0.334869521  * ( ZAGG_NETW30_0019  - 2.8970287321  ) / 2.2793289431
+ 0.43567613  * ( ZAGG_ESTINC30_500  - 1.1998439071  ) / 1.461424864
+ 0.5130658149  * ( ZAGG_ESTINC30_017  - 2.8110525763  ) / 2.2036353415
+ 0.3380809743  * ( ZAGG_ESTINC30_045  - 5.4364296757  ) / 2.8111072307

allvars$PC_INCOME_ZAGG3_RBO = 
  -0.036477928  * ( ZAGG_NETW30_1000  - 3.6476853728  ) / 2.6383972298
+ 0.0853901552  * ( ZAGG_NETW30_0000  - 5.5043050615  ) / 2.8037259593
+ 0.43682773  * ( ZAGG_NETW30_0019  - 2.8970287321  ) / 2.2793289431
+ 0.5510926548  * ( ZAGG_ESTINC30_500  - 1.1998439071  ) / 1.461424864
+ -0.524442365  * ( ZAGG_ESTINC30_017  - 2.8110525763  ) / 2.2036353415
+ 0.4709739995  * ( ZAGG_ESTINC30_045  - 5.4364296757  ) / 2.8111072307

allvars$PC_INCOME_ZAGG4_RBO =
  -0.095776045  * ( ZAGG_NETW30_1000  - 3.6476853728  ) / 2.6383972298
+ 0.055018037  * ( ZAGG_NETW30_0000  - 5.5043050615  ) / 2.8037259593
+ 0.5120630609  * ( ZAGG_NETW30_0019  - 2.8970287321  ) / 2.2793289431
+ 0.4608344334  * ( ZAGG_ESTINC30_500  - 1.1998439071  ) / 1.461424864
+ 0.3822893886  * ( ZAGG_ESTINC30_017  - 2.8110525763  ) / 2.2036353415
+ -0.605869487  * ( ZAGG_ESTINC30_045  - 5.4364296757  ) / 2.8111072307

allvars$PC_INCOME_CEN1_RBO =
  0.7071067812  * ( C210HMI  - 5.077976396  ) / 2.5437837567
+ 0.7071067812  * ( C210CIP  - 4.8427464341  ) / 2.4255774746

allvars$PC_BANK1_RBO =
  0.7071067812  * ( BNKI  - 0.0046527684  ) / 0.0680525041
+ 0.7071067812  * ( BANK  - 0.0093605695  ) / 0.0962963949

allvars$PC_HIGHBALANCE1_RBO =
  0.4729264062  * ( HIGHCRD_NONMTGCREDIT  - 5.3378410154  ) / 2.8131903212
+ 0.3802901339  * ( HIGHCRD_HELOC_NEW  - 0.3041659788  ) / 0.6460832447
+ 0.4754299079  * ( BAL_NONAGNFIRSTMTG  - 2.4600787469  ) / 2.0755754141
+ 0.3702073795  * ( BAL_HELOC_NEW  - 0.3188146947  ) / 0.7906059085
+ 0.5182981064  * ( BAL_1STMTGCREDIT  - 4.5065363892  ) / 2.8128625755

allvars$PC_HIGHBALANCE2_RBO =
  -0.277934113  * ( HIGHCRD_NONMTGCREDIT  - 5.3378410154  ) / 2.8131903212
+ 0.5934059231  * ( HIGHCRD_HELOC_NEW  - 0.3041659788  ) / 0.6460832447
+ -0.311362616  * ( BAL_NONAGNFIRSTMTG  - 2.4600787469  ) / 2.0755754141
+ 0.6049138518  * ( BAL_HELOC_NEW  - 0.3188146947  ) / 0.7906059085
+ -0.328259947  * ( BAL_1STMTGCREDIT  - 4.5065363892  ) / 2.8128625755

allvars$PC_HIGHBALANCE3_RBO =
  0.7464584871  * ( HIGHCRD_NONMTGCREDIT  - 5.3378410154  ) / 2.8131903212
+ -0.014829631  * ( HIGHCRD_HELOC_NEW  - 0.3041659788  ) / 0.6460832447
+ -0.663027386  * ( BAL_NONAGNFIRSTMTG  - 2.4600787469  ) / 2.0755754141
+ -0.012559545  * ( BAL_HELOC_NEW  - 0.3188146947  ) / 0.7906059085
+ -0.05307309  * ( BAL_1STMTGCREDIT  - 4.5065363892  ) / 2.8128625755

allvars$PC_SEVCREDIT1_RBO =
  0.5861898496  * ( NUM_NONMTGCREDIT_60DPD  - 1.9994546755  ) / 2.1717814539
+ 0.5853748817  * ( HH_TOTALALLCREDIT_SEVEREDEROG  - 1.5083274548  ) / 1.8308503906
+ 0.5601050866  * ( HH_BANKCARDCREDIT_60DPD  - 0.9872073884  ) / 1.3635200617

allvars$PC_SEVCREDIT2_RBO =
  -0.38517257  * ( NUM_NONMTGCREDIT_60DPD  - 1.9994546755  ) / 2.1717814539
+ -0.406853035  * ( HH_TOTALALLCREDIT_SEVEREDEROG  - 1.5083274548  ) / 1.8308503906
+ 0.8283192014  * ( HH_BANKCARDCREDIT_60DPD  - 0.9872073884  ) / 1.3635200617

allvars$PC_DS_NEO1_RBO =
  0.7071067812  * ( BINARY_NM_TRANS  - 0.0469165085  ) / 0.0627210593
+ -0.707106781  * ( BINARY_NF_TRANS  - 0.0459499932  ) / 0.0471548751


varList_NFO = c(
  "NFO_IND",
  "CNT_RBO_PREV1",
  "DECISION_STAGE",
  "NUM_DECISIONS",
  "POLICY_AGE_2",
  "RATE_INCR",
  "RATE_INCR_PRE",
  "IF_SPOUSE_DISC",
  "SHARED_POLICY",
  "TOTAL_AMOUNT_PAID",
  "CLAIM_COUNT_DYNAMIC",
  "CLAIM_COUNT_NP_CLOSED_DYNAMIC",
  "BINARY_HERNIA_TRANS",
  "BINARY_NM_TRANS",
  "SEV_NT_TRANS",
  "BANK",
  "C210BPVT",
  "C210WHT",
  "HHMARST",
  "IOAPPLE",
  "ITCV",
  "NETW",
  "PLIPITOR",
  "ZINCDEC",
  "ZAGG_C210MOB",
  "ZAGG_ILOR_15",
  "BIO_BEFORE_NO_BIO",
  "FREQ_BFOR_M",
  "FREQ_BFOR_Q",
  "IFA_GROUP_STATE_PROD_FA",
  "IFA_GROUP_STATE_PROD_FAWP",
  "STATE_AZ",
  "STATE_CT",
  "STATE_MD",
  "STATE_PARTNERSHIP",
  "STATE_TX",
  "BINARY_BP_TRANS",
  "SEV_RESP_TRANS",
  "AANG",
  "APT_C",
  "APT_M",
  "APT_T",
  "C210MYS_3",
  "CA00",
  "FAMP_O",
  "HHCOMP_J",
  "ICPL",
  "IHCR",
  "IHDIY",
  "IHIGHSCL",
  "IHORSE",
  "IMPS",
  "INIRA",
  "IOMO",
  "IOR_5",
  "IORV",
  "IOTR",
  "IRNA",
  "MEDSUP",
  "N2029",
  "N3039",
  "OACHILD",
  "OCOLLECT",
  "PARTHRTS",
  "PHORM",
  "PLASERVC",
  "RBOOKS",
  "SPENDPAT",
  "SSON",
  "TUOC_G",
  "TUOC_H",
  "VAC",
  "BAL_AGENCYFIRSTMTG",
  "BAL_AUTOFINANCE_60DPD",
  "BAL_HELOC_90TO119DPD",
  "BAL_TOTALALLCREDIT_60TO89DPD",
  "HH_HELOAN_30TO59DPD",
  "HH_HELOAN_COLLECTIONS",
  "NUM_AGENCY1STMORG_30TO59DPD",
  "NUM_MTG_60TO89DPD",
  "NUM_RETAILCREDIT_60DPD",
  "NUM_TOTALALLCREDIT_BANKRUPTCY",
  "PRCNT_AUTOLEASEMATURATION",
  "PRCNT_NONMTGCREDIT",
  "ZAGG_APT_P",
  "ZAGG_C210B200",
  "ZAGG_C210PBL",
  "ZAGG_CCW",
  "ZAGG_ITRV",
  "ZAGG_MEDSUP_9",
  "ZAGG_NETW30_0087",
  "ZAGG_OAPETITE",
  "ZAGG_OARTS",
  "ZAGG_PBRONCH",
  "ZAGG_PHEARTAT",
  "ZAGG_PVIAGRA",
  "ZAGG_RDOLLARS",
  "ZAGG_RELECTNC",
  "ZAGG_RSTATION",
  "ZAGG_SPENDPAT_16",
  "ZAGG_SSON",
  "ZAGG_VOTE",
  "PC_FINANCIAL1_NFO",
  "PC_FINANCIAL2_NFO",
  "PC_FINANCIAL3_NFO",
  "PC_FINANCIAL4_NFO",
  "PC_BUYER1_NFO",
  "PC_BUYER2_NFO",
  "PC_BUYER3_NFO",
  "PC_BUYER4_NFO",
  "PC_BUYER5_NFO",
  "PC_BUYER6_NFO",
  "PC_CREDIT1_NFO",
  "PC_CREDIT2_NFO",
  "PC_MRTGAGE1_NFO",
  "PC_EQUITY1_NFO",
  "PC_CHILD1_NFO",
  "PC_AGE_DS1_NFO",
  "PC_AGE_DS2_NFO",
  "PC_SURVEY1_NFO",
  "PC_HOMSTAT1_NFO",
  "PC_HOMSTAT2_NFO",
  "tree_NFO1",
  "tree_NFO2",
  "RSPLIT1"
)

varList_RBO = c(
  "RBO_IND",
  "ACNK",
  "AIRR",
  "ANNL_PREM_BFOR",
  "AOSA",
  "APT_P",
  "ASTK",
  "BAL_NONAGN1STMORG_BANKRUPTC",
  "BAL_TOTALALLCREDIT_90TO119DPD",
  "BENE_TRM_BFOR",
  "BENEFIT_POOL_ASSET_RATIO",
  "BINARY_BP_TRANS",
  "BINARY_CARDIO_TRANS",
  "BINARY_CERV_TRANS",
  "BINARY_DRUG_TRANS",
  "BINARY_NO_TRANS",
  "BINARY_NS_TRANS",
  "BIO_BEFORE_5_COMPOUND",
  "BIO_BEFORE_NO_BIO",
  "C210MOB",
  "C210MYS_7",
  "CGN1_F",
  "CLAIM_COUNT_CONTACT_DYNAMIC",
  "CNT_FPO_PREV1",
  "CNT_RBO_PREV1",
  "DECISION_STAGE",
  "ESTDII30",
  "HH_INCOME",
  "FAMP_O",
  "FMLY_PRSNC",
  "FREQ_BFOR_M",
  "FREQ_BFOR_Q",
  "HH_AUTOBANK_60DPD",
  "HH_BANKCARD_60TO89DPD",
  "HH_HELOC",
  "HH_NONAGN1STMORG_90TO119DPD",
  "HH_TOTALALLCREDIT_60TO89DPD",
  "HH_TOTALALLCREDIT_BANKRUPTCY",
  "HHCOMP_J",
  "HHMARST",
  "HIGHCRD_BANKCARDCREDIT_NEW",
  "HOMSTAT_P",
  "HOMSTAT_R",
  "HOMSTAT_T",
  "ICCO",
  "IF_SPOUSE_DISC",
  "IFA_GROUP_STATE_PROD_FA",
  "IFA_GROUP_STATE_PROD_FAWP",
  "IHCA",
  "IMPS",
  "INMEDI",
  "IOAPPLE",
  "IOMG",
  "IRNA",
  "N65P",
  "NETW",
  "NUM_1STMTG_SEVEREDEROG",
  "NUM_AGENCY1STMORG_30TO59DPD",
  "NUM_DECISIONS",
  "NUM_MTG_60TO89DPD",
  "NUM_STUDENTLOAN_60DPD",
  "OACHILD",
  "ORIG_DBA_BFOR",
  "PALLERGY",
  "PANXIETY",
  "PATHFOOT",
  "PHEARTRX",
  "POLICY_AGE_2",
  "POLICYHOLDER_AGE",
  "PRCNT_AUTOLEASEMATURATION",
  "PRCNT_NONMTGCREDIT",
  "RATE_INCR",
  "RATE_INCR_AFFORD",
  "RATE_INCR_PRE",
  "RAUTO",
  "RELECTNC",
  "RFOOD",
  "RHEALTH",
  "RJEWELRY",
  "RLINENS",
  "SEV_DIAB_TRANS",
  "SEV_HYPT_TRANS",
  "SEV_NT_TRANS",
  "SHARED_POLICY",
  "SPENDPAT",
  "STATE_AZ",
  "STATE_CT",
  "STATE_MD",
  "STATE_PARTNERSHIP",
  "STATE_TX",
  "STATE_VA",
  "TOTAL_AMOUNT_PAID",
  "TUOCS_B",
  "TUOCS_D",
  "TUOCS_E",
  "ZAGG_AGJOINT",
  "ZAGG_BAVGONDL",
  "ZAGG_C210B200",
  "ZAGG_ESTINC30_055",
  "ZAGG_ESTINC30_067",
  "ZAGG_FINB",
  "ZAGG_HOMSTAT_U",
  "ZAGG_IHGREEN",
  "ZAGG_IHIGHSCL",
  "ZAGG_IMGO",
  "ZAGG_IOLA",
  "ZAGG_IRAS",
  "ZAGG_MOBPLUS_P",
  "ZAGG_OATEEN",
  "ZAGG_OJEWELRY",
  "ZAGG_OSPECFD",
  "ZAGG_PHRTMEDS",
  "ZAGG_PRHEUMTS",
  "ZAGG_PVIAGRA",
  "ZAGG_RARTS",
  "ZAGG_RATEEN",
  "ZAGG_RELECTNC",
  "ZAGG_RSTATION",
  "ZAGG_SSON",
  "ZINCDEC",
  "PC_INCOME_ZAGG1_RBO",
  "PC_INCOME_ZAGG2_RBO",
  "PC_INCOME_ZAGG3_RBO",
  "PC_INCOME_ZAGG4_RBO",
  "PC_INCOME_CEN1_RBO",
  "PC_BANK1_RBO",
  "PC_HIGHBALANCE1_RBO",
  "PC_HIGHBALANCE2_RBO",
  "PC_HIGHBALANCE3_RBO",
  "PC_SEVCREDIT1_RBO",
  "PC_SEVCREDIT2_RBO",
  "PC_DS_NEO1_RBO",
  "tree_var1",
  "tree_var2",
  "tree_var3",
  "tree_var4",
  "CLAIM_COUNT_DYNAMIC",
  "BINARY_DIAB_TRANS",
  "BINARY_HYPT_TRANS",
  "BAVGONDL",
  "RSPLIT1"
)

# Filter the NFO data by varList_NFO
allvars_NFO = allvars[,colnames(allvars) %in% varList_NFO]
varList_NFO %in% colnames(allvars_NFO)

allvars_RBO = allvars[,colnames(allvars) %in% varList_RBO]
varList_RBO %in% colnames(allvars_RBO)

# Separate train/test/validation by RSPLIT1
unique(allvars$RSPLIT1)
allvars_NFO_train = allvars_NFO[allvars_NFO$RSPLIT1=="TRN"| allvars_NFO$RSPLIT1=="TST",]
allvars_NFO_val = allvars_NFO[allvars_NFO$RSPLIT1=="VAL",]

allvars_RBO_train = allvars_RBO[allvars_RBO$RSPLIT1=="TRN"| allvars_RBO$RSPLIT1=="TST",]
allvars_RBO_val = allvars_RBO[allvars_RBO$RSPLIT1=="VAL",]

################# Modeling ###################
allvars_NFO_train$NFO_IND = as.factor(ifelse(allvars_NFO_train$NFO_IND==0,'non.NFO','NFO'))
allvars_RBO_train$RBO_IND = as.factor(ifelse(allvars_RBO_train$RBO_IND==0,'non.RBO','RBO'))
ctrl = trainControl(method="cv",number=3,summaryFunction = twoClassSummary,classProbs = T)

rm(allvars)
rm(allvars_NFO)
rm(allvars_RBO)
gc()


################# GBM ###################
gbmGrid_nor = expand.grid(interaction.depth=c(3,5),
                      n.trees=c(200,250),
                      shrinkage=0.1,
                      n.minobsinnode=10)
# NFO
system.time(gbm_nfo <- train(as.factor(NFO_IND)~.-RSPLIT1,data=allvars_NFO_train,
                             method="gbm",trControl=ctrl,metric="ROC",tuneGrid=gbmGrid_nor)) # 1209s
# 1695s for interaction=c(3,5); n.trees=200;shrinkage=0.1,n.minobs=10 
gbm_nfo
gbm_nfo_pred = predict(gbm_nfo,newdata=allvars_NFO_val,type="prob")
head(gbm_nfo_pred)
tapply(gbm_nfo_pred$NFO,allvars_NFO_val$NFO_IND,mean)
nfo_ROC = prediction(gbm_nfo_pred$NFO,allvars_NFO_val$NFO_IND)
nfo_ROC_perf = performance(nfo_ROC,measure="tpr",x.measure="fpr")
plot(nfo_ROC_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
nfo_AUC = performance(nfo_ROC,measure="auc")
unlist(nfo_AUC@y.values) # 0.88

plotLift(gbm_nfo_pred$NFO,allvars_NFO_val$NFO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(gbm_nfo_pred$NFO,allvars_NFO_val$NFO_IND) # 5.81

gbm_nfo_gain = data.frame(NFO_IND = allvars_NFO_val$NFO_IND,NFO_prob = gbm_nfo_pred$NFO)
gbm_nfo_gain = gbm_nfo_gain[order(-gbm_nfo_gain$NFO_prob),]
gbm_nfo_gain = mutate(gbm_nfo_gain,quantile=ntile(gbm_nfo_gain$NFO_prob,10))
gbm_nfo_gain$quantile = 11-gbm_nfo_gain$quantile
gbm_nfo_gain = as.data.table(gbm_nfo_gain)
gbm_nfo_gain_table = gbm_nfo_gain[,.N,by=c("quantile","NFO_IND")]
gbm_nfo_gain_table = gbm_nfo_gain_table[NFO_IND==1,]
gbm_nfo_gain_table$NFO_cum = cumsum(gbm_nfo_gain_table$N)
gbm_nfo_gain_table$gain_index = gbm_nfo_gain_table$NFO_cum/(gbm_nfo_gain_table$quantile*max(gbm_nfo_gain_table$NFO_cum)/10)
write.csv(gbm_nfo_gain_table,"gbm_nfo_lift.csv")

# RBO
system.time(gbm_rbo <- train(as.factor(RBO_IND)~.-RSPLIT1,data=allvars_RBO_train,
                             method="gbm",trControl=ctrl,metric="ROC",tuneGrid=gbmGrid_nor)) # 1343s
# 1695s for interaction=c(3,5); n.trees=250;shrinkage=0.1,n.minobs=10 
gbm_rbo
gbm_rbo_pred = predict(gbm_rbo,newdata=allvars_RBO_val,type="prob")
head(gbm_rbo_pred)
tapply(gbm_rbo_pred$RBO,allvars_RBO_val$RBO_IND,mean)
rbo_ROC = prediction(gbm_rbo_pred$RBO,allvars_RBO_val$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 0.80

plotLift(gbm_rbo_pred$RBO,allvars_RBO_val$RBO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(gbm_rbo_pred$RBO,allvars_RBO_val$RBO_IND) # 4.60

gbm_rbo_gain = data.frame(RBO_IND = allvars_RBO_val$RBO_IND,RBO_prob = gbm_rbo_pred$RBO)
gbm_rbo_gain = gbm_rbo_gain[order(-gbm_rbo_gain$RBO_prob),]
gbm_rbo_gain = mutate(gbm_rbo_gain,quantile=ntile(gbm_rbo_gain$RBO_prob,10))
gbm_rbo_gain$quantile = 11-gbm_rbo_gain$quantile
gbm_rbo_gain = as.data.table(gbm_rbo_gain)
gbm_rbo_gain_table = gbm_rbo_gain[,.N,by=c("quantile","RBO_IND")]
gbm_rbo_gain_table = gbm_rbo_gain_table[RBO_IND==1,]
gbm_rbo_gain_table$RBO_cum = cumsum(gbm_rbo_gain_table$N)
gbm_rbo_gain_table$gain_index = gbm_rbo_gain_table$RBO_cum/(gbm_rbo_gain_table$quantile*max(gbm_rbo_gain_table$RBO_cum)/10)
write.csv(gbm_rbo_gain_table,"gbm_rbo_lift.csv")

############### SMOTE for NFO and RBO ##############
set.seed(12191)
system.time(nfo_SMOTE <- SMOTE(NFO_IND~.,data=allvars_NFO_train))
prop.table(table(nfo_SMOTE$NFO_IND))

set.seed(12192)
system.time(rbo_SMOTE <- SMOTE(RBO_IND~.,data=allvars_RBO_train)) # 1495s
prop.table(table(rbo_SMOTE$RBO_IND))

################# GBM SMOTE ###################
gbmGrid = expand.grid(interaction.depth=c(3,5),
                      n.trees=c(200,250),
                      shrinkage=0.1,
                      n.minobsinnode=10)
# NFO
system.time(gbm_nfo <- train(as.factor(NFO_IND)~.-RSPLIT1,data=nfo_SMOTE,
                             method="gbm",trControl=ctrl,metric="ROC",tuneGrid=gbmGrid)) # 222.54s
# 375s (interaction.depth = interaction.depth=c(3,5), n.trees=c(200,250),shrinkage=0.1,n.minobsinnode=10)
gbm_nfo
gbm_nfo_pred = predict(gbm_nfo,newdata=allvars_NFO_val,type="prob")
head(gbm_nfo_pred)
tapply(gbm_nfo_pred$NFO,allvars_NFO_val$NFO_IND,mean)
nfo_ROC = prediction(gbm_nfo_pred$NFO,allvars_NFO_val$NFO_IND)
nfo_ROC_perf = performance(nfo_ROC,measure="tpr",x.measure="fpr")
plot(nfo_ROC_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
nfo_AUC = performance(nfo_ROC,measure="auc")
unlist(nfo_AUC@y.values) # 0.88

plotLift(gbm_nfo_pred$NFO,allvars_NFO_val$NFO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(gbm_nfo_pred$NFO,allvars_NFO_val$NFO_IND) # 5.61

gbm_nfo_gain = data.frame(NFO_IND = allvars_NFO_val$NFO_IND,NFO_prob = gbm_nfo_pred$NFO)
gbm_nfo_gain = gbm_nfo_gain[order(-gbm_nfo_gain$NFO_prob),]
gbm_nfo_gain = mutate(gbm_nfo_gain,quantile=ntile(gbm_nfo_gain$NFO_prob,10))
gbm_nfo_gain$quantile = 11-gbm_nfo_gain$quantile
gbm_nfo_gain = as.data.table(gbm_nfo_gain)
gbm_nfo_gain_table = gbm_nfo_gain[,.N,by=c("quantile","NFO_IND")]
gbm_nfo_gain_table = gbm_nfo_gain_table[NFO_IND==1,]
gbm_nfo_gain_table$NFO_cum = cumsum(gbm_nfo_gain_table$N)
gbm_nfo_gain_table$gain_index = gbm_nfo_gain_table$NFO_cum/(gbm_nfo_gain_table$quantile*max(gbm_nfo_gain_table$NFO_cum)/10)
write.csv(gbm_nfo_gain_table,"gbm_nfo_lift.csv")

# RBO
system.time(gbm_rbo_SMOTE <- train(as.factor(RBO_IND)~.-RSPLIT1,data=rbo_SMOTE,
                             method="gbm",trControl=ctrl,metric="ROC",tuneGrid=gbmGrid)) # 2900s(n.trees=250,interaction.depth=5,shrinkage=0.1,n.min=10)

gbm_rbo_SMOTE
gbm_rbo_pred = predict(gbm_rbo_SMOTE,newdata=allvars_RBO_val,type="prob")
head(gbm_rbo_pred)
tapply(gbm_rbo_pred$RBO,allvars_RBO_val$RBO_IND,mean)
rbo_ROC = prediction(gbm_rbo_pred$RBO,allvars_RBO_val$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 0.79

plotLift(gbm_rbo_pred$RBO,allvars_RBO_val$RBO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(gbm_rbo_pred$RBO,allvars_RBO_val$RBO_IND) # 4.33

gbm_rbo_gain = data.frame(RBO_IND = allvars_RBO_val$RBO_IND,RBO_prob = gbm_rbo_pred$RBO)
gbm_rbo_gain = gbm_rbo_gain[order(-gbm_rbo_gain$RBO_prob),]
gbm_rbo_gain = mutate(gbm_rbo_gain,quantile=ntile(gbm_rbo_gain$RBO_prob,10))
gbm_rbo_gain$quantile = 11-gbm_rbo_gain$quantile
gbm_rbo_gain = as.data.table(gbm_rbo_gain)
gbm_rbo_gain_table = gbm_rbo_gain[,.N,by=c("quantile","RBO_IND")]
gbm_rbo_gain_table = gbm_rbo_gain_table[RBO_IND==1,]
gbm_rbo_gain_table$RBO_cum = cumsum(gbm_rbo_gain_table$N)
gbm_rbo_gain_table$gain_index = gbm_rbo_gain_table$RBO_cum/(gbm_rbo_gain_table$quantile*max(gbm_rbo_gain_table$RBO_cum)/10)
write.csv(gbm_rbo_gain_table,"gbm_rbo_lift.csv")


############## xgboost ###############
# NFO
system.time(xgbLinear_nfo <- train(as.factor(NFO_IND)~.-RSPLIT1,data=allvars_NFO_train,
                                   method="xgbLinear",trControl=ctrl,metric="ROC")) # 
xgbLinear_nfo
xgbLinear_nfo_pred = predict(xgbLinear_nfo,newdata=allvars_NFO_val,type="prob")
head(xgbLinear_nfo_pred)
tapply(xgbLinear_nfo_pred$NFO,allvars_NFO_val$NFO_IND,mean)
nfo_ROC = prediction(xgbLinear_nfo_pred$NFO,allvars_NFO_val$NFO_IND)
nfo_ROC_perf = performance(nfo_ROC,measure="tpr",x.measure="fpr")
plot(nfo_ROC_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
nfo_AUC = performance(nfo_ROC,measure="auc")
unlist(nfo_AUC@y.values) # 

plotLift(xgbLinear_nfo_pred$NFO,allvars_NFO_val$NFO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(xgbLinear_nfo_pred$NFO,allvars_NFO_val$NFO_IND) # 

# RBO
system.time(xgbLinear_rbo <- train(as.factor(RBO_IND)~.-NFO_IND-PLCY_REF-EFF_DT,data=rbo_SMOTE,
                                   method="xgbLinear",trControl=ctrl,metric="ROC")) # 
xgbLinear_rbo
xgbLinear_rbo_pred = predict(xgbLinear_rbo,newdata=allvars_final_val_rbo,type="prob")
head(xgbLinear_rbo_pred)
tapply(xgbLinear_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(xgbLinear_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 

plotLift(xgbLinear_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(xgbLinear_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND) # 


################## Random Forest #####################
set.seed(2225)
system.time(rf_rbo <- train(as.factor(RBO_IND)~.-RSPLIT1,data=rbo_SMOTE,
                            method="rf",trControl=ctrl,metric="ROC")) # 3946.7s
rf_rbo
rf_rbo_pred = predict(rf_rbo,newdata=allvars_final_val_rbo,type="prob")
head(rf_rbo_pred)
tapply(rf_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(rf_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 0.81

plotLift(rf_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,cumulative=T,n.buckets=10)
TopDecileLift(rf_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND) # 4.80

################## GLM(SMOTE) #####################


################## CART #####################
set.seed(2225)
system.time(rpart_rbo <- train(as.factor(RBO_IND)~.-RSPLIT1,data=rbo_SMOTE,
                               method="rpart",trControl=ctrl,metric="ROC")) # 
rpart_rbo
rpart_rbo_pred = predict(rpart_rbo,newdata=allvars_final_val_rbo,type="prob")
head(rpart_rbo_pred)
tapply(rpart_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(rpart_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)
rbo_ROC_perpart = perpartormance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perpart, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = perpartormance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 

plotLift(rpart_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,cumulative=T,n.buckets=10)
TopDecileLift(rpart_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND) # 







################## Jan 6th SMOTE on NFO without NUM_DECISIONS ####################
set.seed(2225)
ctrl = trainControl(method="cv",number=3,summaryFunction = twoClassSummary,classProbs = T)
system.time(rf_nfo <- train(as.factor(NFO_IND)~.-RSPLIT1-NUM_DECISIONS,data=nfo_SMOTE,
                            method="rf",trControl=ctrl,metric="ROC")) # 