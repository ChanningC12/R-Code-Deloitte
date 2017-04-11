interaction = read.csv("../Desktop/Pfizer/Segmentation/Data/deloitte_sample_interactions.csv")
interaction_dup = interaction[duplicated(interaction),] # no duplicated observation
interaction_sub = interaction %>%
  group_by(interaction_id) %>%
  mutate(Len = n()) %>%
  filter(Len>1)
table(interaction_sub$Len)

interaction_sub_5 = interaction_sub %>%
  filter(Len>5)

write.csv(interaction_sub_5,"../Desktop/interaction_sub_5.csv")

nrow(unique(interaction[c("interaction_id","intractn_sts_grouped","email_seq_num")])) # unique by

table(interaction$intractn_type_cd)

REPEMAIL = interaction %>% 
  filter(intractn_type_cd=="REPEMAIL")

write.csv(REPEMAIL,"../Desktop/REPEMAIL.csv")


### Sample
sample = read.csv("../Desktop/deloitte_sample.csv")
nrow(unique(sample[c("pfz_cust_id","brand")]))
