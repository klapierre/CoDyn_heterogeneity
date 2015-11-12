setwd("~/Dropbox/CoDyn/Import")

library(gtools)
library(reshape2)
library(tidyr)
library(dplyr)

AND_postlog<-read.csv("AND_postlog.csv")%>%
  gather(species, abundance, sp1:sp189)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

AND_cascade<-read.csv("andrews_cascade_wide.csv")%>%
  gather(species, abundance, sp1:sp24)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

BNZ_Tree<-read.csv("BNZ_Tree3.csv")%>%
  gather(species, abundance, sp1:sp12)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

BNZ_Understory<-read.csv("BNZ_Understory2.csv")%>%
  gather(species, abundance, sp1:sp96)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

EBRP<-read.csv("lh_ebrp.csv")%>%
  gather(species, abundance, sp1:sp71)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

Hays<-read.csv("lh_hay.csv")%>%
  gather(species, abundance, sp1:sp65)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

JRG<-read.csv("lh_jrg.csv")%>%
  gather(species, abundance, sp1:sp35)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

JRN<-read.csv("lh_jrn.csv")%>%
  gather(species, abundance, sp1:sp90)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

KNZ<-read.csv("lh_knz.csv")%>%
  gather(species, abundance, sp1:sp84)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

SGS<-read.csv("lh_sgs.csv")%>%
  gather(species, abundance, sp1:sp39)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

LUQ_past<-read.csv("luq_past.csv")%>%
  gather(species, abundance, sp1:sp142)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

LUQ_reveg<-read.csv("LUQ_reveg2.csv")%>%
  gather(species, abundance, sp1:sp55)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

SAG<-read.csv("SAG_bay_aquatic_data_plot_means_subset_for_time.csv")%>%
  gather(species, abundance, sp1:sp150)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

SEV_core<-read.csv("sev5.csv")%>%
  gather(species, abundance, sp1:sp157)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

MtStHelens1<-read.csv("mtsthel_abraham_grid_sp.csv")%>%
  gather(species, abundance, sp1:sp85)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

MtStHelens2<-read.csv("mtsthel_butte_sp.csv")%>%
  gather(species, abundance, sp1:sp85)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

MtStHelens3<-read.csv("mtsthel_pine_protected_sp.csv")%>%
  gather(species, abundance, sp1:sp85)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

MtStHelens4<-read.csv("mtsthel_pine_sp.csv")%>%
  gather(species, abundance, sp1:sp85)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

MtStHelens5<-read.csv("mtsthel_pumice_sp.csv")%>%
  gather(species, abundance, sp1:sp85)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

MtStHelens6<-read.csv("mtsthel_toutle_sp.csv")%>%
  gather(species, abundance, sp1:sp85)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

NTLFish<-read.csv("NTL_FISH.csv")%>%
  gather(species, abundance, sp1:sp89)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

WiRiverFish<-read.csv("wi_river_fish.csv")%>%
  gather(species, abundance, sp1:sp66)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

Phytos<-read.csv("CAS_PHYTOS.csv")%>%
  gather(species, abundance, sp1:sp289)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

FCE_fish<-read.csv("FCE_fish_activity_means_wetseason.csv")%>%
  gather(species, abundance, sp1:sp45)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

FCE_invert<-read.csv("FCE_invert_activity_means_wetseason.csv")%>%
  gather(species, abundance, sp1:sp50)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

KNZ_grasshoppers<-read.csv("KNZ_grasshoppers2.csv")%>%
  gather(species, abundance, sp1:sp53)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

kuparuk_insects<-read.csv("kuparuk_insects.csv")%>%
  gather(species, abundance, sp1:sp20)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

Miss_plant<-read.csv("miss_aquatic_plants_final.csv")%>%
  gather(species, abundance, sp1:sp100)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

Miss_fish<-read.csv("miss_fish2.csv")%>%
  gather(species, abundance, sp1:sp158)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

OND_BINVERTS<-read.csv("OND_BINVERTS.csv")%>%
  gather(species, abundance, sp1:sp27)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

ONEIDA_ZOOPS<-read.csv("ONEIDA_ZOOPS.csv")%>%
  gather(species, abundance, sp1:sp31)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

SEV_grasshoppers<-read.csv("SEV_grasshoppers2.csv")%>%
  gather(species, abundance, sp1:sp56)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

KBS_insect<-read.csv("kbs_T7_inverts.csv")%>%
  gather(species, abundance, sp1:sp21)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

NTL_zoo<-read.csv("NTL_zoo.csv")%>%
  gather(species, abundance, sp1:sp148)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

LUQ_snails<-read.csv("LUQ_snails.csv")%>%
  gather(species, abundance, sp1:sp18)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

OND_PHYTOS<-read.csv("OND_PHYTOS.csv")%>%
  gather(species, abundance, sp1:sp149)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

FCE_algae<-read.csv("FCE_algae_CERP_2005-2011.csv")%>%
  gather(species, abundance, sp1:sp96)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

Phytos_Erie<-read.csv("Phytos_Erie.csv")%>%
  gather(species, abundance, sp1:sp392)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

Phytos_Mich<-read.csv("Phytos_Michigan.csv")%>%
  gather(species, abundance, sp1:sp273)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

Phytos_Ont<-read.csv("Phytos_Ontario.csv")%>%
  gather(species, abundance, sp1:sp229)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

Phytos_Sup<-read.csv("Phytos_Supperior.csv")%>%
  gather(species, abundance, sp1:sp332)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

Phytos_Huron<-read.csv("Phytos_Huron.csv")%>%
  gather(species, abundance, sp1:sp345)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

IL_Fish<-read.csv("IL_River_Fish2.csv")%>%
  gather(species, abundance, sp1:sp78)%>%
  mutate(plot=as.factor(plot))%>%
  select(-data_type, -broad_ecosystem_type, -system)

#merge all datasets

Main<-rbind(AND_postlog, AND_cascade, BNZ_Tree, BNZ_Understory, EBRP, Hays, JRG, JRN, KNZ, SGS, LUQ_past, LUQ_reveg, MtStHelens1, MtStHelens2, MtStHelens3, MtStHelens4, MtStHelens5, MtStHelens6, Phytos, WiRiverFish, SAG, NTLFish, ONEIDA_ZOOPS, FCE_fish, FCE_invert, Miss_plant, kuparuk_insects, OND_BINVERTS, Miss_fish,  KBS_insect, NTL_zoo, LUQ_snails, OND_PHYTOS, FCE_algae, Phytos_Erie, Phytos_Mich, Phytos_Ont, Phytos_Sup, Phytos_Huron, IL_Fish)%>%
  mutate(community_type=0)
ALL<-rbind(Main, KNZ_grasshoppers, SEV_grasshoppers, SEV_core)%>%
  select(site_code, project_name, community_type) %>%
  unique()
order(ALL$site_code)

unique(ALL$community_type)

write.csv(All2, "NewData_NCEAS.csv")

