rm(list=ls())
source(here::here("0-config.R"))

# load telo-growth data
d <- read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.csv"))
names(d)

# load in child development datasets
setwd("C:/Users/Sophia/Documents/WASH/WASHB CD data from Kishor/2-child-development-outcomes-datasets")

# create childid
get_childid <- function(v1, v2){
  as.numeric(paste(as.character(v1), as.character(v2), sep=""))
}

cdiy1 <- read_dta("C:/Users/Sophia/Box/WASHB Child Development/washb_cdiyr1_std_5mar2021.dta") 
names(cdiy1)
cdiy1_select <- cdiy1 %>%
  mutate(childid = get_childid(dataid, tchild)) %>% 
  select(childid, agedays, month, z_age2mo_cdi_undyr1_all_no4, z_age2mo_cdi_sayyr1_all_no4) %>%
  rename(agedays_cdi_t2 = agedays,
         month_cdi_t2 = month,
         z_cdi_und_t2 = z_age2mo_cdi_undyr1_all_no4,
         z_cdi_say_t2 = z_age2mo_cdi_sayyr1_all_no4)

cdiy2 <- read_dta("C:/Users/Sophia/Box/WASHB Child Development/washb_cdiyr2_std_5mar2021.dta") 
names(cdiy2)
cdiy2_select <- cdiy2 %>%
  mutate(childid = get_childid(dataid, tchild)) %>% 
  select(childid, agedays, month, z_age2mo_cdi_undyr2_all_no4, z_age2mo_cdi_sayyr2_all_no4) %>%
  rename(agedays_cdi_t3 = agedays,
         month_cdi_t3 = month,
         z_cdi_und_t3 = z_age2mo_cdi_undyr2_all_no4,
         z_cdi_say_t3 = z_age2mo_cdi_sayyr2_all_no4)

easq <- read_dta("C:/Users/Sophia/Box/WASHB Child Development/wash-b_easq_std_5mar2021.dta") 
names(easq)
easq_select <- easq %>%
  mutate(childid = get_childid(dataid, tchild)) %>% 
  select(childid, agedays, month, z_age2mo_personal_no4,  z_age2mo_motor_no4, 
         z_age2mo_com_no4, z_age2mo_combined_no4) %>%
  rename(agedays_easq = agedays,
         month_easq = month,
         z_personal_easq = z_age2mo_personal_no4,
         z_motor_easq = z_age2mo_motor_no4,
         z_comm_easq = z_age2mo_com_no4,
         z_combined_easq = z_age2mo_combined_no4)

home1 <- read.dta("washb-bangladesh-home-year1.dta")%>%
  mutate(childid = substr(childid, 2, 2)) %>%
  mutate(childid = get_childid(dataid, childid)) %>%
  select(childid, midline_stimulation) %>%
  rename(fci_t2 = midline_stimulation)

home2 <- read.dta("washb-bangladesh-home-year2.dta")%>%
  mutate(childid = substr(childid, 2, 2)) %>%
  mutate(childid = get_childid(dataid, childid)) %>%
  select(childid, endline_stimulation) %>%
  rename(fci_t3 = endline_stimulation)

motor <- read.csv("washb-bangladesh-motormile-year1.csv")%>%
  mutate(childid = get_childid(dataid, tchild)) %>% 
  select(childid, agedays, month, sit_nosupp, crawl_nosupp, stand_supp, 
         walk_supp, stand_nosupp, walk_nosupp) %>%
  rename(agedays_motor = agedays,
         month_motor = month,
         who_sit = sit_nosupp, who_crawl = crawl_nosupp, 
         who_stand_supp = stand_supp, who_walk_supp = walk_supp, 
         who_stand_nosupp = stand_nosupp, who_walk_nosup = walk_nosupp) %>%
  mutate(sum_who = who_stand_supp+who_walk_supp+who_stand_nosupp+who_walk_nosup)

# join separate development datasets 
development <- motor %>% full_join(cdiy1_select, by="childid") %>% 
  full_join(cdiy2_select, by= 'childid') %>% full_join(easq_select, by = 'childid')

dev_with_fci <- development %>% left_join(home1, 'childid') %>% left_join(home2, 'childid') 

# save development dataset
saveRDS(dev_with_fci, paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-development.RDS"))

# join with telo-covariates dataset
telo_dev <- left_join(d, dev_with_fci, "childid")

# Z-score of telomere measurements
telo_dev <- telo_dev %>% 
  mutate(TS_t2_Z = scale(TS_t2, center=TRUE, scale=TRUE)[,1]) %>%
  mutate(TS_t3_Z = scale(TS_t3, center=TRUE, scale=TRUE)[,1]) %>%
  mutate(delta_TS_Z = scale(delta_TS, center=TRUE, scale=TRUE)[,1])

# merge in hhwealth 
d_hhwealth <- read.csv("C:/Users/Sophia/Documents/ee-secondary/sophia scripts/hhwealth.csv")
telo_dev <- left_join(telo_dev, d_hhwealth, "dataid")


# check covariate missingness
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "HHwealth", 
         "fci_t2", "diar7d_t2", "cesd_sum_t2", "life_viol_any_t3", "tr")

#Add in time varying covariates:
H2_W <- c(Wvars, "ageday_ht2", "month_ht2", "fci_t3", 
          "diar7d_t3", "laz_t2", "waz_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3")
H1_W <- c(H2_W, "ageday_ht3", "month_ht3")
H3_W <- c(Wvars, "ageday_ht2", "agedays_motor",	"month_ht2", "month_motor", "laz_t1", "waz_t1") 
H4_W <- c(Wvars, "ageday_ht3", "month_ht3", "fci_t3",
          "diar7d_t3", "laz_t2", "waz_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3")
H1_W[!(H1_W %in% colnames(telo_dev))]
H2_W[!(H2_W %in% colnames(telo_dev))]
H3_W[!(H3_W %in% colnames(telo_dev))]
H4_W[!(H4_W %in% colnames(telo_dev))]


add_t3_covariates <- function(j, W){
  if(grepl("easq", j)){return (c(W, "agedays_easq", "month_easq"))}
  else if(grepl("cdi", j) & grepl("t3", j)){return (c(W, "agedays_cdi_t3", "month_cdi_t3"))}
}

generate_miss_tbl <- function(Wvars, d){
  W <- d %>% select(all_of(Wvars))  
  miss <- data.frame(name = names(W), missing = colSums(is.na(W))/nrow(W), row.names = c(1:ncol(W)))
  for (i in 1:nrow(miss)) {
    miss$class[i] <- class(W[,which(colnames(W) == miss[i, 1])])
  }
  miss 
}

generate_miss_tbl(Wvars, telo_dev)

# add missingness category to IPV covariate
telo_dev$life_viol_any_t3<-as.factor(telo_dev$life_viol_any_t3)
summary(telo_dev$life_viol_any_t3)
telo_dev$life_viol_any_t3<-addNA(telo_dev$life_viol_any_t3)
levels(telo_dev$life_viol_any_t3)[length(levels(telo_dev$life_viol_any_t3))]<-"Missing"
summary(telo_dev$life_viol_any_t3)

# add missingness category to caregiver report covariates
summary(telo_dev$diar7d_t2)
telo_dev$diar7d_t2<-as.factor(telo_dev$diar7d_t2)
telo_dev$diar7d_t2<-addNA(telo_dev$diar7d_t2)
levels(telo_dev$diar7d_t2)[length(levels(telo_dev$diar7d_t2))]<-"Missing"
summary(telo_dev$diar7d_t2)

Xvars <- c("delta_TS")            
Yvars <- c("z_comm_easq", "z_motor_easq", "z_personal_easq", "z_combined_easq", 
           "z_cdi_say_t3", "z_cdi_und_t3") 

for (i in Xvars){
  for (j in Yvars){
    print(i)
    print(j)
    Wvars = add_t3_covariates(j, H1_W)
    d_sub <- subset(telo_dev, !is.na(telo_dev[,i]) & !is.na(telo_dev[,j]))
    print(generate_miss_tbl(Wvars, d_sub))
  }
}

Xvars <- c("TS_t2_Z")            
Yvars <- c("z_comm_easq", "z_motor_easq", "z_personal_easq", "z_combined_easq", 
           "z_cdi_say_t3", "z_cdi_und_t3") 

for (i in Xvars){
  for (j in Yvars){
    print(i)
    print(j)
    Wvars = add_t3_covariates(j, H2_W)
    d_sub <- subset(telo_dev, !is.na(telo_dev[,i]) & !is.na(telo_dev[,j]))
    print(generate_miss_tbl(Wvars, d_sub))
  }
}

Xvars <- c("TS_t2_Z")            
Yvars <- c("sum_who", "z_cdi_say_t2", "z_cdi_und_t2")

for (i in Xvars){
  for (j in Yvars){
    print(i)
    print(j)
    Wvars = H3_W
    d_sub <- subset(telo_dev, !is.na(telo_dev[,i]) & !is.na(telo_dev[,j]))
    print(generate_miss_tbl(Wvars, d_sub))
  }
}

Xvars <- c("TS_t3_Z")            
Yvars <- c("z_comm_easq", "z_motor_easq", "z_personal_easq", "z_combined_easq", 
           "z_cdi_say_t3", "z_cdi_und_t3") 

for (i in Xvars){
  for (j in Yvars){
    print(i)
    print(j)
    Wvars = add_t3_covariates(j, H4_W)
    d_sub <- subset(telo_dev, !is.na(telo_dev[,i]) & !is.na(telo_dev[,j]))
    print(generate_miss_tbl(Wvars, d_sub))
  }
}

# add missingness category to caregiver report covariates
summary(telo_dev$diar7d_t3)
telo_dev$diar7d_t3<-as.factor(telo_dev$diar7d_t3)
telo_dev$diar7d_t3<-addNA(telo_dev$diar7d_t3)
levels(telo_dev$diar7d_t3)[length(levels(telo_dev$diar7d_t3))]<-"Missing"
summary(telo_dev$diar7d_t3)

# create factor variable with missingness level for growth measurements at year 1 and year 2
growth.var <- c("laz_t1", "waz_t1", "laz_t2", "waz_t2")
for (i in growth.var) {
  cutpoints <- c(-3, -2, -1, -0)  
  cuts <- c(min(telo_dev[[i]], na.rm = T), cutpoints, max(telo_dev[[i]], na.rm = T))
  new_var <- paste(i, "_cat", sep="")
  telo_dev[[new_var]] <- cut(telo_dev[[i]], 
                          cuts,
                          right = FALSE,
                          include.lowest = TRUE)
  telo_dev[[new_var]] <- as.factor(telo_dev[[new_var]])
  telo_dev[[new_var]] <- fct_explicit_na(telo_dev[[new_var]], "Missing")
  telo_dev[[new_var]] <- factor(telo_dev[[new_var]], levels = levels(telo_dev[[new_var]]))
}

# check missingness of categorical growth covariates
generate_miss_tbl(paste(growth.var, "_cat", sep=""), telo_dev)

saveRDS(telo_dev, paste0(dropboxDir,"Data/Cleaned/Audrie/bangladesh-ee-telo-development-covariates.RDS"))
