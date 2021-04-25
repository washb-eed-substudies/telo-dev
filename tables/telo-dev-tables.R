rm(list=ls())

source(here::here("0-config.R"))
source(here::here("table-functions.R"))

# load enrollment characteristics and results
# d <- read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-stress-growth-covariates-stresslab-anthro.csv"))
H1 <- readRDS(here('results/unadjusted/H1_res.RDS'))
H2 <- readRDS(here('results/unadjusted/H2_res.RDS'))
H3 <- readRDS(here('results/unadjusted/H3_res.RDS'))
H4 <- readRDS(here('results/unadjusted/H4_res.RDS'))
HR <- readRDS(here('results/unadjusted/HR_res.RDS'))

H1adj <- readRDS(here('results/adjusted/H1_adj_res.RDS'))
H2adj <- readRDS(here('results/adjusted/H2_adj_res.RDS'))
H3adj <- readRDS(here('results/adjusted/H3_adj_res.RDS'))
H4adj <- readRDS(here('results/adjusted/H4_adj_res.RDS'))
HRadj <- readRDS(here('results/adjusted/HR_adj_res.RDS'))

unadj <- rbind(H1, H2, H3, H4)
adj <- rbind(H1adj, H2adj, H3adj, H4adj)

#### Table 2 ####
# y1 immune markers and all otucome

exposure <- c("TS_t2_Z") 
outcome <- c("sum_who", "z_cdi_und_t2", "z_cdi_say_t2", 
             "z_comm_easq", "z_motor_easq", "z_personal_easq", "z_combined_easq",
             "z_cdi_und_t3", "z_cdi_say_t3")
expo_var <- c("Telomere length Z-score Year 1") 
out_var <- c("Sum of 2nd, 4th, 5th, and 6th WHO motor milestones", "CDI comprehension Z-score Year 1", "CDI expressive language Z-score Year 1",
             "EASQ communication Z-score Year 2", "EASQ gross motor Z-score Year 2", "EASQ personal social Z-score Year 2", "EASQ combined Z-score Year 2",
             "CDI comprehension Z-score Year 2", "CDI expressive language Z-score Year 2")

tbl2 <- growth_tbl(" ", expo_var, out_var, exposure, outcome, unadj, adj, T)
tbl2flex <- growth_tbl_flex(" ", expo_var, out_var, exposure, outcome, unadj, adj, T, 1.5, 1.6)
tbl2supp <- growth_tbl(" ", expo_var, out_var, exposure, outcome, unadj, adj)
tbl2flexsupp <- growth_tbl_flex(" ", expo_var, out_var, exposure, outcome, unadj, adj, F, .8, 1)


#### Table 3 ####
# concurrent y1 immune markers and y1 who motor milestones hazard ratios

exposure <- c("TS_t2_Z") 
outcome <- c("who_crawl", "who_stand_supp", 
             "who_walk_supp", "who_stand_nosupp", "who_walk_nosup")
expo_var <- c("Telomere length Z-score Year 1") 
out_var <- c("Hands-and-knees crawling", "Standing with assistance",
             "Walking with assistance", "Standing alone", "Walking alone")

tbl3 <- hr_tbl(" ", expo_var, out_var, exposure, outcome, HR, HRadj, T)
tbl3flex <- hr_tbl_flex(" ", expo_var, out_var, exposure, outcome, HR, HRadj, T, 1.5, 1.3)
tbl3supp <- hr_tbl(" ", expo_var, out_var, exposure, outcome, HR, HRadj)
tbl3flexsupp <- hr_tbl_flex(" ", expo_var, out_var, exposure, outcome, HR, HRadj, F, 1, .8)

#### Table 4 ####
# y2 telomeres and development

exposure <- c("TS_t3_Z")   
outcome <- c("z_comm_easq", "z_motor_easq", "z_personal_easq", "z_combined_easq",
             "z_cdi_und_t3", "z_cdi_say_t3")  
expo_var <- c("Telomere length Z-score Year 2") 
out_var <- c("EASQ communication Z-score", "EASQ gross motor Z-score", "EASQ personal social Z-score", "EASQ combined Z-score",
             "CDI comprehension Z-score", "CDI expressive language Z-score") 

tbl4 <- growth_tbl(" ", expo_var, out_var, exposure, outcome, unadj, adj, T)
tbl4flex <- growth_tbl_flex(" ", expo_var, out_var, exposure, outcome, unadj, adj, T, 1.5, 1.6)
tbl4supp <- growth_tbl(" ", expo_var, out_var, exposure, outcome, unadj, adj)
tbl4flexsupp <- growth_tbl_flex(" ", expo_var, out_var, exposure, outcome, unadj, adj, F, .8, 1)

#### Table 5 ####
# change in telomere and development

exposure <- c("delta_TS_Z")   
outcome <- c("z_comm_easq", "z_motor_easq", "z_personal_easq", "z_combined_easq",
             "z_cdi_und_t3", "z_cdi_say_t3")  
expo_var <- c("Change in telomere length Z-score from Year 1 to Year 2") 
out_var <- c("EASQ communication Z-score", "EASQ gross motor Z-score", "EASQ personal social Z-score", "EASQ combined Z-score",
             "CDI comprehension Z-score", "CDI expressive language Z-score") 
    
tbl5 <- growth_tbl(" ", expo_var, out_var, exposure, outcome, unadj, adj, T)
tbl5flex <- growth_tbl_flex(" ", expo_var, out_var, exposure, outcome, unadj, adj, T, 1.6, 1.6)
tbl5supp <- growth_tbl(" ", expo_var, out_var, exposure, outcome, unadj, adj)
tbl5flexsupp <- growth_tbl_flex(" ", expo_var, out_var, exposure, outcome, unadj, adj)


#### SAVE TABLES ####

write.csv(tbl2, here('tables/main/telo-dev-table1.csv'))
write.csv(tbl3, here('tables/main/telo-dev-table2.csv'))
write.csv(tbl4, here('tables/main/telo-dev-table3.csv'))
write.csv(tbl5, here('tables/main/telo-dev-table4.csv'))

write.csv(tbl2supp, here('tables/supplementary/telo-dev-table1.csv'))
write.csv(tbl3supp, here('tables/supplementary/telo-dev-table2.csv'))
write.csv(tbl4supp, here('tables/supplementary/telo-dev-table3.csv'))
write.csv(tbl5supp, here('tables/supplementary/telo-dev-table4.csv'))


save_as_docx("Table 1: Association Between Telomere Length at Year 1 and Child Development" = tbl2flex, 
             "Table 2: Hazard Ratio for Motor Milestone Attainment for Telomere Length at Year 1" = tbl3flex, 
             "Table 3: Association Between Telomere Length at Year 2 and Child Development" = tbl4flex, 
             "Table 4: Association Between Change in Telomere Length and Child Development" = tbl5flex, 
             path='C:/Users/Sophia/Documents/WASH/WASH Telomeres and Child Development/telo-dev main tables v2.docx', pr_section = sect_properties)

save_as_docx("Table 1: Association Between Telomere Length at Year 1 and Child Development" = tbl2flexsupp, 
             "Table 2: Hazard Ratio for Motor Milestone Attainment for Telomere Length at Year 1" = tbl3flexsupp, 
             "Table 3: Association Between Telomere Length at Year 2 and Child Development" = tbl4flexsupp, 
             "Table 4: Association Between Change in Telomere Length and Child Development" = tbl5flexsupp, 
             path='C:/Users/Sophia/Documents/WASH/WASH Telomeres and Child Development/telo-dev supplementary tables v2.docx', pr_section = sect_properties)
