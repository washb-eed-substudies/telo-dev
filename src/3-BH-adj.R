#### Adjust all pvalues with BH procedure ####
rm(list=ls())

source(here::here("0-config.R"))

# load all results
H1 <- readRDS(here('results/unadjusted/H1_res.RDS'))
H2 <- readRDS(here('results/unadjusted/H2_res.RDS'))
H3 <- readRDS(here('results/unadjusted/H3_res.RDS'))
H4 <- readRDS(here('results/unadjusted/H4_res.RDS'))
HR <- readRDS(here('results/unadjusted/HR_res.RDS'))

H1_adj <- readRDS(here('results/adjusted/H1_adj_res.RDS'))
H2_adj <- readRDS(here('results/adjusted/H2_adj_res.RDS'))
H3_adj <- readRDS(here('results/adjusted/H3_adj_res.RDS'))
H4_adj <- readRDS(here('results/adjusted/H4_adj_res.RDS'))
HR_adj <- readRDS(here('results/adjusted/HR_adj_res.RDS'))

H1_res <- H1 %>% select(X, Y, Pval)
H2_res <- H2 %>% select(X, Y, Pval)
H3_res <- H3 %>% select(X, Y, Pval)
H4_res <- H4 %>% select(X, Y, Pval)
HR_res <- HR %>% select(X, Y, Pval)

H1_adj_res <- H1_adj %>% select(X, Y, Pval)
H2_adj_res <- H2_adj %>% select(X, Y, Pval)
H3_adj_res <- H3_adj %>% select(X, Y, Pval)
H4_adj_res <- H4_adj %>% select(X, Y, Pval)
HR_adj_res <- HR_adj %>% select(X, Y, Pval)

H1_res$H = 1
H2_res$H = 2
H3_res$H = 3
H4_res$H = 4
HR_res$H = 3

H1_adj_res$H = 1
H2_adj_res$H = 2
H3_adj_res$H = 3
H4_adj_res$H = 4
HR_adj_res$H = 3

full_res <- rbind(H1_res, H2_res, H3_res, H4_res, HR_res)
full_adj_res <- rbind(H1_adj_res, H2_adj_res, H3_adj_res, H4_adj_res, HR_adj_res)

full_res <- full_res %>% group_by(H) %>% 
  mutate(BH.Pval=p.adjust(Pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()

full_adj_res <- full_adj_res %>% group_by(H) %>% 
  mutate(BH.Pval=p.adjust(Pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()

who_mm <- c("who_sit","who_crawl","who_stand_supp","who_walk_supp",
            "who_stand_nosupp","who_walk_nosup")

H1$BH.Pval = (full_res %>% filter(H==1))$BH.Pval
H2$BH.Pval = (full_res %>% filter(H==2))$BH.Pval
H3$BH.Pval = (full_res %>% filter(H==3 & !(Y %in% who_mm)))$BH.Pval
H4$BH.Pval = (full_res %>% filter(H==4))$BH.Pval
HR$BH.Pval = (full_res %>% filter(H==3 & Y %in% who_mm))$BH.Pval

H1_adj$BH.Pval = (full_adj_res %>% filter(H==1))$BH.Pval
H2_adj$BH.Pval = (full_adj_res %>% filter(H==2))$BH.Pval
H3_adj$BH.Pval = (full_adj_res %>% filter(H==3 & !(Y %in% who_mm)))$BH.Pval
H4_adj$BH.Pval = (full_adj_res %>% filter(H==4))$BH.Pval
HR_adj$BH.Pval = (full_adj_res %>% filter(H==3 & Y %in% who_mm))$BH.Pval

saveRDS(H1, here("results/unadjusted/H1_res.RDS"))
saveRDS(H2, here("results/unadjusted/H2_res.RDS"))
saveRDS(H3, here("results/unadjusted/H3_res.RDS"))
saveRDS(H4, here("results/unadjusted/H4_res.RDS"))
saveRDS(HR, here("results/unadjusted/HR_res.RDS"))

saveRDS(H1_adj, here("results/adjusted/H1_adj_res.RDS"))
saveRDS(H2_adj, here("results/adjusted/H2_adj_res.RDS"))
saveRDS(H3_adj, here("results/adjusted/H3_adj_res.RDS"))
saveRDS(H4_adj, here("results/adjusted/H4_adj_res.RDS"))
saveRDS(HR_adj, here("results/adjusted/HR_adj_res.RDS"))
