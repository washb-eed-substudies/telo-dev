rm(list=ls())
library("xtable")
source(here::here("0-config.R"))

#Telo and Child Development Tables
#Adjusted results
R1 <- readRDS(here("results/adjusted/H1_adj_res.RDS"))
R2 <- readRDS(here("results/adjusted/H2_adj_res.RDS"))
R3 <- readRDS(here("results/adjusted/H3_adj_res.RDS"))
R4 <- readRDS(here("results/adjusted/H4_adj_res.RDS"))

#Unadjusted results
U1 <- readRDS(here("results/unadjusted/H1_res.RDS"))
U2 <- readRDS(here("results/unadjusted/H2_res.RDS"))
U3 <- readRDS(here("results/unadjusted/H3_res.RDS"))
U4 <- readRDS(here("results/unadjusted/H4_res.RDS"))

#Table 2: Relationship between change in telomere length between Year 1 and Year 2 and child development at Year 2

#Create vectors

outcomes2 <- c("Communication Score", "Gross Motor Score", "Personal Social Score", "Combined", "A not B Score", "Tower Test Score")

outcomedomains2 <- c("Extended Ages and Stages Questionnaire", "", "", "", "Executive Function Evalulation", "")

#Adjusted

Q1.2 <- as.character(round(R1$q1, 2))

Q3.2 <- as.character(round(R1$q3, 2))

pointdiff2 <- as.character(round(R1$point.diff, 2))

lb2 <- as.character(round(R1$lb.diff, 2))

ub2 <- as.character(round(R1$ub.diff, 2))

pval2 <- as.character(round(R1$Pval, 2))

CI2 <- paste0(pointdiff2, "(" , lb2, ",", ub2, ")")

BH2 <- as.character(round(R1$BH.Pval, 2))


#unadjusted

n2 <- as.character(U1$N)

U.pointdiff2 <- as.character(round(U1$point.diff, 2))

U.lb2 <- as.character(round(U1$lb.diff, 2))

U.ub2 <- as.character(round(U1$ub.diff, 2))

U.pval2 <- as.character(round(U1$Pval, 2))

U.CI2 <- paste0(U.pointdiff2, "(" , U.lb2, ",", U.ub2, ")")

#Create table
?data.table()

tbl2 <- data.table(
  HEADER = 
  "Outcome Domain" = outcomedomains2,
  "Outcome" = outcomes2,
  "n" = n2,
  "Q1 mean" = Q1.2,
  "Q3 mean" = Q3.2,
  "Unadjusted Difference (95% CI)" = U.CI2, 
  "Unadjusted P-value" = U.pval2,
  "Adjusted Difference (95% CI)" = CI2, 
  "Adjusted P-value" = pval2,
  "FDR Corrected P-value" = BH2
)
tbl2

#save table
write.csv(tbl2, file=here('tables/telo-dev_table2.csv'))
print(xtable(tbl2), type="html", file=here("tables/telo-dev_table2.html"))

#Table 3: Relationship between telomere length at Year 1 (t2) and child development

#Create vectors

outcomes3 <- c("Communication Score", "Gross Motor Score", "Personal Social Score", "Combined", "A not B Score", "Tower Test Score")

outcomedomains3 <- c("Extended Ages and Stages Questionnaire", "", "", "", "Executive Function Evalulation", "")

#Adjusted

Q1.3 <- as.character(round(R2$q1, 2))

Q3.3 <- as.character(round(R2$q3, 2))

pointdiff3 <- as.character(round(R2$point.diff, 2))

lb3 <- as.character(round(R2$lb.diff, 2))

ub3 <- as.character(round(R2$ub.diff, 2))

pval3 <- as.character(round(R2$Pval, 2))

CI3 <- paste0(pointdiff3, "(" , lb3, ",", ub3, ")")

BH3 <- as.character(round(R2$BH.Pval, 2))

#unadjusted

n3 <- as.character(U2$N)

U.pointdiff3 <- as.character(round(U2$point.diff, 2))

U.lb3 <- as.character(round(U2$lb.diff, 2))

U.ub3 <- as.character(round(U2$ub.diff, 2))

U.pval3 <- as.character(round(U2$Pval, 2))

U.CI3 <- paste0(U.pointdiff3, "(" , U.lb3, ",", U.ub3, ")")

#Create table

tbl3 <- data.table(
  "Outcome Domain" = outcomedomains3,
  "Outcome" = outcomes3,
  "n" = n3,
  "Q1 mean" = Q1.3,
  "Q3 mean" = Q3.3,
  "Unadjusted Difference (95% CI)" = U.CI3, 
  "Unadjusted P-value" = U.pval3,
  "Adjusted Difference (95% CI)" = CI3, 
  "Adjusted P-value" = pval3,
  "FDR Corrected P-value" = BH3
)
tbl3

#save table
write.csv(tbl3, file=here('tables/telo-dev_table3.csv'))
print(xtable(tbl3), type="html", file=here("tables/telo-dev_table3.html"))

#Table 4: Relationship between change in telomere length at Year 1 and CDI Score

#Create vectors

outcomes4 <- c("WHO Motor Milestone", "Understanding", "Expressing")


#Adjusted

Q1.4 <- as.character(round(R3$q1, 2))

Q3.4 <- as.character(round(R3$q3, 2))

pointdiff4 <- as.character(round(R3$point.diff, 2))

lb4 <- as.character(round(R3$lb.diff, 2))

ub4 <- as.character(round(R3$ub.diff, 2))

pval4 <- as.character(round(R3$Pval, 2))

CI4 <- paste0(pointdiff4, "(" , lb4, ",", ub4, ")")

BH4 <- as.character(round(R3$BH.Pval, 2))

#unadjusted

n4 <- as.character(U3$N)

U.pointdiff4 <- as.character(round(U3$point.diff, 2))

U.lb4 <- as.character(round(U3$lb.diff, 2))

U.ub4 <- as.character(round(U3$ub.diff, 2))

U.pval4 <- as.character(round(U3$Pval, 2))

U.CI4 <- paste0(U.pointdiff4, "(" , U.lb4, ",", U.ub4, ")")

#Create table

tbl4 <- data.table(
  "Outcome" = outcomes4,
  "n" = n4,
  "Q1 mean" = Q1.4,
  "Q3 mean" = Q3.4,
  "Unadjusted Difference (95% CI)" = U.CI4, 
  "Unadjusted P-value" = U.pval4,
  "Adjusted Difference (95% CI)" = CI4, 
  "Adjusted P-value" = pval4,
  "FDR Corrected P-value" = BH4
)
tbl4

#save table
write.csv(tbl4, file=here('tables/telo-dev_table4.csv'))
print(xtable(tbl4), type="html", file=here("tables/telo-dev_table4.html"))

#Table 5: Relationship between telomere length at Year 2 and child development at Year 2

#Create vectors

outcomes5 <- c("Communication Score", "Gross Motor Score", "Personal Social Score", "Combined", "A not B Score", "Tower Test Score")

outcomedomains5 <- c("Extended Ages and Stages Questionnaire", "", "", "", "Executive Function Evalulation", "")

#Adjusted

Q1.5 <- as.character(round(R4$q1, 2))

Q3.5 <- as.character(round(R4$q3, 2))

pointdiff5 <- as.character(round(R4$point.diff, 2))

lb5 <- as.character(round(R4$lb.diff, 2))

ub5 <- as.character(round(R4$ub.diff, 2))

pval5 <- as.character(round(R4$Pval, 2))

CI5 <- paste0(pointdiff5, "(" , lb5, ",", ub5, ")")

BH5 <- as.character(round(R4$BH.Pval, 2))

#unadjusted

n5 <- as.character(U4$N)

U.pointdiff5 <- as.character(round(U4$point.diff, 2))

U.lb5 <- as.character(round(U4$lb.diff, 2))

U.ub5 <- as.character(round(U4$ub.diff, 2))

U.pval5 <- as.character(round(U4$Pval, 2))

U.CI5 <- paste0(U.pointdiff5, "(" , U.lb5, ",", U.ub5, ")")

#Create table

tbl5 <- data.table(
  "Outcome Domain" = outcomedomains5,
  "Outcome" = outcomes5,
  "n" = n5,
  "Q1 mean" = Q1.5,
  "Q3 mean" = Q3.5,
  "Unadjusted Difference (95% CI)" = U.CI5, 
  "Unadjusted P-value" = U.pval5,
  "Adjusted Difference (95% CI)" = CI5, 
  "Adjusted P-value" = pval5,
  "FDR Corrected P-value" = BH5
)
tbl5

#save table
write.csv(tbl5, file=here('tables/telo-dev_table5.csv'))
print(xtable(tbl5), type="html", file=here("tables/telo-dev_table5.html"))