rm(list=ls())

source(here::here("0-config.R"))
# source(here::here("src/0-gam-functions.R"))

d<-readRDS(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-ee-telo-development-covariates.RDS"))

#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "HHwealth", 
         "fci_t2", "diar7d_t2", "cesd_sum_t2", "life_viol_any_t3", "tr")

Wvars[!(Wvars %in% colnames(d))]



#Add in time varying covariates:
H2_W <- c(Wvars, "ageday_ht2", "month_ht2", "fci_t3", 
          "diar7d_t3", "laz_t2", "waz_t2", "cesd_sum_ee_t3", "pss_sum_mom_t3")
H1_W <- c(H2_W, "ageday_ht3", "month_ht3")
H3_W <- c(Wvars, "ageday_ht2", "agedays_motor",	"month_ht2", "month_motor", "laz_t1_cat", "waz_t1_cat") 
H4_W <- c(Wvars, "ageday_ht3", "month_ht3", "fci_t3",
          "diar7d_t3", "laz_t2_cat", "waz_t2_cat", "cesd_sum_ee_t3", "pss_sum_mom_t3")
H1_W[!(H1_W %in% colnames(d))]
H2_W[!(H2_W %in% colnames(d))]
H3_W[!(H3_W %in% colnames(d))]
H4_W[!(H4_W %in% colnames(d))]


add_t3_covariates <- function(j, W){
  if(grepl("easq", j)){return (c(W, "agedays_easq", "month_easq"))}
  else if(grepl("cdi", j) & grepl("t3", j)){return (c(W, "agedays_cdi_t3", "month_cdi_t3"))}
}

#Loop over exposure-outcome pairs

#### Hypothesis 1 ####
# change in telomere length between y1 and y2 and development year 2
Xvars <- c("delta_TS")            
Yvars <- c("z_comm_easq", "z_motor_easq", "z_personal_easq", "z_combined_easq", 
           "z_cdi_say_t3", "z_cdi_und_t3") 

#Fit models
H1_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=add_t3_covariates(j, H1_W))
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}



#Get primary contrasts
H1_adj_res <- NULL
for(i in 1:nrow(H1_adj_models)){
  res <- data.frame(X=H1_adj_models$X[i], Y=H1_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H1_adj_models$fit[i][[1]], d=H1_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1_adj_res <-  bind_rows(H1_adj_res , preds$res)
}

#Make list of plots
H1_adj_plot_list <- NULL
H1_adj_plot_data <- NULL
for(i in 1:nrow(H1_adj_models)){
  res <- data.frame(X=H1_adj_models$X[i], Y=H1_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H1_adj_models$fit[i][[1]], H1_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1_adj_plot_list[[i]] <-  simul_plot$p
  H1_adj_plot_data <-  rbind(H1_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
saveRDS(H1_adj_models, here("models/H1_adj_models.RDS"))

#Save results
saveRDS(H1_adj_res, here("results/adjusted/H1_adj_res.RDS"))


#Save plots
#saveRDS(H1_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H1_adj_splines.RDS"))

#Save plot data
saveRDS(H1_adj_plot_data, here("figure-data/H1_adj_spline_data.RDS"))




#### Hypothesis 2 ####
# Telomere at y1 v. development year 2
Xvars <- c("TS_t2_Z")            
Yvars <- c("z_comm_easq", "z_motor_easq", "z_personal_easq", "z_combined_easq", 
           "z_cdi_say_t3", "z_cdi_und_t3") 

#Fit models
H2_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=add_t3_covariates(j, H2_W))
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_adj_models <- bind_rows(H2_adj_models, res)
  }
}

#Get primary contrasts
H2_adj_res <- NULL
for(i in 1:nrow(H2_adj_models)){
  res <- data.frame(X=H2_adj_models$X[i], Y=H2_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H2_adj_models$fit[i][[1]], d=H2_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2_adj_res <-  bind_rows(H2_adj_res, preds$res)
}

#Make list of plots
H2_adj_plot_list <- NULL
H2_adj_plot_data <- NULL
for(i in 1:nrow(H2_adj_models)){
  res <- data.frame(X=H2_adj_models$X[i], Y=H2_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H2_adj_models$fit[i][[1]], H2_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2_adj_plot_list[[i]] <-  simul_plot$p
  H2_adj_plot_data <-  rbind(H2_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
saveRDS(H2_adj_models, here("models/H2_adj_models.RDS"))

#Save results
saveRDS(H2_adj_res, here("results/adjusted/H2_adj_res.RDS"))


#Save plots
#saveRDS(H2_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H2_adj_splines.RDS"))

#Save plot data
saveRDS(H2_adj_plot_data, here("figure-data/H2_adj_spline_data.RDS"))




#### Hypothesis 3 ####
# telomere length at year 1 v. development at year 1
Xvars <- c("TS_t2_Z")            
Yvars <- c("sum_who", "z_cdi_say_t2", "z_cdi_und_t2")

#Fit models
H3_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=H3_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H3_adj_models <- bind_rows(H3_adj_models, res)
  }
}

#Get primary contrasts
H3_adj_res <- NULL
for(i in 1:nrow(H3_adj_models)){
  res <- data.frame(X=H3_adj_models$X[i], Y=H3_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H3_adj_models$fit[i][[1]], d=H3_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H3_adj_res <-  bind_rows(H3_adj_res , preds$res)
}

#Make list of plots
H3_adj_plot_list <- NULL
H3_adj_plot_data <- NULL
for(i in 1:nrow(H3_adj_models)){
  res <- data.frame(X=H3_adj_models$X[i], Y=H3_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H3_adj_models$fit[i][[1]], H3_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H3_adj_plot_list[[i]] <-  simul_plot$p
  H3_adj_plot_data <-  rbind(H3_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
saveRDS(H3_adj_models, here("models/H3_adj_models.RDS"))

#Save results
saveRDS(H3_adj_res, here("results/adjusted/H3_adj_res.RDS"))


#Save plots
#saveRDS(H3_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H3_adj_splines.RDS"))

#Save plot data
saveRDS(H3_adj_plot_data, here("figure-data/H3_adj_spline_data.RDS"))



#### Hypothesis 4 ####
#Telomere length at year 2 v. development at year 2
Xvars <- c("TS_t3_Z")            
Yvars <- c("z_comm_easq", "z_motor_easq", "z_personal_easq", "z_combined_easq", 
           "z_cdi_say_t3", "z_cdi_und_t3") 

#Fit models
H4_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=add_t3_covariates(j, H4_W))
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H4_adj_models <- bind_rows(H4_adj_models, res)
  }
}

#Get primary contrasts
H4_adj_res <- NULL
for(i in 1:nrow(H4_adj_models)){
  res <- data.frame(X=H4_adj_models$X[i], Y=H4_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H4_adj_models$fit[i][[1]], d=H4_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H4_adj_res <-  bind_rows(H4_adj_res , preds$res)
}

#Make list of plots
H4_adj_plot_list <- NULL
H4_adj_plot_data <- NULL
for(i in 1:nrow(H4_adj_models)){
  res <- data.frame(X=H4_adj_models$X[i], Y=H4_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H4_adj_models$fit[i][[1]], H4_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H4_adj_plot_list[[i]] <-  simul_plot$p
  H4_adj_plot_data <-  rbind(H4_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
saveRDS(H4_adj_models, here("models/H4_adj_models.RDS"))

#Save results
saveRDS(H4_adj_res, here("results/adjusted/H4_adj_res.RDS"))


#Save plots
#saveRDS(H4_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H4_adj_splines.RDS"))

#Save plot data
saveRDS(H4_adj_plot_data, here("figure-data/H4_adj_spline_data.RDS"))



#### hazard ratio for WHO motor milestones

Xvars <- c("TS_t2_Z") 
Yvars <- grep("who_", colnames(d), value=T)

HR_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_HR_GAM(d=d, X=i, Y=j, age="agedays_motor", W=H3_W)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    HR_models <- bind_rows(HR_models, res)
  }
}

HR_res <- NULL
for(i in 1:nrow(HR_models)){
  res <- data.frame(X=HR_models$X[i], Y=HR_models$Y[i])
  preds <- predict_gam_HR(fit=HR_models$fit[i][[1]], d=HR_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  HR_res <-  bind_rows(HR_res , preds$res)
}

#Make list of plots
HR_plot_list <- NULL
HR_plot_data <- NULL
for(i in 1:nrow(HR_models)){
  res <- data.frame(X=HR_models$X[i], Y=HR_models$Y[i])
  simul_plot <- gam_simul_CI(HR_models$fit[i][[1]], HR_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  HR_plot_list[[i]] <-  simul_plot$p
  HR_plot_data <-  rbind(HR_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
saveRDS(HR_models, here("models/HR_adj_models.RDS"))

#Save results
saveRDS(HR_res, here("results/adjusted/HR_adj_res.RDS"))

#Save plot data
saveRDS(HR_plot_data, here("figure-data/HR_adj_spline_data.RDS"))

