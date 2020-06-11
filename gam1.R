library(ggplot2)
library(tidyverse)



g <- glimpse
h <- head
s <- summary

da <- read_csv("selected-file_2012_2017_machinereadable.csv")

da1 <- da %>%
        select(FAC_NAME,
               BEG_DATE,
               END_DATE,
               Variable,
               Amount) %>%
        filter(Variable %in% c("OP_MIN_IP",
                               "OP_MIN_OP",
                               "NET_PT_REV")) %>%
        spread(Variable, Amount) %>%
        gather(Category, Value, c(OP_MIN_IP, OP_MIN_OP)) %>%
        mutate(Category = ifelse(Category == "OP_MIN_IP",
                                 "Inpatients",
                                 "Outpatients"),
               Category = factor(Category))

ggplot(da1, aes(x = Category, y = Value, fill = Category)) + 
        geom_boxplot()

ggplot(da1, aes(x = Category, y = Value, fill = Category)) + 
        geom_boxplot() +
        scale_y_log10()
        

da2 <- da1 %>%
        filter(Value < 2000000, 
               Value > 1500,
               NET_PT_REV < 3000000000,
               NET_PT_REV > 1000000)



da3 <- spread(da2, Category, Value)

da4 <- da3[complete.cases(da3), ]




################################### Modeling ###################################
library(mgcv)

fmIn_gam <- as.formula(log(NET_PT_REV) ~ s(log(Inpatients)))
fmOut_gam <- as.formula(log(NET_PT_REV) ~ s(log(Outpatients)))
fmCombo_gam <- as.formula(log(NET_PT_REV) ~ s(log(Inpatients)) + s(log(Outpatients)))


library(vtreat) 
da4_gam <- da4 %>%
        mutate(In_Pred = 0,
               Out_Pred = 0,
               Combo_Pred = 0)

set.seed(15)
splitPlan <- kWayCrossValidation(nrow(da4), 4, NULL, NULL)
for (i in 1:4) {
        split <- splitPlan[[i]]
        model <- gam(fmIn_gam, data = da4_gam[split$train, ], family = gaussian)
        da4_gam$In_Pred[split$app] <- exp(predict(model, newdata = da4_gam[split$app, ]))
}


set.seed(981)
splitPlan <- kWayCrossValidation(nrow(da4), 4, NULL, NULL)
for (i in 1:4) {
        split <- splitPlan[[i]]
        model <- gam(fmIn_gam, data = da4_gam[split$train, ], family = gaussian)
        da4_gam$Out_Pred[split$app] <- exp(predict(model, newdata = da4_gam[split$app, ]))
}

set.seed(1530)
splitPlan <- kWayCrossValidation(nrow(da4), 4, NULL, NULL)
for (i in 1:4) {
        split <- splitPlan[[i]]
        model <- gam(fmIn_gam, data = da4_gam[split$train, ], family = gaussian)
        da4_gam$Combo_Pred[split$app] <- exp(predict(model, newdata = da4_gam[split$app, ]))
}


#################################### Evaluation ####################################
da4_gam <- da4_gam %>% 
        mutate(In_resid = In_Pred - NET_PT_REV,
               Out_resid = Out_Pred - NET_PT_REV,
               Combo_resid = Combo_Pred - NET_PT_REV,
               In_relerr = In_resid / NET_PT_REV, 
               Out_relerr = Out_resid / NET_PT_REV, 
               Combo_relerr = Combo_resid / NET_PT_REV)
        

da5_gam <- gather(da4_gam, Model, Value, c(In_Pred, Out_Pred, Combo_Pred)) %>%
        mutate(Model = factor(Model, levels = c("In_Pred",
                                                "Out_Pred",
                                                "Combo_Pred"))) %>% 
        gather(Residual, resid_val, c(In_resid, Out_resid, Combo_resid)) %>%
        gather(Relative_Error, rel_err, c(In_relerr, Out_relerr, Combo_relerr)) 

        

da5_gam_cor <- da5_gam %>%
        group_by(Model) %>%
        summarize(correlation = cor(Value, NET_PT_REV))
        

da5_gam_RMSE <- da5_gam %>% 
        group_by(Residual) %>%
        summarize(Value = sqrt(mean(resid_val^2))) %>%
        rename(Category = Residual)
        

da5_gam_SD <- data.frame(Category = "SD",
                     Value = sd(da4$NET_PT_REV))

da5_gam_RMSE_SD <- rbind(da5_gam_RMSE, da5_gam_SD) %>%
        mutate(Category = str_replace_all(Category, "resid", "RMSE"),
               Category = factor(Category, 
                                 levels = c("In_RMSE",
                                            "Out_RMSE",
                                            "Combo_RMSE",
                                            "SD")))


da5_gam_RMS_relerr <- da5_gam %>%
        group_by(Relative_Error) %>%
        summarize(Value = sqrt(mean(rel_err^2))) %>%
        rename(RMS_relerr = Relative_Error) %>%
        mutate(RMS_relerr = str_replace_all(RMS_relerr, "relerr", "RMS_Rel_err"),
               RMS_relerr = factor(RMS_relerr, 
                                   levels = c("In_RMS_Rel_err",
                                              "Out_RMS_Rel_err",
                                              "Combo_RMS_Rel_err")))




var_gam_e_In <- var(da4_gam$In_resid)
var_gam_e_Out <- var(da4_gam$Out_resid)
var_gam_e_Combo <- var(da4_gam$Combo_resid)
var_gam_o <- var(da4_gam$NET_PT_REV)


da5_gam_Rsquared <- data.frame(
        Rsquared = factor(c("In_rs",
                            "Out_rs",
                            "Combo_rs"), 
                          levels = c("In_rs",
                                     "Out_rs",
                                     "Combo_rs")),
        Value = c(1 - var_gam_e_In/var_gam_o, 
                  1 - var_gam_e_Out/var_gam_o,
                  1 - var_gam_e_Combo/var_gam_o))

gam_mod1 <- gam(fmIn_gam, data = da4_gam, family = gaussian)
gam_mod2 <- gam(fmOut_gam, data = da4_gam, family = gaussian)
gam_mod3 <- gam(fmCombo_gam, data = da4_gam, family = gaussian)

pseudoR2_mod1 <- 
summary(gam_mod1)$


################################## Plotting ####################################

plot(gam_mod1)
plot(gam_mod2)
plot(gam_mod3)
