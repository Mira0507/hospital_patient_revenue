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

da_gam2 <- da1 %>%
        filter(NET_PT_REV < 3000000000,
               NET_PT_REV > 1000000,
               Value < 2000000,
               Value > 1500)

da_gam3 <- spread(da_gam2, Category, Value) 
da_gam4 <- da_gam3[complete.cases(da_gam3), ]




################################### Modeling ###################################
library(mgcv)

fmIn_gamlog <- as.formula(log(NET_PT_REV) ~ s(log(Inpatients)))
fmOut_gamlog <- as.formula(log(NET_PT_REV) ~ s(log(Outpatients)))
fmCombo_gamlog <- as.formula(log(NET_PT_REV) ~ s(log(Inpatients)) + s(log(Outpatients)))


library(vtreat) 
da4_gamlog <- da_gam4 %>%
        mutate(In_Pred = 0,
               Out_Pred = 0,
               Combo_Pred = 0)

set.seed(15)
splitPlan <- kWayCrossValidation(nrow(da4_gamlog), 4, NULL, NULL)
for (i in 1:4) {
        split <- splitPlan[[i]]
        model <- gam(fmIn_gamlog, data = da4_gamlog[split$train, ], family = gaussian)
        da4_gamlog$In_Pred[split$app] <- exp(predict(model, newdata = da4_gamlog[split$app, ]))
}


set.seed(15)
splitPlan <- kWayCrossValidation(nrow(da4_gamlog), 4, NULL, NULL)
for (i in 1:4) {
        split <- splitPlan[[i]]
        model <- gam(fmOut_gamlog, data = da4_gamlog[split$train, ], family = gaussian)
        da4_gamlog$Out_Pred[split$app] <- exp(predict(model, newdata = da4_gamlog[split$app, ]))
}

set.seed(15)
splitPlan <- kWayCrossValidation(nrow(da4_gamlog), 4, NULL, NULL)
for (i in 1:4) {
        split <- splitPlan[[i]]
        model <- gam(fmCombo_gamlog, data = da4_gamlog[split$train, ], family = gaussian)
        da4_gamlog$Combo_Pred[split$app] <- exp(predict(model, newdata = da4_gamlog[split$app, ]))
}


#################################### Evaluation ####################################

# computing residuals
da4_gamlog <- da4_gamlog %>% 
        mutate(In_resid = In_Pred - NET_PT_REV,
               Out_resid = Out_Pred - NET_PT_REV,
               Combo_resid = Combo_Pred - NET_PT_REV,
               In_relerr = In_resid / NET_PT_REV, 
               Out_relerr = Out_resid / NET_PT_REV, 
               Combo_relerr = Combo_resid / NET_PT_REV)
        

da5_gamlog <- gather(da4_gamlog, Model, Value, c(In_Pred, Out_Pred, Combo_Pred)) %>%
        mutate(Model = factor(Model, levels = c("In_Pred",
                                                "Out_Pred",
                                                "Combo_Pred"))) %>% 
        gather(Residual, resid_val, c(In_resid, Out_resid, Combo_resid)) %>%
        gather(Relative_Error, rel_err, c(In_relerr, Out_relerr, Combo_relerr)) 

        
# computing correlation
da5_gamlog_cor <- da5_gamlog %>%
        group_by(Model) %>%
        summarize(correlation = cor(Value, NET_PT_REV))
        

# computing RMSE
da5_gamlog_RMSE <- da5_gamlog %>% 
        group_by(Residual) %>%
        summarize(Value = sqrt(mean(resid_val^2))) %>%
        rename(Category = Residual)
        

da5_gamlog_SD <- data.frame(Category = "SD",
                     Value = sd(da_gam4$NET_PT_REV))

da5_gamlog_RMSE_SD <- rbind(da5_gamlog_RMSE, da5_gamlog_SD) %>%
        mutate(Category = str_replace_all(Category, "resid", "RMSE"),
               Category = factor(Category, 
                                 levels = c("In_RMSE",
                                            "Out_RMSE",
                                            "Combo_RMSE",
                                            "SD")),
               Regression = "GAM (Log)")

da5_compare_RMSE_SD <- rbind(da_linear_gam_RMSE_SD, 
                             da5_gamlog_RMSE_SD) %>%
        mutate(Regression = factor(Regression, 
                                   levels = c("Linear (Log)",
                                              "GAM",
                                              "GAM (Log)")))


# Computing pseudoR^2 
gamlog_mod1 <- gam(fmIn_gamlog, data = da_gam4, family = gaussian)
gamlog_mod2 <- gam(fmOut_gamlog, data = da_gam4, family = gaussian)
gamlog_mod3 <- gam(fmCombo_gamlog, data = da_gam4, family = gaussian)

gamlog_pseudoR2 <- data.frame(Rsquared = c("In_rs", 
                                    "Out_rs",
                                    "Combo_rs"),
                       Value = c(summary(gamlog_mod1)$dev.expl,
                                 summary(gamlog_mod2)$dev.expl,
                                 summary(gamlog_mod3)$dev.expl),
                       Regression = "GAM (Log)")

R2_compare <- rbind(R2_linear_gam, gamlog_pseudoR2) %>%
        mutate(Regression = factor(Regression,
                                   levels = c("Linear (Log)",
                                              "GAM",
                                              "GAM (Log)")))









################################## Plotting ####################################

library(gridExtra)

# Filtered data inspection
plot_gamlog_insp6 <-
        ggplot(da_gam2, 
               aes(x = Value, y = NET_PT_REV, color = Category)) + 
        geom_point(alpha = 0.2) + 
        geom_smooth(se = F) +
        theme_bw() + 
        scale_y_log10() + 
        scale_x_log10() + 
        xlab("Surgery Time (Log-transformed Minutes)") +
        ylab("Net Patient Revenue (Log-transformed)") + 
        ggtitle("Relationship between Patient Revenue and Surgeries (After Outlier Filtering)")

# outcome vs prediction
plot_gamlog_eval1 <- 
        ggplot(da5_gamlog, aes(x = Value, y = NET_PT_REV, color = Model)) + 
        geom_jitter() + 
        geom_smooth(method = "lm", se = F, color = "black") + 
        facet_grid(Model ~.) + 
        theme_bw() + 
        xlab("Predicted Patient Revenue") + 
        ylab("Original Patient Revenue") + 
        ggtitle("Relationship between Predicted and Original Outcomes")

# plotting residual
plot_resid <- function(df, xname, tit, c) {
        ggplot(df, aes(x = xname, y = In_resid)) + 
                geom_point(alpha = 0.3, color = c) + 
                geom_smooth(method = "lm", se = F, color = "black") + 
                theme_bw() + 
                ylab("Residual") + 
                xlab("Prediction") + 
                ggtitle(tit)
}


plot_gamlog_In_resid <- plot_resid(da4_gamlog, da4_gamlog$In_Pred, "Residual in Model 1", "#FF9999")

plot_gamlog_Out_resid <- plot_resid(da4_gamlog, da4_gamlog$Out_Pred, "Residual in Model 2", "#009933")

plot_gamlog_Combo_resid <- plot_resid(da4_gamlog, da4_gamlog$Combo_Pred, "Residual in Model 3", "#3399FF")

grid.arrange(plot_gamlog_In_resid, 
             plot_gamlog_Out_resid, 
             plot_gamlog_Combo_resid, 
             nrow = 1)

library(WVPlots)
# Gain Curves
gain_curve <- function(df, model, tit) {
        GainCurvePlot(df, model, "NET_PT_REV", tit) + 
                theme_bw() +
                xlab("Fraction Items in Sort Order") + 
                ylab("Fraction Total Sum Net Patient Revenue")
}

gain_curve_gamlog_In <- gain_curve(da4_gamlog, "In_Pred", "")
gain_curve_gamlog_Out <- gain_curve(da4_gamlog, "Out_Pred", "")
gain_curve_gamlog_Combo <- gain_curve(da4_gamlog, "Combo_Pred", "")

grid.arrange(gain_curve_gamlog_In, 
             gain_curve_gamlog_Out, 
             gain_curve_gamlog_Combo, 
             ncol = 1)

# RMSE
plot_RMSE_gamlog <-
        ggplot(da5_compare_RMSE_SD, 
               aes(x = Category, 
                   y = Value,
                   fill = Regression,
                   color = Category)) + 
        geom_bar(stat = "identity", 
                 position = position_dodge(width = 0.7),
                 alpha = 0.7,
                 size = 1) + 
        theme_bw() +
        xlab("By Model") + 
        ylab("RMSE or SD") + 
        ggtitle("RMSE")


# R^2
plot_R2_compare <- 
        ggplot(R2_compare, 
               aes(x = Rsquared, 
                   y = Value, 
                   fill = Regression,
                   color = Rsquared)) + 
        geom_bar(stat = "identity",
                 position = position_dodge(width = 0.7),
                 size = 1,
                 alpha = 0.7) +
        theme_bw() + 
        xlab("By Model") + 
        ylab("R^2 or Pseudo-R^2") + 
        ggtitle("R Squared")

grid.arrange(plot_RMSE_gamlog, 
             plot_R2_compare, 
             ncol = 1)