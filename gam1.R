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

fmGAMIn <- as.formula(NET_PT_REV ~ s(Inpatients))
fmGAMOut <- as.formula(NET_PT_REV ~ s(Outpatients))
fmGAMCombo <- as.formula(NET_PT_REV ~ s(Inpatients) + s(Outpatients))

library(vtreat) 
library(mgcv)
da_gam4 <- da_gam4 %>%
        mutate(In_Pred = 0,
               Out_Pred = 0,
               Combo_Pred = 0)

set.seed(15)
splitPlan <- kWayCrossValidation(nrow(da_gam4), 4, NULL, NULL)
for (i in 1:4) {
        split <- splitPlan[[i]]
        model <- gam(fmGAMIn, data = da_gam4[split$train, ], family = gaussian)
        da_gam4$In_Pred[split$app] <- predict(model, newdata = da_gam4[split$app, ])
}

set.seed(981)
splitPlan <- kWayCrossValidation(nrow(da_gam4), 4, NULL, NULL)
for (i in 1:4) {
        split <- splitPlan[[i]]
        model <- gam(fmGAMOut, data = da_gam4[split$train, ], family = gaussian)
        da_gam4$Out_Pred[split$app] <- predict(model, newdata = da_gam4[split$app, ])
}

set.seed(1530)
splitPlan <- kWayCrossValidation(nrow(da_gam4), 4, NULL, NULL)
for (i in 1:4) {
        split <- splitPlan[[i]]
        model <- gam(fmGAMCombo, data = da_gam4[split$train, ], family = gaussian)
        da_gam4$Combo_Pred[split$app] <- predict(model, newdata = da_gam4[split$app, ])
}

#################################### Evaluation ####################################

# computing residual
da_gam4 <- 
        da_gam4 %>% 
        mutate(In_resid = In_Pred - NET_PT_REV,
               Out_resid = Out_Pred - NET_PT_REV,
               Combo_resid = Combo_Pred - NET_PT_REV,
               In_relerr = In_resid / NET_PT_REV, 
               Out_relerr = Out_resid / NET_PT_REV, 
               Combo_relerr = Combo_resid / NET_PT_REV)

da_gam5 <- 
        gather(da_gam4, Model, Value, c(In_Pred, Out_Pred, Combo_Pred)) %>%
        mutate(Model = factor(Model, levels = c("In_Pred",
                                                "Out_Pred",
                                                "Combo_Pred"))) %>% 
        gather(Residual, resid_val, c(In_resid, Out_resid, Combo_resid)) %>%
        gather(Relative_Error, rel_err, c(In_relerr, Out_relerr, Combo_relerr)) 

# computing RMSE
da_gam5_RMSE <- da_gam5 %>% 
        group_by(Residual) %>%
        summarize(Value = sqrt(mean(resid_val^2))) %>%
        rename(Category = Residual)

da_gam4_SD <- data.frame(Category = "SD",
                     Value = sd(da4$NET_PT_REV))

da_gam5_RMSE_SD <- rbind(da_gam5_RMSE, da_gam4_SD) %>%
        mutate(Category = str_replace_all(Category, "resid", "RMSE"),
               Category = factor(Category, 
                                 levels = c("In_RMSE",
                                            "Out_RMSE",
                                            "Combo_RMSE",
                                            "SD")),
               Regression = "GAM")

da_linear_gam_RMSE_SD <- rbind(da5_RMSE_SD, da_gam5_RMSE_SD) %>%
        mutate(Regression = factor(Regression, 
                                   levels = c("Linear",
                                              "GAM")))
                                   


# Computing pseudoR^2 
gam_mod1 <- gam(fmGAMIn, data = da_gam4, family = gaussian)
gam_mod2 <- gam(fmGAMOut, data = da_gam4, family = gaussian)
gam_mod3 <- gam(fmGAMCombo, data = da_gam4, family = gaussian)

pseudoR2 <- data.frame(Rsquared = c("In_rs", 
                                    "Out_rs",
                                    "Combo_rs"),
                       Value = c(summary(gam_mod1)$dev.expl,
                                 summary(gam_mod2)$dev.expl,
                                 summary(gam_mod3)$dev.expl),
                       Regression = "GAM")

R2_linear_gam <- rbind(da5_Rsquared, pseudoR2) %>%
        mutate(Regression = factor(Regression,
                                   levels = c("Linear",
                                              "GAM")))


#################################### Plotting #################################

# primary data inspection (before filtering)
plot_gam_insp1 <- 
        ggplot(da1, aes(x = Category, y = Value, fill = Category)) + 
        geom_boxplot(outlier.alpha = 0.3) + 
        theme_bw() +
        theme(axis.text.x = element_blank()) +
        ylab("Surgery Time (Minutes)") +
        xlab("Patients") + 
        ggtitle("Distribution of Surgery Time") + 
        geom_hline(yintercept = 2000000, color = "red", size = 1)

plot_gam_insp2 <-
        ggplot(da1, aes(x = Category, y = Value, fill = Category)) + 
        geom_boxplot(outlier.alpha = 0.3) +
        scale_y_log10() + 
        theme_bw() +
        theme(axis.text.x = element_blank()) +
        ylab("Surgery Time (Log-transformed Minutes)") +
        xlab("Patients") + 
        ggtitle("Distribution of Surgery Time") + 
        geom_hline(yintercept = 1500, color = "blue", size = 1)



plot_gam_insp3 <- 
        ggplot(da1, aes(y = NET_PT_REV, fill = "#339999")) + 
        geom_boxplot(outlier.alpha = 0.3) +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_blank()) +
        ylab("Patient Revenue") +
        ggtitle("Distribution of Patient Revenue") + 
        geom_hline(yintercept = 3000000000, color = "red", size = 1)


plot_gam_insp4 <- 
        ggplot(da1, aes(y = NET_PT_REV, fill = "#339999")) + 
        geom_boxplot(outlier.alpha = 0.3) +
        scale_y_log10() +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_blank()) +
        ylab("Log-transformed Patient Revenue") +
        ggtitle("Distribution of Patient Revenue") + 
        geom_hline(yintercept = 1000000, color = "blue", size = 1)


# data inspection (after filtering) 
plot_gam_insp5 <- 
        ggplot(da_gam2, aes(x = Value, y = NET_PT_REV, color = Category)) + 
        geom_point(alpha = 0.2) + 
        geom_smooth(se = F) + 
        theme_bw() + 
        xlab("Surgery Time (Minutes)") + 
        ylab("Net Patient Revenue)") + 
        ggtitle("Relationship between Patient Revenue and Surgeries")

plot_gam_eval1 <- 
        ggplot(da_gam5, aes(x = Value, y = NET_PT_REV, color = Model)) + 
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


plot_gam_In_resid <- plot_resid(da_gam4, da_gam4$In_Pred, "Residual in Model 1", "#FF9999")

plot_gam_Out_resid <- plot_resid(da_gam4, da_gam4$Out_Pred, "Residual in Model 2", "#009933")

plot_gam_Combo_resid <- plot_resid(da_gam4, da_gam4$Combo_Pred, "Residual in Model 3", "#3399FF")

library(gridExtra)
grid.arrange(plot_gam_In_resid, 
             plot_gam_Out_resid, 
             plot_gam_Combo_resid, 
             nrow = 1)

library(WVPlots)
# Gain Curves
gain_curve <- function(df, model, tit) {
        GainCurvePlot(df, model, "NET_PT_REV", tit) + 
                theme_bw() +
                xlab("Fraction Items in Sort Order") + 
                ylab("Fraction Total Sum Net Patient Revenue")
}

gain_curve_gam_In <- gain_curve(da_gam4, "In_Pred", "")
gain_curve_gam_Out <- gain_curve(da_gam4, "Out_Pred", "")
gain_curve_gam_Combo <- gain_curve(da_gam4, "Combo_Pred", "")

grid.arrange(gain_curve_gam_In, 
             gain_curve_gam_Out, 
             gain_curve_gam_Combo, 
             ncol = 1)



plot_RMSE_linear_gam <-
        ggplot(da_linear_gam_RMSE_SD, 
               aes(x = Category, 
                   y = Value,
                   fill = Regression,
                   color = Category)) + 
        geom_bar(stat = "identity", 
                 position = position_dodge(width = 0.5),
                 alpha = 0.7,
                 size = 1) + 
        theme_bw() +
        xlab("By Model") + 
        ylab("RMSE or SD") + 
        ggtitle("RMSE")

plot_R2_linear_gam <-
        ggplot(R2_linear_gam, 
               aes(x = Rsquared, 
                   y = Value, 
                   fill = Regression,
                   color = Rsquared)) + 
        geom_bar(stat = "identity",
                 position = position_dodge(width = 0.5),
                 size = 1,
                 alpha = 0.7) +
        theme_bw() + 
        xlab("By Model") + 
        ylab("R^2 or Pseudo-R^2") + 
        ggtitle("R Squared")

grid.arrange(plot_RMSE_linear_gam,
             plot_R2_linear_gam,
             ncol = 1)



