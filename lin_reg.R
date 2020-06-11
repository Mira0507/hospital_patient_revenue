library(ggplot2)
library(tidyverse)



g <- glimpse
h <- head
s <- summary


da <- read_csv("selected-file_2012_2017_machinereadable.csv")

#################################### Data Cleaning ####################################

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


da2 <- da1 %>%
        filter(Value < 2000000, 
               Value > 1500,
               NET_PT_REV < 3000000000,
               NET_PT_REV > 1000000)



da3 <- spread(da2, Category, Value)

da4 <- da3[complete.cases(da3), ]


################################### Modeling ###################################

fmIn <- as.formula(log(NET_PT_REV) ~ log(Inpatients))
fmOut <- as.formula(log(NET_PT_REV) ~ log(Outpatients))
fmCombo <- as.formula(log(NET_PT_REV) ~ log(Inpatients) + log(Outpatients))


library(vtreat) 
da4 <- da4 %>%
        mutate(In_Pred = 0,
               Out_Pred = 0,
               Combo_Pred = 0)

set.seed(15)
splitPlan <- kWayCrossValidation(nrow(da4), 4, NULL, NULL)
for (i in 1:4) {
        split <- splitPlan[[i]]
        model <- lm(fmIn, data = da4[split$train, ])
        da4$In_Pred[split$app] <- exp(predict(model, newdata = da4[split$app, ]))
}


set.seed(981)
splitPlan <- kWayCrossValidation(nrow(da4), 4, NULL, NULL)
for (i in 1:4) {
        split <- splitPlan[[i]]
        model <- lm(fmOut, data = da4[split$train, ])
        da4$Out_Pred[split$app] <- exp(predict(model, newdata = da4[split$app, ]))
}

set.seed(1530)
splitPlan <- kWayCrossValidation(nrow(da4), 4, NULL, NULL)
for (i in 1:4) {
        split <- splitPlan[[i]]
        model <- lm(fmCombo, data = da4[split$train, ])
        da4$Combo_Pred[split$app] <- exp(predict(model, newdata = da4[split$app, ]))
}

#################################### Evaluation ####################################
da4 <- da4 %>%
        mutate(In_resid = In_Pred - NET_PT_REV,
               Out_resid = Out_Pred - NET_PT_REV,
               Combo_resid = Combo_Pred - NET_PT_REV,
               In_relerr = In_resid / NET_PT_REV, 
               Out_relerr = Out_resid / NET_PT_REV, 
               Combo_relerr = Combo_resid / NET_PT_REV)

da5 <- gather(da4, Model, Value, c(In_Pred, Out_Pred, Combo_Pred)) %>%
        mutate(Model = factor(Model, levels = c("In_Pred",
                                                "Out_Pred",
                                                "Combo_Pred"))) %>% 
        gather(Residual, resid_val, c(In_resid, Out_resid, Combo_resid)) %>%
        gather(Relative_Error, rel_err, c(In_relerr, Out_relerr, Combo_relerr)) 

da5_cor <- da5 %>%
        group_by(Model) %>%
        summarize(correlation = cor(Value, NET_PT_REV))

da5_RMSE <- da5 %>% 
        group_by(Residual) %>%
        summarize(Value = sqrt(mean(resid_val^2))) %>%
        rename(Category = Residual)

da5_SD <- data.frame(Category = "SD",
                     Value = sd(da4$NET_PT_REV))

da5_RMSE_SD <- rbind(da5_RMSE, da5_SD) %>%
        mutate(Category = str_replace_all(Category, "resid", "RMSE"),
               Category = factor(Category, 
                                 levels = c("In_RMSE",
                                            "Out_RMSE",
                                            "Combo_RMSE",
                                            "SD")),
               Regression = "Linear")


da5_RMS_relerr <- da5 %>%
        group_by(Relative_Error) %>%
        summarize(Value = sqrt(mean(rel_err^2))) %>%
        rename(RMS_relerr = Relative_Error) %>%
        mutate(RMS_relerr = str_replace_all(RMS_relerr, "relerr", "RMS_Rel_err"),
               RMS_relerr = factor(RMS_relerr, 
                                 levels = c("In_RMS_Rel_err",
                                            "Out_RMS_Rel_err",
                                            "Combo_RMS_Rel_err")))

                
var_e_In <- var(da4$In_resid)
var_e_Out <- var(da4$Out_resid)
var_e_Combo <- var(da4$Combo_resid)
var_o <- var(da4$NET_PT_REV)


da5_Rsquared <- data.frame(
        Rsquared = factor(c("In_rs",
                            "Out_rs",
                            "Combo_rs"), 
                          levels = c("In_rs",
                                     "Out_rs",
                                     "Combo_rs")),
        Value = c(1 - var_e_In/var_o, 
                  1 - var_e_Out/var_o,
                  1 - var_e_Combo/var_o),
        Regression = "Linear")



                          
        
##################################### Plotting #####################################

library(gridExtra)

# Primary Data Inspection without filtering
plot_insp0 <-
        ggplot(da1, aes(x = Value, y = NET_PT_REV, color = Category)) + 
        geom_point(alpha = 0.2) + 
        geom_smooth(method = "lm", se = F) +
        theme_bw() +
        xlab("Surgery Time") +
        ylab("Net Patient Revenue") + 
        ggtitle("Relationship between Patient Revenue and Surgeries")

plot_insp1 <-
        ggplot(da1, aes(x = Value, y = NET_PT_REV, color = Category)) + 
        geom_point(alpha = 0.2) + 
        geom_smooth(method = "lm", se = F) +
        theme_bw() +
        scale_y_log10() + 
        scale_x_log10() + 
        xlab("Surgery Time (Log-transformed Minutes)") +
        ylab("Net Patient Revenue (Log-transformed)") + 
        ggtitle("Relationship between Patient Revenue and Surgeries")

plot_insp2 <- 
        ggplot(da1, aes(y = Value, fill = Category)) + 
        geom_boxplot(outlier.alpha = 0.3) + 
        theme_bw() + 
        geom_hline(yintercept = 2000000, color = "red", size = 1) + 
        xlab("Patients") + 
        ylab("Surgery Time") + 
        ggtitle("Distribution of Surgery Time") + 
        theme(axis.text.x = element_blank())

plot_insp3 <- 
        ggplot(da1, aes(y = Value, fill = Category)) + 
        geom_boxplot(outlier.alpha = 0.3) + 
        theme_bw() + 
        scale_y_log10() + 
        geom_hline(yintercept = 1500, color = "blue", size = 1) + 
        xlab("Patients") + 
        ylab("Surgery Time (Log-transformed Minutes)") + 
        ggtitle("Distribution of Surgery Time")  + 
        theme(axis.text.x = element_blank())

grid.arrange(plot_insp2,
             plot_insp3, ncol = 1)
plot_insp4 <- 
        ggplot(da1, aes(y = NET_PT_REV, fill = "#339999")) +
        geom_boxplot(outlier.alpha = 0.3) +
        theme_bw() +
        theme(legend.position = "none") + 
        geom_hline(yintercept = 3000000000, color = "red", size = 1) +
        theme(axis.text.x = element_blank()) +
        ylab("Patient Revenue") + 
        ggtitle("Distribution of Patient Revenue") 

plot_insp5 <- 
        ggplot(da1, aes(y = NET_PT_REV, fill = "#339999")) +
        geom_boxplot(outlier.alpha = 0.3) +
        theme_bw() +
        theme(legend.position = "none") +
        geom_hline(yintercept = 1000000, color = "blue", size = 1) +
        theme(axis.text.x = element_blank()) +
        scale_y_log10() +
        ylab("Log-transformed Patient Revenue") + 
        ggtitle("Distribution of Patient Revenue")

grid.arrange(plot_insp4,
             plot_insp5, ncol = 1)

# Data Inspection after filtering
plot_insp6 <-
        ggplot(da2, aes(x = Value, y = NET_PT_REV, color = Category)) + 
        geom_point(alpha = 0.2) + 
        geom_smooth(method = "lm", se = F) +
        theme_bw() + 
        scale_y_log10() + 
        scale_x_log10() + 
        xlab("Surgery Time (Log-transformed Minutes)") +
        ylab("Net Patient Revenue (Log-transformed)") + 
        ggtitle("Relationship between Patient Revenue and Surgeries (After Outlier Filtering)")

plot_insp7 <-
        ggplot(da2,
               aes(x = log(Value),
                   fill = Category)) + 
        geom_density(alpha = 0.3) + 
        theme_bw() +
        xlab("Surgery Time (Log Minutes)") + 
        ylab("Density") +
        ggtitle("Distribution of Surgery time")

plot_insp8 <-
        ggplot(da2,
               aes(x = log(NET_PT_REV),
                   fill = Category)) + 
        geom_density(alpha = 0.3) + 
        theme_bw() +
        xlab("Net Patient Revenue (Log-transformed)") + 
        ylab("Density") +
        ggtitle("Distribution of Patient Revenue")
        
        
grid.arrange(plot_insp7,
             plot_insp8,
             ncol = 1)        
        
        
# Model Evaluation                   
plot_eval1 <- 
        ggplot(da5, aes(x = Value, y = NET_PT_REV, color = Model)) + 
        geom_jitter() + 
        geom_smooth(method = "lm", se = F, color = "black") + 
        facet_grid(Model ~.) + 
        theme_bw() + 
        scale_x_log10() +
        scale_y_log10() +
        xlab("Predicted Patient Revenue (Log-transformed)") + 
        ylab("Original Patient Revenue (Log-transformed)") + 
        ggtitle("Relationship between Predicted and Original Outcomes")

plot_resid <- function(df, xname, tit, c) {
        ggplot(df, aes(x = xname, y = In_resid)) + 
                geom_point(alpha = 0.3, color = c) + 
                geom_smooth(method = "lm", se = F, color = "black") + 
                theme_bw() + 
                ylab("Residual") + 
                xlab("Prediction") + 
                ggtitle(tit)
}



plot_In_resid <- plot_resid(da4, da4$In_Pred, "Residual in Model 1", "#FF9999")

plot_Out_resid <- plot_resid(da4, da4$Out_Pred, "Residual in Model 2", "#009933")

plot_Combo_resid <- plot_resid(da4, da4$Combo_Pred, "Residual in Model 3", "#3399FF")

grid.arrange(plot_In_resid,
             plot_Out_resid,
             plot_Combo_resid,
             nrow = 1)

library(WVPlots)
# Gain Curves
gain_curve <- function(df, model, tit) {
        GainCurvePlot(df, model, "NET_PT_REV", tit) + 
                theme_bw() +
                xlab("Fraction Items in Sort Order") + 
                ylab("Fraction Total Sum Net Patient Revenue")
}

gain_curve_In <- gain_curve(da4, "In_Pred", "")
gain_curve_Out <- gain_curve(da4, "Out_Pred", "")
gain_curve_Combo <- gain_curve(da4, "Combo_Pred", "")

grid.arrange(gain_curve_In, 
             gain_curve_Out,
             gain_curve_Combo, ncol = 1)

eval_plot <- function(df, xname, ylabel, tit) {
        ggplot(df,
               aes(x = xname, 
                   y = Value,
                   fill = xname)) +
                geom_bar(stat = "identity", width = 0.8) + 
                theme_bw() + 
                ylab(ylabel) + 
                ggtitle(tit) +
                xlab("By Model") +
                theme(legend.position = "none")
}


plot_RMSE <- eval_plot(da5_RMSE_SD, 
                       da5_RMSE_SD$Category, 
                       "RMSE or SD", 
                       "RMSE") + 
        coord_cartesian(ylim = c(100000000, 240000000))


plot_RMS_relerr <- eval_plot(da5_RMS_relerr, 
                             da5_RMS_relerr$RMS_relerr, 
                             "RMS Relative Error", 
                             "RMS Relative Error") +
        coord_cartesian(ylim = c(0.5, 1.7))

plot_Rsqaured <- eval_plot(da5_Rsquared, 
                           da5_Rsquared$Rsquared, 
                           "R^2", 
                           "R Squared") + 
        coord_cartesian(ylim = c(0.5, 0.75))

grid.arrange(plot_RMSE,
             plot_RMS_relerr,
             plot_Rsqaured, 
             ncol = 1) 
             

