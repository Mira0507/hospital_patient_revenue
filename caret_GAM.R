library(ggplot2)
library(data.table)
library(tidyverse)



g <- glimpse
h <- head
s <- summary

#################################### Data Importing ####################################

da <- fread("selected-file_2012_2017_machinereadable.csv")

#################################### Data Cleaning ####################################

# primary data cleaning
da1 <- da %>% 
        filter(Variable %in% c("OP_MIN_IP",
                               "OP_MIN_OP",
                               "NET_PT_REV")) %>%
        mutate(Variable = str_replace(Variable, "OP_MIN_IP", "Inpatient"),
               Variable = str_replace(Variable, "OP_MIN_OP", "Outpatient")) %>%
        spread(Variable, Amount)


# Raw data inspection
RawPlot <- da1 %>% 
        gather(Surgery, Surgery_time, c(Inpatient, Outpatient)) %>%
        ggplot(aes(x = Surgery_time,
                   y = NET_PT_REV,
                   color = Surgery)) + 
        geom_point(alpha = 0.3) + 
        theme_bw() + 
        ylab("Net Patient Revenue") + 
        xlab("Surgery Time") + 
        ggtitle("Relationship between Patient Revenue and Surgeries")


# zero_obs: observation having zero in outcome or predictors 
zero_obs <- da1 %>% 
        filter(NET_PT_REV == 0 | Inpatient == 0 | Outpatient == 0) %>%
        select(Inpatient, Outpatient, NET_PT_REV)


# nonzero_obs: observation NOT having zero in outcome or predictors
nonzero_obs <- da1 %>% 
        filter(NET_PT_REV != 0, Inpatient != 0, Outpatient != 0) %>%
        select(Inpatient, Outpatient, NET_PT_REV)

# data cleaning for raw data inspection
nonzero_obs_gathered <- nonzero_obs %>%
        gather(Surgery, Time, -NET_PT_REV) %>%
        ggplot(aes(x = Time,
                   y = NET_PT_REV,
                   color = Surgery)) + 
        geom_point(alpha = 0.3) +
        theme_bw() +
        geom_smooth(method = "gam", se = F)

# plotting for raw data w or wo log-transformation 
GAMRawInspRaw <- nonzero_obs_gathered +
        xlab("Surgery Time") + 
        ylab("Net Patient Revenue") +
        ggtitle("Relationship between Patient Revenue and Surgeries")

GAMRawInspLog <- nonzero_obs_gathered +
        xlab("Surgery Time (Log)") + 
        ylab("Net Patient Revenue (Log)") +
        ggtitle("Relationship between Patient Revenue and Surgeries") +
        scale_x_log10() + 
        scale_y_log10()

library(gridExtra)

grid.arrange(GAMRawInspRaw, GAMRawInspLog, ncol = 1)


# Linearity Inspection
v <- c("Inpatient vs Patient Revenue",
       "Outpatient vs Patient Revenue",
       "log(Inpatient) vs log(Patient Revenue)",
       "log(Outpatient) vs log(Patient Revenue)")

corRaw <- data.frame(
        Correlation = c(cor(nonzero_obs$Inpatient, nonzero_obs$NET_PT_REV),
                        cor(nonzero_obs$Outpatient, nonzero_obs$NET_PT_REV),
                        cor(log(nonzero_obs$Inpatient), log(nonzero_obs$NET_PT_REV)),
                        cor(log(nonzero_obs$Outpatient), log(nonzero_obs$NET_PT_REV))),
        Variable = factor(v,
                          levels = v))

corRaw_plot <- 
        ggplot(corRaw, aes(x = Variable,
                           y = Correlation,
                           fill = Variable,
                           label = round(Correlation, 2))) + 
        geom_bar(stat = "identity", width = 0.8) + 
        theme_bw() +
        theme(axis.text.x = element_blank()) + 
        ylab("Correlation Coefficient (Pearson's r)") + 
        ggtitle("Linearity between Patient Revenue and Surgeries") + 
        geom_label(vjust = -0.5) + 
        ylim(NA, 1.0)


RawDensity <- nonzero_obs %>% 
        gather(Surgery, Time, c(Inpatient, Outpatient)) %>%
        ggplot(aes(x = Time,
                   fill = Surgery,
                   color = Surgery)) +
        geom_density(alpha = 0.4) + 
        ylab("Density") + 
        ggtitle("Distribution of Surgery Time") + 
        theme_bw() + 
        theme(legend.position = "top")

RawDensityRaw <- RawDensity + 
        xlab("Surgery Time (Minutes)") 


RawDensityLog <- RawDensity + 
        xlab("Surgery Time (Minutes, Log)") + 
        scale_x_log10()

RawOutcome <- ggplot(nonzero_obs,
                     aes(x = NET_PT_REV,
                         fill = "CC66CC")) +
        geom_density(alpha = 0.4) + 
        theme_bw() + 
        ylab("Density") + 
        ggtitle("Distribution of Patient Revenue") + 
        theme(legend.position = "none")

RawOutcomeRaw <- RawOutcome + 
        xlab("Net Patient Revenue")

RawOutcomeLog <-  RawOutcome + 
        scale_x_log10() +
        xlab("Net Patient Revenue (Log)")

grid.arrange(RawDensityRaw, 
             RawDensityLog,
             RawOutcomeRaw,
             RawOutcomeLog,
             nrow = 2)

################################### Modeling ###################################
GAMfmIn <- as.formula(NET_PT_REV ~ Inpatient)
GAMfmOut <- as.formula(NET_PT_REV ~ Outpatient)
GAMfmCombo <- as.formula(NET_PT_REV ~ Inpatient + Outpatient)

# 10-fold cross-validation
library(caret)

set.seed(52) 
myFold <- createFolds(nonzero_obs$NET_PT_REV, k = 10)

# Inspecting Folds
s(nonzero_obs$NET_PT_REV)
s(nonzero_obs$NET_PT_REV[myFold$Fold01])
s(nonzero_obs$NET_PT_REV[myFold$Fold02])

# Training
myControl <- trainControl(method = "cv",
                          number = 10,
                          savePrediction = TRUE,
                          verboseIter = TRUE,
                          index = myFold)

Train_fn <- function(mod) {
        train(mod,
              nonzero_obs, 
              method = "gam",
              family = "gaussian",
              trControl = myControl)
}


GAMModelIn <- Train_fn(GAMfmIn)
GAMModelOut <- Train_fn(GAMfmOut)
GAMModelCombo <- Train_fn(GAMfmCombo)

####################################### Comparing Models #########################################

# Computing Goodness of Fit 
GAMmodels <- list(Inpatient = GAMModelIn,
               Outpatient = GAMModelOut,
               Combo = GAMModelCombo)


GAMrsam <- resamples(GAMmodels)

dotplot(GAMrsam)

GAM_new_name <- colnames(GAMrsam$values) %>%
        str_replace("~", "_") %>%
        str_replace("Resample", "Fold")


GAMrsam_fit <- GAMrsam$values 

names(GAMrsam_fit) <- GAM_new_name


# Plotting Goodness of Fit
GAM_TrainFit_data <- gather(GAMrsam_fit, Model, Value, -Fold) %>% 
        separate(Model, c("Surgery",
                          "Metric")) %>%
        mutate(Surgery = factor(Surgery,
                                levels = c("Inpatient",
                                           "Outpatient",
                                           "Combo")),
               Metric = factor(Metric, 
                               levels = c("RMSE",
                                          "MAE",
                                          "Rsquared"))) 

GAM_TrainFit <- GAM_TrainFit_data %>%
        ggplot(aes(x = Surgery, y = Value, fill = Surgery)) + 
        geom_boxplot() + 
        facet_grid(Metric ~ ., scales = "free_y") + 
        theme_bw() + 
        ggtitle("Model Fit with Training Data")

####################################### Prediction #########################################

# Computing Prediction and Residual
nonzero_obs <- nonzero_obs %>% 
        mutate(Pred_GInpatient = predict(GAMModelIn, nonzero_obs),
               Pred_GOutpatient = predict(GAMModelOut, nonzero_obs),
               Pred_GCombo = predict(GAMModelCombo, nonzero_obs),
               Resid_GInpatient = Pred_GInpatient - NET_PT_REV,
               Resid_GOutpatient = Pred_GOutpatient - NET_PT_REV,
               Resid_GCombo = Pred_GCombo - NET_PT_REV)


# Plotting Prediction vs Residual
GAMPvsO <- nonzero_obs %>%
        gather(Model, Value, c(Pred_GInpatient,
                               Pred_GOutpatient,
                               Pred_GCombo)) %>%
        mutate(Model = str_replace(Model, "Pred_G", ""),
               Model = factor(Model,
                              levels = c("Inpatient",
                                         "Outpatient",
                                         "Combo"))) %>%
        ggplot(aes(x = Value,
                   y = NET_PT_REV,
                   color = Model)) +
        geom_point(alpha = 0.3) +
        facet_grid(Model~.) + 
        geom_smooth(se = F, color = "black") +
        theme_bw() + 
        scale_x_log10() +
        scale_y_log10() + 
        xlab("Predicted Patient Revenue") + 
        ylab("Actual Patient Revenue") + 
        ggtitle("Comparison between Prediction and Actual Outcome")

# Plotting Correlation Coefficient
GAM_corr <- summarize(nonzero_obs,
                  G_Inpatient = cor(NET_PT_REV, Pred_GInpatient),
                  G_Outpatient = cor(NET_PT_REV, Pred_GOutpatient),
                  G_Combo = cor(NET_PT_REV, Pred_GCombo)) %>% 
        gather(Model, Correlation) %>%
        mutate(Model = str_replace(Model, "G_", ""),
               Model = factor(Model, 
                              levels = c("Inpatient",
                                         "Outpatient",
                                         "Combo"))) %>%
        ggplot(aes(x = Model, y = Correlation, 
                   fill = Model,
                   label = round(Correlation, 3))) +
        geom_bar(stat = "identity", width = 0.8) + 
        theme_bw() +
        theme(axis.text.x = element_blank()) + 
        ylab("Correlation Coefficient (Pearson's r)") + 
        ggtitle("Correlation between Actual Outcome and Prediction") +
        geom_label(vjust = -0.5) + 
        ylim(NA, 1.0)


             

# Plotting Residuals
resid_fn <- function(df, xcol, ycol, c, tit) {
        ggplot(df, 
               aes(x = xcol,
                   y = ycol)) + 
                geom_point(alpha = 0.3, color = c) + 
                geom_smooth(method = "lm", se = F) + 
                theme_bw() + 
                xlab("Prediction") + 
                ylab("Residual") + 
                ggtitle(tit) 
}

Gresid_in <- resid_fn(nonzero_obs,
                     nonzero_obs$Pred_GInpatient,
                     nonzero_obs$Resid_GInpatient,
                     "#FF9999",
                     "Inpatient Model")

Gresid_out <- resid_fn(nonzero_obs,
                      nonzero_obs$Pred_GOutpatient,
                      nonzero_obs$Resid_GOutpatient,
                      "#009933",
                      "Outpatient Model")

Gresid_combo <- resid_fn(nonzero_obs,
                        nonzero_obs$Pred_GCombo,
                        nonzero_obs$Resid_GCombo,
                        "#3399FF",
                        "Combo Model")


grid.arrange(Gresid_in,
             Gresid_out,
             Gresid_combo,
             nrow = 1)

library(WVPlots)
# Gain Curves
gain_curve <- function(df, model, tit) {
        GainCurvePlot(df, model, "NET_PT_REV", tit) + 
                theme_bw() +
                xlab("Fraction Items in Sort Order") + 
                ylab("Fraction Total Sum Net Patient Revenue")
}


GAMGain_curve_In <- gain_curve(nonzero_obs, "Pred_GInpatient", "Inpatient Model")
GAMGain_curve_Out <- gain_curve(nonzero_obs, "Pred_GOutpatient", "Outpatient Model")
GAMGain_curve_Combo <- gain_curve(nonzero_obs, "Pred_GCombo", "Combo Model")

grid.arrange(GAMGain_curve_In, 
             GAMGain_curve_Out,
             GAMGain_curve_Combo, ncol = 1)


NewGAMColName <- str_replace(colnames(nonzero_obs), "_G", "_")
names(nonzero_obs) <- NewGAMColName


# Model fit with Test data 

GAM_TrainFit_rename <- GAM_TrainFit_data %>%
        rename(Model = Surgery)


TestStat_fn <- function(mod) {
        postResample(pred = predict(mod, nonzero_obs),
             obs = nonzero_obs$NET_PT_REV)
}

GAMInStat <- TestStat_fn(GAMModelIn)
GAMOutStat <- TestStat_fn(GAMModelOut)
GAMComboStat <- TestStat_fn(GAMModelCombo)

GAMTestFit <- data.frame(rbind(GAMInStat, 
                                 GAMOutStat, 
                                 GAMComboStat)) %>%
        mutate(Model = c("Inpatient", 
                         "Outpatient",
                         "Combo")) %>%
        gather(Metric, Value, -Model) %>%
        mutate(Metric = factor(Metric,
                               levels = c("RMSE", 
                                          "MAE", 
                                          "Rsquared")), 
               Model = factor(Model, 
                              levels = c("Inpatient", 
                                         "Outpatient",
                                         "Combo"))) %>%
        ggplot(aes(x = Model,
                   y = Value,
                   fill = Model)) + 
        geom_bar(stat = "identity", width = 0.8, alpha = 0.5) + 
        facet_grid(Metric ~., scales = "free_y") + 
        geom_boxplot(data = GAM_TrainFit_rename,
                     aes(x  = Model, y = Value, fill = Model)) +
        theme_bw() + 
        ggtitle("Model Fit with Test Data") 

