################################## Setup ##################################
# Load packages
library(ggplot2)
library(RColorBrewer)


###################### Reading Data & Preprocessing ######################
# Import the dataset
VotingData <- read.csv("ReferendumResults.csv", na.strings = "-1")

# Take a preliminary look at the dataset
cat("\nSummary of VotingData:\n")
cat("========================\n")
cat("\nVariables in data set:\n")
cat("--------------------\n")
print(str(VotingData))
cat("\nSummary statistics:\n")
cat("-----------------\n")
print(summary(VotingData))

# Convert categorical variables to factors
VotingData$RegionName <- as.factor(VotingData$RegionName)
VotingData$AreaType <- as.factor(VotingData$AreaType)



###################### Part 1: Exploratory Analysis ######################
####################### Response Variable Overview #######################
# Define the response variable as the proportion of Leave votes
VotingData$LeaveProp <- VotingData$Leave / VotingData$NVotes

# Check the range of LeaveProp
cat("\nSummary statistics of LeaveProp:\n")
cat("-----------------\n")
print(summary(VotingData$LeaveProp))
cat("\nCheck the range of LeaveProp:\n")
cat("------------------------------\n")
cat(sprintf("Minimum: %.3f\n", min(VotingData$LeaveProp, na.rm = TRUE)))
cat(sprintf("Maximum: %.3f\n", max(VotingData$LeaveProp, na.rm = TRUE)))

### Fig.1: Histogram with density curve of LeaveProp
# Histogram of LeaveProp
par(mfrow = c(1, 1))
hist(VotingData$LeaveProp, breaks = 35, probability = TRUE, 
     col = "lightblue", border = "white",
     ylim = c(0,4.5),
     main = "Distribution of Leave Vote Proportion",
     xlab = "LeaveProp",
     ylab = "Density")

# Add the density curve
lines(density(na.omit(VotingData$LeaveProp)), col="darkblue", lwd=1.5)

dev.copy(pdf, file = "Fig.1_Histogram of LeaveProp.pdf", 
         width = 10, height = 6)
dev.off()

# QQ Plot of LeaveProp
par(mfrow = c(1, 1))
qqnorm(VotingData$LeaveProp, main="Q-Q Plot of LeaveProp")
qqline(VotingData$LeaveProp, col="red")

dev.copy(pdf, file = "Fig.2_QQ Plot of LeaveProp.pdf", width = 6, height = 6)
dev.off()


########################## Correlation Analysis ##########################
# Select numerical variable
numeric_vars <- VotingData[, sapply(VotingData, is.numeric)]
numeric_vars <- numeric_vars[, colSums(!is.na(numeric_vars)) > 0]

# Correlation matrix
cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")

# Highly correlated pairs (absolute correlation ≥ 0.9)
high_cor_idx <- which(abs(cor_matrix) >= 0.9 & abs(cor_matrix) != 1, 
                      arr.ind = TRUE)
HighCorMatrix <- data.frame(
  row = rownames(cor_matrix)[high_cor_idx[, 1]],
  col = colnames(cor_matrix)[high_cor_idx[, 2]],
  value = cor_matrix[high_cor_idx]
)
HighCorMatrix <- HighCorMatrix[!duplicated(HighCorMatrix$value), ]
HighCorMatrix <- HighCorMatrix[order(HighCorMatrix$row, HighCorMatrix$col), ]
cat("\nPairs of covariates with absolute correlation ≥ 0.9:\n")
cat("------------------------------\n")
print(HighCorMatrix)


############################## Key Visuals ##############################
### Fig.3: Covariates related to education levels, including NoQuals, 
### L1Quals, L4Quals_plus
# Remove rows with missing Leave vote data and store it for subsequent plotting
VotingDataPlot <- VotingData[!is.na(VotingData$Leave), ]

# Set colours to distinguish between different groups
qual_colors <- brewer.pal(3, "Set2")
names(qual_colors) <- c("NoQuals", "L1Quals", "L4Quals_plus")

par(mfrow = c(1,1), mar = c(5,6,4,4))
with(VotingDataPlot, {
  plot(
    NA,
    xlim = range(c(NoQuals, L1Quals, L4Quals_plus), na.rm = TRUE),
    ylim = range(LeaveProp, na.rm = TRUE),
    xlab = "Percentage of Permanent Residents",
    ylab = "Leave Vote Proportion",
    main = "Leave Vote Proportion by Education Level"
  )
  
  add_series <- function(x, y, col) {
    points(x, y, pch = 16, cex = 0.8, col = adjustcolor(col, alpha.f = 0.3))
    lines(lowess(x, y), col = col, lwd = 2.5)
  }
  
  add_series(NoQuals, LeaveProp, qual_colors["NoQuals"])
  add_series(L1Quals, LeaveProp, qual_colors["L1Quals"])
  add_series(L4Quals_plus, LeaveProp, qual_colors["L4Quals_plus"])
  
  legend(
    "topright",
    legend = names(qual_colors),
    col = qual_colors,
    pch = 16,
    cex = 0.8,
    lwd = 2,
    bty = "n"
  )
})

dev.copy(pdf, file="Fig.3_Scatterplot of NoQuals, L1Quals, and L4Quals_plus.pdf", 
         width = 9, height = 6)
dev.off()

### Fig.4: Boxplots of covariates related to age, ethnicity and region groups
# Define new summary variables for age
VotingData$Age0_17 <- VotingData$Age_0to4 +
  VotingData$Age_5to7 +
  VotingData$Age_8to9 +
  VotingData$Age_10to14 +
  VotingData$Age_15 +
  VotingData$Age_16to17

VotingData$Age18_29 <- VotingData$Age_18to19 +
  VotingData$Age_20to24 +
  VotingData$Age_25to29

VotingData$Age30_64 <- VotingData$Age_30to44 +
  VotingData$Age_45to59 +
  VotingData$Age_60to64

VotingData$Age_65plus <- VotingData$Age_65to74 +
  VotingData$Age_75to84 +
  VotingData$Age_85to89 +
  VotingData$Age_90plus

age_group <- factor(rep(c("0-17", "18-29", "30-64", "65+"), 
                        each = nrow(VotingData)))
age_val <- c(VotingData$Age0_17, VotingData$Age18_29, VotingData$Age30_64, 
             VotingData$Age_65plus)

eth_group <- factor(rep(c("White", "Black", "Asian", "Indian", "Pakistani"), 
                        each = nrow(VotingData)))
eth_val <- c(VotingData$White, VotingData$Black, VotingData$Asian, 
             VotingData$Indian, VotingData$Pakistani)

par(mfrow = c(1,3), mar = c(12,4,4,1))
# Boxplot of four age groups
boxplot(age_val ~ age_group,
        col = "skyblue", border = "darkblue",
        main = "LeaveProp by Age",
        xlab = "", ylab = "Leave Vote Proportion", cex=0.8, las=2)

# Boxplot of five ethnic groups
boxplot(eth_val ~ eth_group,
        col = "lightgreen", border = "darkgreen",
        main = "LeaveProp by Ethnicity",
        xlab = "", ylab = "Leave Vote Proportion", las=2)

# Boxplot of nine region groups
boxplot(VotingData$LeaveProp ~ VotingData$RegionName,
        col = "lightpink", border = "darkred",
        main = "LeaveProp by Region",
        xlab = "", ylab = "Leave Vote Proportion", cex=0.8, las=2)

dev.copy(pdf, file = "Fig.4_Boxplots of age, ethnicity, and region.pdf",
         width = 12, height = 8)
dev.off()

### Fig.5: Scatterplots of covariates related to housing, 
### unemployment, density, deprivation, and social grades
# Derive individual social grade components from aggregated C1C2DE and C2DE
VotingData$C1 <- VotingData$C1C2DE - VotingData$C2DE
VotingData$C2 <- VotingData$C2DE - VotingData$DE
VotingDataPlot$C1 <- VotingDataPlot$C1C2DE - VotingDataPlot$C2DE
VotingDataPlot$C2 <- VotingDataPlot$C2DE - VotingDataPlot$DE

plot_key_cov <- function(data, var_list, layout = c(2, 5)) {
  n <- length(var_list)
  colors <- rainbow(n)  
  par(mfrow = layout, mar = c(4, 4, 2, 1))  
  
  for (i in seq_along(var_list)) {
    var <- var_list[i]
    x <- data[[var]]
    y <- data$LeaveProp
    
    plot(x, y,
         main = paste("LeaveProp vs", var),
         xlab = paste(var, "(%)"),
         ylab = "LeaveProp",
         pch = 20,
         col = "grey")
    
    lines(lowess(x, y), col = colors[i], lwd = 2)
  }
}

plot_key_cov(data = VotingDataPlot,
             var_list = c("Owned", "OwnedOutright", "SocialRent", "PrivateRent", 
                          "Unemp", "Density", "MultiDepriv", "C1", "C2", "DE")
)

dev.copy(pdf, file = "Fig.5_Scatterplots of key covariates.pdf",
         width = 12, height = 8)
dev.off()

### Fig.6: Scatterplots and histogram of students
par(mfrow = c(1, 3), mar = c(6, 6, 2, 1))

# Plot 1: Scatterplot of Students vs LeaveProp
plot(VotingDataPlot$Students, VotingDataPlot$LeaveProp,
     main = "LeaveProp vs Students",
     xlab = "Percentage of Students",
     ylab = "LeaveProp",
     pch = 19, col = rgb(0, 0, 0, 0.3))
lines(lowess(VotingDataPlot$Students, VotingDataPlot$LeaveProp),
      col = "blue", lwd = 2)

# Plot 2: Histogram of Students
hist(VotingDataPlot$Students, freq = TRUE,
     main = "Histogram of Students",
     xlab = "Percentage of Students",
     ylab = "Frequency", ylim = c(0,500),
     col = "gray",
     border = "white",
     breaks = 30)

# Plot 3: Scatterplot of log(Students) vs LeaveProp
VotingDataPlot$LogStudents <- log(VotingDataPlot$Students)

plot(VotingDataPlot$LogStudents, VotingDataPlot$LeaveProp,
     main = "LeaveProp vs Students",
     xlab = "log(Students)",
     ylab = "LeaveProp",
     pch = 19, col = rgb(0, 0, 0, 0.3))
lines(lowess(VotingDataPlot$LogStudents, VotingDataPlot$LeaveProp),
      col = "blue", lwd = 2)

dev.copy(pdf, file = "Fig.6_Scatterplots and histogram of students.pdf",
         width = 10, height = 6)
dev.off()



######################### Part 2: Model Building #########################
############################# Initial Model #############################
###### GLM_LeaveProp0 ######
# Fit all the potential covariates suggested by the EDA
GLM_LeaveProp0 <- glm(LeaveProp ~ 
                        RegionName + 
                        Residents + Households +
                        Age0_17 + Age18_29 + Age30_64 + Age_65plus +
                        White + Black + Indian + Pakistani + Asian +
                        Owned + OwnedOutright + SocialRent + PrivateRent +
                        NoQuals + L1Quals + L4Quals_plus +
                        log(Students) +
                        Unemp +
                        C1 + C2 + DE +
                        Density +
                        MultiDepriv,
                      weights = NVotes, 
                      data = VotingData,
                      contrasts = list(RegionName = "contr.sum"),
                      family=binomial(link="logit") )
cat("\nSummary of our initial model (GLM_LeaveProp0):\n")
cat("------------------------------\n")
print(summary(GLM_LeaveProp0))

# Check for overdispersion by computing the variance of the Pearson residuals
dispersion_glm0 <- sum(
  resid(GLM_LeaveProp0, type = "pearson")^2) / GLM_LeaveProp0$df.residual
cat("\nDispersion parameter of our initial model is:\n")
cat("-----------------\n")
print(round(dispersion_glm0,4))

# Check other model assumptions by looking at the diagnostic plots
par(mfrow=c(2,2),lwd=2,mar=c(3,3,2,2),mgp=c(2,0.75,0))
plot(GLM_LeaveProp0,which=1:4)
dev.copy(pdf, file = "Diagnostic plots of our initial model.pdf", 
         width = 10, height = 8)
dev.off()


######################## Choice of link functions ########################
###### GLM_Logit, GLM_Probit, and GLM_Cloglog ######
# Compare 3 link functions to determine the best fit
GLM_Logit <- update(GLM_LeaveProp0, family=quasibinomial(link="logit"))
GLM_Probit <- update(GLM_LeaveProp0, family=quasibinomial(link="probit"))
GLM_Cloglog <- update(GLM_LeaveProp0, family=quasibinomial(link="cloglog"))

cat("\nCompare 3 link functions:\n")
cat("-----------------\n")
cat("Residual deviance for Logit link is", deviance(GLM_Logit), "\n")
cat("Residual deviance for Probit link is", deviance(GLM_Probit), "\n")
cat("Residual deviance for Complementary log-log links is",
    deviance(GLM_Cloglog), "\n")

###### GLM_LeaveProp01 ######
# Choose the above three models with the smallest residual deviance
GLM_LeaveProp01 <- GLM_Logit


########################## Hierarchical clustering ##########################
# Extract the estimated (n-1) region coefficients from GLM_LeaveProp01
region_coef <- coef(GLM_LeaveProp01)[grep("RegionName", 
                                          names(coef(GLM_LeaveProp01)))]

# Reconstruct the full vector of region effects
region_effects <- setNames(c(region_coef, -sum(region_coef)), 
                           levels(VotingData$RegionName))
cat("\nCheck the estimated coefficients for each region:\n")
cat("------------------------------\n")
print(region_effects)

# Perform the hierarchical clustering
Distances <- dist(region_effects)                # Pairwise Euclidean distances
ClusTree <- hclust(Distances, method="complete") # Do the clustering
NewGroups <- cutree(ClusTree, k=5)               # Cut tree to form 5 groups
cat("\nRegion clusters:\n")
cat("------------------------------\n")
print(NewGroups, width=90)

# Add new groups to the original data frame
VotingData$NewGroup <- NewGroups[match(VotingData$RegionName, names(NewGroups))]
VotingData$NewGroup <- as.factor(VotingData$NewGroup)
cat("\nCheck the cluster distribution by region:\n")
cat("------------------------------\n")
print(table(VotingData[, c("NewGroup", "RegionName")], 
            dnn = c("Group", "Region")))

###### GLM_LeaveProp02 ######
# Refit the model using `NewGroup` instead of `RegionName`
GLM_LeaveProp02 <- update(GLM_LeaveProp01, . ~ . - RegionName + NewGroup, 
                          contrasts = list(NewGroup = "contr.sum"))
cat("\nSummary of GLM_LeaveProp02 model:\n")
cat("------------------------------\n")
print(summary(GLM_LeaveProp02))


######################### Interaction Analysis #########################
# Select all the covariates in GLM_LeaveProp02 to draw the interaction plots
Covariates <- c("Residents", "Households",
                "Age0_17", "Age18_29", "Age30_64", "Age_65plus",
                "White", "Black", "Indian", "Pakistani", "Asian",
                "Owned", "OwnedOutright", "SocialRent", "PrivateRent",
                "NoQuals", "L1Quals", "L4Quals_plus",
                "log(Students)", "Unemp", "C1", "C2", "DE",
                "Density", "MultiDepriv")
ReportCovariates <- c("log(Students)", "Age18_29")

Rename <- c(
  "log(Students)" = "Fig.7_InteractionPlot_log(Students).pdf",
  "Age18_29"  = "Fig.7_InteractionPlot_Age18_29.pdf"
)

# Function to draw all interaction plots with different colours of NewGroups
for (v in Covariates) {
  
  suppressWarnings(
    InterPlot <- ggplot(
      VotingData,
      aes_string(x = v, y = "LeaveProp", color = "NewGroup")
    ) +
      geom_point(shape = 16, size = 1.5, alpha = 0.6, na.rm = TRUE) +
      stat_smooth(
        method = "glm",
        method.args = list(family = quasibinomial(link = "logit")),
        se = FALSE, size = 1, na.rm = TRUE
      ) +
      scale_color_manual(values = c("1" = "red",   "2" = "orange", "3" = "blue",
                                    "4" = "green", "5" = "pink")
      ) +
      labs(x = v, y = "LeaveProp", title = paste("LeaveProp vs.", v)) +
      theme_minimal()
  )
  
  suppressMessages(print(InterPlot))
  
  if (v %in% ReportCovariates) {
    filename <- Rename[[v]]
  } else {
    filename <- paste0("InteractionPlot_", v, ".pdf")
  }
  
  # Save all plots
  dev.copy(pdf, file = filename, width = 10, height = 6)
  dev.off()
  
}

###### GLM_LeaveProp03 - 14 ######
# Test each covariate’s interaction with NewGroup by comparing nested models
# via ANOVA F‑tests
InteractionAnovasTest <- function(base_model,vars,group_var = "NewGroup",
                                  test = "F") {
  # base_model: The original model without any interactions.
  # vars:   	The variables or covariates to add interactions with group_var.
  # group_var:  The grouping variable.
  # test:   	The test you want for ANOVA. The default is an F-test.
  current_model <- base_model
  p.values <- numeric(length(vars))
  names(p.values) <- vars
  
  for (i in seq_along(vars)) {
    v <- vars[i]
    
    # Update formula: remove v, add v * group_var
    upd_call <- paste0(". ~ . - ", v, " + ", v, "*", group_var)
    new_model <- update(current_model, upd_call)
    
    # Run the ANOVA between the two nested models
    an <- anova(current_model, new_model, test = test)
    
    # Extract the p-value for the added interaction (row 2 of the table)
    pval <- an$`Pr(>F)`[2]
    
    # Print the result
    cat(sprintf("Interaction with %-12s: p = %.4g\n", v, pval))
    
    # Store and move on
    p.values[i] <- pval
    current_model <- new_model
  }
  return(list(p.values = p.values, final_model = current_model))
}

TestVars <- c("Residents", "MultiDepriv", "Age18_29",
              "NoQuals", "L4Quals_plus", "L1Quals",
              "Black", "White", "Indian", "Asian", "Pakistani",
              "log(Students)")

cat("\nANOVA F‑test results for NewGroup × covariate interactions:\n")
cat("------------------------------\n")
pvals <- InteractionAnovasTest(GLM_LeaveProp02,
                               vars = TestVars,
                               group_var = "NewGroup",
                               test = "F")

###### GLM_LeaveProp15 & 16 ######
# Fit the ‘full’ model including all candidate NewGroup interactions
GLM_LeaveProp15 <- pvals$final_model

# Fit the ‘reduced’ model after dropping the interaction with Pakistani and
# log(Students)
GLM_LeaveProp16 <- update(GLM_LeaveProp15, . ~ . - Pakistani:NewGroup
                          - log(Students):NewGroup)

cat(
  "\nComparing GLM_LeaveProp16 (reduced) against GLM_LeaveProp15 (full)",
  "with an ANOVA F‑test:\n"
)
cat("------------------------------\n")
print(anova(GLM_LeaveProp16, GLM_LeaveProp15, test="F"))

# Show the summary output of the final chosen model with interactions
cat("\nSummary of the selected interaction model (GLM_LeaveProp16):\n")
cat("------------------------------\n")
print(summary(GLM_LeaveProp16))  # Residual deviance: 27197


############################## Final Model ##############################
###### GLM_LeaveProp17 - GLM_LeaveProp21 ######
# Sequentially drop non‑significant terms based on the parsimonious rule
GLM_LeaveProp17 <- update(GLM_LeaveProp16, . ~ . - PrivateRent)
GLM_LeaveProp18 <- update(GLM_LeaveProp17, . ~ . - Age0_17)
GLM_LeaveProp19 <- update(GLM_LeaveProp18, . ~ . - OwnedOutright)
GLM_LeaveProp20 <- update(GLM_LeaveProp19, . ~ . - log(Students))
GLM_LeaveProp21 <- update(GLM_LeaveProp20, . ~ . - Age_65plus)

# Perform ANOVA F‑tests after each term removal
cat("\np‑values of ANOVA F‑tests for each term removal:\n")
cat("------------------------------\n")
cat(
  "PrivateRent   : p = ",
  anova(GLM_LeaveProp17, GLM_LeaveProp16, test = "F")$`Pr(>F)`[2], "\n")
cat(
  "Age0_17       : p = ",
  anova(GLM_LeaveProp18, GLM_LeaveProp17, test = "F")$`Pr(>F)`[2], "\n")
cat(
  "OwnedOutright : p = ",
  anova(GLM_LeaveProp19, GLM_LeaveProp18, test = "F")$`Pr(>F)`[2], "\n")
cat(
  "log(Students) : p = ",
  anova(GLM_LeaveProp20, GLM_LeaveProp19, test = "F")$`Pr(>F)`[2], "\n")
cat(
  "Age_65plus    : p = ",
  anova(GLM_LeaveProp21, GLM_LeaveProp20, test = "F")$`Pr(>F)`[2], "\n")

# Show the summary output of our final selected model
cat("\nSummary of our final model (GLM_LeaveProp21):\n")
cat("------------------------------\n")
print(summary(GLM_LeaveProp21)) # Residual deviance: 27401

### Fig.8: Diagnostic plots of our final model
# Check the final model assumptions by looking at the diagnostic plots
par(mfrow=c(2,2),lwd=2,mar=c(3,3,2,2),mgp=c(2,0.75,0))
plot(GLM_LeaveProp21,which=1:4)
dev.copy(pdf, file = "Fig.8_Diagnostic plots of our final model.pdf", 
         width = 10, height = 8)
dev.off()



########################### Part 3: Prediction ###########################
##  Definitions (for the ith ward):
##  Y:           the actual value of voting ‘Leave’ (unknown)
##  p_hat:       the predicted probability of voting ‘Leave’
##  var_phat:    the variance of predicted probability p_hat
##  var_Y:       the estimated variance of Y
##  sigma_hat:   the estimated standard deviation of prediction error

# Select the 267 wards with missing voting data
PredData <- VotingData[is.na(VotingData$LeaveProp), ]

# Predict on the response scale using our final model: GLM_LeaveProp21
LeavingProp.pred <- predict(GLM_LeaveProp21,
                            newdata = PredData,
                            type = "response",
                            se.fit=TRUE)

# Extract the predicted probability
p_hat <- LeavingProp.pred$fit

# Check the range of predicted LeaveProp
cat("\nCheck the range of predicted LeaveProp:\n")
cat("------------------------------\n")
cat(sprintf("Minimum: %.3f\n", min(p_hat)))
cat(sprintf("Maximum: %.3f\n", max(p_hat)))

# Compute the variance of predicted probability by squaring the 
# standard error of p_hat
var_phat <- (LeavingProp.pred$se.fit)^2

# Estimate the variance of Y under the quasibinomial model using logit link
dispersion_final_glm <- summary(GLM_LeaveProp21)$dispersion
var_Y <- dispersion_final_glm * p_hat * (1 - p_hat) / PredData$NVotes

# Estimate the standard deviation of prediction error
sigma_hat <- sqrt(var_Y + var_phat) # Y and p_hat are independent,
                                    # variance of prediction error is thus 
                                    # equal to var_Y + var_phat

# Output the file as required
# The first column: the ward identifier
# The second column: the predicted proportion of ‘Leave’ votes for that ward
# The third column: the standard deviation of our prediction error
outfile <- paste0("ICA2_Group24_pred.dat")
out <- data.frame(ID = PredData$ID,
                  LeaveProp_Pred = p_hat,
                  PredError_sd = sigma_hat)

# Separate the columns by spaces and write with no header
write.table(out, file = outfile, sep = " ",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

cat("\nFinal step:\n")
cat("------------------------------\n")
cat("Predictions saved to", outfile, "\n")
