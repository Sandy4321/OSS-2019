# Author: Felipe Fronchetti
# Contact: fronchetti@usp.br

if (!require("rms")) {
  install.packages("rms", dependencies = TRUE)
  library("rms")
}

setwd("/mnt/SSD-DATA/FAPESP-2018")

#########################################################
#                 Loading Dataset                       #
#########################################################

projects <- read.csv("CSV/summary.csv", header=TRUE, # IMPORTANT: Below you will find the column name, its type and description.
                     colClasses=c("name" = "character", # The name of the project
                                  "owner" = "character", # The owner of the project
                                  "url" = "character", # The url of the project
                                  "pulls_merged_total" = "numeric", # Total of pull-requestsç merged into the repository
                                  "commits_total" = "numeric", # Total of commits merged into the repository
                                  "stars_total" = "numeric", # Total of stars received in the project
                                  "forks_total" = "numeric", # Total of forks created in the project
                                  "has_contributing" = "logical", # Does the project have a contributing.md file? True for yes False for no (STRATIFICATION FACTOR)
                                  "has_readme" = "logical", # Does the project have a readme.md file? True for yes False for no (STRATIFICATION FACTOR)
                                  "used_languages_total" = "numeric", # Number of languages used in the repository
                                  "open_issues_total" = "numeric", # Number of issues that are still open (Based on the collection date, July 20 2018)
                                  "age" = "numeric", # Age (years) (STRATIFICATION FACTOR)  
                                  "domain" = "character", # Application domain that the project belongs to (STRATIFICATION FACTOR)
                                  "main_language" = "character", # The most used language in the repository (STRATIFICATION FACTOR)
                                  "owner_type" = "character", # Owner type used in the repository (User or Organization) (STRATIFICATION FACTOR)
                                  "license" = "character", # License type used in the repository (GPL, Mozilla, etc)
                                  "newcomers_total" = "numeric", # Total of newcomers in the project
                                  "contributors_total" = "numeric", # Total of contributors in the project
                                  "core_members_total" = "numeric", # Total of core contributors in the project
                                  "lines_added_median" = "numeric", # Median of lines added in merged pull-requests
                                  "lines_deleted_median" = "numeric", # Median of lines deleted in merged pull-requests
                                  "lines_modified_median" = "numeric", # Median of lines modified in merged pull-requests
                                  "files_modified_median" = "numeric", # Median of files changed in merged pull-requests
                                  "time_for_merge_median" = "numeric", # Median of time between the create and merge date of merged pull-requests
                                  "time_for_first_review_median" = "numeric" # Median of time between the create and first review date of merged pull-requests
))


explanatory_variables <- c("age_1_2", "age_3_4", "age_5_6", "age_7_8", "age_9_10", "domain", "main_language", "owner_type", "license")

# We set is_attractive to TRUE if the number of newcomers is above the median. If not, it is equal FALSE.
projects = within(projects, {
  is_attractive = ifelse(projects$newcomers_total > median(projects$newcomers_total), TRUE, FALSE)
})

response_variable <- projects$is_attractive

dd <- datadist(projects[,c("is_attractive", explanatory_variables)])
options(datadist = "dd")

#########################################################
#           Correlation and Redundancy Analysis         #
#########################################################
# Variable Clustering Technique (VARCLUS) -- High Correlation Analysis
# References: https://www.jstatsoft.org/article/view/v050i13/v50i13.pdf
# Tutorial: https://rpubs.com/pjmurphy/269609

# Hierarchal Clustring Algorithm (Dendogram)
hierarchal_tree <- varclus(~ ., data=projects[,explanatory_variables], trans="abs")
plot(hierarchal_tree)
# The threshold for spearman correlation is 0.7
spearman_threshold <- 0.7
abline(h=1 - spearman_threshold, col = "red", lty = 2)
# Looking at the dendogram, you can figure out which variables have higher correlation. (> 0.7)

# Removing variables with high correlation
# Taking into consideration the onboarding of newcomers, we preferred to keep with pull-requests and forks than stars and commits.
rejected_explanatory_variables <- c()
explanatory_variables <- explanatory_variables[!(explanatory_variables %in% rejected_explanatory_variables)]

# Redundancy Detection -- Redundancy Analysis
redundant_variables <- redun(~., data=projects[,explanatory_variables], nk=0)
print(redundant_variables)
rejected_explanatory_variables <- redundant_variables$Out
explanatory_variables <- explanatory_variables[!(explanatory_variables %in% rejected_explanatory_variables)]
hierarchal_tree <- varclus(~ ., data=projects[,explanatory_variables], trans="abs")
plot(hierarchal_tree)
# To detect redundant variables, we use redun function. Models where R^2 is > 0.9 should be removed.

#########################################################
#               Degrees of Freedom Allocation           #
#########################################################
T <- nrow(projects[which(projects$is_attractive == TRUE),]) # Number of rows where project > median
F <- nrow(projects[which(projects$is_attractive == FALSE),]) # Number of rows where project <= median

# We set the budget for degrees of freedom using the formula below (Harrell Jr, 2002, p.60):
budget <- floor(min(T, F) / 15)

spearman_degrees <- spearman2(formula(paste("is_attractive" ," ~ ",paste0(explanatory_variables, collapse=" + "))), data=projects, p=2)
# Variables with larger ρ2 values are allocated with more degrees of freedom.
plot(spearman_degrees)

#########################################################
#               Logistic Regression Model               #
#########################################################

fit <- lrm(is_attractive ~ main_language + owner_type + license + domain + age_1_2 + age_3_4 + age_5_6 + age_7_8, data=projects, x=T, y=T)

#########################################################
#                   Model Analysis                      #
#########################################################
val <- validate(fit, B=1000)
AUC = 0.5 + val[1,1]/2
AUC_optimism_reduced = (0.5 + val[1,5]/2) 
AUC_optimism = AUC - AUC_optimism_reduced
print(c("AUC"=AUC,"AUC_optimism"=AUC_optimism))

explanatory_power = anova(fit,test='Chisq')
# If the resulting P for a variable is > 0.05 (Greater than 5%), then the variable might not have a huge impact in the logistic regression.
print(explanatory_power)

patial_effect = summary(fit)
print(patial_effect)

