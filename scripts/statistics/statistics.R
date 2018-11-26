if (!require("effsize")) {
  install.packages("effsize", dependencies = TRUE)
  require("effsize")
}
if (!require("oddsratio")) {
  install.packages("oddsratio", dependencies = TRUE)
  library("oddsratio")
}

setwd("/mnt/SSD-DATA/FAPESP-2018") # Working directory (Home)

#########################################################
#                 Loading Dataset                       #
#########################################################

projects <- read.csv("Spreadsheets/summary.csv", header=TRUE, # IMPORTANT: Below you will find the column name, its type and description.
                     colClasses=c("name" = "character", # The name of the project
                                  "owner" = "character", # The owner of the project
                                  "created_at" = "character", # The date when the project was created
                                  "github_url" = "character", # The url of the project
                                  "pulls_merged_total" = "numeric", # Total of pull-requests merged into the repository
                                  "commits_total" = "numeric", # Total of commits merged into the repository
                                  "stars_total" = "numeric", # Total of stars received in the project
                                  "forks_total" = "numeric", # Total of forks created in the project
                                  "has_contributing" = "logical", # Does the project have a contributing.md file? True for yes False for no (STRATIFICATION FACTOR)
                                  "has_readme" = "logical", # Does the project have a readme.md file? True for yes False for no (STRATIFICATION FACTOR)
                                  "used_languages_total" = "numeric", # Number of languages used in the repository
                                  "open_issues_total" = "numeric", # Number of issues that are still open (Based on the collection date, July 20 2018)
                                  "age" = "numeric", # Age (years) (STRATIFICATION FACTOR)  
                                  "application_domain" = "character", # Application domain that the project belongs to (STRATIFICATION FACTOR)
                                  "main_language" = "character", # The most used language in the repository (STRATIFICATION FACTOR)
                                  "owner_type" = "character", # Owner type used in the repository (User or Organization) (STRATIFICATION FACTOR)
                                  "software_license" = "character", # License type used in the repository (GPL, Mozilla, etc)
                                  "newcomers_total" = "numeric", # Total of newcomers in the project
                                  "contributors_total" = "numeric", # Total of contributors in the project
                                  "core_members_total" = "numeric", # Total of core contributors in the project
                                  "pulls_merged_code_churn" = "numeric",
                                  "time_for_first_review_median" = "numeric", # Median of time between the create and first review date of merged pull-requests
                                  "time_for_merge_median" = "numeric")) # Median of time between the create and merge date of merged pull-requests

#########################################################################
#  TABLE: (SPEARMAN CORRELATION) Total Newcomers x Analytical Measures  #
#########################################################################
# Total Newcomers x Total Stars
cor.test(x=projects$newcomers_total, y=projects$stars_total, method = 'spearman')
# Total Newcomers x Pull-requests
cor.test(x=projects$newcomers_total, y=projects$pulls_merged_total, method = 'spearman')
# Total Newcomers x Forks
cor.test(x=projects$newcomers_total, y=projects$forks_total, method = 'spearman')
# Total Newcomers x Total Commits
cor.test(x=projects$newcomers_total, y=projects$commits_total, method = 'spearman')
# Total Newcomers x Total Open issues
cor.test(x=projects$newcomers_total, y=projects$open_issues_total, method = 'spearman')
# Total Newcomers x Used languages
cor.test(x=projects$newcomers_total, y=projects$used_languages_total, method = 'spearman')

########################################################################################
#  TABLE: (Wilcox & Cliffs) Total Newcomers (Divided by median) x Analytical Measures  #
########################################################################################
median_of_newcomers <- median(projects$newcomer_total)
projects_under_median <- projects[which(projects$newcomer_total < median_of_newcomers),]
projects_above_median <- projects[which(projects$newcomer_total >= median_of_newcomers),]

# Analytical measures: Total of commits, forks, stars, open issues, used languages, pull-requests (merged)

wilcox.test(projects_above_median$commit_total, projects_under_median$commit_total)
cliff.delta(projects_above_median$commit_total, projects_under_median$commit_total)
oddsratio(projects_above_median$commit_total, projects_under_median$commit_total)

wilcox.test(projects_above_median$fork_total, projects_under_median$fork_total)
cliff.delta(projects_above_median$fork_total, projects_under_median$fork_total)

wilcox.test(projects_above_median$star_total, projects_under_median$star_total)
cliff.delta(projects_above_median$star_total, projects_under_median$star_total)

wilcox.test(projects_above_median$fork_total, projects_under_median$number_of_open_issues)
cliff.delta(projects_above_median$fork_total, projects_under_median$number_of_open_issues)

wilcox.test(projects_above_median$pull_merged_total, projects_under_median$pull_merged_total)
cliff.delta(projects_above_median$pull_merged_total, projects_under_median$pull_merged_total)
