#########################################################
#             Installing/Loading Packages               #
#########################################################

if (!require("effsize")) {
  install.packages("effsize", dependencies = TRUE)
  require("effsize")
}
if (!require("oddsratio")) {
  install.packages("oddsratio", dependencies = TRUE)
  library("oddsratio")
}

setwd("/mnt/SSD-DATA/FAPESP-2018")

#########################################################
#                 Loading Dataset                       #
#########################################################

projects <- read.csv("spreadsheets/summary.csv", header=TRUE,
                     colClasses=c("name" = "character",
                                  "owner" = "character",
                                  "created_at" = "character",
                                  "github_url" = "character",
                                  "pulls_merged_total" = "numeric",
                                  "commits_total" = "numeric",
                                  "stars_total" = "numeric",
                                  "forks_total" = "numeric",
                                  "has_contributing" = "logical",
                                  "has_readme" = "logical",
                                  "used_languages_total" = "numeric",
                                  "open_issues_total" = "numeric",
                                  "age" = "numeric",
                                  "application_domain" = "character",
                                  "main_language" = "character",
                                  "owner_type" = "character",
                                  "software_license" = "character",
                                  "newcomers_total" = "numeric",
                                  "contributors_total" = "numeric",
                                  "core_members_total" = "numeric",
                                  "pulls_merged_code_churn" = "numeric",
                                  "time_for_first_review_median" = "numeric",
                                  "time_for_merge_median" = "numeric"))

#############################################################
#  TABLE: (SPEARMAN) Total Newcomers x Analytical Measures  #
#############################################################
# Total Newcomers x Stars
cor.test(x=projects$newcomers_total, y=projects$stars_total, method='spearman')
# Total Newcomers x Pull-requests
cor.test(x=projects$newcomers_total, y=projects$pulls_merged_total, method='spearman')
# Total Newcomers x Forks
cor.test(x=projects$newcomers_total, y=projects$forks_total, method='spearman')
# Total Newcomers x Commits
cor.test(x=projects$newcomers_total, y=projects$commits_total, method='spearman')
# Total Newcomers x Open issues
cor.test(x=projects$newcomers_total, y=projects$open_issues_total, method='spearman')
# Total Newcomers x Used languages
cor.test(x=projects$newcomers_total, y=projects$used_languages_total, method='spearman')

########################################################################################
#  TABLE: (Wilcox & Cliffs) Total Newcomers (Divided by median) x Analytical Measures  #
########################################################################################
newcomers.median <- median(projects$newcomers_total)
projects.under.median <- projects[which(projects$newcomers_total < newcomers.median),]
projects.above.median <- projects[which(projects$newcomers_total >= newcomers.median),]

wilcox.test(projects.above.median$commits_total, projects.under.median$commits_total)
cliff.delta(projects.above.median$commits_total, projects.under.median$commits_total)

wilcox.test(projects.above.median$forks_total, projects.under.median$forks_total)
cliff.delta(projects.above.median$forks_total, projects.under.median$forks_total)

wilcox.test(projects.above.median$stars_total, projects.under.median$stars_total)
cliff.delta(projects.above.median$stars_total, projects.under.median$stars_total)

wilcox.test(projects.above.median$open_issues_total, projects.under.median$open_issues_total)
cliff.delta(projects.above.median$open_issues_total, projects.under.median$open_issues_total)

wilcox.test(projects.above.median$pulls_merged_total, projects.under.median$pulls_merged_total)
cliff.delta(projects.above.median$pulls_merged_total, projects.under.median$pulls_merged_total)
