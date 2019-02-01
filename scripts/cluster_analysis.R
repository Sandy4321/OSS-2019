# Author: Felipe Fronchetti
# E-mail: fronchetti@usp.br

#########################################################
#                 Loading Dataset                       #
#########################################################

projects <- read.csv("projects-summary.csv", header=TRUE,
                     colClasses=c("name" = "character",
                                  "owner" = "character",
                                  "created_at" = "character",
                                  "github_url" = "character",
                                  "stars" = "numeric",
                                  "forks" = "numeric",
                                  "has_contributing" = "logical",
                                  "has_readme" = "logical",
                                  "has_wiki" = "logical",
                                  "has_code_of_conduct" = "logical",
                                  "has_pull_request_template" = "logical",
                                  "has_issue_template" = "logical",
                                  "has_license" = "logical",
                                  "languages" = "numeric",
                                  "age" = "numeric",
                                  "domain" = "character",
                                  "main_language" = "character",
                                  "owner_type" = "character",
                                  "newcomers" = "numeric",
                                  "contributors" = "numeric",
                                  "integrators" = "numeric",
                                  "time_for_merge" = "numeric"))

above_equal_median <- subset(projects, newcomers >= 100)
below_median <- subset(projects, newcomers >= 100)
