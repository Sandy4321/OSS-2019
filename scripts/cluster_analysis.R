# Author: Felipe Fronchetti
# E-mail: fronchetti@usp.br

#########################################################
#                 Loading Packages                      #
#########################################################

if (!require("dplyr")) {
  install.packages("dplyr", dependencies = TRUE)
  library("dplyr")
}

setwd("/mnt/SSD-DATA/oss-2019")

#########################################################
#                 Loading Dataset                       #
#########################################################

projects <- read.csv("spreadsheets/summary.csv", header=TRUE,
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

C0 <- subset(projects, cluster == 0)
C1 <- subset(projects, cluster == 1)
C2 <- subset(projects, cluster == 2)
