#########################################################
#     Installing and loading required packages          #
#########################################################
if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}
if (!require("ggplot2")) {
  install.packages("ggplot2", dependencies = TRUE)
  require("ggplot2")
}
if (!require("dplyr")) {
  install.packages("dplyr", dependencies = TRUE)
  library("dplyr")
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
                                  "pulls_merged_code_churn" = "numeric",
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
                                  "time_for_first_review_median" = "numeric",
                                  "time_for_merge_median" = "numeric"
                                  ))

boxplot_distribution <- function(n_row, n_column, columns, labels) {
    par(mfrow=c(n_row, n_column))

    for (column in seq_len(ncol(columns))) {
        par(mar=c(2, 5, 2, 1) + 0.1)
        boxplot(columns[,column], ylab=labels[[column]], outline=FALSE, cex.lab = 1.6, cex.axis = 1.8)
    }
}

##############################################
#    FIGURE: Summary (Forks, Stars, Age) #
##############################################
distribution <- subset(projects, select=c("forks_total","stars_total","open_issues_total"))
labels <- list("# Forks", "# Stars", "# Open Issues")
boxplot_distribution(1, 3, distribution, labels)

##########################################################
#    FIGURE: Summary (Merged pull-requests, Commits) #
##########################################################
distribution <- subset(projects, select=c("pulls_merged_total","commits_total"))
labels <- list("# Pull-requests", "# Commits")
boxplot_distribution(1, 2, distribution, labels)

##############################################
#    FIGURE: Proportion (Projects by domain) #
##############################################
distribution <- subset(projects, select=c("application_domain"))
barplot_distribution(distribution)

scatterplot_correlation <- function(column_x, column_y, label_x, label_y) {
  log10_column_x <- log10(column_x)
  log10_column_y <- log10(column_y)
  
  plot(log10_column_x, log10_column_y, xlab=label_x, ylab=label_y)
  #regression = lm(log10_column_y ~ log10_column_x)
  #abline(regression, untf=T, col="red")
}

barplot_distribution <- function(distribution) {
  par(mar=c(2, 2, 2, 2) + 0.1, las=2)
  table <- table(distribution)
  barplot(sort(table))
}

##################################
#    FIGURE: Correlation         #
##################################
# Correlation of the total of newcomers by project with a set of measures: 
# Total of stars, total of pull-requests merged, forks, commits, open issues and number of used languages.
# ~ Technical Warning ~ Since most measures were too disproportional, which could obscure the visual interpretation, 
# we normalized the values using log10.

# Correlation: Total of Newcomers x Stars
column_x <- projects[["newcomers_total"]]
column_y <- projects[["stars_total"]]
scatterplot_correlation(column_x, column_y, "# Newcomers", "# Stars")

# Correlation: Total of Newcomers x Pull-requests
column_x <- projects[["newcomers_total"]]
column_y <- projects[["pulls_merged_total"]]
scatterplot_correlation(column_x, column_y, "# Newcomers", "# Pull-requests (Merged)")

# Correlation: Total of Newcomers x Forks
column_x <- projects[["newcomers_total"]]
column_y <- projects[["forks_total"]]
scatterplot_correlation(column_x, column_y, "# Newcomers", "# Forks")

# Correlation: Total of Newcomers x Commits
column_x <- projects[["newcomers_total"]]
column_y <- projects[["commits_total"]]
scatterplot_correlation(column_x, column_y, "# Newcomers", "# Commits")

# Correlation: Total of Newcomers x Number of used languages
column_x <- projects[["newcomers_total"]]
column_y <- projects[["used_languages_total"]]
scatterplot_correlation(column_x, column_y, "# Newcomers", "# Prog. Languages")

# Correlation: Total of Newcomers x Open issues
column_x <- projects[["newcomers_total"]]
column_y <- projects[["open_issues_total"]]
scatterplot_correlation(column_x, column_y, "# Newcomers", "# Open issues")

#########################################################
#       CLUSTERS: DIVIDED BY STRATIFICATION FACTORS     #
#########################################################

# Age 
pdf("./Images/percentage_age.pdf", width=12, height=7)
percentage <- projects %>%  group_by(age, cluster) %>% summarise(count=n()) %>% mutate(percent = count/sum(count))
ggplot(percentage, aes(x = factor(age), y = percent * 100, fill = factor(cluster))) + geom_bar(stat="identity", width = 0.7, colour="black") + 
  labs(x = "Age", y = "Percent", fill = "Cluster") + coord_flip() + scale_fill_grey() + theme_classic()
dev.off()

# Domain 
pdf("./Images/percentage_domain.pdf", width=12, height=7)
percentage <- projects %>%  group_by(domain, cluster) %>% summarise(count=n()) %>% mutate(percent = count/sum(count))
ggplot(percentage, aes(x = factor(domain), y = percent * 100, fill = factor(cluster))) + geom_bar(stat="identity", width = 0.7, colour="black") + 
  labs(x = "Domain", y = "Percent", fill = "Cluster") + coord_flip() + scale_fill_grey() + theme_classic()
dev.off()

# Main Language 
pdf("./Images/percentage_main_language.pdf", width=12, height=7)
percentage <- projects %>%  group_by(main_language, cluster) %>% summarise(count=n()) %>% mutate(percent = count/sum(count))
ggplot(percentage, aes(x = factor(main_language), y = percent * 100, fill = factor(cluster))) + geom_bar(stat="identity", width = 0.7, colour="black") + 
  labs(x = "Language", y = "Percent", fill = "Cluster") + coord_flip() + scale_fill_grey() + theme_classic()
dev.off()

# Has README.md
pdf("./Images/percentage_readme.pdf", width=12, height=7)
percentage <- projects %>%  group_by(owner_type, cluster) %>% summarise(count=n()) %>% mutate(percent = count/sum(count))
ggplot(percentage, aes(x = factor(owner_type), y = percent * 100, fill = factor(cluster))) + geom_bar(stat="identity", width = 0.7, colour="black") + 
  labs(x = "Owner", y = "Percent", fill = "Cluster") + coord_flip() + scale_fill_grey() + theme_classic()
dev.off()

# Has CONTRIBUTING.md
pdf("./Images/percentage_contributing.pdf", width=12, height=7)
percentage <- projects %>%  group_by(has_contributing, cluster) %>% summarise(count=n()) %>% mutate(percent = count/sum(count))
ggplot(percentage, aes(x = factor(has_contributing), y = percent * 100, fill = factor(cluster))) + geom_bar(stat="identity", width = 0.7, colour="black") + 
  labs(x = "Owner", y = "Percent", fill = "Cluster") + coord_flip() + scale_fill_grey() + theme_classic()
dev.off()