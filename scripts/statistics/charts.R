#########################################################
#             Installing/Loading Packages               #
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

setwd("/mnt/SSD-DATA/oss-2019")

#########################################################
#                 Loading Dataset                       #
#########################################################
#########################################################
#                 Loading Dataset                       #
#########################################################

projects <- read.csv("spreadsheets/summary.csv", header=TRUE,
                     colClasses=c("name" = "character",
                                  "owner" = "character",
                                  "created_at" = "character",
                                  "github_url" = "character",
                                  "pulls_merged" = "numeric",
                                  "commits" = "numeric",
                                  "stars" = "numeric",
                                  "forks" = "numeric",
                                  "has_contributing" = "logical",
                                  "has_readme" = "logical",
                                  "has_wiki" = "logical",
                                  "has_code_of_conduct" = "logical",
                                  "has_pull_requests_template" = "logical",
                                  "has_issues_template" = "logical",
                                  "has_documentation_link" = "logical",
                                  "has_license" = "logical",
                                  "languages" = "numeric",
                                  "age" = "numeric",
                                  "domain" = "character",
                                  "main_language" = "character",
                                  "owner_type" = "character",
                                  "newcomers" = "numeric",
                                  "contributors" = "numeric",
                                  "core_contributors" = "numeric"))

DistributionBoxplot <- function(n.rows, n.columns, columns, labels) {
    par(mfrow=c(n.rows, n.columns))

    for (index in seq_len(ncol(columns))) {
        par(mar=c(2, 5, 2, 1) + 0.1)
        boxplot(columns[[index]], ylab=labels[[index]], outline=FALSE, cex.lab = 1.6, cex.axis = 1.8)
    }
}

CorrelationScatterplot <- function(column.x, column.y, label.x, label.y) {
  column.x.log10 <- log10(column.x)
  column.y.log10 <- log10(column.y)
  
  plot(column.x.log10, column.y.log10, xlab=label.x, ylab=label.y)
  abline(lm(column.x.log10 ~ column.y.log10), col="red")
}

3##############################################
#    FIGURE: Proportion (Projects by domain) #
##############################################
# Figure Size: 900 x 300
distribution <- subset(projects, select=c("domain"))
names = c("Documentation", "Application", "System", "Non-web libs/frameworks", "Web libs/frameworks", "Tools")

par(mar=c(2, 20, 0, 0) + 0.1, las=2)
table <- table(distribution)
barplot(sort(table), names.arg = names, cex.names=2, cex.axis= 2, horiz=TRUE, las=1)

##############################################
#    FIGURE: Proportion (Projects by age) #
##############################################
distribution <- subset(projects, select=c("age"))

par(mar=c(2, 5, 0, 0) + 0.1, las=2)
table <- table(distribution)
barplot(sort(table), cex.names=2, cex.axis= 2, horiz=TRUE, las=1, ylab="Years", cex.lab=2)

##############################################
#    FIGURE: Summary (Forks, Stars, Age) #
##############################################
distribution <- subset(projects, select=c("forks","stars","open_issues"))
labels <- list("# Forks", "# Stars", "# Open Issues")
DistributionBoxplot(1, 3, distribution, labels)

##########################################################
#    FIGURE: Summary (Merged pull-requests, Commits) #
##########################################################
distribution <- subset(projects, select=c("pulls_merged","commits"))
labels <- list("# Pull-requests", "# Commits")
DistributionBoxplot(1, 2, distribution, labels)


##################################
#    FIGURE: Correlation         #
##################################
# Correlation of the total of newcomers by project with a set of measures: 
# Total of stars, total of pull-requests merged, forks, commits, open issues and number of used languages.
# ~ Technical Warning ~ Since most measures were too disproportional, which could obscure the visual interpretation, 
# we normalized the values using log10.

# Correlation: Total of Newcomers x Stars
column.x <- projects[["newcomers"]]
column.y <- projects[["stars"]]
CorrelationScatterplot(column.x, column.y, "# Newcomers", "# Stars")

# Correlation: Total of Newcomers x Pull-requests
column.x <- projects[["newcomers"]]
column.y <- projects[["age"]]
CorrelationScatterplot(column.x, column.y, "# Newcomers", "# Pull-requests (Merged)")

# Correlation: Total of Newcomers x Forks
column.x <- projects[["newcomers"]]
column.y <- projects[["core_contributors"]]
CorrelationScatterplot(column.x, column.y, "# Newcomers", "# Forks")

# Correlation: Total of Newcomers x Commits
column.x <- projects[["newcomers"]]
column.y <- projects[["languages"]]
CorrelationScatterplot(column.x, column.y, "# Newcomers", "# Commits")

# Correlation: Total of Newcomers x Number of used languages
column.x <- projects[["newcomers"]]
column.y <- projects[["time_for_merge"]]
CorrelationScatterplot(column.x, column.y, "# Newcomers", "# Prog. Languages")

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