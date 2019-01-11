#########################################################
#     Installing and loading required packages          #
#########################################################

if (!require("gamlss")) {
  install.packages("gamlss", dependencies = TRUE)
  library(gamlss)
}
if (!require("tidyverse")) {
  install.packages("tidyverse", dependencies = TRUE)
  library(tidyverse)
}
if (!require("dummies")) {
  install.packages("dummies", dependencies = TRUE)
  library(dummies)
}
if (!require("apcluster")) {
  install.packages("apcluster", dependencies = TRUE)
  library(apcluster)
}
if (!require("magrittr")) {
  install.packages("magrittr", dependencies = TRUE)
  require("magrittr")
}
if (!require("rms")) {
  install.packages("rms", dependencies = TRUE)
  library("rms")
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
                                  "pulls_merged" = "numeric",
                                  "commits" = "numeric",
                                  "stars" = "numeric",
                                  "forks" = "numeric",
                                  "has_contributing" = "logical",
                                  "has_readme" = "logical",
                                  "languages" = "numeric",
                                  "age" = "numeric",
                                  "domain" = "character",
                                  "main_language" = "character",
                                  "owner_type" = "character",
                                  "license" = "character",
                                  "newcomers" = "numeric",
                                  "contributors" = "numeric",
                                  "core_contributors" = "numeric",
                                  "cluster" = "numeric"))

projects %<>%
  filter(newcomers != 0)

projects %<>% 
  mutate(age = case_when(
    age < 4 ~ "Young",
    age > 3 & age < 8 ~ "Adult",
    age > 7 ~ "Old"))

projects %<>%
  mutate(license = case_when(
    license == "MIT License" | license == "BSD 2-Clause \"Simplified\" License" | license == "Creative Commons Attribution 4.0" | license == "Creative Commons Attribution 4.0 International" | license == "BSD 3-Clause \"New\" or \"Revised\" License" | license == "Apache License 2.0" | license == "Do What The F*ck You Want To Public License" | license == "ISC License" | license == "Artistic License 2.0" ~ "Permissive",
    license == "GNU General Public License v3.0" | license == "GNU General Public License v2.0" | license == "GNU Affero General Public License v3.0" ~ "TotalReciprocal",
    license == "Mozilla Public License 2.0" | license == "Eclipse Public License 1.0" | license == "GNU Lesser General Public License v2.1" | license == "GNU Lesser General Public License v2.1" | license == "The Unlicense" ~ "PartialReciprocal",
    license == "Other" | license == "NOASSERTION" ~ "Others"))

#########################################################
#       High Correlation and Redundancy Analysis        #
#########################################################

independent.variables <- projects[,c("age", "languages", "forks", "stars", "core_contributors", "owner_type", "license", "domain", "has_readme", "has_contributing")]

hierarchal.tree <- varclus(~., data=independent.variables)
spearman.threshold <- 0.7
plot(hierarchal.tree)
abline(h=1 - spearman.threshold, col="red", lty=2)

redundant.variables <- redun(~., data=independent.variables, nk=0)
print(redundant.variables) # Redundant variables (R^2  > 0.9) can be removed.

#########################################################
#                   Regression Model                    #
#########################################################
# The class of gamma regression models is based on the assumption that the dependent
# variable is gamma distributed and that its mean is related to a set of regressors
# through a linear predictor with unknown coefficients and a link function. This link
# can be the identity, the inverse or the logarithm function. The model also includes
# a shape parameter, which may be constant or dependent on a set of regressors
# through a link function, as the logarithm function.

projects = na.omit(projects)
fit_project <- gamlss(newcomers ~ age + languages + forks + stars + core_contributors + owner_type + license + domain + has_readme + has_contributing, data = projects, family = GA())
predict(fit_project)
plot(fit_project)
summary(fit_project)

# Usar o tamanho do software como variável independente (por loc) ***

#########################################################
#                      References                       #
#########################################################
# Licenses Categorization:
# http://www.teses.usp.br/teses/disponiveis/45/45134/tde-14032012-003454/pt-br.php (Paper)

# Clustering Variables (Varclus):
# https://rpubs.com/pjmurphy/269609 (Tutorial)
# https://www.jstatsoft.org/article/view/v050i13/v50i13.pdf (Paper)
