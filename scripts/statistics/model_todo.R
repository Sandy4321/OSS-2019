# The class of gamma regression models is based on the assumption that the dependent
# variable is gamma distributed and that its mean is related to a set of regressors
# through a linear predictor with unknown coefficients and a link function. This link
# can be the identity, the inverse or the logarithm function. The model also includes
# a shape parameter, which may be constant or dependent on a set of regressors
# through a link function, as the logarithm function.

# The class of beta regression models is commonly used by practitioners to model variables
# that assume values in the standard unit interval (0, 1). It is based on the assumption
# that the dependent variable is beta-distributed and that its mean is related to a set of
# regressors through a linear predictor with unknown coefficients and a link function

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

if (!require("magrittr")) {
  install.packages("magrittr", dependencies = TRUE)
  require("magrittr")
}

if (!require("rms")) {
  install.packages("rms", dependencies = TRUE)
  library("rms")
}

#########################################################
#                 Loading Dataset                       #
#########################################################

setwd("/mnt/SSD-DATA/FAPESP-2018") # Working directory (Home)

projects <- read.csv("spreadsheets/summary.csv", header=TRUE, # IMPORTANT: Below you will find the column name, its type and description.
                     colClasses=c("name" = "character", # The name of the project
                                  "owner" = "character", # The owner of the project
                                  "url" = "character", # The url of the project
                                  "pulls_merged_total" = "numeric", # Total of pull-requestsç merged into the repository
                                  "commits_total" = "numeric", # Total of commits merged into the repository
                                  "stars_total" = "numeric", # Total of stars received in the project
                                  "forks_total" = "numeric", # Total of forks created in the project
                                  "has_contributing" = "logical", # Does the project have a contributing.md file? True for yes False for no
                                  "has_readme" = "logical", # Does the project have a readme.md file? True for yes False for no
                                  "used_languages_total" = "numeric", # Number of languages used in the repository
                                  "open_issues_total" = "numeric", # Number of issues that are still open (Based on the collection date, July 20 2018)
                                  "age" = "numeric", # Age (years)
                                  "domain" = "character", # Application domain that the project belongs to
                                  "main_language" = "character", # The most used language in the repository
                                  "owner_type" = "character", # Owner type used in the repository (User or Organization)
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

projects %<>%
  filter(newcomers_total != 0)

projects %<>% 
  mutate(agecat = case_when(
    age < 4 ~ "young",
    age > 3 & age < 8 ~ "adult",
    age > 7 ~ "old"))

#########################################################
#       High Correlation and Redundancy Analysis        #
#########################################################
explanatory_variables <- c("agecat", "main_language", "forks_total", "pulls_merged_total", "owner_type", "core_members_total")

# Variable Clustering Technique -- High Correlation Analysis
# References: https://www.jstatsoft.org/article/view/v050i13/v50i13.pdf
# Tutorial: https://rpubs.com/pjmurphy/269609

# Hierarchal Clustring Algorithm (Dendogram)
hierarchal_tree <- varclus(~ ., data=projects[,explanatory_variables], trans="abs")
plot(hierarchal_tree)
# The threshold for spearman correlation is 0.7
spearman_threshold <- 0.7
# Looking at the dendogram, you can figure out which variables have higher correlation (p > 0.7)
abline(h=1 - spearman_threshold, col = "red", lty = 2)


# Redundancy Detection
redundant_variables <- redun(~., data=projects[,explanatory_variables], nk=0)
# To detect redundant variables, we use redun function. Variables where R^2 is greater than 0.9 should be removed
print(redundant_variables)

#########################################################
#                   Regression Model                    #
#########################################################
projects %>% 
  ggplot(aes(x = newcomers_total)) +
  geom_histogram()

# How we define the categorization of licenses:
# http://www.teses.usp.br/teses/disponiveis/45/45134/tde-14032012-003454/pt-br.php

# License does not show a significant impact in projects attractivity
# The usage of projects domain reduces the significante of languages
projects %<>%
  mutate(licensecat = case_when(
    license == "MIT License" | license == "BSD 2-Clause \"Simplified\" License" | license == "Creative Commons Attribution 4.0" | license == "BSD 3-Clause \"New\" or \"Revised\" License" | license == "Apache License 2.0" | license == "Do What The F*ck You Want To Public License" | license == "ISC License" | license == "Artistic License 2.0" ~ "Permissive",
    license == "GNU General Public License v3.0" | license == "GNU General Public License v2.0" | license == "GNU Affero General Public License v3.0" ~ "TotalReciprocal",
    license == "Mozilla Public License 2.0" | license == "Eclipse Public License 1.0" | license == "GNU Lesser General Public License v2.1" | license == "GNU Lesser General Public License v2.1" | license == "The Unlicense" ~ "PartialReciprocal",
    license == "Other" ~ "Others"))

fit_project <- gamlss(newcomers_total ~ agecat + domain + licensecat + main_language + owner_type + domain*main_language, data = projects, family = GA())
# Rodar o stepwise sem as interações
# Rodar com as interações (sem stepwise). Se a interação for significativa (domain*main_language), eu mantenho as variáveis (inclusive as não significantes).
plot(fit_project)
summary(fit_project)

# Mostrar variações para Milene. Ao adicionar e remover variáveis os resultados mudam drasticamente.
# O modelo como está no momento é um dos mais estáveis que conseguimos.

# Calcular correlação das variáveis independentes com a dependente
# É necessário? Seria uma boa estratégia para remover variáveis altamente correlacionadas com a dependente?

# Usar o tamanho do software como variável independente (por loc) ***

# Adicionar readme e contributing como variáveis independentes

