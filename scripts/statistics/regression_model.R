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

projects %<>%
  filter(newcomers_total != 0)

projects %<>% 
  mutate(agecat = case_when(
    age < 4 ~ "young",
    age > 3 & age < 8 ~ "adult",
    age > 7 ~ "old"))

projects %<>%
  mutate(licensecat = case_when(
    software_license == "MIT License" | software_license == "BSD 2-Clause \"Simplified\" License" | software_license == "Creative Commons Attribution 4.0" | software_license == "Creative Commons Attribution 4.0 International" | software_license == "BSD 3-Clause \"New\" or \"Revised\" License" | software_license == "Apache License 2.0" | software_license == "Do What The F*ck You Want To Public License" | software_license == "ISC License" | software_license == "Artistic License 2.0" ~ "Permissive",
    software_license == "GNU General Public License v3.0" | software_license == "GNU General Public License v2.0" | software_license == "GNU Affero General Public License v3.0" ~ "TotalReciprocal",
    software_license == "Mozilla Public License 2.0" | software_license == "Eclipse Public License 1.0" | software_license == "GNU Lesser General Public License v2.1" | software_license == "GNU Lesser General Public License v2.1" | software_license == "The Unlicense" ~ "PartialReciprocal",
    software_license == "Other" | software_license == "NOASSERTION" ~ "Others"))

#########################################################
#       High Correlation and Redundancy Analysis        #
#########################################################

explanatory.variables <- c("agecat", "main_language", "forks_total", "pulls_merged_total", "owner_type", "core_members_total")
hierarchal.tree <- varclus(~ ., data=projects[,explanatory.variables], trans="abs")
# To detect high correlated variables, we use Spearman coefficient. Variables with high correlation (p > 0.7) can be removed.
spearman.threshold <- 0.7
plot(hierarchal.tree)
abline(h=1 - spearman.threshold, col="red", lty=2)

redundant.variables <- redun(~., data=projects[,explanatory.variables], nk=0)
# To detect redundant variables, we use the redun function. Redundant variables (R^2  > 0.9) can be removed.
print(redundant.variables)

#########################################################
#                   Regression Model                    #
#########################################################

fit_project <- gamlss(newcomers_total ~ agecat + application_domain + licensecat + main_language + owner_type, data = projects, family = GA())

plot(fit_project)
summary(fit_project)

# Mostrar variações para Milene. Ao adicionar e remover variáveis os resultados mudam drasticamente.
# O modelo como está no momento é um dos mais estáveis que conseguimos.

# Calcular correlação das variáveis independentes com a dependente
# É necessário? Seria uma boa estratégia para remover variáveis altamente correlacionadas com a dependente?

# Usar o tamanho do software como variável independente (por loc) ***

# Adicionar readme e contributing como variáveis independentes

#########################################################
#                      References                       #
#########################################################
# Licenses Categorization:
# http://www.teses.usp.br/teses/disponiveis/45/45134/tde-14032012-003454/pt-br.php (Paper)

# Clustering Variables (Varclus):
# https://rpubs.com/pjmurphy/269609 (Tutorial)
# https://www.jstatsoft.org/article/view/v050i13/v50i13.pdf (Paper)
