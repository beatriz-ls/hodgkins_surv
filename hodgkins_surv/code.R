# Libs

library(readxl) # load data
library(survival) # survival analysis
library(survminer) # graph plot

# Load data

dt <- read_excel("Lista2_Hodgkins.xlsx")

# Data preprocessing

dt$sex <- factor(dt$sex,
                 levels = c("0","1"),
                 labels = c("Feminino","Masculino"))

dt$stage <- factor(dt$stage,
                   levels = c("0","1"),
                   labels = c("inicial", "avançado"))

dt$hist <- factor(dt$hist,
                  levels = c(1,2,3),
                  labels = c("esclerose nodular", "misto celular",
                             "depleção linfótica"))

dt$age_cat <- ifelse(dt$age < 25, "Menos de 25",
                     ifelse(dt$age <= 37, "25-37",
                            ifelse(dt$age <= 52, "38-52", "53 ou mais")))

# Survival object

dt_surv <- Surv(time = dt$survivaltime, event = dt$dead)

# Kaplan-Meyer estimation

fit_sex <- survfit(Surv(time = survivaltime, event = dead) ~ sex,
                       data = dt)

fit_hist <- survfit(Surv(time = survivaltime, event = dead) ~ hist,
                       data = dt)

fit_age <- survfit(Surv(time = survivaltime, event = dead) ~ age_cat,
                               data = dt)


# Ploting graph with kapal meyer curves of sex

ggsurvplot(fit_sex, data = dt,
           pval = TRUE, conf.int = TRUE, conf.int.style = "step",
           ylab = "Sobrevida", xlab = "Tempo em dias", legend.title = "Sexo")
           #surv.median.line = "hv")

ggsurvplot(fit_hist, data = dt,
           pval = TRUE, conf.int = TRUE, conf.int.style = "step",
           ylab = "Sobrevida", xlab = "Tempo em dias",
           legend.title = "Histologia")
           #surv.median.line = "hv")

ggsurvplot(fit_age, data = dt,
           pval = TRUE, conf.int = TRUE, conf.int.style = "step",
           ylab = "Sobrevida", xlab = "Tempo em dias", legend.title = "Idade")
           #surv.median.line = "hv")

# Log rank test

# Sex curves

logrank_sex <- survdiff(Surv(time = survivaltime, event = dead) ~ sex,
                data = dt)

# Histology curves

logrank_hist <- survdiff(Surv(time = survivaltime, event = dead) ~ hist,
                        data = dt)

# age curves

logrank_age <- survdiff(Surv(time = survivaltime, event = dead) ~ age_cat,
                        data = dt)

# Wilcoxon test

# sex curves

wilcoxon_sex <- survdiff(Surv(time = survivaltime, event = dead) ~ sex, rho = 1,
                          data = dt)

# histology curves

wilcoxon_hist <- survdiff(Surv(time = survivaltime, event = dead) ~ hist, rho = 1,
                         data = dt)

# age curves

wilcoxon_age <- survdiff(Surv(time = survivaltime, event = dead) ~ age_cat, rho = 1,
                          data = dt)




