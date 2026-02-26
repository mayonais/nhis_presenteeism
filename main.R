library(dplyr)
library(broom)
library(haven)
library(MASS)
library(ggplot2)
library(hexbin)
library(car)
library(tidyr)

data <- read_dta("nhis_00006.dta") %>%
  rename_with(tolower) #convert to lowercase

#create dataset for modeling
data_model <- data %>%
  mutate(
    #outcome (valid 0–90; drop 96–99)
    work_while_sick = ifelse(wrksickday <= 90, wrksickday, NA_real_),
    work_missed_sick = ifelse(wldaysick <= 90, wldaysick, NA_real_),
    
    #skip meds (1=no, 2=yes)
    skip_meds = case_when(
      yskipmedyr == 2 ~ 1,
      yskipmedyr == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    #headache severity (keep 2–4 only)
    headache_severity = ifelse(painhead3m %in% 2:4, painhead3m, NA_real_),
    #level of pain, last time had pain
    pain_level = ifelse(painfeelevl %in% 1:3, painfeelevl, NA_real_),
    #sex (drop 7/8/9)
    sex_factor = factor(ifelse(sex %in% c(1,2), sex, NA))
  ) %>% 
  drop_na(work_while_sick, skip_meds, headache_severity, 
            pain_level, age, sex_factor, work_missed_sick)

#checking whether there's enough data
nrow(data_model)

---------------------------
#linear regression - outcome: number of sick days
lm_fit <- lm(log(work_while_sick + 1) ~ skip_meds + headache_severity + pain_level + age + sex_factor,
                  data = data_model, na.action = na.omit)

#residuals vs fitted should be scattered
#Q-Q should follow the line
#scale-location: some heteroscedasticity
#residuals vs leverage: vertical lines (many zeros influencing fit)
res <- residuals(lm_fit)
par(mfrow = c(2,2)) 
plot(lm_fit)
#many zeros and variance increases with mean (overdispersion)
hist(res, breaks = 20, main = "Residuals of LM", xlab = "Residual")

---------------------------
#poisson
pois_fit <- glm(work_while_sick ~ skip_meds + headache_severity + pain_level + age + sex_factor,
                data = data_model,
                family = poisson)

#compute Pearson residuals
pearson_resid <- residuals(pois_fit, type = "pearson")
#compute overdispersion ratio
dispersion_ratio <- sum(pearson_resid^2) / pois_fit$df.residual
dispersion_ratio #>1 = overdispension

---------------------------
  
#non binomial: explicitly models overdispersion
nb_fit <- glm.nb(work_while_sick ~ skip_meds + headache_severity +
                   pain_level + age + sex_factor + work_missed_sick,  
                   data = data_model)

exp(coef(nb_fit))
tidy(nb_fit) %>%
  mutate(IRR = exp(estimate),
         percent_change = (IRR - 1) * 100)

-------------------
AIC(nb_fit) 

# a vif of 1 = zero correlation between that variable and others in the model
vif(nb_fit)

# range of the IRR: including 1 = no effect
exp(confint(nb_fit))

# create a null model (intercept only)
nb_null <- MASS::glm.nb(work_while_sick ~ 1, data = data_model)

# compare them
anova(nb_null, nb_fit) #chi = 0 indicates statistical significance