library(tidyverse)
library(gtsummary)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


# Customization of `tbl_summary()`

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir))


#Change the label names
tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing")


#Add bolding, p value, footnotes

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**")


###########################################################################################################
#In Class Example:
#Make a tbl_summary(). Include categorical region, race/ethnicity, income, and the sleep variables
#(use a helper function to select those) and make sure they are nicely labeled.
#Stratify the table by sex. Add a p-value comparing the sexes and an overall column combining both sexes.
#For the income variable, show the 10th and 90th percentiles of income with 3 digits,
#For the sleep variables, show the min and the max with 1 digit

tbl_summary(
	nlsy,
	by = sex_cat,

	include = c(region_cat, race_eth_cat, income, starts_with("sleep")),

	label = list(
		region_cat ~ "Region",
		race_eth_cat ~ "Race/Ethnicity",
		income ~ "Income",
		sleep_wkdy ~ "Weekday Hours of Sleep",
		sleep_wknd ~ "Weekend Hours of Sleep"
	),
	statistic = list(income ~ "{p10} - {p90}",
									 starts_with("sleep") ~ "min = {min}; max= {max}"),
	digits = list(income ~ c(3,3),
								starts_with("sleep") ~ c(1,1))) %>%
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test"))|>
	add_overall(col_label = "**Total**") %>%

	modify_table_styling(columns = label,
											 rows = label == "Race/ethnicity",
											 footnote = "See https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data")


###########################################################################################################
#Regression tables
ibrary(tidyverse)
library(gtsummary)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


# Univariate regression

tbl_uvregression(
	nlsy,
	y = income,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, income, age_bir),
	method = lm)


tbl_uvregression(
	nlsy,
	y = glasses,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	method = glm,
	method.args = list(family = binomial()),
	exponentiate = TRUE)


## Multivariable regressions

## Some regressions

linear_model <- lm(income ~ sex_cat + age_bir + race_eth_cat,
									 data = nlsy)


linear_model_int <- lm(income ~ sex_cat*age_bir + race_eth_cat,
											 data = nlsy)


logistic_model <- glm(glasses ~ eyesight_cat + sex_cat + income,
											data = nlsy, family = binomial())


## Tables

tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))


tbl_regression(
	logistic_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight",
		income ~ "Income"
	))


tbl_no_int <- tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))

tbl_int <- tbl_regression(
	linear_model_int,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth",
		`sex_cat:age_bir` ~ "Sex/age interaction"
	))

## Table comparing the models with and without interaction

tbl_merge(list(tbl_no_int, tbl_int),
					tab_spanner = c("**Model 1**", "**Model 2**"))

###############################################################################################
#In Class Examples


#3. Create a univariate regression table looking at the association between sex (sex_cat) as the x = variable and each of nsibs, sleep_wkdy, and sleep_wknd, and income.


tbl_uvregression(
	nlsy,
	x = sex_cat,
	include =c(nsibs, sleep_wkdy,
						 sleep_wknd, income),
	method = lm)

#4. Fit a Poisson regression (family = poisson()) for the number of siblings, using at least 3 predictors of your choice. Create a nice table displaying your Poisson regression and its exponentiated coefficients.


poisson_model <- glm(nsibs ~ eyesight_cat + sex_cat + income,
											data = nlsy, family = poisson())


tbl_regression(
	poisson_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight",
		income ~ "Income"
	))


#5. Instead of odds ratios for wearing glasses, as in the example, we want risk ratios. We can do this by specifying in the regression family = binomial(link = "log").
# Regress glasses on eyesight_cat sex_cat and create a table showing the risk ratios and confidence intervals from this regression.
#6. Since family = binomial(link = "log") often doesn’t converge, we often use Poisson regression with robust standard errors to estimate risk ratios. Fit a Poisson regression instead
#of the log-binomial regression in the last question. Then create a table using tidy_fun = partial(tidy_robust, vcov = "HC1"). It will prompt you to install new package(s) (yes!). See this page for more on custom tidiers.


eyesbinomial <- glm(glasses ~ eyesight_cat + sex_cat,
										 data = nlsy, family = binomial(link=log))

eyespoisson <- glm(glasses ~ eyesight_cat + sex_cat,
										data = nlsy, family = poisson())

tbl1_eyes_binomial<-tbl_regression(
	eyesbinomial,
	exponentiate = TRUE	)

tbl1_eyes_poisson<-tbl_regression(
	eyespoisson,
	exponentiate = TRUE,
	tidy_fun = partial(tidy_robust, vcov = "HC1"))



#7. Make a table comparing the log-binomial and the log-Poisson results.


tbl_merge(list(tbl1_eyes_binomial, tbl1_eyes_poisson),
					tab_spanner = c("**Log Binomial**", "**Poisson**"))

###########################################################################################
#Finer Control over Statistics with Broom package
install.packages("tidycat")
library(broom)
library(tidycat)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")

nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")))

mod_sex_cat <- lm(income ~ sex_cat, data = nlsy)
mod_race_eth_cat <- lm(income ~ race_eth_cat, data = nlsy)
mod_eyesight_cat <- lm(income ~ eyesight_cat, data = nlsy)
mod_age_bir <- lm(income ~ age_bir, data = nlsy)

tidy_sex_cat <- tidy(mod_sex_cat, conf.int = TRUE)
tidy_race_eth_cat <- tidy(mod_race_eth_cat, conf.int = TRUE)
tidy_eyesight_cat <- tidy(mod_eyesight_cat, conf.int = TRUE)
tidy_age_bir <- tidy(mod_age_bir, conf.int = TRUE)

bind_rows(
	sex_cat = tidy_sex_cat,
	race_eth_cat = tidy_race_eth_cat,
	eyesight_cat = tidy_eyesight_cat,
	age_bir = tidy_age_bir, .id = "model") |>
	mutate(
		term = str_remove(term, model),
		term = ifelse(term == "", model, term))


logistic_model <- glm(glasses ~ eyesight_cat + sex_cat + income,
											data = nlsy, family = binomial())

tidy(logistic_model, conf.int = TRUE, exponentiate = TRUE) |>
	tidycat::tidy_categorical(logistic_model, exponentiate = TRUE) |>
	select(-c(3:5))

tidy(logistic_model, conf.int = TRUE, exponentiate = TRUE) |>
	tidycat::tidy_categorical(logistic_model, exponentiate = TRUE) |>
	slice(-1) |> # remove intercept
	ggplot(mapping = aes(x = level, y = estimate,
											 ymin = conf.low, ymax = conf.high)) +
	geom_point() +
	geom_errorbar() +
	facet_grid(cols = vars(variable), scales = "free", space = "free") +
	scale_y_log10()
