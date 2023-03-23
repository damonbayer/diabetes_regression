library(tidyverse)
library(httr)
library(glue)
library(tidyjson)
hours_divisor <- 60 * 60 * 1000

# Edit This ---------------------------------------------------------------
email <- "your_tidepool_email"
password <- "your_tidepool_password"
current_ISF <- your_current_isf
current_CIR <- your_current_CIR

# Can input dates with ymd("YYYY-MM-DD")
end_date <- today()
start_date <- end_date - weeks(4)

# Pull Data ---------------------------------------------------------------

login_url <- "https://api.tidepool.org/auth/login"
# https://github.com/tidepool-org/data-science-tidepool-api-python/blob/master/data_science_tidepool_api_python/makedata/tidepool_api.py

api_date_format <- function(x) {
  x %>% 
    as_datetime %>% 
    format_ISO8601(usetz = "Z") %>% 
    I()
}

login_response <- POST(url = login_url, config = authenticate(user = email, password = password))
login_headers <- add_headers(
  c(`content-type` = login_response$headers$`content-type`,
    `x-tidepool-session-token` = login_response$headers$`x-tidepool-session-token`))

user_id <- content(login_response)$userid
user_data_url <- glue("https://api.tidepool.org/data/{user_id}")

tidepool_data <- GET(url = user_data_url %>%
             modify_url(
               query = list(
                 startDate = api_date_format(start_date),
                 endDate = api_date_format(end_date)
               )),
           config = login_headers)

# Process Data ------------------------------------------------------------
tidepool_data_tbl_sub <- 
  tidepool_data %>%
  content() %>% 
  spread_values(type = jstring(type),
                time = jstring(time),
                rate = jstring(rate),
                duration = jstring(duration),
                normal = jstring(normal),
                nutrition.carbohydrate.net = jstring(nutrition, carbohydrate, net)) %>% 
  as_tibble() %>% 
  filter(type %in% c("basal", "bolus", "food")) %>% 
  mutate(across(c(rate, duration, normal, nutrition.carbohydrate.net), as.numeric))

tidepool_data_daily_totals <- 
  tidepool_data_tbl_sub %>% 
  mutate(duration_hrs = duration / hours_divisor) %>% 
  mutate(basal_amount = rate * duration_hrs) %>% 
  group_by(date = date(time)) %>% 
  summarize(total_basal = sum(basal_amount, na.rm = T),
            total_bolus = sum(normal, na.rm = T),
            total_carbs = sum(nutrition.carbohydrate.net, na.rm = T)) %>% 
  mutate(total_insulin = total_basal + total_bolus)

tidepool_data_daily_totals

# Estimate Settings -------------------------------------------------------
tidepool_data_daily_totals %>% 
  ggplot(aes(total_carbs, total_insulin)) +
  geom_point() +
  xlim(c(0, NA)) + 
  stat_smooth(method = "lm", fullrange = T) +
  theme_minimal()

lm_fit <- lm(formula = total_insulin ~ total_carbs, data = tidepool_data_daily_totals)
current_CSF <- current_ISF / current_CIR

est_basal_rate <- unname(lm_fit$coefficients["(Intercept)"]) / 24
est_CIR <- unname(1 / lm_fit$coefficients["total_carbs"])
est_ISF <- insulin_sensitivity_factor <- est_CIR * current_CSF


# Output ------------------------------------------------------------------
glue("Current ISF: {current_ISF}
     Current CIR: {current_CIR}
     Current CSF: {current_CSF}
     
     Estimated basal rate: {round(est_basal_rate, 2)}
     Estimated CIR: {round(est_CIR, 1)}
     Estimated ISF: {round(est_ISF,1)}")
