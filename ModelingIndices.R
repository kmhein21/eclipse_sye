# Modelling the eclipse data 
# using GAMs 

# Packages
library(tidyverse)
library(splines)
library(here)
library(mgcv)
library(modelr)
library(hms)
# Loading in the full data set 
eclipse_start<-hms(00,00,14)
eclipse_end<-hms(00,50,16)

fullAudio<-rbind(A001_SD001, A002_SD013, A003_SD005, A004_SD012,A005_SD002, A006_SD006, A007_SD017, A008_SD007, A009_SD009, A010_SD014, A011_SD018, A013_SD016)|>
  group_by(folder_name)|>
  mutate(fullACI = sapply(ACI_all,sum))|>
  mutate(fullADI = sapply(ADI_all, sum))

# Creating an eclipse or not variable 

modelEclipse_df<-fullAudio|> mutate(eclipse = ifelse(day == "2024-04-08", 
                                                     "eclipse", 
                                                     "not_eclipse"))|>
  mutate(eclipse = as_factor(eclipse))|>
  mutate(hour_numeric = as.numeric(hour))|>
  filter(hour>= eclipse_start & hour<= eclipse_end &
           day %in% c("2024-04-06", "2024-04-07", "2024-04-08",
                    "2024-04-09", "2024-04-11"))|>
  select(hour, hour_numeric, everything())

modelBeforeEclipse_df<- fullAudio|> mutate(eclipse = ifelse(day == "2024-04-08", 
                                                            "eclipse", 
                                                            "not_eclipse"))|>
  mutate(eclipse = as_factor(eclipse))|>
  mutate(hour_numeric = as.numeric(hour))|>
  filter(hour < eclipse_start & day %in% c("2024-04-06", "2024-04-07", "2024-04-08",
                   "2024-04-09", "2024-04-11")) 

# Creating a similar model to mh_exploration 
# Spline on Time, Least squares for 'eclipse or not', and interaction between the two 

bei_mod<- mgcv::gam(bei~ s(hour_numeric, by = eclipse)+
                      eclipse 
                    , data = modelEclipse_df)

summary(bei_mod)

grid <- modelEclipse_df |> ungroup() |>
  data_grid(hour_numeric = seq_range(hour_numeric, n = 40),
    eclipse = modelEclipse_df |> pull(eclipse) |> levels()
  )

gam_aug <- broom::augment(bei_mod, newdata = grid)


# visual across the time of eclipse
ggplot(data = gam_aug, aes(x = hour_numeric, y = .fitted)) +
  geom_point(data = modelEclipse_df, aes(y = bei, colour = eclipse), alpha = 0.1) +
  geom_line(aes(colour = factor(eclipse)),
            linewidth = 2) +
  scale_colour_viridis_d() +
  theme_minimal() +
  labs(colour = "eclipse_or_not",
       y = "bei")

# x-axis: find some way to denote when totality is? 

# model and visual including time before eclipse

bei_mod2<- mgcv::gam(bei~ s(hour_numeric, by = eclipse)+
                      eclipse
                    , data = modelBeforeEclipse_df)
summary(bei_mod2)

grid <- modelBeforeEclipse_df |> ungroup() |>
  data_grid(hour_numeric = seq_range(hour_numeric, n = 40),
            eclipse = modelBeforeEclipse_df |> pull(eclipse) |> levels()
  )

gam_aug <- broom::augment(bei_mod2, newdata = grid)

ggplot(data = gam_aug, aes(x = hour_numeric, y = .fitted)) +
  geom_point(data = modelBeforeEclipse_df, aes(y = bei, colour = eclipse), alpha = 0.1) +
  geom_line(aes(colour = factor(eclipse)),
            linewidth = 2) +
  scale_colour_viridis_d() +
  theme_minimal() +
  labs(colour = "eclipse_or_not",
       y = "bei")








## Practice making other models 
# Potential model for AEI 

aei_mod<- mgcv::gam(aei~ s(hour_numeric, by = eclipse)+
                      eclipse 
                    , data = modelEclipse_df)
summary(aei_mod)

grid <- modelEclipse_df |> ungroup() |>
  data_grid(hour_numeric = seq_range(hour_numeric, n = 40),
            eclipse = modelEclipse_df |> pull(eclipse) |> levels()
  )

gam_aug <- broom::augment(aei_mod, newdata = grid)

ggplot(data = gam_aug, aes(x = hour_numeric, y = .fitted)) +
  geom_point(data = modelEclipse_df, aes(y = aei, colour = eclipse), alpha = 0.1) +
  geom_line(aes(colour = factor(eclipse)),
            linewidth = 2) +
  scale_colour_viridis_d() +
  theme_minimal() +
  labs(colour = "eclipse_or_not",
       y = "aei")

# Looking at time before
aei_mod2<- mgcv::gam(aei~ s(hour_numeric, by = eclipse)+
                       eclipse
                     , data = modelBeforeEclipse_df)
summary(aei_mod2)

grid <- modelBeforeEclipse_df |> ungroup() |>
  data_grid(hour_numeric = seq_range(hour_numeric, n = 40),
            eclipse = modelBeforeEclipse_df |> pull(eclipse) |> levels()
  )

gam_aug <- broom::augment(aei_mod2, newdata = grid)

ggplot(data = gam_aug, aes(x = hour_numeric, y = .fitted)) +
  geom_point(data = modelBeforeEclipse_df, aes(y = aei, colour = eclipse), alpha = 0.1) +
  geom_line(aes(colour = factor(eclipse)),
            linewidth = 2) +
  scale_colour_viridis_d() +
  theme_minimal() +
  labs(colour = "eclipse_or_not",
       y = "aei")

# Potential model for ADI 

ADI_mod<- mgcv::gam(fullADI~ s(hour_numeric, by = eclipse)+
                      eclipse 
                    , data = modelEclipse_df)
summary(ADI_mod)

grid <- modelEclipse_df |> ungroup() |>
  data_grid(hour_numeric = seq_range(hour_numeric, n = 40),
            eclipse = modelEclipse_df |> pull(eclipse) |> levels()
  )

gam_aug <- broom::augment(ADI_mod, newdata = grid)

ggplot(data = gam_aug, aes(x = hour_numeric, y = .fitted)) +
  geom_point(data = modelEclipse_df, aes(y = fullADI, colour = eclipse), alpha = 0.1) +
  geom_line(aes(colour = factor(eclipse)),
            linewidth = 2) +
  scale_colour_viridis_d() +
  theme_minimal() +
  labs(colour = "eclipse_or_not",
       y = "aei")

# Looking at time before
ADI_mod2<- mgcv::gam(fullADI~ s(hour_numeric, by = eclipse)+
                       eclipse
                     , data = modelBeforeEclipse_df)
summary(ADI_mod2)

grid <- modelBeforeEclipse_df |> ungroup() |>
  data_grid(hour_numeric = seq_range(hour_numeric, n = 40),
            eclipse = modelBeforeEclipse_df |> pull(eclipse) |> levels()
  )

gam_aug <- broom::augment(ADI_mod2, newdata = grid)

ggplot(data = gam_aug, aes(x = hour_numeric, y = .fitted)) +
  geom_point(data = modelBeforeEclipse_df, aes(y = fullADI, colour = eclipse), alpha = 0.1) +
  geom_line(aes(colour = factor(eclipse)),
            linewidth = 2) +
  scale_colour_viridis_d() +
  theme_minimal() +
  labs(colour = "eclipse_or_not",
       y = "aei")







