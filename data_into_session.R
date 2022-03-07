data_2020 <- load_data_adherence("2020", range = "A2:DN130",
                                 missing_cols = c("date_first_ambulance" = NA))
data_2018 <- load_data_adherence("2018", range = "A2:BU276",
                                 missing_cols = list(
                                   "lab_HbA1c" = NA_real_,
                                   "EF_first_contact" = NA_character_,
                                   "adherent.furosemide" = NA_integer_,
                                   "level.furosemide.furosemide" = "unavailable",
                                   "adherent.other_diuretics" = NA_integer_,
                                   "questionnaire_1" = NA_integer_,
                                   "questionnaire_2" = NA_integer_,
                                   "questionnaire_3" = NA_integer_,
                                   "questionnaire_4" = NA_integer_,
                                   "has.other_diuretics" = NA_integer_
                                 ) )

for(mc in setdiff(names(data_2020$wide), names(data_2018$wide))) {
  data_2018$wide[[mc]] <- NA
}


long_raw <- rbind(data_2018$long_raw, data_2020$long_raw)
long_for_checks <- rbind(data_2018$long_for_checks, data_2020$long_for_checks)
data_wide <- rbind(data_2018$wide, data_2020$wide)
data_long <- rbind(data_2018$long, data_2020$long)
