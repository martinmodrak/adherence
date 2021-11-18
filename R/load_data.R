column_map <- {
  all <- read.table(here::here("column_map.txt"), encoding = "UTF-8")
  res <- all$map
  names(res) <- rownames(all)
  res
}

other_diuretics <- c("indapamide", "amilorid", "HCTZ", "chlorthalidone")

main_targets <- c("ACEi", "ARNI", "sartan", "MRA", "BB")


nyha_from_excel_dates <- function(nyha_dates){
  d <- janitor::excel_numeric_to_date(suppressWarnings(as.numeric(nyha_dates)))
  paste0(lubridate::day(d), "-", lubridate::month(d))
}

fix_NYHA <- function(nyha_vals) {
  valid_vals <- c(as.character(1:4), paste0(1:3,"-",2:4))
  res <- case_when(is.na(nyha_vals) ~ NA_character_,
                   nyha_vals %in% valid_vals ~ nyha_vals,
                   nyha_vals == "1+" ~ "1-2",
                   nyha_vals == "2+" ~ "2-3",
                   nyha_vals == "3+" ~ "3-4",
                   startsWith(nyha_vals, "4") ~ nyha_from_excel_dates(nyha_vals),
                   TRUE ~ NA_character_
  )

  if(!all(is.na(nyha_vals) == is.na(res))) {
    print(nyha_vals[!is.na(nyha_vals) & is.na(res)])
    stop("NAs introduced")
  }

  invalid <- !is.na(res) & !(res %in% valid_vals)
  if(any(invalid)) {
    print(res[invalid])
    stop("Invalid vals NYHA")
  }
  res
}

EF_to_numeric <- function(EF) {
  res <- case_when(
    grepl("^[0-9]{2}-[0-9]{2}", EF) ~ as.numeric(gsub("-[0-9]{2}", "", EF)) + 2.5,
    grepl("^[0-9]{2}(\\.[0-9])?$", EF) ~ suppressWarnings(as.numeric(EF))
  )

  if(!all(is.na(EF) == is.na(res))) {
    print(EF[!is.na(EF) & is.na(res)])
    stop("NAs introduced")
  }

  res
}

fix_EF <- function(EF) {
  res <- case_when(
    grepl("^[0-9]{2}-[0-9]{2}", EF) ~ EF,
    grepl("^[0-9]{2}(\\.[0-9])?$", EF) ~ {
      intEF <-  suppressWarnings(as.integer(EF))
      base <- round(intEF / 5) * 5
      paste0(base,"-", base + 5)
    }
  )

  if(!all(is.na(EF) == is.na(res))) {
    print(EF[!is.na(EF) & is.na(res)])
    stop("NAs introduced")
  }

  invalid <- !is.na(res) & !grepl("^[0-9]{2}-[0-9]{2}", res)
  if(any(invalid)) {
    print(res[invalid])
    stop("Invalid vals EF")
  }
  res
}

load_data_adherence1 <- function() {
  data_raw <- read_excel(here::here("private_data", "Adherence LEVEL-CHF 2020.xlsx"), sheet = "Seznam pacient\u016f", range = "A2:DM130")

  data_wide <- data_raw %>% select(all_of(names(column_map)))
  names(data_wide) <- column_map[names(data_wide)]

  data_wide <- data_wide %>%
    mutate(across(starts_with("has."), as.integer),
           across(starts_with("adherent."), as.character),
           NYHA = fix_NYHA(NYHA),
           NYHA_spiro_subj = fix_NYHA(NYHA_spiro_subj),
           across(all_of(c("EF", "EF_first_amublance", "EF_first_contact")), EF_to_numeric, .names = "{.col}_numeric"),
           across(all_of(c("EF", "EF_first_amublance", "EF_first_contact")), fix_EF),

           sex = factor(sex, levels = c(0,1), labels = c("F", "M")),
           lab_GFR = as.numeric(if_else(lab_GFR == ">1,5", "1.6", lab_GFR)),
           VO2_max = as.numeric(if_else(VO2_max == "6,5 (technick\u00e1 chyba)", NA_character_, VO2_max)),
           questionnaire_4 = as.numeric(if_else(questionnaire_4 == "neodpov\u011bd\u011bl", NA_character_, questionnaire_4))
           )



  if(length(unique(data_wide$subject_id)) != nrow(data_wide)) {
    stop("Repeated subjects")
  }

  ###################################################################

  spec_level <- build_longer_spec(data_wide, cols = starts_with("level."), names_prefix = "level.",
                                  names_to = c("drug_class", "measured_product"), names_sep = "\\.", values_to = "level")

  spec_has <- build_longer_spec(data_wide, cols = starts_with("has."), names_prefix = "has.",
                                names_to = c("drug_class"), values_to = "has")


  spec_adherent <- build_longer_spec(data_wide, cols = starts_with("adherent."), names_prefix = "adherent.",
                                     names_to = c("drug_class"), values_to = "adherent")

  spec_dose <- build_longer_spec(data_wide, cols = starts_with("dose."), names_prefix = "dose.",
                                 names_to = c("drug_class"), values_to = "dose")


  drug_class_product <- spec_level %>% select(drug_class, measured_product)


  spec_joint <-
    rbind(spec_level,
          rbind(spec_has,
                spec_adherent,
                spec_dose)  %>% inner_join(drug_class_product, by = "drug_class")
    )

  ###################################################################

  data_long_raw <- data_wide  %>%
    select(-starts_with("which")) %>%
    mutate(across(c(starts_with("level."), starts_with("dose")), as.character)) %>%
    pivot_longer_spec(spec_joint)



  unrecognized_levels <- data_long_raw %>% select(subject_id, drug_class, level) %>%
    filter(!is.na(level), !grepl("^(<|>|>>)?[0-9]+([.,][0-9]*)?(E[+\\-][0-9]+)?$", level), !(level %in% c("nem\u011b\u0159ena", "neprok\u00e1z\u00e1n", "stopa")))

  if(nrow(unrecognized_levels) > 0) {
    print(unrecognized_levels)
    stop("Unrecognized levels")
  }


  inconsistent_long <-  data_long_raw %>% group_by(subject_id, drug_class) %>%
    filter(length(unique(has)) > 1 | length(unique(adherent)) > 1)

  if(nrow(inconsistent_long) > 0) {
    stop("Inconsistent long")
  }

  ###################################################################

  data_long_for_checks <- data_long_raw %>%
    mutate(adherent_manual = factor(adherent, levels = c("0","1","2","3","nev\u00edme"), labels = c("not_using", "yes", "no", "cannot_measure", "unknown")),
           adherent.other_diuretics_manual  = factor(adherent.other_diuretics, levels = c(0,1,2,3), labels = c("not_using", "yes", "no", "cannot_measure")),
           level_measured = !is.na(level) & level != "nem\u011b\u0159ena",
           detected = level_measured & !(grepl("^<", level) | level == "0" | level == "neprok\u00e1z\u00e1n"),
           is_other_diuretic = drug_class %in% other_diuretics) %>%
    group_by(subject_id, drug_class) %>%
    mutate(has = any(!is.na(level)),
           adherent = case_when(!unique(has) ~ "not_using",
                                any(detected) ~ "yes",
                                all(!level_measured) ~ "cannot_measure",
                                TRUE ~ "no"
           )
    ) %>%
    group_by(subject_id,is_other_diuretic) %>%
    mutate(
      adherent.other_diuretics = if_else(is_other_diuretic,
                                         case_when(
                                           any(adherent == "no") ~ "no",
                                           all(adherent == "not_using") ~ "not_using",
                                           any(adherent == "yes") ~ "yes",
                                           TRUE ~ "cannot_measure"
                                         ), NA_character_)
    ) %>%
    ungroup()


  ###################################################################
  inconsistent_has_diuretics <-  data_long_for_checks %>%
    filter(drug_class %in% other_diuretics) %>%
    group_by(subject_id) %>%
    filter(has.other_diuretics != any(!is.na(level)))

  if(nrow(inconsistent_has_diuretics) > 0) {
    stop("Inconsistent has diuretics")
  }


  adherent_diuretics_mismatch <- data_long_for_checks %>%
    filter(drug_class %in% other_diuretics) %>%
    filter(is.na(adherent.other_diuretics) | adherent.other_diuretics != adherent.other_diuretics_manual) %>%
    select(subject_id, adherent.other_diuretics, adherent.other_diuretics_manual) %>% distinct()

  if(nrow(adherent_diuretics_mismatch)) {
    print(adherent_diuretics_mismatch)
    stop("Mismatch")
  }

  data_long <- data_long_for_checks %>%
    mutate(
      drug_class = if_else(drug_class %in% other_diuretics, "other_diuretics", drug_class),
      drug_superclass = if_else(drug_class %in% main_targets, drug_class, "other"),
      adherent = if_else(drug_class == "other_diuretics", as.character(adherent.other_diuretics), adherent),
        adherent_int = case_when(adherent == "yes" ~ 1L,
                               adherent == "no" ~ 0L,
                               TRUE ~ NA_integer_),
      has = if_else(drug_class == "other_diuretics", as.logical(has.other_diuretics), has)
    ) %>%
    select(-has.other_diuretics, -adherent.other_diuretics, -adherent.other_diuretics_manual, -adherent_manual,
           -is_other_diuretic)

  inconsistent_long <-  data_long %>% group_by(subject_id, drug_class) %>%
    filter(length(unique(has)) > 1 | length(unique(adherent)) > 1)

  if(nrow(inconsistent_long) > 0) {
    stop("Inconsistent")
  }

  data_long <- data_long %>%
    select(-dose, -level, -measured_product, -level_measured, -detected) %>%
    distinct()


  if(nrow(data_long) != nrow(data_wide) * length(unique(data_long$drug_class))) {
    stop("Bad restructuring")
  }

  mismatch_has <- data_long %>% filter(has, !(adherent %in% c("yes", "no")))
  if(nrow(mismatch_has) > 0) {
    stop("Mismatch has")
  }


  list(
    raw = data_raw,
    wide = data_wide,
    long_raw = data_long_raw,
    long_for_checks = data_long_for_checks,
    long = data_long
  )
}

data_adherence1_to_long_raw <- function(data_wide) {

}

