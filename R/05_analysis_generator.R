#  -----------------------------------------------------------------------------
rm(list = ls())

# Load Packages ----------------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  readxl,
  writexl,
  openxlsx,
  randomcoloR,
  sf,
  anytime,
  gsheet,
  DT,
  cluster,
  survey,
  srvyr,
  knitr,
  tcltk
)

# Github IMPACT Packages
# install.packages("devtools")
# devtools::install_github("impact-initiatives/impactR4PHU")
# devtools::install_github("impact-initiatives/analysistools")
# devtools::install_github("impact-initiatives/presentresults")
# pak::pak("impact-initiatives-hppu/humind")
# pak::pak("gnoblet/impactR.utils")

library(impactR.utils)
library(impactR4PHU)
library(analysistools)
library(presentresults)
# library(humind) # nolint: commented_code_linter.

source("support_files/functions.R")
source("support_files/openxlsx_styles.R")
source("support_files/formated_output.R")
source("./analysis_generator_support_functions.R")
# Parameters/Options Values ----------------------------------------------------
options(scipen = 999)
options(dplyr.summarise.inform = FALSE)
dap <- "resources/iom_dap.xlsx"

sampling_frame <- read.xlsx("./resources/sampling_frame.xlsx")

# load kobo tool ---------------------------------------------------------------

cat("\n- LOADING Kobo tool ...\n")
tool_survey <- read_excel(
  "./resources/iom_tool.xlsx",
  sheet = "survey",
  col_types = "text"
) %>%
  rename_with(., tolower) %>%
  filter(!is.na(name))

tool_choices <- read_excel(
  "./resources/iom_tool.xlsx",
  sheet = "choices",
  col_types = "text"
) %>%
  rename_with(., tolower) %>%
  filter(!is.na(list_name)) %>%
  select(list_name, name, `label`) %>%
  distinct(label, .keep_all = T)

pick_option_labels <- read_excel(
  "./resources/iom_tool.xlsx",
  sheet = "choices",
  col_types = "text"
) %>%
  rename_with(., tolower) %>%
  filter(!is.na(name))

####################################################################################
### LOAD DATASET
####################################################################################
# -------------------------------------------------------
# uuid to match
uuid_to_match_main <- read.xlsx("output/analysis/uuid_to_keep_main.xlsx") %>%
  mutate(
    analysis_var_value = ifelse(
      is.na(analysis_var_value),
      paste0("var_", analysis_var),
      analysis_var_value
    )
  )

my_data_with_indicators_main <- openxlsx::read.xlsx(
  "./output/final/clean_data_final.xlsx"
)

only_nas <- my_data_with_indicators_main %>%
  summarise(across(.cols = everything(), .fns = function(x) {
    sum(is.na(x)) == nrow(my_data_with_indicators_main)
  })) %>%
  do.call(c, .)

my_data_main <- my_data_with_indicators_main[, !only_nas]

# view(data.frame(only_nas))

#-------------------------------------------------------------------------------
# uuid to match
uuid_to_match_koop <- read.xlsx("output/analysis/uuid_to_keep_loop.xlsx") %>%
  mutate(
    analysis_var_value = ifelse(
      is.na(analysis_var_value),
      paste0("var_", analysis_var),
      analysis_var_value
    )
  )

my_data_with_indicators_loop <- openxlsx::read.xlsx(
  "./output/final/roster/clean_data_final.xlsx"
)

only_nas <- my_data_with_indicators_loop %>%
  summarise(across(.cols = everything(), .fns = function(x) {
    sum(is.na(x)) == nrow(my_data_with_indicators_loop)
  })) %>%
  do.call(c, .)

my_data_loop <- my_data_with_indicators_loop[, !only_nas]

# view(data.frame(only_nas))

#----------------------------------------------------------------------
# generate weights
#----------------------------------------------------------------------

location_details <- my_data_main %>%
  mutate(
    sample_location = case_when(
      location %in%
        c("Juba IDP Camp 1", "Juba IDP Camp 3") ~ "Juba IDP Camp 1&3",
      TRUE ~ location
    )
  ) %>%
  group_by(sample_location) %>%
  summarise(count = n())

my_data_main <- my_data_main %>%
  mutate(
    sample_location = case_when(
      location %in%
        c("Juba IDP Camp 1", "Juba IDP Camp 3") ~ "Juba IDP Camp 1&3",
      TRUE ~ location
    )
  )

my_data_main <- add_weights(
  dataset = my_data_main,
  sample_data = sampling_frame,
  strata_column_dataset = "sample_location",
  strata_column_sample = "location",
  population_column = "target"
)

write.xlsx(my_data_main, "./output/final_cleaned_data_main.xlsx")

#######################################################################
### Load list of analysis (loa), add survey design, and create analysis table
#######################################################################

#----------------------------------------------------------------------
# main analysis
#----------------------------------------------------------------------
my_data_main <- my_data_main %>%
  mutate(
    aggregated_locations = case_when(
      location %in%
        c(
          "Juba IDP Camp 1&3",
          # "Juba IDP Camp 3",
          "Don Bosco",
          "Mangateen"
        ) ~ "formal idp sites",
      location %in% c("Gorom") ~ "refugee site",
      location %in% c("Mangalla") ~ "informal idp site",
      location %in%
        c("Kator", "Muniki", "Northern_Bari", "Luri") ~ "neighbourhoods",
      TRUE ~ NA_character_ # in case some locations are not listed
    )
  )

# weights <- my_data_main %>%
#   select(sample_location, weights) %>%
#   distinct(sample_location, .keep_all = T)

# write.xlsx(weights, "./resources/weights.xlsx")
# dap_main <- create_analysis_plan(
#   raw_dataset = my_data_main,
#   kobo_survey = tool_survey
# )
# write.xlsx(dap_main, "./resources/iom_dap.xlsx")
# pick group varaible
group_variable <- c(
  "Overall",
  "location",
  "aggregated_locations",
  "interviewee_status",
  "Gender"
)
# rm(uuid_to_match_main)
# bring_uuid <- read.xlsx("output/analysis/labels/All_New_Arrivals_main.xlsx", sheet = 2)
# uuid_to_match_main1 <- uuid_to_match_main %>%
#   filter(uuid %in% bring_uuid$uuid)
# write.xlsx(uuid_to_match_main1, "output/analysis/uuid_to_keep_main.xlsx")
rm(uuid_to_match_main)
# iterate through the group_variables one at a time
for (var in group_variable) {
  if (var == "Overall") {
    # create LOA
    my_loa <- read_excel(dap, sheet = "main") %>%
      # filter(analysis_type != "skip", dataset == "main") %>%
      mutate(group_var = ifelse(var == "Overall", group_var, var)) %>%
      filter(
        analysis_var != var,
        # analysis_var %in% c("hoh_age", "hh_size_cat")
      )

    my_loa <- my_loa %>%
      filter(analysis_var %in% colnames(my_data_main))

    my_design <- srvyr::as_survey_design(
      my_data_main,
      weights = "weights",
      strata = "sample_location"
    )

    my_results <- analysistools::create_analysis(
      my_design,
      loa = my_loa,
      sm_separator = "."
    )

    # save results table to a variable and add more column from LOA
    my_results_table <- my_results$results_table %>%
      left_join(my_loa)

    test <- presentresults::review_kobo_labels(
      kobo_survey_sheet = tool_survey,
      kobo_choices_sheet = tool_choices,
      label_column = "label",
      exclude_type = c(
        "begin_group",
        "end_group",
        "beging_repeat",
        "end_repeat",
        "note"
      ),
      results_table = my_results_table
    )

    dictionary <- create_label_dictionary(
      kobo_survey_sheet = tool_survey,
      kobo_choices_sheet = tool_choices,
      label_column = "label",
      analysis_type_dictionary = NULL,
      results_table = my_results_table
    )

    label_results <- add_label_columns_to_results_table(
      results_table = my_results_table,
      dictionary = dictionary
    )

    # use custom functions to add more columns from results table
    label_results_final <- label_results %>%
      my_create_table_variable_x_group(
        analysis_key = "label_analysis_key",
        value_columns = c("stat", "n", "n_total"),
        extra_columns = c("order", "number", "sector", "indicator", "subset")
      )
    # %>%
    #   mutate(across(
    #     starts_with("stat_"),
    #     ~ case_when(
    #       analysis_type %in% c("Mean", "Median") ~ as.character(round(., 2)),  # Round and convert to character
    #       TRUE ~ sprintf("%.1f%%", . * 100)  # Convert to percentage as character
    #     )
    #   ))

    label_results_final1 <- label_results_final %>%
      mutate(
        analysis_var_value = ifelse(
          analysis_var_value == "NA",
          paste0("var_", analysis_var),
          analysis_var_value
        )
      ) %>%
      left_join(select(
        uuid_to_match_main,
        uuid,
        analysis_var,
        analysis_var_value
      ))

    # use custom function to dynamically create the excel output
    my_create_xlsx_variable_x_group(
      label_results_final1,
      total_columns = c("n"),
      file_path = paste0("output/analysis/iom_main.xlsx"),
      # file_path = paste0("output/analysis/labels/", Sys.Date(), "_",
      #                    "main", "_UGA2407_New_Arrivals_main.xlsx"),
      # table_sheet_name = main,
      overwrite = T
    )
  } else {
    # create LOA
    my_loa <- read_excel(dap, sheet = "main") %>%
      # filter(analysis_type != "skip", dataset == "main") %>%
      mutate(group_var = ifelse(var == "Overall", group_var, var)) %>%
      filter(
        analysis_var != var,
        # analysis_var %in% c("hoh_age", "hh_size_cat")
      )

    my_loa <- my_loa %>%
      filter(analysis_var %in% colnames(my_data_main))

    my_design <- srvyr::as_survey_design(
      my_data_main,
      weights = "weights",
      strata = "sample_location"
    )

    my_results <- analysistools::create_analysis(
      my_design,
      loa = my_loa,
      sm_separator = "."
    )

    # save results table to a variable and add more column from LOA
    my_results_table <- my_results$results_table %>%
      left_join(my_loa)

    test <- presentresults::review_kobo_labels(
      kobo_survey_sheet = tool_survey,
      kobo_choices_sheet = tool_choices,
      label_column = "label",
      exclude_type = c(
        "begin_group",
        "end_group",
        "beging_repeat",
        "end_repeat",
        "note"
      ),
      results_table = my_results_table
    )

    dictionary <- create_label_dictionary(
      kobo_survey_sheet = tool_survey,
      kobo_choices_sheet = tool_choices,
      label_column = "label",
      analysis_type_dictionary = NULL,
      results_table = my_results_table
    )

    label_results <- add_label_columns_to_results_table(
      results_table = my_results_table,
      dictionary = dictionary
    )

    # use custom functions to add more columns from results table
    label_results_final <- label_results %>%
      my_create_table_variable_x_group(
        analysis_key = "label_analysis_key",
        value_columns = c("stat", "n", "n_total"),
        extra_columns = c("order", "number", "sector", "indicator", "subset")
      )
    # %>%
    #   mutate(across(
    #     starts_with("stat_"),
    #     ~ case_when(
    #       analysis_type %in% c("Mean", "Median") ~ as.character(round(., 2)),  # Round and convert to character
    #       TRUE ~ sprintf("%.1f%%", . * 100)  # Convert to percentage as character
    #     )
    #   ))

    label_results_final1 <- label_results_final %>%
      mutate(
        analysis_var_value = ifelse(
          analysis_var_value == "NA",
          paste0("var_", analysis_var),
          analysis_var_value
        )
      ) %>%
      left_join(select(
        uuid_to_match_main,
        uuid,
        analysis_var,
        analysis_var_value
      ))
    # %>%
    #   select(-c(sector, indicator, analysis_type, analysis_var, analysis_var_value))

    all_analysis <- read.xlsx(
      "output/analysis/iom_main.xlsx",
      sheet = 2
    )

    label_results_final_final <- all_analysis %>%
      left_join(label_results_final1) %>%
      relocate(uuid)

    # use custom function to dynamically create the excel output
    my_create_xlsx_variable_x_group(
      label_results_final_final,
      total_columns = c("n"),
      file_path = paste0("output/analysis/iom_main.xlsx"),
      # file_path = paste0("output/analysis/labels/", Sys.Date(), "_",
      #                    "main", "_UGA2407_New_Arrivals_main.xlsx"),
      # table_sheet_name = main,
      overwrite = T
    )
  }
}

# after the last labelling of results
label_results_final_final_original_output <- label_results_final_final %>%
  mutate(
    analysis_var_value = ifelse(
      grepl("^var_", analysis_var_value),
      "NA",
      analysis_var_value
    )
  ) %>%
  rename(
    question = analysis_var,
    option = analysis_var_value,
    stat_overall = "stat_NA",
    n_overall = "n_NA",
    N_total_overall = "n_total_NA"
  ) %>%
  filter(!(analysis_type != "Mean" & option == "NA"))

# rename labels
label_results_with_option <- apply_kobo_labels(
  dataset = label_results_final_final_original_output,
  column_name = "option",
  kobo_choices = tool_choices
)

# replace NA% with NA
label_results_with_option[label_results_with_option == "NA%"] <- "NA"
label_results_with_option[label_results_with_option == "NaN%"] <- "NA"
label_results_with_option[label_results_with_option == "NaN"] <- "NA"

label_results_with_option <- openxlsx::read.xlsx(
  "output/datasets/results_table_main.xlsx"
)
openxlsx::write.xlsx(
  label_results_with_option,
  "output/datasets/results_table_main.xlsx"
)

label_results_with_formats <- insert_empty_rows(
  label_results_with_option,
  "question"
)

format_my_xlsx_variable_x_group(
  label_results_with_formats,
  total_columns = c("n", "n_total"),
  file_path = paste0("output/analysis/IOM_main_default.xlsx"),
  # file_path = paste0("output/analysis/labels/", Sys.Date(), "_",
  #                    "main", "_UGA2407_New_Arrivals_main.xlsx"),
  table_sheet_name = "main",
  overwrite = T
)

# generate uuid column
# if (!"uuid" %in% names(label_results_final)) {
#   label_results_final_uuid <- label_results_final %>%
#     mutate(uuid = uuid::UUIDgenerate(n = nrow(label_results_final)))%>%
#     relocate(uuid)

# }
# #
# uuid_to_keep_main <- label_results_final_uuid %>%
#   select(c(uuid, analysis_type, analysis_var, analysis_var_value))

# write.xlsx(uuid_to_keep_main, "output/analysis/uuid_to_keep_main.xlsx")

# ------------------------------------------------------------------------------
# loop analysis
# ------------------------------------------------------------------------------
my_data_loop <- my_data_loop %>%
  left_join(
    select(
      my_data_main,
      uuid,
      aggregated_locations,
      Gender,
      weights,
      sample_location
    ),
    by = c("submission_uuid" = "uuid")
  )

my_data_loop <- my_data_loop %>%
  filter(!is.na(weights))

# write.xlsx(my_data_loop, "./output/final_cleaned_data_loop.xlsx")

dap_loop <- create_analysis_plan(
  raw_dataset = my_data_loop,
  kobo_survey = tool_survey
)

# write.xlsx(dap_loop, "./resources/dap_loop.xlsx")

# pick group varaible
group_variable_loop <- c(
  "Overall",
  "location",
  "aggregated_locations",
  "interviewee_status",
  "Gender"
)


# iterate through the group_variables one at a time
for (var in group_variable_loop) {
  if (var == "Overall") {
    my_loa_loop <- read_excel(dap, sheet = "loop") %>%
      # filter(analysis_type != "skip", dataset == "loop") %>%
      mutate(group_var = ifelse(var == "Overall", group_var, var)) %>%
      filter(analysis_var != var)

    # Identify columns in `my_data_loop` that need to be converted
    columns_to_convert <- my_loa_loop %>%
      filter((analysis_type == "mean") | (analysis_type == "median")) %>%
      pull(analysis_var)

    # Convert the identified columns to numeric in `my_data_loop`
    my_data_loop <- my_data_loop %>%
      mutate(across(all_of(columns_to_convert), as.numeric, .names = "{.col}"))

    my_design <- srvyr::as_survey_design(
      my_data_loop,
      weights = "weights",
      strata = "sample_location"
    )

    my_results <- analysistools::create_analysis(
      my_design,
      loa = my_loa_loop,
      sm_separator = "."
    )

    my_results_table <- my_results$results_table

    # save results table to a variable and add more column from LOA
    my_results_table <- my_results$results_table %>%
      left_join(my_loa_loop)

    test <- presentresults::review_kobo_labels(
      kobo_survey_sheet = tool_survey,
      kobo_choices_sheet = tool_choices,
      label_column = "label",
      exclude_type = c(
        "begin_group",
        "end_group",
        "beging_repeat",
        "end_repeat",
        "note"
      ),
      results_table = my_results_table
    )

    dictionary <- create_label_dictionary(
      kobo_survey_sheet = tool_survey,
      kobo_choices_sheet = tool_choices,
      label_column = "label",
      analysis_type_dictionary = NULL,
      results_table = my_results_table
    )

    label_results <- add_label_columns_to_results_table(
      results_table = my_results_table,
      dictionary = dictionary
    )

    # use custom functions to add more columns from results table
    label_results_final <- label_results %>%
      my_create_table_variable_x_group(
        analysis_key = "label_analysis_key",
        value_columns = c("stat", "n", "n_total"),
        extra_columns = c("number", "sector", "indicator", "subset")
      )

    label_results_final1 <- label_results_final %>%
      mutate(
        analysis_var_value = ifelse(
          analysis_var_value == "NA",
          paste0("var_", analysis_var),
          analysis_var_value
        )
      ) %>%
      left_join(select(
        uuid_to_match_koop,
        uuid,
        analysis_var,
        analysis_var_value
      ))

    # use custom function to dynamically create the excel output
    my_create_xlsx_variable_x_group(
      label_results_final1,
      total_columns = c("n"),
      file_path = paste0("output/analysis/iom_loop.xlsx"),
      # file_path = paste0("output/analysis/labels/", Sys.Date(), "_",
      #                    group_variable, "_UGA2407_New_Arrivals_loop.xlsx"),
      # table_sheet_name = group_variable,
      overwrite = T
    )
  } else {
    my_loa_loop <- read_excel(dap, sheet = "loop") %>%
      # filter(analysis_type != "skip", dataset == "loop") %>%
      mutate(group_var = ifelse(var == "Overall", group_var, var)) %>%
      filter(analysis_var != var)

    # Identify columns in `my_data_loop` that need to be converted
    columns_to_convert <- my_loa_loop %>%
      filter((analysis_type == "mean") | (analysis_type == "median")) %>%
      pull(analysis_var)

    # Convert the identified columns to numeric in `my_data_loop`
    my_data_loop <- my_data_loop %>%
      mutate(across(all_of(columns_to_convert), as.numeric, .names = "{.col}"))

    my_design <- srvyr::as_survey_design(
      my_data_loop,
      weights = "weights",
      strata = "sample_location"
    )

    my_results <- analysistools::create_analysis(
      my_design,
      loa = my_loa_loop,
      sm_separator = "."
    )

    my_results_table <- my_results$results_table

    # save results table to a variable and add more column from LOA
    my_results_table <- my_results$results_table %>%
      left_join(my_loa_loop)

    test <- presentresults::review_kobo_labels(
      kobo_survey_sheet = tool_survey,
      kobo_choices_sheet = tool_choices,
      label_column = "label",
      exclude_type = c(
        "begin_group",
        "end_group",
        "beging_repeat",
        "end_repeat",
        "note"
      ),
      results_table = my_results_table
    )

    dictionary <- create_label_dictionary(
      kobo_survey_sheet = tool_survey,
      kobo_choices_sheet = tool_choices,
      label_column = "label",
      analysis_type_dictionary = NULL,
      results_table = my_results_table
    )

    label_results <- add_label_columns_to_results_table(
      results_table = my_results_table,
      dictionary = dictionary
    )

    # use custom functions to add more columns from results table
    label_results_final <- label_results %>%
      my_create_table_variable_x_group(
        analysis_key = "label_analysis_key",
        value_columns = c("stat", "n", "n_total"),
        extra_columns = c("number", "sector", "indicator", "subset")
      )

    label_results_final1 <- label_results_final %>%
      mutate(
        analysis_var_value = ifelse(
          analysis_var_value == "NA",
          paste0("var_", analysis_var),
          analysis_var_value
        )
      ) %>%
      left_join(select(
        uuid_to_match_koop,
        uuid,
        analysis_var,
        analysis_var_value
      ))

    all_analysis <- read.xlsx(
      "output/analysis/iom_loop.xlsx",
      sheet = 2
    )

    label_results_final_final <- all_analysis %>%
      left_join(label_results_final1) %>%
      relocate(uuid)

    # use custom function to dynamically create the excel output
    my_create_xlsx_variable_x_group(
      label_results_final_final,
      total_columns = c("n"),
      file_path = paste0("output/analysis/iom_loop.xlsx"),
      # file_path = paste0("output/analysis/labels/", Sys.Date(), "_",
      #                    group_variable, "_UGA2407_New_Arrivals_loop.xlsx"),
      # table_sheet_name = group_variable,
      overwrite = T
    )
  }
}

# after the last labelling of results
label_results_final_final_original_output_loop <- label_results_final_final %>%
  mutate(
    analysis_var_value = ifelse(
      grepl("^var_", analysis_var_value),
      "NA",
      analysis_var_value
    )
  ) %>%
  rename(
    question = analysis_var,
    option = analysis_var_value,
    stat_overall = "stat_NA",
    n_overall = "n_NA",
    N_total_overall = "n_total_NA"
  ) %>%
  filter(!(analysis_type != "Mean" & option == "NA"))

# rename labels
label_results_with_option <- apply_kobo_labels(
  dataset = label_results_final_final_original_output_loop,
  column_name = "option",
  kobo_choices = tool_choices
)

# replace NA% with NA
label_results_with_option[label_results_with_option == "NA%"] <- "NA"
label_results_with_option[label_results_with_option == "NaN%"] <- "NA"
label_results_with_option[label_results_with_option == "NaN"] <- "NA"

# label_results_with_option <- openxlsx::read.xlsx("output/datasets/results_table_loop.xlsx")
openxlsx::write.xlsx(
  label_results_with_option,
  "output/datasets/results_table_loop.xlsx"
)

label_results_with_formats <- insert_empty_rows(
  label_results_with_option,
  "question"
)

format_my_xlsx_variable_x_group(
  label_results_with_formats,
  total_columns = c("n", "n_total"),
  file_path = paste0("output/analysis/IOM_loop_default.xlsx"),
  # file_path = paste0("output/analysis/labels/", Sys.Date(), "_",
  #                    "main", "_UGA2407_New_Arrivals_main.xlsx"),
  table_sheet_name = "loop",
  overwrite = T
)

# generate uuid column
# if (!"uuid" %in% names(label_results_final)) {
#   label_results_final_uuid <- label_results_final %>%
#     mutate(uuid = uuid::UUIDgenerate(n = nrow(label_results_final))) %>%
#     relocate(uuid)
# }

# uuid_to_keep_loop <- label_results_final_uuid %>%
#   select(c(uuid, analysis_type, analysis_var, analysis_var_value))

# write.xlsx(uuid_to_keep_loop, "output/analysis/uuid_to_keep_loop.xlsx")
