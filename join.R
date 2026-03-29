library(readr)
library(dplyr)
library(stringr)
library(stringdist) # For fuzzy matching

# 1. Load data
wqs <- read.csv("C:/Desktop/drive-download-20260223T010016Z-3-001/wqs.csv")
npdes <- read.csv("C:/Desktop/drive-download-20260223T010016Z-3-001/npdes.csv")
dmr <- read.csv("C:/Desktop/drive-download-20260223T010016Z-3-001/dmr.csv")

# 2. Advanced Cleaning Functions
clean_cas_id <- function(x) {
  x <- gsub("-", "", as.character(x))
  x <- gsub("^0+", "", trimws(x))
  ifelse(x == "NA" | x == "" | is.na(x), NA, x)
}

clean_pollutant_names <- function(x) {
  x <- tolower(x)
  # Remove common suffixes that prevent matching
  x <- gsub(", total recoverable", "", x)
  x <- gsub(", total", "", x)
  x <- gsub(" \\(as n\\)", "", x)
  x <- gsub(" \\(as caco3\\)", "", x)
  
  # Grouping aliases (Regex)
  x <- ifelse(grepl("biochemical oxygen demand|bod", x), "bod", x)
  x <- ifelse(grepl("total suspended solids|tss", x), "tss", x)
  x <- ifelse(grepl("ph ", x) | x == "ph", "ph", x)
  x <- ifelse(grepl("temperature", x), "temperature", x)
  x <- ifelse(grepl("nitrate/nitrite|nitrates", x), "nitrates", x)
  x <- ifelse(grepl("chlorine", x), "chlorine", x)
  
  # Remove all non-alphanumeric characters for final bridge
  x <- str_replace_all(x, "[^a-z0-9]", "")
  return(trimws(x))
}

# 3. Apply Cleaning
npdes_proc <- npdes %>%
  mutate(join_cas = clean_cas_id(CAS),
         join_name = clean_pollutant_names(Pollutant))

wqs_proc <- wqs %>%
  mutate(join_cas = clean_cas_id(WQS_CAS_NO),
         join_name = clean_pollutant_names(WQS_POLLUTANT_NAME))

dmr_proc <- dmr %>%
  mutate(join_cas = clean_cas_id(CHEMICAL_ABSTRACT_SERVICE_NMBR),
         join_name = clean_pollutant_names(POLLUTANT_DESC))

# 4. The Tiered Join Strategy
# We use a full_join to ensure we don't lose any data for the reviewers

# Step A: Match NPDES to WQS
joined_wqs <- npdes_proc %>%
  left_join(wqs_proc, by = "join_cas", suffix = c("", ".wqs")) %>%
  # Fallback: If CAS didn't match, try Name
  mutate(WQS_NAME_MATCH = ifelse(is.na(WQS_POLLUTANT_NAME), 
                                 match(join_name, wqs_proc$join_name), NA)) %>%
  # Map the missing WQS values based on name match
  mutate(WQS_POLLUTANT_NAME = ifelse(is.na(WQS_POLLUTANT_NAME), 
                                     wqs_proc$WQS_POLLUTANT_NAME[WQS_NAME_MATCH], 
                                     WQS_POLLUTANT_NAME),
         WQS_CRITERION_VALUE = ifelse(is.na(WQS_CRITERION_VALUE), 
                                      wqs_proc$WQS_CRITERION_VALUE[WQS_NAME_MATCH], 
                                      WQS_CRITERION_VALUE))

# Step B: Match the result to DMR
final_joined <- joined_wqs %>%
  left_join(dmr_proc, by = "join_cas", suffix = c("", ".dmr")) %>%
  # Fallback for DMR: Try Name
  mutate(DMR_NAME_MATCH = ifelse(is.na(POLLUTANT_DESC), 
                                 match(join_name, dmr_proc$join_name), NA)) %>%
  mutate(POLLUTANT_DESC = ifelse(is.na(POLLUTANT_DESC), 
                                 dmr_proc$POLLUTANT_DESC[DMR_NAME_MATCH], 
                                 POLLUTANT_DESC),
         PARAMETER_CODE = ifelse(is.na(PARAMETER_CODE), 
                                 dmr_proc$PARAMETER_CODE[DMR_NAME_MATCH], 
                                 PARAMETER_CODE))

# 5. Add a "Linkage Quality" column for your reviewers
final_joined <- final_joined %>%
  mutate(Match_Status = case_when(
    !is.na(WQS_CRITERION_VALUE) & !is.na(PARAMETER_CODE) ~ "Full Link (Standard + Data)",
    !is.na(WQS_CRITERION_VALUE) & is.na(PARAMETER_CODE)  ~ "No DMR Data Available",
    is.na(WQS_CRITERION_VALUE) & !is.na(PARAMETER_CODE)  ~ "No State Standard Found",
    TRUE ~ "Unlinked / Manually Review"
  ))

# Clean up temporary helper columns
final_output <- final_joined %>%
  select(-join_cas, -join_name, -WQS_NAME_MATCH, -DMR_NAME_MATCH)

write_csv(final_joined_data, "C:/Desktop/drive-download-20260223T010016Z-3-001/final_joined_data.csv")
