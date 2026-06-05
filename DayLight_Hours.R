library(tidyr)
library(dplyr)
library(readxl)
library(stringr)

setwd("~/GitHub/LA_Tower")


# Read without column names
df <- read_xlsx(
  "Data/Day_Light_Hrs_2019_Houma.xlsx",
  sheet = "2019",
  skip = 6,
  col_names = FALSE
)

# First two rows are headers
header1 <- as.character(unlist(df[1, ]))
header2 <- as.character(unlist(df[2, ]))

# Fill month names across the blank cells
header1 <- tidyr::fill(data.frame(month = header1), month)$month

# Combine month and Rise/Set
new_names <- ifelse(
  is.na(header1) | header1 == "",
  header2,
  paste(header1, header2, sep = "_")
)

new_names
# Apply names and remove header rows
names(df) <- new_names
df <- df[-c(1, 2, 3), ]

head(df)


daylight_long <- df %>%
  pivot_longer(
    cols = -Day,
    names_to = c("Month", ".value"),
    names_sep = "_"
  ) %>%
  mutate(
    Month = match(Month,
                  c("Jan","Feb","Mar","Apr","May","June",
                    "July","Aug","Sept","Oct","Nov","Dec")),
    Date = make_date(2019, Month, as.integer(Day))
  ) %>%
  select(Date, Rise, Set) %>%
  arrange(Date)

daylight_long


##### Function to apply to all sheets #####

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)

file <- "Data/Day_Light_Hrs_2019_Houma.xlsx"

process_sheet <- function(sheet_name) {
  
  df <- read_xlsx(
    file,
    sheet = sheet_name,
    skip = 6,
    col_names = FALSE
  )
  
  # headers
  header1 <- as.character(unlist(df[1, ]))
  header2 <- as.character(unlist(df[2, ]))
  
  header1 <- tidyr::fill(
    data.frame(month = header1),
    month
  )$month
  
  new_names <- ifelse(
    is.na(header1) | header1 == "",
    header2,
    paste(header1, header2, sep = "_")
  )
  
  names(df) <- new_names
  
  # remove header rows
  df <- df[-c(1, 2, 3), ]
  
  month_lookup <- c(
    Jan = 1, Feb = 2, Mar = 3, Apr = 4,
    May = 5, June = 6, July = 7, Aug = 8,
    Sept = 9, Oct = 10, Nov = 11, Dec = 12
  )
  
  df_long <- df %>%
    pivot_longer(
      cols = -Day,
      names_to = c("Month", ".value"),
      names_sep = "_"
    ) %>%
    mutate(
      Year = as.integer(sheet_name),
      Date = make_date(Year, month_lookup[Month], as.integer(Day))
    ) %>%
    select(Date, Rise, Set) %>%
    arrange(Date) %>%
    
    # ✅ fill missing rows using previous day
    fill(Rise, Set, .direction = "down") %>%
    
    # optional safety: also fill first row if needed
    fill(Rise, Set, .direction = "up") %>%
    
    # convert to datetime
    mutate(
      Rise = as.POSIXct(
        paste(Date, sprintf("%04d", as.integer(Rise))),
        format = "%Y-%m-%d %H%M",
        tz = "America/Chicago"
      ),
      Set = as.POSIXct(
        paste(Date, sprintf("%04d", as.integer(Set))),
        format = "%Y-%m-%d %H%M",
        tz = "America/Chicago"
      )
    )
  
  return(df_long)
}

# Get all sheet names
sheets <- excel_sheets(file)

# Process and combine all sheets
daylight_all <- map_dfr(sheets, process_sheet) %>%
  mutate(
    Rise = Rise + 3600,   # +1 hour
    Set  = Set  - 3600    # -1 hour
  )


head(daylight_all)


