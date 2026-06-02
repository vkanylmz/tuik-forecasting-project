# data_import.R
# Purpose: Find and import the selected TÜİK quarterly domestic tourism table through tuikr.

parse_numeric_tuik <- function(x) {
  x_chr <- as.character(x)
  x_chr <- gsub("\\u00a0", " ", x_chr, fixed = TRUE)
  x_chr <- gsub("[^0-9,.-]", "", x_chr)
  x_chr <- ifelse(grepl(",", x_chr) & !grepl("\\.", x_chr), gsub(",", ".", x_chr), x_chr)
  x_chr <- gsub(",", "", x_chr)
  suppressWarnings(as.numeric(x_chr))
}

import_domestic_tourism_data <- function() {
  # The table is an istab table. It is first located through tuikr, then its official table_url is requested in R.
  tables_14 <- tuikr::statistical_tables(theme = "14")

  selected_table <- tables_14[tables_14$table_name == "Number of Trips and Overnights of Domestic Visitors by Quarter", ]

  if (nrow(selected_table) == 0) {
    selected_table <- tables_14[grepl("Number of Trips and Overnights of Domestic Visitors by Quarter",
                                     tables_14$table_name,
                                     ignore.case = TRUE), ]
  }

  if (nrow(selected_table) == 0) {
    stop("Selected TÜİK table could not be found through tuikr::statistical_tables(theme = '14').")
  }

  selected_table <- selected_table[1, ]
  url <- selected_table$table_url[1]

  response <- httr::GET(
    url,
    httr::add_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
      `Referer` = "https://veriportali.tuik.gov.tr/",
      `Accept` = "application/vnd.ms-excel,*/*"
    )
  )

  if (httr::status_code(response) != 200) {
    stop(paste("TÜİK table URL request failed with HTTP status", httr::status_code(response)))
  }

  raw_content <- httr::content(response, "raw")
  if (length(raw_content) < 1000) {
    stop("Downloaded TÜİK file is unexpectedly small. The response may be Access denied or empty.")
  }

  temp_file <- tempfile(fileext = ".xls")
  writeBin(raw_content, temp_file)

  raw_data <- readxl::read_excel(temp_file, sheet = 1, col_names = TRUE)

  # The TÜİK Excel layout has year in column 1, quarter labels in column 2,
  # trips in column 3, and overnights in column 4. Annual subtotal rows are excluded.
  working <- raw_data[, 1:4]
  names(working) <- c("Year_Raw", "Quarter_Label", "Trips_Raw", "Overnights_Raw")

  current_year <- NA_integer_
  cleaned_rows <- list()
  row_id <- 1

  for (i in seq_len(nrow(working))) {
    year_candidate <- as.character(working$Year_Raw[i])
    if (!is.na(year_candidate) && grepl("^[0-9]{4}$", year_candidate)) {
      current_year <- as.integer(year_candidate)
    }

    q_label <- trimws(as.character(working$Quarter_Label[i]))
    if (!is.na(q_label) && q_label %in% c("I", "II", "III", "IV") && !is.na(current_year)) {
      quarter_number <- match(q_label, c("I", "II", "III", "IV"))
      trips_value <- parse_numeric_tuik(working$Trips_Raw[i])
      overnights_value <- parse_numeric_tuik(working$Overnights_Raw[i])

      if (!is.na(trips_value)) {
        cleaned_rows[[row_id]] <- data.frame(
          Year = current_year,
          Quarter_Label = q_label,
          Quarter = quarter_number,
          Period = paste0(current_year, " Q", quarter_number),
          Time_Index = row_id,
          Trips = trips_value,
          Overnights = overnights_value,
          stringsAsFactors = FALSE
        )
        row_id <- row_id + 1
      }
    }
  }

  if (length(cleaned_rows) == 0) {
    stop("No quarterly observations could be extracted from the TÜİK table.")
  }

  domestic_data <- dplyr::bind_rows(cleaned_rows) |>
    dplyr::arrange(Year, Quarter) |>
    dplyr::mutate(Time_Index = dplyr::row_number())

  if (anyDuplicated(domestic_data$Period) > 0) {
    stop("Duplicate quarterly periods detected after cleaning.")
  }

  list(
    selected_table = selected_table,
    data_access_date = Sys.Date(),
    data = domestic_data,
    raw_data = raw_data
  )
}
