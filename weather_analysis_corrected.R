library(zoo)
library(lubridate)
library(tidyverse)
library(evd)
#----SETUP OF THE DATA FILES----
weather <- read.csv(file = "complete_weather_combined.csv", header = TRUE, sep = ",", dec = ".")
# FIX: the airport file was read twice, the second time with the typo "transmutate". One read is enough.
airport_weather <- read.csv(file = "airport_weather.csv", header = TRUE, sep = ",", dec = ".") %>% transmute(Date = as.Date(date), airport_mm = tp)
# FIX: suspect_days / EXCLUDE_SUSPECT were defined twice; defined once now
suspect_days <- as.Date("2020-02-03")
EXCLUDE_SUSPECT <- TRUE
READINGS_PER_DAY <- 288      # NEW: 24 h * 12 readings/h (5-minute data)
MIN_COVERAGE <- 0.8          # NEW: a day needs >= 80% valid readings to get daily statistics (set 0 to switch off)
# NEW: barometer regime change. The daily median jumps from ~1008 hPa (28 Jul 2023) to ~754 hPa (29 Jul 2023).
# 29 Apr - 30 Jun 2013 sits at the low level too (~720) and is left out of both segments. Check Bar_range.png.
bar_segments <- tibble(name  = c("Bar_2013_2023", "Bar_2023_on"),
                       start = as.Date(c("2013-07-01", "2023-07-29")),
                       end   = as.Date(c("2023-07-29", "2100-01-01")))
# FIX: spacing in "Temperature (°C)". NOTE: the assignment lists wind speed in m/s, but your 1.6 steps point to km/h
# (1 mph = 1.609 km/h). Confirm the logger unit. To convert, uncomment the next two lines:
# weather$Wind_Speed <- suppressWarnings(as.numeric(weather$Wind_Speed)) / 3.6
# units_lookup["Wind_Speed"] <- "Wind speed (m/s)"
units_lookup <- c(Temp_Out = "Temperature (°C)", Rain = "Rain (mm)", Solar_Rad = "Solar radiation (W/m2)", Bar = "Pressure (hPa)", Out_Hum = "Relative humidity (%)", Wind_Speed = "Wind speed (km/h)")
TIME <- weather$Time
DATE <- weather$Date
TIME_FRAME <- data.frame(DATE = c(DATE), TIME = c(TIME))
DATE_TIME <- TIME_FRAME %>% mutate(DATETIME = paste(DATE, TIME)) %>% mutate(across('DATETIME', ~ as.POSIXct(.x, format = "%d/%m/%y", tz = "America/La_Paz")))

find_gaps <- function(dates) {
    dates <- sort(unique(dates))
    full <- seq(min(dates), max(dates), by ="day")
    missing <- full[!full %in% dates]
    if (length(missing)==0) return(tibble(start = as.Date(character()),end = as.Date(character()), days = integer()))
    tibble(Date = missing) %>% mutate(run = cumsum(c(1, diff(Date) != 1))) %>% group_by(run) %>% summarise(start=min(Date), end = max(Date), days = n(), .groups = "drop") %>% select(-run)
}

# NEW (Part 1 of the assignment): describe the data set and summarise missing values
describe_dataset <- function(weather, variables) {
    ts <- as.POSIXct(paste(weather$Date, weather$Time), format = "%d/%m/%y %H:%M", tz = "America/La_Paz")
    step_min <- median(as.numeric(diff(ts), units = "mins"), na.rm = TRUE)
    info <- tibble(
        first_record = format(min(ts, na.rm = TRUE)),
        last_record = format(max(ts, na.rm = TRUE)),
        n_records = nrow(weather),
        n_days_with_data = n_distinct(as.Date(ts), na.rm = TRUE),
        sampling_interval_min = step_min,
        records_per_full_day = 1440 / step_min,
        n_variables = length(variables)
    )
    qc <- map_dfr(variables, function(v) {
        num <- suppressWarnings(as.numeric(weather[[v]]))   # placeholders such as "---" become NA and are counted as missing
        tibble(variable = v, n_records = length(num), n_missing = sum(is.na(num)),
               pct_missing = round(100 * mean(is.na(num)), 2),
               min = min(num, na.rm = TRUE), max = max(num, na.rm = TRUE))
    })
    glimpse(info)
    print(qc)
    write.csv(info, "dataset_description.csv", row.names = FALSE)
    write.csv(qc, "quality_control_summary.csv", row.names = FALSE)
    invisible(list(info = info, qc = qc))
}

# FIX: n_na now counts empty READINGS (passed in), and the range uses range_vals (default = the vector analysed).
# The old default "range_vals = values" pointed to an object that does not exist, and the range always used daily$min_daily / max_daily.
basic_statistics_function <- function(daily, var ,var_name, n_na = NA_integer_, range_vals = var) {

    stats <- tibble(
         variable = var_name,
         median = median(var, na.rm = TRUE),
         mean = mean(var, na.rm = TRUE),
         sample_size = nrow(daily),
         n_na = n_na,
         range_low = min(range_vals, na.rm = TRUE),
         range_high = max(range_vals, na.rm = TRUE)
    )
    print(paste("---Basic Statistical Values of: ", var_name, " ---"))
    print(paste("Median Value: ", stats$median))
    print(paste("Mean Value: ", stats$mean))
    print(paste("Sample Size (across all years): ", stats$sample_size))
    print(paste("Number of NaNs (empty readings)", stats$n_na))
    print(paste("Range: Lowest value = ",stats$range_low," Highest value:" ,stats$range_high))
    return(stats)
}

# FIX: the black line used aes(color = "black"), which maps the TEXT "black" to a default colour. It is now a
# 30-day moving mean with a real black colour. Days missing from the record become NA so lines BREAK at data gaps
# instead of joining across them. Units are added to the y axis, and the x axis is labelled.
# NEW arguments (all optional): ylab, show_min, hi_label, mid_label.
temporal_series <- function(data, var_name, date, min, max, median, ylab = var_name, show_min = TRUE, hi_label = "Maximum", mid_label = "30-day mean"){
    d <- tibble(date = date, lo = min, hi = max, mid = median) %>%
        complete(date = seq(base::min(date), base::max(date), by = "day")) %>%
        mutate(ma30 = zoo::rollapply(mid, 30, mean, na.rm = TRUE, fill = NA, partial = TRUE),
               ma30 = ifelse(is.nan(ma30), NA, ma30))
    cols <- c("#F8766D", "#00BFC4", "black")
    names(cols) <- c(hi_label, "Minimum", mid_label)
    plot <- ggplot(d, aes(x = date))
    if (show_min) {
        plot <- plot +
            geom_ribbon(aes(ymin = lo, ymax = hi), fill = "lightblue", alpha = 0.5) +
            geom_line(aes(y = lo, color = "Minimum"), na.rm = TRUE)
    }
    plot <- plot +
        geom_line(aes(y = hi, color = hi_label), na.rm = TRUE) +
        geom_line(aes(y = ma30, color = mid_label), linewidth = 0.8, na.rm = TRUE) +
        scale_color_manual(values = cols) +
        labs(title = paste("Daily ", var_name, "Range"), x = "Date", y = ylab, color = NULL) + theme_minimal()
    ggsave(paste0(var_name, "_range.png"), plot, width = 10, height =6 )
    return(plot)
}

# NEW argument xlab (units on the x axis) and readable legend names; everything else unchanged
pdf_function <- function(daily, cols, label, bounds = c(-Inf, Inf), xlab = label) {
    long <- daily %>%
        select(all_of(cols)) %>%
        pivot_longer(everything(), names_to = "series", values_to = "value") %>%
        filter(!is.na(value)) %>%
        mutate(series = recode(series, max_daily = "Daily maximum", min_daily = "Daily minimum",
                               mean_daily = "Daily mean", total_daily = "Daily total", midrange_daily = "Daily midrange"))

    plot <- ggplot(long, aes(x = value, fill = series, color = series)) +
        geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.35, position = "identity", color = NA) +
        geom_density(fill = NA, linewidth = 0.9, bounds = bounds) +
        labs(title = paste("Probability density of", label), x = xlab, y = "Density", fill = NULL, color = NULL) +
        theme_minimal()
    ggsave(paste0(label, "_pdf.png"), plot, width = 10, height = 6)
    return(plot)
}

# NEW: added n and trend_p_value to the output table (the p-value ignores autocorrelation, so treat it as indicative)
advanced_statistics_function <- function(daily, var, var_name) {
    keep  <- !is.na(var)
    x     <- var[keep]
    dates <- daily$Date[keep]
    m     <- mean(x)
    s     <- sd(x)
    qs    <- unname(quantile(x, c(0.05, 0.25, 0.75, 0.95)))

    # linear trend (units per year), using the real dates so gaps don't matter
    fit <- lm(x ~ as.numeric(dates))
    trend_year <- unname(coef(fit)[2]) * 365.25
    trend_p <- summary(fit)$coefficients[2, 4]

    # anomalies relative to the climatological mean of each calendar month
    anomalies <- x - ave(x, month(dates))

    stats <- tibble(
        variable = var_name,
        n = length(x),
        clim_mean = m,
        median = median(x),
        sd = s,
        variance = var(x),
        cv_percent = s / m * 100,
        min = min(x),
        max = max(x),
        range = max(x) - min(x),
        p05 = qs[1], p25 = qs[2], p75 = qs[3], p95 = qs[4],
        skewness = mean((x - m)^3) / mean((x - m)^2)^1.5,
        kurtosis = mean((x - m)^4) / mean((x - m)^2)^2,   # normal distribution = 3
        trend_per_year = trend_year,
        trend_p_value = trend_p,
        anomaly_sd = sd(anomalies),
        max_pos_anomaly = max(anomalies),
        max_neg_anomaly = min(anomalies)
    )
    print(paste("---Advanced Statistical Values of: ", var_name, " ---"))
    print(paste("Std Dev: ", round(s, 3), " | CV (%): ", round(stats$cv_percent, 2)))
    print(paste("Percentiles 5/25/75/95: ", paste(round(qs, 2), collapse = " / ")))
    print(paste("Skewness: ", round(stats$skewness, 3), " | Kurtosis: ", round(stats$kurtosis, 3)))
    print(paste("Linear trend per year: ", round(trend_year, 4), " (p = ", signif(trend_p, 3), ")"))
    return(stats)
}

# NEW (Part 4, "anomalias"): monthly anomaly series with a linear trend line, one plot + one CSV per variable.
# Anomaly = monthly value minus the mean of that calendar month (rain uses monthly totals, others monthly means).
anomaly_function <- function(daily, col, var_name, is_rain = FALSE, ylab = var_name) {
    m <- daily %>%
        filter(!is.na(.data[[col]])) %>%
        mutate(year = year(Date), month = month(Date)) %>%
        group_by(year, month) %>%
        summarise(value = if (is_rain) sum(.data[[col]]) else mean(.data[[col]]), n_days = n(), .groups = "drop") %>%
        filter(n_days >= 25) %>%
        group_by(month) %>%
        mutate(anomaly = value - mean(value)) %>%
        ungroup() %>%
        mutate(date = make_date(year, month, 15))
    plot <- ggplot(m, aes(date, anomaly)) +
        geom_col(aes(fill = anomaly > 0), show.legend = FALSE) +
        scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "steelblue")) +
        geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.6) +
        labs(title = paste("Monthly anomaly of", var_name), x = "Date", y = paste("Anomaly:", ylab)) +
        theme_minimal()
    ggsave(paste0(var_name, "_anomalies.png"), plot, width = 10, height = 5)
    write.csv(m, paste0(var_name, "_anomalies.csv"), row.names = FALSE)
    invisible(m)
}

annual_cycle_function <- function(results) {
    cycle_one <- function(daily, col, label, is_rain = FALSE) {
        d <- daily %>% filter(!is.na(.data[[col]])) %>% mutate(year = year(Date), month = month(Date))
        if (is_rain) {
            # monthly total per year first, then average those totals across years
            # FIX: only months with >= 25 valid days count, so gap months don't pull the rain climatology down
            d <- d %>% group_by(year, month) %>% summarise(value = sum(.data[[col]]), n_days = n(), .groups = "drop") %>% filter(n_days >= 25)
        } else {
            d$value <- d[[col]]
        }
        d %>% group_by(month) %>% summarise(value = mean(value), .groups = "drop") %>% mutate(variable = label)
    }
    cycle <- bind_rows(
        cycle_one(results$Temp_Out$daily,  "mean_daily", "Temperature (°C)"),   # FIX: was the midrange column
        cycle_one(results$Rain$daily,      "total_daily",  "Precipitation (mm/month)", is_rain = TRUE),
        cycle_one(results$Out_Hum$daily,   "mean_daily", "Relative humidity (%)")
    )
    plot <- ggplot(cycle, aes(x = month, y = value)) +
        geom_line(color = "steelblue", linewidth = 1) + geom_point(color = "steelblue") +
        facet_wrap(~variable, ncol = 1, scales = "free_y") +
        scale_x_continuous(breaks = 1:12, labels = month.abb) +
        labs(title = "Climatological annual cycle", x = NULL, y = NULL) +
        theme_minimal()
    ggsave("annual_cycle.png", plot, width = 10, height = 9)

    # NEW: the three cycles standardized (z-scores) on ONE axis, so they can be compared directly
    cycle_z <- cycle %>% group_by(variable) %>% mutate(z = (value - mean(value)) / sd(value)) %>% ungroup()
    plot_z <- ggplot(cycle_z, aes(x = month, y = z, color = variable)) +
        geom_line(linewidth = 1) + geom_point() +
        scale_x_continuous(breaks = 1:12, labels = month.abb) +
        labs(title = "Annual cycles compared (standardized)", x = NULL, y = "Standardized value (z-score)", color = NULL) +
        theme_minimal()
    ggsave("annual_cycle_compared.png", plot_z, width = 10, height = 6)

    write.csv(cycle_z, "annual_cycle.csv", row.names = FALSE)
    return(cycle)
}

clean_rain <- function(data, k = 3.5, floor_mm = 5) {
    rain <- suppressWarnings(as.numeric(data$Rain))

    wet <- log(rain[!is.na(rain) & rain > 0])
    # FIX: exp() takes one argument; the floor goes in max()
    threshold <- max(exp(median(wet) + k * mad(wet)), floor_mm)     # mad() is already scaled

    prev_val <- coalesce(dplyr::lag(rain), 0)
    next_val <- coalesce(dplyr::lead(rain), 0)
    isolated <- prev_val < 0.1 * rain & next_val < 0.1 * rain

    flagged <- !is.na(rain) & rain > threshold & isolated

    report <- tibble(
        row = which(flagged),
        Date = data$Date[flagged],
        Time = data$Time[flagged],
        Rain = rain[flagged],
        previous = prev_val[flagged],
        `next` = next_val[flagged],
        Out_Hum = data$Out_Hum[flagged]
    )
    write.csv(report, "rain_removed_readings.csv", row.names = FALSE)

    print(paste("Rain threshold (mm per reading):", round(threshold, 2)))
    print(paste("Readings removed:", sum(flagged)))

    rain[flagged] <- NA          # NA, not 0: we don't know it was dry
    return(rain)
}

# FIX: as.date -> as.Date, and wet_readings must count readings with rain, not sum the rain
rain_check <- function(weather, DATE_TIME, days) {
    d <- as.Date(DATE_TIME$DATETIME, tz = "America/La_Paz")
    weather %>% mutate(Date=d, Rain = as.numeric(Rain), Out_Hum = as.numeric(Out_Hum), Solar_Rad = as.numeric(Solar_Rad)) %>% filter(Date %in% days) %>% group_by(Date) %>% summarise(total = sum(Rain, na.rm = TRUE), wet_readings = sum(Rain > 0, na.rm = TRUE), max_readings = max(Rain, na.rm = TRUE), hum_during_rain = mean(Out_Hum[Rain>0], na.rm = TRUE), solar_during_rain = mean (Solar_Rad[Rain>0], na.rm = TRUE), .groups = "drop") %>% arrange(desc(total))
}

# FIX: min_valid used 268 readings per day (typo); a day has 288. FIX: inner_join referenced an object called
# "airport" that does not exist; the argument is airport_weather.
compare_rain_function <- function(daily, airport_weather, min_valid = 0.9 * READINGS_PER_DAY){ #min_valid is the number of minimum readings that a day needs to be compared, 90% of 288 5-min readings

    d <- daily %>% filter(n_valid >= min_valid) %>% select(Date, uni_mm = total_daily) %>% inner_join(airport_weather, by = "Date") %>% mutate(diff = uni_mm - airport_mm)
    metrics <- function(x, level) tibble(
        level = level, n = nrow(x),
        total_uni = sum(x$uni_mm), total_airport = sum(x$airport_mm),
        ratio = sum(x$uni_mm)/sum(x$airport_mm),
        bias_mm = mean(x$uni_mm - x$airport_mm),
        rmse_mm = sqrt(mean((x$uni_mm - x$airport_mm)^2)),
        pearson = cor(x$uni_mm, x$airport_mm),   # FIX: renamed from pearson_mm (a correlation has no units)
        spearman = cor(x$uni_mm, x$airport_mm, method = "spearman")
        )
    agg <- function(unit, n_needed) d %>% mutate(p= floor_date(Date, unit)) %>% group_by(p) %>% filter(n() >= n_needed) %>% summarise(uni_mm = sum(uni_mm), airport_mm = sum (airport_mm))
    weekly <- agg("week", 7); monthly  <- agg("month", 25)
    summary_tbl <- bind_rows(metrics(d, "daily"), metrics(weekly, "weekly"), metrics(monthly, "monthly"))
    wet <- d %>% summarise(both_wet = sum(uni_mm >= 1 & airport_mm >= 1),
                           uni_only = sum(uni_mm >= 1 & airport_mm < 1),
                           airport_only = sum(uni_mm < 1 & airport_mm >= 1),
                           both_dry = sum(uni_mm < 1 & airport_mm < 1))
    worst <- d %>% arrange(desc(abs(diff))) %>% head(20)
    p1 <- ggplot(d, aes(airport_mm, uni_mm)) + geom_point(alpha = 0.3) +
        geom_abline(slope = 1, intercept = 0, color = "red") +
        scale_x_sqrt() + scale_y_sqrt() +
        labs(title = "Daily rain: university vs airport", x = "Airport (mm/day)", y = "University (mm/day)") + theme_minimal()
    ggsave("rain_comparison_scatter.png", p1, width = 7, height = 7)

    p2 <- monthly %>% pivot_longer(c(uni_mm, airport_mm), names_to = "station", values_to = "mm") %>%
        ggplot(aes(p, mm, color = station)) + geom_line() +
        labs(title = "Monthly rain totals", x = "Month", y = "mm/month", color = NULL) + theme_minimal()
    ggsave("rain_comparison_monthly.png", p2, width = 10, height = 5)

    write.csv(summary_tbl, "rain_comparison_summary.csv", row.names = FALSE)
    write.csv(worst, "rain_comparison_worst_days.csv", row.names = FALSE)
    print(summary_tbl); print(wet); print(worst, n = 20)
    list(daily = d, summary = summary_tbl, wet = wet, worst = worst)
}
rain_pdf_comparison <- function(d) {
    long <- d %>% select(Airport = airport_mm, University = uni_mm) %>%
        pivot_longer(everything(), names_to = "station", values_to = "mm") %>% filter(mm >= 1)
    plot <- ggplot(long, aes(log10(mm), fill = station, color = station)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.3, position = "identity", color = NA) +
        geom_density(fill = NA) +
        labs(title = "Wet-day rain distribution", x = "log10(mm/day)", y = "Density", fill = NULL, color = NULL) +
        theme_minimal()
    ggsave("Rain_pdf_comparison.png", plot, width = 10, height = 6)
    plot
}

main_function <- function(data, var_name, date){
    # FIX: coercion warnings are expected ("---" placeholders) and are reported in quality_control_summary.csv
    df <- data.frame(DATETIME = date$DATETIME, value = suppressWarnings(as.numeric(data[[var_name]])))
    # FIX: dates come from the "date" argument, not the global DATE_TIME
    reading_date <- as.Date(date$DATETIME, tz = "America/La_Paz")
    if(var_name == "Rain") {
        df$value <- clean_rain(data)
        # FIX: EXCLUDE_SUSPECT was defined but never used
        if (EXCLUDE_SUSPECT) df$value[reading_date %in% suspect_days] <- NA
    }
    # NEW (quality control, Part 1): physically impossible humidity (<= 0 %) becomes NA
    if(var_name == "Out_Hum") {
        bad <- !is.na(df$value) & df$value <= 0
        print(paste("Humidity readings <= 0 set to NA:", sum(bad)))
        df$value[bad] <- NA
    }
    # NEW (quality control, Part 1): days whose maximum solar radiation is below 50 W/m2 are sensor outages
    if(var_name == "Solar_Rad") {
        day_max <- ave(df$value, reading_date, FUN = function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE))
        bad <- !is.na(day_max) & day_max < 50
        print(paste("Solar readings on outage days set to NA:", sum(bad & !is.na(df$value))))
        df$value[bad] <- NA
    }
    n_na <- sum(is.na(df$value))    # NEW: empty readings, counted after the cleaning above
    # FIX: median_daily was really the midrange (max+min)/2, so it is renamed midrange_daily.
    # NEW: days with less than MIN_COVERAGE valid readings get NA statistics instead of biased ones.
    daily <- df %>% group_by(Date = reading_date) %>% summarise(n_obs = n(), n_valid = sum(!is.na(value)), ok = n_valid > 0 && n_valid >= MIN_COVERAGE * READINGS_PER_DAY, max_daily = if(ok) max(value, na.rm = TRUE) else NA_real_, min_daily = if(ok) min(value, na.rm = TRUE) else NA_real_, midrange_daily = (max_daily + min_daily)/2, total_daily = if(ok) sum(value, na.rm = TRUE)else NA_real_, mean_daily = if(ok) mean(value, na.rm = TRUE) else NA_real_, .groups = "drop") %>% select(-ok)
    stats_list <- list()
    adv_list <- list()

    if(var_name == "Temp_Out") {
        stats_list[["Maximum_Temperature"]] <- basic_statistics_function(daily, daily$max_daily, "Maximum_Temperature", n_na = n_na)
        stats_list[["Minimum_Temperature"]] <- basic_statistics_function(daily, daily$min_daily, "Minimum_Temperature", n_na = n_na)
        # FIX: "Median Temperature" was the midrange; replaced by the true daily mean
        stats_list[["Mean_Temperature"]] <- basic_statistics_function(daily, daily$mean_daily, "Mean_Temperature", n_na = n_na)
        adv_list[["Maximum_Temperature"]] <- advanced_statistics_function(daily, daily$max_daily, "Maximum_Temperature")
        adv_list[["Minimum_Temperature"]] <- advanced_statistics_function(daily, daily$min_daily, "Minimum_Temperature")
        adv_list[["Mean_Temperature"]] <- advanced_statistics_function(daily, daily$mean_daily, "Mean_Temperature")
        temporal_series(daily, var_name, daily$Date, daily$min_daily, daily$max_daily, daily$mean_daily, ylab = units_lookup[["Temp_Out"]])
        pdf_function(daily, c("max_daily", "min_daily"), "Temperature", xlab = units_lookup[["Temp_Out"]])
        pdf_function(daily, "mean_daily", "Mean_Temperature", xlab = units_lookup[["Temp_Out"]])
        anomaly_function(daily, "mean_daily", var_name, ylab = units_lookup[["Temp_Out"]])   # NEW
    }
    else if (var_name == "Rain"){
        sorted_rain <- daily[order(-daily$total_daily), ]
        print(sorted_rain)
        stats_list[[var_name]] <- basic_statistics_function(daily, daily$total_daily, var_name, n_na = n_na)
        # FIX: the line used the mean of 5-min readings (meaningless for rain). It now shows the daily total with its 30-day mean.
        temporal_series(daily, var_name, daily$Date, daily$min_daily, daily$total_daily, daily$total_daily, ylab = units_lookup[["Rain"]], show_min = FALSE, hi_label = "Daily total", mid_label = "30-day mean of daily total")
        adv_list[[var_name]] <- advanced_statistics_function(daily, daily$total_daily, var_name)
        pdf_function(daily, "total_daily", "Rain", bounds = c(0, Inf), xlab = units_lookup[["Rain"]])
        anomaly_function(daily, "total_daily", var_name, is_rain = TRUE, ylab = "Rain (mm/month)")   # NEW
        write.csv(sorted_rain, "sorted_rain.csv", row.names = FALSE)
    }
    # NEW: the barometer is analysed in two segments because of the regime change (see bar_segments)
    else if (var_name == "Bar"){
        temporal_series(daily, var_name, daily$Date, daily$min_daily, daily$max_daily, daily$mean_daily, ylab = units_lookup[["Bar"]])
        for (i in seq_len(nrow(bar_segments))) {
            nm <- bar_segments$name[i]
            s <- daily %>% filter(Date >= bar_segments$start[i], Date < bar_segments$end[i])
            n_na_seg <- sum(is.na(df$value[reading_date >= bar_segments$start[i] & reading_date < bar_segments$end[i]]))
            stats_list[[nm]] <- basic_statistics_function(s, s$mean_daily, nm, n_na = n_na_seg, range_vals = c(s$min_daily, s$max_daily))
            adv_list[[nm]] <- advanced_statistics_function(s, s$mean_daily, nm)
            pdf_function(s, "mean_daily", nm, xlab = units_lookup[["Bar"]])
            anomaly_function(s, "mean_daily", nm, ylab = units_lookup[["Bar"]])
        }
    }
    else{
        stats_list[[var_name]] <- basic_statistics_function(daily, daily$mean_daily, var_name, n_na = n_na, range_vals = c(daily$min_daily, daily$max_daily))
        # FIX: advanced statistics used median_daily (midrange) while the basic ones used mean_daily
        adv_list[[var_name]] <- advanced_statistics_function(daily, daily$mean_daily, var_name)
        temporal_series(daily, var_name, daily$Date, daily$min_daily, daily$max_daily, daily$mean_daily, ylab = units_lookup[[var_name]])
        pdf_function(daily, "mean_daily", var_name, xlab = units_lookup[[var_name]])
        anomaly_function(daily, "mean_daily", var_name, ylab = units_lookup[[var_name]])   # NEW
    }
    gaps <- find_gaps(daily$Date)
    all_na_days <- daily %>% filter(is.na(max_daily)) %>% pull(Date)   # FIX: also catches days dropped by MIN_COVERAGE
    stats_df <- bind_rows(stats_list)
    advanced_df <- bind_rows(adv_list)
    return(list(daily = daily, gaps = gaps, all_na_days = all_na_days, stats = stats_df, advanced = advanced_df))

}
variables <- c("Temp_Out", "Rain", "Solar_Rad", "Bar", "Out_Hum", "Wind_Speed")
describe_dataset(weather, variables)    # NEW (Part 1): period, sampling frequency, number of records, missing values
results <- lapply(variables, function(v) main_function(weather, v, DATE_TIME))

names(results) <- variables
rain_cmp <- compare_rain_function(results$Rain$daily, airport_weather)
rain_pdf_comparison(rain_cmp$daily)
# NEW: this helper was defined but never called. It checks humidity and solar radiation during the days where the two stations disagree most.
rain_check_tbl <- rain_check(weather, DATE_TIME, rain_cmp$worst$Date)
print(rain_check_tbl)
write.csv(rain_check_tbl, "rain_check_worst_days.csv", row.names = FALSE)
gaps <- results$Temp_Out$gaps
print(paste("Missing calendar days: ", sum(gaps$days), "in", nrow(gaps), "gaps"))
if(nrow(gaps)>0) print(gaps)   # FIX: misplaced parenthesis (nrow(gaps>0))
all_stats <- bind_rows(lapply(results, '[[', "stats"))
write.csv(all_stats, "basic_statistics.csv", row.names = FALSE)
all_advanced <- bind_rows(lapply(results, '[[', "advanced"))
write.csv(all_advanced, "advanced_statistics.csv", row.names = FALSE)
annual_cycle <- annual_cycle_function(results)
write.csv(gaps, "date_gaps.csv", row.names = FALSE)
