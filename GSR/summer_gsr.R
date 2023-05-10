#!/usr/local/bin/Rscript --vanilla

suppressMessages({
    if (!require("gt")) {
        utils::install.packages("gt", repos = "https://cran.rstudio.com/")
    }
    if (!require("tibble")) {
        utils::install.packages("tibble", repos = "https://cran.rstudio.com/")
    }
    if (!require("shiny")) {
        utils::install.packages("shiny", repos = "https://cran.rstudio.com/")
    }
    if (!require("shinyjs")) {
        utils::install.packages("shinyjs", repos = "https://cran.rstudio.com/")
    }
})

hours <- list(
    July       = 168,
    August     = 184,
    September1 = 168,
    September2 = 168,
    June       = 176
)

gsr_levels <- list(
    I   = list(level = "I",   monthly = 5090.00,  hourly = list(), daily = list()),
    II  = list(level = "II",  monthly = 5484.50,  hourly = list(), daily = list()),
    III = list(level = "III", monthly = 5909.58,  hourly = list(), daily = list()),
    IV  = list(level = "IV",  monthly = 6367.58,  hourly = list(), daily = list()),
    V   = list(level = "V",   monthly = 6861.08,  hourly = list(), daily = list()),
    VI  = list(level = "VI",  monthly = 7392.83,  hourly = list(), daily = list())
)

# Add Hourly Rates to gsr_levels
for(month in names(hours)) {
    for(level in names(gsr_levels)) {
        m <- gsr_levels[[level]]$monthly
        gsr_levels[[level]][["hourly"]][[month]] <- m / hours[[month]]
        gsr_levels[[level]][["daily"]][[month]]  <- 8 * (m / hours[[month]])
    }
}

appointment_percents = c(0.25, 0.50, 0.75, 1.00)

limits <- list(
    July = list(
        max_days = 21,
        percents = c(0.25, 0.50, 0.75, 1.00)
    ),
    August = list(
        max_days = 23,
        percents = c(0.25, 0.50, 0.75, 1.00)
    ),
    September1 = list(
        max_days = 16,
        percents = c(0.25, 0.50, 0.75, 1.00)
    ),
    September2 = list(
        max_days = 5,
        percents = c(0.25, 0.50)
    ),
    June = list(
        max_days = 10,
        percents = c(0.25, 0.50)
    )
)

find_best_fit <- function(target_dollars, gsr_level, is_july_ta = FALSE, is_august_ta = FALSE) {
    dfs <- list()
    idx <- 0
    for (appt_perc in appointment_percents) {
        monthly_pay_at_appt_level <- gsr_level$monthly * appt_perc
        full_months_of_pay <- floor(target_dollars/monthly_pay_at_appt_level)
        if (full_months_of_pay == 0) {
            scenarios <- list(
                c("July"),
                c("August"),
                c("September1"),
                c("September2"),
                c("June")
            )
        } else if (full_months_of_pay == 1) {
            scenarios <- list(
                c("July", "August", "September1", "September2", "June"),
                c("August", "July", "September1", "September2", "June"),
                c("August", "September1", "September2", "June"),
                c("July", "September1", "September2", "June")
            )
        } else if (full_months_of_pay == 2) {
            scenarios <- list(
                c("July", "August", "September1", "September2", "June")
            )
        } else if (full_months_of_pay > 2) {
            scenarios <- list()
        }

        sidx <- 0
        for(scenario in scenarios) {
            df <- tibble::tibble(
                percent            = double(0),
                month              = character(0),
                max_days           = integer(0),
                days               = integer(0),
                salary             = double(0),
                fringe             = double(0),
                gael               = double(0),
                salary_fringe      = double(0),
                salary_fringe_gael = double(0)
            )
            scenario_dollars <- target_dollars
            index_bumped <- FALSE
            for (month in scenario) {
                if (month == "July" && is_july_ta && appt_perc > 0.50) {
                    #Too high a percent for a TA month
                    next
                }
                if (month == "August" && is_august_ta && appt_perc > 0.50) {
                    #Too high a percent for a TA month
                    next
                }
                limit <- limits[[month]]
                if (appt_perc %in% limit$percents) {

                    if (scenario_dollars > 0) {
                        daily_dollars <- gsr_level$daily[[month]] * appt_perc
                        work_days <- min(limit$max_days, floor(scenario_dollars/daily_dollars))
                        work_dollars <- work_days * daily_dollars
                        if (work_dollars > 0) {
                            if (!index_bumped) {
                                sidx <- sidx + 1
                                index_bumped <- TRUE
                            }
                            scenario_dollars <- scenario_dollars - work_dollars
                            df <- rbind(
                                df,
                                tibble::tibble(
                                    percent            = sprintf("%s%% Effort - Scenario %s", 100*appt_perc, sidx),
                                    month              = month,
                                    max_days           = limit$max_days,
                                    days               = work_days,
                                    salary             = round(work_dollars, 2),
                                    fringe             = round(work_dollars * 0.02, 2),
                                    gael               = round(work_dollars * 0.009, 2),
                                    salary_fringe      = round(work_dollars, 2) + round(work_dollars * 0.02, 2),
                                    salary_fringe_gael = round(work_dollars, 2) + round(work_dollars * 0.02, 2) + round(work_dollars * 0.009, 2)
                                )
                            )
                        }
                    }
                }
            }
            if (nrow(df) > 0) {
                df$order <- match(df$month, c("June", "July", "August", "September1", "September2"))
                df <- df[order(df$order),]
                df <- df[, !(names(df) %in% "order")]

                idx <- idx + 1
                key <- sprintf("%09i :: %s", round(sum(df$salary), 0), idx)
                dfs[[key]] <- df
            }
        }
    }
    dfs <- dfs[rev(sort(names(dfs)))]

    return(dfs)
}

find_best_fit_test <- function() {
    res <- find_best_fit(
        target_dollars = 11000,
        gsr_level      = gsr_levels$I,
        is_july_ta     = FALSE,
        is_august_ta   = FALSE
    )
    print_df_list(res)

    res <- find_best_fit(
        target_dollars = 5300,
        gsr_level      = gsr_levels$I,
        is_july_ta     = FALSE,
        is_august_ta   = FALSE
    )
    print_df_list(res)

}

blank_row <- function(percent) {
    tibble::tibble(
        percent            = percent,
        month              = ":",
        max_days           = as.integer(NA),
        days               = as.integer(NA),
        salary             = as.double(NA),
        fringe             = as.double(NA),
        gael               = as.double(NA),
        salary_fringe      = as.double(NA),
        salary_fringe_gael = as.double(NA)
    )
}

print_df_list <- function(dfs) {
    df <- NULL
    for(tdf in dfs){
        tdf$salary <- round(tdf$salary, 2)
        tdf <- rbind(
            tdf,
            tibble::tibble(
                percent            = tdf$percent[[1]],
                month              = NA_character_,
                max_days           = NA_character_,
                days               = sum(tdf$days, na.rm = TRUE),
                salary             = round(sum(tdf$salary, na.rm = TRUE), 2),
                fringe             = round(sum(tdf$fringe, na.rm = TRUE), 2),
                gael               = round(sum(tdf$gael, na.rm = TRUE), 2),
                salary_fringe      = round(sum(tdf$salary_fringe, na.rm = TRUE), 2),
                salary_fringe_gael = round(sum(tdf$salary_fringe_gael, na.rm = TRUE), 2)
            )
        )

        tdf <- rbind(tdf, blank_row(percent = tdf$percent[[1]]))

        if (is.null(df)) {
            df <- tdf
        } else {
            df <- rbind(df, tdf)
        }
    }
    gt::gt(df, groupname_col = "percent") |>
        gt::tab_header(title = "Best Fit GSR Appointments", subtitle = "(descending sorted by highest salary payout)") |>
        gt::cols_label(
            month              = gt::html("<b>Month</b>"),
            max_days           = gt::html("<b>Max<br>Days</b>"),
            days               = gt::html("<b>Work<br>Days</b>"),
            salary             = gt::html("<b>Salary</b>"),
            fringe             = gt::html("<b>Fringe<br>Benefits</b>"),
            gael               = gt::html("<b>GAEL</b>"),
            salary_fringe      = gt::html("<b>Salary<br>+<br>Fringe</b>"),
            salary_fringe_gael = gt::html("<b>Salary<br>+<br>Fringe<br>+<br>GAEL</b>"),
        ) |>
        gt::sub_missing(missing_text = "") |>
        gt::tab_options(row_group.font.weight = "bold") |>
        gt::tab_style(style = gt::cell_fill(alpha = 0.35), locations = gt::cells_row_groups()) |>
        gt::tab_style(style = gt::cell_fill(color = "#00FF00", alpha = .35), locations = gt::cells_body(columns = salary, rows = is.na(month) & salary == max(df$salary, na.rm = TRUE))) |>
        gt::tab_style(style = gt::cell_text(weight = "bold"), locations = gt::cells_body(columns = salary, rows = is.na(month))) |>
        gt::tab_style(style = gt::cell_fill(color = "#AAAAAA", alpha = .35), locations = gt::cells_body(columns = salary, rows = salary != max(df$salary, na.rm = TRUE) & is.na(month))) |>
        gt::tab_style(style = gt::cell_text(color = "#BBBBBB"), locations = gt::cells_body(columns = max_days)) |>
        gt::tab_style(style = gt::cell_borders(sides = c("top"), color = "#AAAAAA", weight = gt::px(2)), locations = gt::cells_body(rows = is.na(month))) |>
        gt::tab_style(style = gt::cell_text(size = "20px", color = "#FFFFFF"), locations = gt::cells_body(columns = month, rows = month == ":")) |>
        gt::tab_style(style = gt::cell_borders(sides = c("left", "right"), color = "#CCCCCC"), locations = gt::cells_body(rows = !is.na(days))) |>
        gt::fmt_currency(columns = gt::one_of(c("salary","fringe", "gael", "salary_fringe","salary_fringe_gael")))
}


ui <- shiny::absolutePanel(
    top = 0, left = 0, right = 0, bottom = 0,
    style = "padding: 20px; margin: 5px;",
    shinyjs::useShinyjs(),
    shiny::tags$style("
        body {
            font-family: Calibri,Arial;
        }
        input#target_salary {
            width: 100px;
            height: 20px;
        }
        select#gsr_level {
            width: 87px;
            height: 25px;
        }
        input#gsr_salary {
            width: 115px;
            height: 20px;
        }
        table.gt_table {
            margin-left: 45px!important;
        }
    "),
    shiny::tags$table(
        shiny::tags$tr(
            shiny::tags$td(
                style= "width:125px; max-width: 125px; font-weight:bold; margin-bottom: 5px;",
                shiny::tags$label("Target Salary:")
            ),
            shiny::tags$td(
                style= "width:100px; max-width: 100px; font-weight:bold; margin-bottom: 5px;",
                shiny::tags$label("GSR Level:")
            ),
            shiny::tags$td(
                style= "width:135px; max-width: 135px; font-weight:bold; margin-bottom: 5px;",
                shiny::tags$label("Monthly Salary:")
            ),
            shiny::tags$td(
                style= "width:400px; max-width: 400px; font-weight:bold; margin-bottom: 5px;",
                shiny::tags$label("TA Appointment:")
            )
        ),

        shiny::tags$tr(
            shiny::tags$td(
                style= "width:100px; max-width: 100px",
                shiny::textInput(inputId = "target_salary", label = NULL, value = 5000, width = "100px"),
            ),
            shiny::tags$td(
                style= "width:85px; max-width: 85px",
                shiny::selectInput(inputId = "gsr_level", label = NULL, choices = names(gsr_levels), selectize = FALSE, width = "75px")
            ),
            shiny::tags$td(
                style= "width:100px; max-width: 100px",
                shinyjs::disabled(shiny::textInput(inputId = "gsr_salary", label = NULL, width = "100px"))
            ),
            shiny::tags$td(
                style= "width:200px; max-width: 400px",
                shiny::radioButtons(inputId = "gsr_ta", label = NULL, inline = TRUE, choices = c("None", "July", "August"), selected = "None")
            ),
        ),
        shiny::tags$tr(
            shiny::tags$td(
                style = "padding-top: 10px;",
                colspan = 4,
                gt::gt_output(outputId = "scenarios")

            )
        )

    )
)

server = function(input, output, session) {
    session$onSessionEnded(function() {
        shiny::stopApp()
    })

    shiny::observeEvent(input$gsr_level, {
        shiny::updateTextInput(session = session, inputId = "gsr_salary", value = gsr_levels[[input$gsr_level]]$monthly)
    })

    shiny::observe({
        table <- NULL
        error <- NULL
        tryCatch({

            shiny::withProgress(min = 3, max = 6, message = "Working...", expr = {
                res <- find_best_fit(
                    target_dollars = as.numeric(input$target_salary),
                    gsr_level      = gsr_levels[[input$gsr_level]],
                    is_july_ta     = input$gsr_ta == "July",
                    is_august_ta   = input$gsr_ta == "August"
                )
                if (length(res) > 0) {
                    table <- print_df_list(res)
                }
            })
        }, error = function(e) {
            message(e$message)
            error <<- e$message
        })
        output$scenarios <- gt::render_gt({
            shiny::validate(shiny::need(table, message = "Failed to calculate this scenario"))
            table
        })

    })
}

shiny::shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))







