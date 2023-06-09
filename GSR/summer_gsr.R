#!/usr/local/bin/Rscript --vanilla

suppressMessages({
    tryCatch({
        personal_lib <- Sys.getenv("R_LIBS_USER")
        if (length(personal_lib) > 0 && nchar(personal_lib) > 0) {
            if (!dir.exists(personal_lib)) {
                dir.create(path = personal_lib, recursive = TRUE)
            }
        }
    }, error = function(e){
        message("Error trying to create personal library path. Reason: ", e$message)
    })
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
    scenario_keys <- c()

    for (appt_perc in appointment_percents) {
        monthly_pay_at_appt_level <- gsr_level$monthly * appt_perc
        # If target dollars is greater than 3 * monthly_pay_at_appt_level, then set target dollars to 3 * monthly_pay_at_appt_level (minus 1 dollar so not even 3 months)
        appt_scenario_dollars <- min(target_dollars, ((3*monthly_pay_at_appt_level) - 1))

        full_months_of_pay <- floor(appt_scenario_dollars/monthly_pay_at_appt_level)
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
                c("July", "August", "September1", "September2"),
                c("July", "August", "June", "September1", "September2"),
                c("July", "June", "August", "September1", "September2"),
                c("August", "July", "September1", "September2"),
                c("August", "September1", "September2")
            )
        } else if (full_months_of_pay == 2) {
            scenarios <- list(
                c("July", "August", "September1", "September2", "June"),
                c("July", "August", "June", "September1", "September2")
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
                effort             = character(0),
                salary             = double(0),
                fringe             = double(0),
                gael               = double(0),
                salary_fringe      = double(0),
                salary_fringe_gael = double(0)
            )
            scenario_dollars <- appt_scenario_dollars
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
                                    effort             = sprintf("%s%%", (100*appt_perc)),
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

                scenario_key <- paste0(c(
                    as.character((100*appt_perc)),
                    paste0(df$month, collapse = "::"),
                    paste0(df$days, collapse = "::"),
                    paste0(df$salary, collapse = "::")
                ), collapse = "|")

                if (!(scenario_key %in% scenario_keys)) {
                    # This is a unique scenario
                    scenario_keys <- c(scenario_keys, scenario_key)
                    idx <- idx + 1

                    pct <- as.integer(gsub("^(\\d+)%.*", "\\1", df$percent[[1]]))
                    key <- sprintf("%09i :: %03i :: %05i", round(sum(df$salary), 0), pct, idx)
                    dfs[[key]] <- df
                }
            }
        }
    }
    dfs <- sort_and_rename_scenarios(dfs, target_dollars = target_dollars)


    return(dfs)
}

dollar_format <- function(x) {
    return(formatC(as.numeric(x), format="f", digits=2, big.mark=","))
}

sort_and_rename_scenarios <- function(dfs, target_dollars) {
    # Sort by higest salary (descending)
    dfs <- dfs[rev(sort(names(dfs)))]

    # Rename the scenarios
    sc_idx <- 0
    for(sn in names(dfs)) {
        df <- dfs[[sn]]
        current_scenario_name <- df$percent[[1]]
        pct <- gsub("^(\\d+)%.*", "\\1", current_scenario_name)
        sc_idx <- sc_idx + 1
        total_salary <- sum(df$salary, na.rm = TRUE)
        dollars_left <- round(target_dollars - total_salary, 2)
        # new_scenario_name <- sprintf("Scenario (%s / %s) ::  %s%% Effort :: Total Salary Paid: $%s ($%s Left Over)", sc_idx, length(dfs), pct, dollar_format(total_salary), dollar_format(dollars_left))
        new_scenario_name <- sprintf("Scenario (%s / %s) ::  %s%% Effort :: ($%s Left Over)", sc_idx, length(dfs), pct, dollar_format(dollars_left))
        df$percent <- new_scenario_name
        dfs[[sn]] <- df
    }

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

    res <- find_best_fit(
        target_dollars = 6000,
        gsr_level      = gsr_levels$V,
        is_july_ta     = FALSE,
        is_august_ta   = FALSE
    )
    print_df_list(res)

    debugonce(find_best_fit)
    res <- find_best_fit(
        target_dollars = 17000,
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
        effort             = as.double(NA),
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
                effort             = tdf$effort[[1]],
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
            effort             = gt::html("<b>Effort</b>"),
            salary             = gt::html("<b>Salary</b>"),
            fringe             = gt::html("<b>Fringe<br>Benefits</b>"),
            gael               = gt::html("<b>GAEL</b>"),
            salary_fringe      = gt::html("<b>Salary<br>+<br>Fringe</b>"),
            salary_fringe_gael = gt::html("<b>Salary<br>+<br>Fringe<br>+<br>GAEL</b>"),
        ) |>
        gt::sub_missing(missing_text = "") |>
        gt::tab_options(row_group.font.weight = "bold") |>
        #gt::tab_style(style = gt::cell_text(color = "#005500"), locations = gt::cells_body(columns = gt::everything(), rows = !is.na(max_days) & max_days == days)) |>
        gt::tab_style(style = gt::cell_text(color = "#000055", style = "italic"), locations = gt::cells_body(columns = gt::everything(), rows = !is.na(max_days) & max_days != days)) |>
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

        /*Shiny notify*/
        div.shiny-notification {
            border-radius: 10px;
            font-family: Arial;
            box-shadow: 0px 7px 20px 0px #2f2e2e;
            border-color: #444;
            opacity: 1;
            background-color: #DDDDDD;
            position: relative;
            left: undefined;
            right: undefined;
            bottom: undefined;
            top: 35%;
            margin: auto auto auto auto;
            width: 250px;
            height: auto;
            color: black;
        }
        #shiny-notification-panel {
            position: absolute;
            width: 100%;
            height: 100%;
            bottom: 0;
            right: 0;
            background-color: #646464a6;
            border: none;
            padding: 0px;
            z-index: 99999;
            color: black;
        }

        .progress.active {
            border: 1px solid #555555;
            margin-right: 20px;
            height: 8px;
            color: black;
        }
        .progress-bar {
            background-color: blue;
            height: 8px;
            color: black;
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







