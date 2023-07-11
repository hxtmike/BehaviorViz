fc_null_if_zero <- function(value) {
    if (is.na(value) || value == 0) {
        return(NULL)
    } else {
        return(value)
    }
}

fc_null_if_none <- function(value) {
    if (value == "None") {
        return(NULL)
    } else {
        return(value)
    }
}

fc_ggplotly_if_gg_else_value <- function(value, width, height) { # nolint: object_usage_linter.
    if (class(value)[1] != "shiny.tag") {
        value %>%
            ggplotly( # nolint: object_usage_linter.
                width = width,
                height = height
            )
    } else {
        value
    }
}