dashboardSidebar(
    sidebarMenu(
        id = "menu",
        menuItem(
            "Upload",
            tabName = "upload",
            icon = icon("house")
        ),
        menuItem(
            "Data Table",
            tabName = "datatable",
            icon = icon("database")
        ),
        menuItem(
            "Dashboard",
            tabName = "dashboard",
            icon = icon("gauge"),
            menuSubItem(
                "Time Series",
                tabName = "timeseries",
                icon = icon("timeline")
            ),
            menuSubItem(
                "Multivariable",
                tabName = "multivariable",
                icon = icon("puzzle-piece")
            )
        ),
        menuItem(
            "Setting",
            tabName = "setting",
            icon = icon("gear")
        ),
        menuItem(
            "Source Code",
            icon = icon("github"),
            href = "https://github.com/hxtmike/BehaviorViz"
        )
    )
)