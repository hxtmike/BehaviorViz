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
                "Ending Results",
                tabName = "endingresults",
                icon = icon("puzzle-piece")
            )
        ),
        menuItem(
            "Source Code",
            icon = icon("github"),
            href = "https://github.com/hxtmike/BehaviorViz"
        ),
        menuItem(
            "Donation (if you like this)",
            icon = icon("heart"),
            href = "https://github.com/sponsors/hxtmike/"
        )
    )
)