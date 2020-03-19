ui <- fluidPage(
    titlePanel("Shared peptides"),

    tabsetPanel(
        tabPanel("brief summary",
                 plotOutput("seq_plot"),
                 dataTableOutput("brief_table")),
        tabPanel("summary",
                 plotOutput("detail_plot"),
                 uiOutput("summary_mz"),
                 checkboxInput("only_centroid", "Show only centroids",
                               value = TRUE),
                 dataTableOutput("summary_table"),
                 plotOutput("full_spec")),
        tabPanel("Isotopic peaks",
                 fluidRow(column(4, dataTableOutput("iso_summary")),
                          column(4, offset = 3, dataTableOutput("iso_dist"))),
                 dataTableOutput("iso_tbl"),
                 plotOutput("iso_plot"))
    )
)
