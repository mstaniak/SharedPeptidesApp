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
                 plotOutput("full_spec"))
    )
)
