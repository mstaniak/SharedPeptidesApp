server <- function(input, output) {

    output$summary_table = renderDataTable({
        as.data.frame(peptides_summary)
    })
    summary_data = reactive({
        id = input$summary_table_row_last_clicked
        if(!is.null(id)) {
            row = peptides_summary[id, ]
            all_ms1 %>%
                inner_join(row)
        }
    })
    output$summary_mz = renderUI({
        id = input$summary_table_row_last_clicked
        if(!is.null(id)) {
            center = unique(summary_data()[["target_precursor_mz"]])
            # min_mz = min(summary_data()[["mz"]])
            # max_mz = max(summary_data()[["mz"]])
            min_mz = center - 10
            max_mz = center + 10
            default_value = c(center - 3, center + 3)
            sliderInput("slide_mz", "m/z range:", min_mz, max_mz, default_value)
        }
    })
    output$detail_plot = renderPlot({
        id = input$summary_table_row_last_clicked
        if(!is.null(id)) {
            # req(input$slide_mz)
            df = summary_data()
            df = filter(df,
                        mz > input$slide_mz[1],
                        mz < input$slide_mz[2])
            if(input$only_centroid) {
                df = filter(df, is_max)
            }
            seq = unique(df[["sequence"]])
            charge = unique(df[["charge"]])
            isotopic = data.frame(mz = useBRAIN(getAtomsFromSeq(seq))[["masses"]])
            isotopic[["mz"]] = isotopic[["mz"]]/charge + 1.007276
            ggplot(df, aes(x = mz, ymin = 0, ymax = intensity, color = is_max)) +
                geom_linerange() +
                geom_point(data = mutate(isotopic, intensity = 1),
                           aes(x = mz, y = intensity), inherit.aes = FALSE) +
                # geom_vline(data = isotopic, aes(xintercept = mz), color = "blue") +
                geom_vline(aes(xintercept = target_precursor_mz), color = "red",
                           size = 0.1) +
                theme_bw()
        }
    })
    output$full_spec = renderPlot({
        id = input$summary_table_row_last_clicked
        if(!is.null(id)) {
            df = summary_data()
            ggplot(df, aes(x = mz, ymin = 0, ymax = intensity)) +
                geom_linerange() +
                geom_vline(aes(xintercept = target_precursor_mz), color = "red",
                           size = 0.5) +
                theme_bw()
        }
    })
    output$brief_table = renderDataTable({
        in_brief
    })
    output$seq_plot = renderPlot({
        if(!is.null(input$brief_table_row_last_clicked)) {
            seq = in_brief[["sequence"]][input$brief_table_row_last_clicked]
            chosen_charge = in_brief[["charge"]][input$brief_table_row_last_clicked]
            all_ms1 %>%
                filter(sequence == seq, charge == chosen_charge) %>%
                filter(proteins == .[["proteins"]][1]) %>%
                filter(mz > target_precursor_mz - 5,
                       mz < target_precursor_mz + 5) %>%
                ggplot(aes(x = mz, ymin = 0, ymax = intensity)) +
                geom_linerange() +
                geom_vline(aes(xintercept = target_precursor_mz), color = "red",
                           size = 0.5) +
                theme_bw() +
                facet_wrap(rep~precursor_scan, scales = "free")
        }
    })

    output$iso_summary = renderDataTable({
        iso_summary
    })

    output$iso_dist = renderDataTable({
        row_id = input$iso_summary_row_last_clicked
        if(!is.null(row_id)) {
            seq = iso_summary[row_id, ]
            iso_dist = useBRAIN(getAtomsFromSeq(seq))
            row_detail_id = input$iso_tbl_row_last_clicked
            if(is.null(row_detail_id)) {
                out = as.data.frame(iso_dist)
            } else {
                charge = unique(iso_tbl_df()$charge[row_detail_id])
                out = as.data.frame(iso_dist) %>%
                    mutate(mz = masses/charge + 1.007276)
            }
            out %>%
                mutate_all(function(x) round(x, 4))
        }
    })

    iso_tbl_df = reactive({
        if(is.null(input$iso_summary_row_last_clicked)) {
            iso
        } else {
            iso %>%
                filter(sequence == iso_summary$sequence[input$iso_summary_row_last_clicked])
        }
    })

    output$iso_tbl = renderDataTable({
        iso_tbl_df()
    })

    output$iso_plot = renderPlot({
        row_id = input$iso_tbl_row_last_clicked
        if(!is.null(row_id)) {
            current_ms1 = iso_tbl_df()$precursor_scan[row_id]
            iso %>%
                filter(precursor_scan == current_ms1) %>%
                ggplot(aes(x = mz, ymin = 0, ymax = intensity)) +
                    geom_linerange() +
                    theme_bw() +
                    facet_wrap(~as.character(target_precursor_mz), scales = "free")
        }
    })
}

