library(plotly)
# talk to NASA spectral imaging working group r/e gaps



visualize_prediction <- function(filepath, key_file, column){
    require(leaflet)
    color_map <- create_color_map(key_file, column)
    labels <- create_labels(key_file, column)
    layer <- raster::raster(filepath)
    epsg_code <- 3857
    layer_projected <- project_to_epsg(
        layer, 
        epsg_code, 
        categorical_raster = TRUE)
    map <- leaflet::leaflet() %>%
        leaflet::addTiles(
            'https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png',
             options = providerTileOptions(
                minZoom = 8, 
                maxZoom = 100)) %>%
        leaflet::addRasterImage(
            layer,
            layerId = "layer",
            colors = color_map) %>%
        leaflet::addLegend(
            "bottomleft",
            colors = color_map(labels), 
            labels = labels,
            opacity = 1) %>%
        leaflet.opacity::addOpacitySlider(layerId = "layer")
    return(map)
}

create_color_map <- function(filepath, column){
    levels <- unlist(read.csv(filepath, header = TRUE)[,2])
    num_levels <- length(unique(levels))
    palette <- leaflet::colorFactor(
        grDevices::topo.colors(num_levels), 
        sort(levels))
    return(palette)
}

create_labels <- function(filepath, column){
    return( sort(unique(unlist(read.csv(filepath, header = TRUE)[column]))))
}



plot_quadrat_counts <- function(quadrat_aggregate, filter_missing = TRUE){
    data <- data.frame(quadrat_aggregate)
    if(filter_missing){
        data <- filter_aggregate(quadrat_aggregate)
    }

    plot <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(
        x=data$key,
    ))

    return ( plot )
}

plot_quadrat_proportions <- function(quadrat_aggregate, filter_missing = TRUE, plot_options = list(
    title = "Prediction and Validation",
    xLabel = "Plant Functional Type",
    yLabel = "Proportion",
    legend = c("Prediction", "Validation"),
    legendTitle = ""
)){
    data <- data.frame(quadrat_aggregate)
    if(filter_missing){
        data <- filter_aggregate(data)
    }

    #print("Checking Predicted Proportions (should sum to 1)")
    #print(sum(data$prediction_prop))
    #print("Checking Validation Proportions (should sum to 1)")
    #print(sum(data$validation_prop))

    data_tall <- tidyr::pivot_longer(
        data = data,
        cols = ends_with("_prop"),
        names_to = "Legend",
        values_to = "Proportion"
    )

    plot <- ggplot2::ggplot(data = data_tall, mapping = ggplot2::aes(
        key,
        Proportion,
        fill = Legend
    )) + 
    theme_minimal() + 
    #scale_fill_manual(values=c("#8aedff", "#a3000b")) + 
    labs(title = plot_options$title, x = plot_options$xLabel, y = plot_options$yLabel) + 
    scale_fill_discrete(name = plot_options$legendTitle, labels = plot_options$legend) +

    geom_bar(stat="identity", position = position_dodge()) + 
    theme(axis.text.x = element_text(angle=90,hjust=1,vjust=1))

    return( plot )
}


define_plot_options <- function(
    title = "",
    xLabel = "Plant Functional Type",
    yLabel = "Proportion",
    legend = c("Prediction", "Validation"),
    legendTitle = "Legend",
    save_location = NULL
){
    return( list(
        title = title,
        xLabel = yLabel,
        yLabel = xLabel,
        legend = legend,
        legendTitle = legendTitle,
        saveLocation = save_location
    ))
}

# plots the raster object wioth ggplot using theme_void


#Aboitic: #ffffff
#Forb: #db2a53
#Graminoid: #c9ae69
#Lichen: #faf87d
#Moss: #7dfaf8
#ShrubDecid: #69876b
#EvergreenShrub: #db2ad2
#TreeBroadleaf: #03fc2c
#TreeConifer: #2ac4db
#Unknown: #000000


fg1_palette <- c(
        "#ffffff",
        "#db2a53",
        "#c9ae69",
        "#faf87d",
        "#7dfaf8",
        "#69876b",
        "#db2ad2",
        "#03fc2c",
        "#2ac4db",
        "#000000"
)

fg0_names <- c(
    "Abiotic",
    "BroadleafDecid",
    "ConiferEvergreen",
    "Forb",
    "Graminoid",
    "Lichen",
    "Moss",
    "Unknown"
)

fg1_names <- c(
    "Abiotic",
    "Forb",
    "Graminoid",
    "Lichen",
    "Moss",
    "ShrubDecid",
    "ShrubEvergreen",
    "TreeBroadleaf",
    "TreeConifer",
    "Unknown"
)

fg1_breaks <- c(
    '0','1','2','3','4','5','6','7','8','9'
)

fg1_palette_map <- function(value) {
    # note: value is 0-indexed and R is 1-indexed
    return (fg1_palette[[value]])
}


plot_categorical_raster <- function(ras,  plot_options, colors = fg1_palette) {
    ras_plot <- rasterVis::gplot(ras, 100000000) + 
        theme_classic() +
        labs(
            title = plot_options$title,
            x = plot_options$xLabel,
            y = plot_options$yLabel,
            color = "Plant Type"
            ) + 
        scale_fill_manual(
            breaks = fg1_breaks,
            labels = fg1_names,
            values = fg1_palette, 
            name = "Functional Type"
            ) + 
        geom_tile(aes(
            fill = factor(
                value,
                ordered = FALSE))) + 
        coord_quickmap()


    return(ras_plot)
}



#' Lone line explanation
#'
#' Long Description here
#'
#' @return 
#' @param x
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
plot_agg_results <- function(df, save_file = "Output/results.jpeg") {
    my_plot <- ggplot2::ggplot(data = df) +
        geom_point(aes(df$x, df$y, color=df$z))
    print(my_plot)
    return(my_plot)
}



create_plot <- function(df, pft, legend = FALSE){
    filtered_df <- df[df$key == pft,]
    return(
        plot_ly(filtered_df) %>%
            plotly::add_markers(
                x = ~validation_prop,
                y = ~prediction_prop,
                color = ~site,
                data = filtered_df,
                showlegend = legend
            ) %>% plotly::layout(
                title = pft,
                xaxis = list(
                    title = "Ground Truth Proportion",
                    range = c(0,1)
                ),
                yaxis = list(
                    title = "Predicted Proportion",
                    range = c(0,1)

                ),
                annotations = list(x = 0.0 , y = 1.1, text = pft, showarrow = FALSE, 
                    xref='paper', yref='paper')

            ) %>% plotly::add_lines(
                x = c(0,1),
                y = c(0,1),
                color=I("black"),
                showlegend = FALSE
            )
    )
}

plot_by_pft <- function(df, save_path = NULL, open = TRUE, image_path = NULL, aggregation=1){
    df <- df %>% group_by(site)
    plots <- list()
    if(aggregation == 1){
        plots <- list(
            create_plot(df, "Abiotic", legend = TRUE),
            create_plot(df, "Forb"),
            create_plot(df, "Graminoid"),
            create_plot(df, "Lichen"),
            create_plot(df, "Moss"),
            create_plot(df, "ShrubDecid"),
            create_plot(df, "ShrubEvergreen"),
            create_plot(df, "TreeBroadleaf"),
            create_plot(df, "TreeConifer")
        )
    } else if( aggregation == 0){
        plots <- list(
            create_plot(df, "Abiotic", legend = TRUE),
            create_plot(df, "Forb"),
            create_plot(df, "Graminoid"),
            create_plot(df, "Lichen"),
            create_plot(df, "Moss"),
            create_plot(df, "BroadleafDecid"),
            create_plot(df, "ConiferEvergreen")
        )
    } # should eventually implement the other PFTs, eh?

    fig <- plotly::subplot(
        plots,
        nrows = 5,
        margin = 0.05)
    fig <- fig %>% plotly::layout(
        title = "Prediction and Ground Truth Labels",
        scene = list(
                    aspectratio = list(
                        x = 1,
                        y = 1
                    )
                ),
        hovermode = TRUE
    )

    fig_save_loc <- paste(tempfile('plotly_fig'), 'html', sep = '.')
    if(!is.null(save_path)){
        fig_save_loc <- save_path
    }
    htmlwidgets::saveWidget(fig, fig_save_loc, selfcontained = FALSE)
    if(open){
        browseURL(fig_save_loc)
    }

    if(!is.null(image_path)){
        img_content <- plotly::plotly_IMAGE(
            fig, 
            file = image_path
        )
    }

    return(fig)
}

calculate_r_squared <- function(
    df, 
    independent_var, 
    dependent_var){
    linear_model <- lm(
        df[,dependent_var] ~ df[,independent_var]
    )

    
    return(summary(linear_model)$r.squared)
}



write_validation_table <- function(
    df, 
    save_path=NULL, 
    target_variable = "site",
    grouping_variable = "key",
    open=FALSE
    ){
    
    base_html <- readr::read_file("assets/table_template.html")

    row_values <- unique(df[,grouping_variable]) %>% as.character()
    column_values <- unique(df[,target_variable]) %>% as.character()

    row_string <- "<thead><tr><th>&nbsp</th>"
    for(value in column_values){
        row_string <- paste0(row_string, "<th>", value, "</th>")
    }
    row_string <- paste0(row_string, "</tr></thead>\n<tbody>")

    for(row in row_values){
        col_filtered_df <- df[df[,grouping_variable] == row,]
        row_string <- paste0(row_string, "<tr>", "<td>", row, "</td>")
        for(col in column_values){

            filtered_df <- col_filtered_df[col_filtered_df[,target_variable]==col,]
            r_squared <- calculate_r_squared(
                filtered_df,
                "validation_prop",
                "prediction_prop")
            row_string <- paste0(row_string, "<td>", r_squared, "</td>")
        }

        row_string <- paste0(row_string, "</tr>\n")
    }
    html <- stringr::str_replace(base_html, stringr::fixed("{{table}}"), row_string)

    save_location <- save_path
    if(is.null(save_path)){
        save_location <- tempfile(pattern = "_table.html")
    } 

    write(html, file = save_location)

    if(open){
        utils::browseURL(save_location)
    }
}

