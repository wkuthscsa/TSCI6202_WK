#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Define server logic required to draw a histogram
function(input, output, session) {
    
    # Per-Session App Data ----
    # global.R creates the ordinary objects.
    # server.R copies them into reactiveValues so later filters/widgets
    # can update the app's working data without changing the original objects.
    
    app_data <- reactiveValues(
        demographics = demographics,
        survivalmodel = survivalmodel
    )
    
    # Debug Button ----
    # This only matters if the optional Debug tab/button exists in ui.R.
    observeEvent(input$debug, {
        browser()
    })
    
    # Survival Plot ----
    # Demonstrates:
    # 1. Using reactiveValues data
    # 2. Letting the UI control the grouping variable
    # 3. Letting the UI toggle optional plot layers
    
    output$survivalPlot1 <- renderPlot({
        
        req(input$survival_group)
        
        demo_df <- app_data$demographics
        
        survival_formula <- as.formula(
            paste("survival ~", input$survival_group)
        )
        
        survival_plot <- survfit2(survival_formula, data = demo_df) %>%
            ggsurvfit() +
            ylab("Fraction Alive") +
            xlab("Years Since Birth")
        
        if ("censor" %in% input$survival_layers) {
            survival_plot <- survival_plot + add_censor_mark()
        }
        
        if ("confidence" %in% input$survival_layers) {
            survival_plot <- survival_plot + add_confidence_interval()
        }
        
        if ("quantile" %in% input$survival_layers) {
            survival_plot <- survival_plot + add_quantile()
        }
        
        if ("risktable" %in% input$survival_layers) {
            survival_plot <- survival_plot + add_risktable()
        }
        
        survival_plot
    })
    
    # output$survivalPlot1 <- renderPlot(
    #     survfit2(survival~STATE, demographics) %>%
    #         ggsurvfit() + ylab("Fraction Alive") +
    #         xlab("Years Since Birth") + scale_color_manual(values=c('red','darkgreen')) +
    #         add_censor_mark() +
    #         add_confidence_interval() +
    #         add_quantile() +
    #         add_risktable()
    # )
    # using the same pattern we established for survival, put your 'favorite' boxplot into the Relationships tab.
    
    # Relationship Plot ----
    # Demonstrates:
    # 1. Using reactiveValues data
    # 2. Letting the UI control aes()
    # 3. Letting the UI control geoms and scales
    # 4. Using .data[[...]] for programmable ggplot aesthetics
    
    output$relationshipPlot1 <- renderPlot({
        
        req(input$relationship_x)
        req(input$relationship_y)
        req(input$relationship_geom)
        req(input$relationship_y_scale)
        
        demo_df <- app_data$demographics
        
        if (input$relationship_color == "none") {
            relationship_plot <- ggplot(
                demo_df,
                aes(
                    x = .data[[input$relationship_x]],
                    y = .data[[input$relationship_y]]
                )
            )
        } else {
            relationship_plot <- ggplot(
                demo_df,
                aes(
                    x = .data[[input$relationship_x]],
                    y = .data[[input$relationship_y]],
                    color = .data[[input$relationship_color]]
                )
            )
        }
        
        if (input$relationship_geom == "jitter") {
            relationship_plot <- relationship_plot +
                geom_jitter(width = 0.15, alpha = 0.65)
        }
        
        if (input$relationship_geom == "point") {
            relationship_plot <- relationship_plot +
                geom_point(alpha = 0.65)
        }
        
        if (input$relationship_geom == "boxplot") {
            relationship_plot <- relationship_plot +
                geom_boxplot(outlier.alpha = 0.25)
        }
        
        if (isTRUE(input$relationship_smooth) &&
            is.numeric(demo_df[[input$relationship_x]]) &&
            is.numeric(demo_df[[input$relationship_y]])) {
            
            relationship_plot <- relationship_plot +
                geom_smooth(method = "loess", se = TRUE)
        }
        
        if (input$relationship_y_scale == "log10") {
            relationship_plot <- relationship_plot +
                scale_y_log10()
        }
        
        relationship_plot +
            labs(
                title = "Relationship Explorer",
                subtitle = "UI controls the mapped variables, geometry, smoothing, and scale.",
                x = input$relationship_x,
                y = input$relationship_y,
                color = ifelse(input$relationship_color == "none", NULL, input$relationship_color)
            ) +
            theme_classic()
    })
    
    # output$relationshipPlot1 <- renderPlot(
    #     ggplot(demographics, aes(x=RACE, y=timetoevent, color=GENDER)) +
    #         geom_boxplot(outliers=FALSE, notch=FALSE, color="black", fill='white') +
    #         geom_jitter(aes(color=as.numeric(timetoevent)), width=0.1) +
    #         #geom_jitter(color="brown", width=0.1, data=subset(demographics,STATE!="Massachusetts"),shape=22) +
    #         scale_color_continuous(palette = c("black","white"),
    #                                aesthetics = "color",
    #                                guide = "colourbar",
    #                                na.value = "red",
    #                                type = getOption("ggplot2.continuous.colour")) 
    # )
    #Extra challenge: create multiple plots, one after the other, in the same tab.
    output$relationshipPlot2 <- renderPlot(
        ggplot(demographics, aes(x=INCOME, y=HEALTHCARE_EXPENSES))+
            geom_point(color="darkgreen", size=0.5, alpha=0.11)+
            geom_smooth(fill="blue",color="darkred",alpha=0.2)+
            geom_smooth(method="lm", se=FALSE)+
            geom_abline(slope=1,intercept=0,color="darkorange",linetype=2)+
            theme_classic()
    )
    output$Geomap1<-renderTmap(
        {tmap_mode("view")
            tm_shape(fqhc_county2)+
                tm_polygons(fill="clinic_count_raw")+
                tm_shape(shape_file_street_address)+
                tm_dots(fill="darkgreen",fill_alpha=0.5, size=0.2)}
    )
}
