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
    output$survivalPlot1 <- renderPlot(
        survfit2(survival~STATE, demographics) %>%
            ggsurvfit() + ylab("Fraction Alive") +
            xlab("Years Since Birth") + scale_color_manual(values=c('red','darkgreen')) +
            add_censor_mark() +
            add_confidence_interval() +
            add_quantile() +
            add_risktable()
    )
    # using the same pattern we established for survival, put your 'favorite' boxplot into the Relationships tab.
    output$relationshipPlot1 <- renderPlot(
        ggplot(demographics, aes(x=RACE, y=timetoevent, color=GENDER)) +
            geom_boxplot(outliers=FALSE, notch=FALSE, color="black", fill='white') +
            geom_jitter(aes(color=as.numeric(timetoevent)), width=0.1) +
            #geom_jitter(color="brown", width=0.1, data=subset(demographics,STATE!="Massachusetts"),shape=22) +
            scale_color_continuous(palette = c("black","white"),
                                   aesthetics = "color",
                                   guide = "colourbar",
                                   na.value = "red",
                                   type = getOption("ggplot2.continuous.colour")) 
    )
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
