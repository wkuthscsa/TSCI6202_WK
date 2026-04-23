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


}
