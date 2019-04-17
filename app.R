future_value<-function(amount,rate,years){
  amount_after=amount*(1+rate)^years
  return (amount_after)
}
annuity<-function(contrib,rate,years){
  annuity_future=contrib*(((1+rate)^years-1)/rate)
  return (annuity_future)
}
growing_annuity<-function(contrib,rate,growth,years){
  growth_a=contrib*(((1+rate)^years-(1+growth)^years)/(rate-growth))
  return (growth_a)
}

library(ggplot2)
library(shiny)
# Define UI for slider demo application
ui=fluidPage(
  #  Application title
  titlePanel("Investment calculation APP"),
  # Sidebar with sliders that demonstrate various available options
  fluidRow(
    column(4,sliderInput("initial_amount", "Initial Amount", 
                         min=1, max=100000,value=1000,step=500,pre="$",sep=",",animate = TRUE
    ),
    sliderInput("annual_contribution", "Annual Contribution", 
                min=0, max=50000,value=2000,step=500,pre="$",sep=",",animate = TRUE
    )
    ),
    column(4,sliderInput("return_rate", "Return Rate (in %)", 
                         min=0, max=20,value=5,step=0.1,sep=",",animate = TRUE
    ),sliderInput("growth_rate", "Growth Rate (in %)", 
                  min=0, max=20,value=2,step=0.1,sep=",",animate = TRUE
    )),
    column(4,
           sliderInput("year", "Years", 
                       min=0, max=50,value=10,step=1,sep=",",animate = TRUE
           ),
           selectInput('facet', 'Facet?', c("No", "Yes")))
  ),
  #conditionalPanel("input.facet=No",plotOutput("scatterPlot",height=300)),
  #   # Show a table summarizing the values entered
  hr(),
  h4('Timelines'),
  plotOutput(outputId = "scatterplot",width=1000),
  hr(),
  h4('Balances'),
  #tableOutput(outputId = "future_value")
  verbatimTextOutput('future_value')
)


# Define server logic for slider examples
server=function(input, output) {
  modality=reactive({
    no_contrib=c()
    for (i in 1:(input$year+1)){
      no_contrib[i]=future_value(input$initial_amount,i-1,input$return_rate/100)
    }
    fixed_contrib=c()
    for (i in 1:(input$year+1)){
      fixed_contrib[i]=future_value(input$initial_amount,i-1,rate=input$return_rate/100)+annuity(contrib = input$annual_contribution,rate=(input$return_rate)/100,years=i-1)
    }
    
    growing_contrib=c()
    for (i in 1:(input$year+1)){
      growing_contrib[i]=future_value(input$initial_amount,i-1,rate=input$return_rate/100)+growing_annuity(input$annual_contribution,rate=input$return_rate/100,growth = input$growth_rate/100,years=i-1)
    }
    no_contrib=as.data.frame(no_contrib)
    fixed_contrib=as.data.frame(fixed_contrib)
    growing_contrib=as.data.frame(growing_contrib)
    year=matrix(seq(0,input$year),input$year+1,1)
    modality=cbind(year,no_contrib,fixed_contrib,growing_contrib)
    modality=as.data.frame(modality)
    modality
  })
  
  modality2=reactive({
    combine=as.data.frame(matrix(0,3*(input$year+1),3))
    for (i in 1:(input$year+1)){
      combine[i,1]=future_value(input$initial_amount,i-1,input$return_rate/100)
      combine[i+input$year+1,1]=future_value(input$initial_amount,i-1,rate=input$return_rate/100)+annuity(contrib = input$annual_contribution,rate=(input$return_rate)/100,years=i-1)
      combine[i+2*(input$year+1),1]=future_value(input$initial_amount,i-1,rate=input$return_rate/100)+growing_annuity(input$annual_contribution,rate=input$return_rate/100,growth = input$growth_rate/100,years=i-1)
    }
    combine[,2] = c(rep("no_contirb", input$year+1), rep("fixed_contrib", input$year+1), rep("growing_contrib", input$year+1))
    combine[,3]=c(rep(0:input$year,3))
    
    names(combine)=c("Amount","mode","year")
    combine
    
    
  })
  
  output$scatterplot=renderPlot({
    if (input$facet=="No"){
      ggplot(modality(),aes(x=year,y=value)) +
        labs(title="future amount of three modes")+
        geom_line(data = modality(), aes(x = year, y = no_contrib, color = "no_contrib"))+
        geom_line(data = modality(), aes(x = year, y =fixed_contrib, color = "fixed_contrib"))+
        geom_line(data = modality(), aes(x = year, y = growing_contrib, color = "growing_contrib"))+
        geom_point(data = modality(), aes(x = year, y = no_contrib, color = "no_contrib"))+
        geom_point(data = modality(), aes(x = year, y =fixed_contrib, color = "fixed_contrib"))+
        geom_point(data = modality(), aes(x = year, y = growing_contrib, color = "growing_contrib"))+theme_bw()
    }
    else{
      ggplot(data = modality2(),aes(x=year,y=value))+
        # labs(title="Three modeds of investing")+
        geom_line(data = modality2(), aes(x = year, y = Amount, color=mode))+
        geom_point(data = modality2(), aes(x = year, y = Amount, color = mode), size=0.7)+
        facet_grid(.~mode)+geom_area(data=modality2(),aes(x=year,y=Amount,color=mode,fill=mode),alpha=0.5)+
        xlab("year")+
        ylab("value")
      #scale_color_discrete(name = "modality", labels = c("no_contri", "fixed_contri","growing_contri"))
    }
  })
  
  
  output$future_value=renderPrint({
    modality()
    
  })
}


#
shinyApp(ui=ui,server=server)
# 