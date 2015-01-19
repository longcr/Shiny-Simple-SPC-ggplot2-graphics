##### server.R #####


# load packages ---------------------------------------------------------------

library(shiny)
library(reshape2)
library(qcc)
library(plyr)
library(chron)
library(ggplot2)
library(gridExtra)
library(scales)


# load the data ---------------------------------------------------------------

wd.datapath = paste0(getwd(),"/data")
wd.init = getwd()
setwd(wd.datapath)

d.in = read.table("data by bu fac metric.txt", sep = '\t', header = TRUE)

setwd(wd.init)


# convert year and month into date 
d.in$date = as.Date(ISOdate(d.in$year, d.in$monthnum, 1))


# select SPC response variable (using proportions) ----------------------------

# shiny server body -----------------------------------------------------------

shinyServer(function(input, output, session) { 
  
  output$ui <- renderUI({
    sidebarPanel(

      includeHTML("include.html"),

      selectInput(inputId = "in.bu", 
                  label = "Choose Business Unit", 
                  choices = levels(d.in$business_unit),
                  selectize = FALSE),
            
      
      selectInput(inputId = "in.facility", 
                  label = "Choose Facility", 
                  "",
                  selectize = FALSE),
            
      
      selectInput(inputId = "in.metric", 
                  label = "Choose Metric",
                  choices = c("Metric 1", "Metric 2"),
                  selectize = FALSE)
            
  ) })

  
  # this section is used to change the facility list based on the business unit
  # placed outside of the 'ui 'call but within the 'server' call
  # example is here:
  # http://stackoverflow.com/questions/21465411/r-shiny-passing-reactive-to-selectinput-choices
  
  outVar = reactive({
    levels(droplevels(d.in[d.in$business_unit == input$in.bu,]$facility))
  })
  
  observe({
    updateSelectInput(session, "in.facility",
                      choices = outVar()
    )})
  

  
# Individuals (X) chart -------------------------------------------------------

  output$plotDisplay <- renderPlot({

    # GET REACTIVE DATA -----
    
    # select subset for specific business unit AND facility
    
    strata.bu = input$in.bu
    strata.facility = input$in.facility
    strata.metric = input$in.metric
    
    
    # FOR TESTING without Shiny
    # strata.bu = "busunit_1"
    # strata.facility = "facility_701"

    
    
    # subset by business and facility
    d.strata = subset(d.in, d.in$business == strata.bu & d.in$facility == strata.facility) 
    
    
    # clean up the factor levels - drops unused factor levels within the subset
    # levels(d.strata$business); levels(d.strata$facility)
    d.strata = droplevels(d.strata)
    
  
    # ensure data is ordered by week
    d.strata = d.strata[order(d.strata$date, decreasing = FALSE),]  
    
    
    # set metric
        
    # FOR TESTING without Shiny
    # strata.metric = "Metric 1"
    
    if (strata.metric == "Metric 1") {qccvar = "metric_1"}
    if (strata.metric == "Metric 2") {qccvar = "metric_2"}
    if (strata.metric == "Metric 3") {qccvar = "metric_3"}

    
    
    
    # THIS SECTION ALLOWS US TO SELECT SUBSET OF DATA TO USE FOR CALCULATING SPC LIMITS
    
    # TEMPORARY ALTERNATIVE #1
    # get subset of weekly data based on last 6 months
    # sub.limit.dat = subset(d.strata, date >= max(date)-180)
    
    
    # TEMPORARY ALTERNATIVE #2
    baseline.window.begin.date = as.Date("2013-11-01")
    baseline.window.end.date = as.Date("2014-10-31")
    
    sub.limit.dat = subset(d.strata, date >= baseline.window.begin.date & date <= baseline.window.end.date)
    
    # ALTERNATIVE #3
    # Could use a Shiny slider to select the window.
    
    
    # NOTE - using only a few periods for an SPC chart is not usually recommended
    # We take this approach in this code only to show how to use a subset of the
    # data to get SPC limits that are then applied to the larger set of data.
    # For more info, see text on SPC; recommend Montgomery.
    
    
    # used to calculate SPC limits based on subset
    qcc.limits = qcc(sub.limit.dat[,qccvar], 
                     type = 'xbar.one', 
                     plot = FALSE,
                     labels = sub.limit.dat$date)  # kept labels in case needed to display graph
    
    
    
    # CREATE SPC CHART

    # Generates qcc object that is later used in ggplot graphics
    # MODIFIED SPC CODE - uses precalculated SPC control chart limits based on defined range above
    # Individuals (X) chart
    qcc.chart = suppressWarnings( qcc( d.strata[,qccvar], 
                                       type = 'xbar.one', 
                                       plot = TRUE, 
                                       center = qcc.limits$center, 
                                       limits = qcc.limits$limits,
                                       labels = d.strata$date)  # end qcc
    )  # end supressWarnings
    
    # Not included - MR chart
    
    
    ### NEW SPC GRAPHICS CODE #################################################
    # use ggplot2
    # based on example from: http://tomhopper.me/tag/spc/
    
    
    # prune the data for spc charting (dates, values)
    spc.data <- data.frame(x = factor(d.strata$date), y = d.strata[,qccvar])
    
    
  
    # used for convenient formatting of x-axis in SPC chart
    spc.data$xdate = as.Date(spc.data$x)
    

    
    # THE CODE FOR THE FANCY SPC CHART --------------------------------------
    
    q.center = qcc.limits$center 
    q.limits = qcc.limits$limits 
    
    v.limits = qcc.chart$violations$beyond.limits
    v.runs = qcc.chart$violations$violating.runs
    
    
    
    # library(ggplot2)
    # continuing with creating the plot
    
    p.size = 4    # controls point size in plot
    
    
    # the main plot
    spc.ggplot <- ggplot(spc.data, aes(x = xdate, y = y)) + 
      geom_line(aes(x = xdate, y = y)) +                              # adds lines between points
      geom_point(shape = 20, size = (p.size*1.7), color="white") +    # space between dots and lines
      geom_point(shape = 20, size = p.size) +                         # adds the dots
      scale_x_date(breaks = "3 months") +                              # uses subset of dates on x-axis
      scale_y_continuous(labels = percent) +                          # formats y-tick labels as pct
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +    # rotates axis text
      theme(axis.title.x = element_blank()) +                         # suppress x-axis label
      ylab("Percent") 
    
    
    # adds horizontal lines for SPC control chart limits
    spc.ggplot <- spc.ggplot + 
      geom_hline(yintercept = q.center, linetype = "dotted") +     # adds center line
      geom_hline(yintercept = q.limits[1], linetype = "dashed") +  # adds LCL
      geom_hline(yintercept = q.limits[2], linetype = "dashed")    # adds UCL

    
    # identify runs-rules violations with orange point
    
    v.runs.subdata <- spc.data[v.runs,]
    
    spc.ggplot <- spc.ggplot +
      geom_point(data = v.runs.subdata, aes(x = xdate, y = y), size = p.size, col = "orange")
    
    
    # identify limits violations with red point
    
    v.limits.subdata <- spc.data[v.limits,]
    
    spc.ggplot <- spc.ggplot +
      geom_point(data = v.limits.subdata, aes(x = xdate, y = y), size = p.size, col = "red")
    
    
    # adds chart title
    
    title.text = paste("Individuals SPC Chart for ", 
                       strata.facility, 
                       "\nData through week of",
                       max(spc.data$xdate),
                       "\nLimits calculated between",
                       min(sub.limit.dat$date), 
                       "and",
                       max(sub.limit.dat$date) ) 
    
    
    spc.ggplot <- spc.ggplot + ggtitle(title.text)
    
    
    # renders the final plot
    # print(spc.ggplot)
    
    
    # ADD MARGINAL HISTOGRAM or DENSITY PLOT
    # From: http://stackoverflow.com/questions/8545035/scatterplot-with-marginal-histograms-in-ggplot2?rq=1
    
    x.labels = levels(spc.data$x) # used in a hack to get plots to align
    
    dens.left <-  ggplot(spc.data) + geom_density(aes(y), fill = 'darkgray') +  
      scale_y_discrete(breaks = 1, labels = x.labels[1]) + 
      coord_flip() +                                                # rotates the plot
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +    # rotates axis text
      theme(axis.text.x = element_text(color="white")) +            # see note A
      theme(axis.title.x = element_blank()) +                       # suppresses x-axis title
      theme(axis.ticks = element_blank()) +                         # suppresses tick marks
      theme(axis.title.y = element_blank())                         # suppresses y-axis title
    
    dens.left <- dens.left + 
      ggtitle(title.text) +
      theme(plot.title = element_text(color = "white"))             # a hack to get plot tops to align
    
    
    
    # Note A:  This is a 'hack' to get the vertical dimension of the density plot to match
    #          those of the SPC chart.  It uses the date-text from the SPC chart
    #          on the bottom axis of the density plot, flips the text 90 degrees.
    #          This is needed due to the alignment of the dates on the SPC chart x-axis
    #          and the resulting effect on the SPC chart dimensions.
    
    
    # from: http://stackoverflow.com/questions/24765686/plotting-2-different-ggplot2-charts-with-the-same-y-axis
    # assign common axis to both plots
    
    spc.ylim = ggplot_build(spc.ggplot)$panel$ranges[[1]]$y.range
    spc.ymaj = ggplot_build(spc.ggplot)$panel$ranges[[1]]$y.major_source
    
    p1.common.y <- spc.ggplot + scale_y_continuous(limits = spc.ylim, breaks = spc.ymaj, labels = percent)
    p2.common.y <- dens.left + scale_x_continuous(limits = spc.ylim, breaks = spc.ymaj, labels = percent)
    
    
    # build the plots 
    p1.common.y <- ggplot_gtable(ggplot_build(p1.common.y))
    p2.common.y <- ggplot_gtable(ggplot_build(p2.common.y))
    
    # copy the plot height from p1 to p2
    p2.common.y$heights <- p1.common.y$heights
    
    
    
    # another method - uses gridExtra
    # library(gridExtra)
    # using print seems to overcome a difficulty when using Shiny with grid.arrange
    print( grid.arrange(arrangeGrob(p2.common.y, p1.common.y, ncol=2, 
                                    widths=c(1,4)), heights=c(1,1)) )
    
    
    

  })  # end renderPlot

})    # end shinyServer


### END CODE ###