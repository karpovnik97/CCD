server <- function(input, output) {
  
  # Rendering different controls for tabs
  
  output$sidebarinput <-  renderUI({
    
    if (input$tabs == "FA") { # Forest area tabs will use indicator selected by user to render plot
      
      selectInput(
        inputId = "FC_I",
        label = "Select Indicator:",
        choices = unique(Forest_Area$Indicator),
        selected = "Forest area")
      
    } else if (input$tabs == "USA-TS") { # this tab will render the plot showing species with statuses that user desires
      
      checkboxGroupInput("USA-TS-Status", 
                         label="Select Species Status:", 
                         choices = Choices_USA,
                         selected = c("Under Review","Endangered",
                                      "Species of Concern", "Threatened"),
                         inline = TRUE )
      
    } else if (input$tabs == "EU-TS") { # here user has option to select countries
      
      selectInput(
        inputId = "EU_C",
        label = "Select Country:",
        choices = unique(Threatened_Species$`Country Name`),
        selected = c("Germany", "France", "Italy", "Spain","Netherlands",
                     "Belgium","Sweden", "Austria", "Greece", "Portugal"),
        multiple = TRUE )
      
    }
    
    
  })
  
  ## EU Species Plots
  
  output$Species <- renderPlotly({ # this plot will depict factors that made species endangered
    
    ggplot(Threatened_Species %>% filter(`Country Name` %in% input$EU_C))+
      aes(x=featuretype,fill=`Threat Group`)+
      geom_bar(position = "fill")+
      labs(y="Percentage %",x="")+
      ggtitle("What threatens species & their habitats?")+
      coord_flip()+
      scale_fill_viridis(option = "D",discrete = TRUE)+
      theme(  # here i used custom theme to center the plot title and make it nicer
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black")
      )
  })  # Agriculture and using land for settlement are main factors endangering species and their habitats 
  
  
  output$Species2 <- renderPlotly({ # Plot showing distribution of threat factors in selected countries 
    
    ggplot(Threatened_Species %>% filter(featuretype == "species", `Country Name` %in% input$EU_C))+
      geom_bar(position = "fill")+
      aes(x=`Country Name`,fill=`Threat Group`)+
      labs(x="County",y="Frequency")+ 
      coord_flip()+
      theme_bw()+ 
      ggtitle("Are Threat Groups simmilary distributed in EU Countries?")+
      scale_fill_viridis(option = "D",discrete = TRUE)
    
  }) # It could be said that the factors endangering species are evenly distributed in EU countries, execpt Sweden
  
  
  output$USA_TS1 <- renderPlotly({ # Plot showing how frequent selected species statuses are un USA - How many species are endangered...
    
    ggplot(Species_USA %>% filter(`ESA Listing Status` %in% input$`USA-TS-Status`) )+
      aes(x=reorder(`ESA Listing Status`,`ESA Listing Status`,FUN=length),fill=`Species Group`)+
      geom_bar()+
      coord_flip()+
      ggtitle(paste(paste(input$`USA-TS-Status`, collapse = ", "),"Species in USA"))+
      labs(y="Frequency",x="Status")+
      theme_bw()+
      scale_fill_viridis(discrete=TRUE,option = "A") # color pallet for better appearance
    
  }) # most of the endangered species are mammals, most of the species of concern are flowering plans
  
  output$USA_TS2 <- renderPlotly({  # Plot showing how species with different statuses are distributed 
    
    ggplot(Species_USA %>% filter(`State Name` %in% input$USATS & `ESA Listing Status` %in% input$`USA-TS-Status`))+
      aes(x=`State Name`,fill=`ESA Listing Status`)+
      geom_bar(alpha=0.7)+
      ggtitle(paste(paste(input$`USA-TS-Status`, collapse = ", "),"Species in",paste(input$USATS, collapse = ", ")))+
      labs(y="Frequency",x="Status")+
      theme_bw()+
      scale_fill_viridis(discrete=TRUE,option = "C")
    
  }) # user can easily compare trend in different states and see that number of listed species in terms of
  #listing status are more or less evenly distributed
  
  # This plot will have several options for fill argument, user will be able to select using with variable to fill the bars
  
  output$USA_TS3 <- renderPlotly({ # Plot showing at witch date were species listed in endangered species list
    
    if (input$USA_TS_Date==1) { # This plot will use Species Group as fill variable 
      ggplot(Species_USA %>% filter(`ESA Listing Status` %in% input$`USA-TS-Status`& `ESA Listing Date` >= input$AllDate[1] & `ESA Listing Date` <= input$AllDate[2]))+
        aes(x=`ESA Listing Date`,fill=`Species Group`)+
        geom_histogram(binwidth = 5,alpha=0.8)+
        scale_x_continuous(breaks = c(1960,1965,1970,1975,1980,1985,1990,1995,2000,
                                      2005,2010,2015,2020,2025))+
        scale_fill_viridis(discrete=TRUE,option = "H")+
        theme_light()+
        ggtitle("Which year did Species appeare in Endangered Animals Report?")+
        labs(y="")
      
      # Most of the species that are listed are from following groups: Mammals, flowering plans, birds and clams
      # Most of them are very sensitive towards the environment they live, wildfires, habitat destruction and pollution
      # Affects them the most
      
    } else if (input$USA_TS_Date==2) {  # This plot will use ESA Listing Status as fill variable 
      
      ggplot(Species_USA %>% filter(`ESA Listing Status` %in% input$`USA-TS-Status`& `ESA Listing Date` >= input$AllDate[1] & `ESA Listing Date` <= input$AllDate[2]))+
        aes(x=`ESA Listing Date`,fill=`ESA Listing Status`)+
        geom_histogram(binwidth = 5,alpha=0.8)+
        scale_x_continuous(breaks = c(1960,1965,1970,1975,1980,1985,1990,1995,2000,
                                      2005,2010,2015,2020,2025))+
        scale_fill_viridis(discrete=TRUE,option = "H")+
        theme_light()+
        ggtitle("Which year did Species appeare in Endangered Animals Report?")+
        labs(y="")
      
      # Most of the species that appeared on the list are either endangered or threatened 
      
      
    } else if (input$USA_TS_Date==3) { # This plot will use State Name as fill variable 
      
      ggplot(Species_USA %>% filter(`ESA Listing Status` %in% input$`USA-TS-Status` & `ESA Listing Date` >= input$AllDate[1] & `ESA Listing Date` <= input$AllDate[2]))+
        aes(x=`ESA Listing Date`,fill=`State Name`)+
        geom_histogram(binwidth = 5,alpha=0.8)+
        scale_x_continuous(breaks = c(1960,1965,1970,1975,1980,1985,1990,1995,2000,
                                      2005,2010,2015,2020,2025))+
        scale_fill_viridis(discrete=TRUE,option = "H")+ # colors 
        theme_light()+
        ggtitle("Which year did Species appeare in Endangered Animals Report?")+
        labs(y="")
      
    } 
    
  }) # Most of species were listed in 1990 and 2015, the could be numerios factors affecting this including
  # increased goverment funding/commitment  or Wildfires, oil spills 
  
  
  
  
  ## Sea levels line chart
  
  # Creating 3 types of chart for each selection
  # User has option to choose what type of plot he/she wants to see, options are following:
  # "Line Chart" = 1, "Smoothed Line" = 2,"Line Chart + Smoothed Line" = 3,"Bar Chart"
  # Despite the plot option, all plots are interactive and take users selected date and region 
  
  output$Sea_Level <- renderPlotly({ # This plot shows how the sea levels change in different regions
    
    if (input$SeaA==1)  { # This renders Line Charts
      ggplotly(
        ggplot(Sea_Levels %>% filter(Measure==input$Sea_Inp & Year >= input$AllDate[1] & Year <= input$AllDate[2]))+
          aes(y=Value,x=Date,colour=Measure)+
          ggtitle(paste("Sea Levels at",paste(input$Sea_Inp,collapse = ", ")))+
          labs(y="Millimeters")+
          geom_line()+
          theme_bw() ) }
    
    else if (input$SeaA==2)  { # This renders Smoothed line chart
      ggplotly(
        ggplot(Sea_Levels %>% filter(Measure==input$Sea_Inp & Year >= input$AllDate[1] & Year <= input$AllDate[2]))+
          aes(y=Value,x=Date, group=Measure, fill=Measure,col=Measure)+
          ggtitle(paste("Sea Levels at",paste(input$Sea_Inp,collapse = ", ")))+
          labs(y="Millimeters")+
          geom_smooth()+
          theme_bw(), tooltip = "x" ) } # smoothed line chart makes trend more apparent compared to line chart and it is easy to implement :)
    
    else if (input$SeaA==3) { # This renders line and smoothed line chart
      ggplotly(
        ggplot(Sea_Levels %>% filter(Measure==input$Sea_Inp & Year >= input$AllDate[1] & Year <= input$AllDate[2]))+
          aes(y=Value,x=Date, group=Measure, colour=Measure)+
          ggtitle(paste("Sea Levels at",paste(input$Sea_Inp,collapse = ", ")))+
          labs(y="Millimeters")+
          geom_line()+
          geom_smooth()+
          theme_bw(), tooltip = "x" ) } 
    
    else if (input$SeaA==4) { # This renders bar chart that plots sea levels trend with every regions data
      ggplotly(
        ggplot(Sea_Levels %>% filter(Year >= input$AllDate[1] & Year <= input$AllDate[2]))+
          aes(x=Year,y=Value,fill=Measure)+
          ggtitle("Global Sea Levels")+
          labs(y="Millimeters", x="Date")+
          geom_bar(stat = "identity",position = "stack")+
          scale_fill_viridis(option = "C",discrete = TRUE)+ # pretty colors
          theme_minimal()) }
    
  }) # Sea levels are rising in most of the regions, however trend in not alarming 
  
  output$Sea_Level2 <- renderPlotly({  # This is a boxlplot that shows distribution of sea levels alteration - sea levels annual change numerical value
    
    Sea_Countries <- Sea_Levels %>% 
      filter(Date=="2022-11-08") %>% 
      arrange(desc(Value)) %>%
      select(Measure)
    
    x <- input$Sea_Slider # User can select number of regions he/she wants to see
    y <- as.vector( Sea_Countries[c(1:x),]) # R will select selected number of region names from the dataframe
    y <- unlist(y) # and assign it to new variables y that is used in plot_ly function to filter the data
    
    plot_ly(data=Sea_Levels %>% filter( Measure == y & Year >= input$AllDate[1] & Year <= input$AllDate[2]) %>%
              select(Measure,Value),x= ~Value, type = "box",color= ~Measure)  %>%
      layout(title = print(paste("Distribution of Sea levels from",x, "Regions")))
    
  })# We already know that sea levels are slowly rising but with box plots we can see the degree to which sea level scan change in different regions - 
  # some are more predictable than others, for example Baltic sea levels are changing to greater degree compared to Caribbean sea - see the rend by selecting numbner 10 
  
  
  
  # Air Pollution Chart
  
  output$Air <- renderPlotly({ # This chart will display overall trend of air pollution, is it getting worse or better in different countries
    # Plot takes countries and date selected by user to plot and it also shows mean and median for air pollution
    ggplotly(
      ggplot()+
        geom_line(data=Air_Pollution %>% filter(Air_Pollution$Country==input$AP_C & Year >= input$AllDate[1] & Year <= input$AllDate[2]) %>% na.omit(),aes(x=Year,y=Value,col=Country))+
        geom_point(data=Air_Pollution %>% filter(Air_Pollution$Country==input$AP_C & Year >= input$AllDate[1] & Year <= input$AllDate[2]),aes(x=Year,y=Value,col=Country),size=3)+
        
        geom_line(data=Meadina_Air, aes(x=Year, y=Median), color='blue4',linetype = "dashed")+ # Median Line
        geom_line(data=Mean_Air, aes(x=Year, y=Mean), color='brown1',linetype = "dashed")+ # Mean Line
        
        scale_x_continuous (breaks = c(1990,1995,2000,2005,2010,2011,2012,2013,2014,2014,2016,2017),limits = c(1990,2017))+
        ggtitle(paste("Pollution in Selected Countries:", paste(input$AP_C, collapse = ", ")))+
        theme_bw(), toolkit="x")
    
  }) # Plot gives users ability to see weather trend in selected countries is over the mean/median or under
  
  # This reactive element is used to select countries with highest pollution, it takes number from user and thenm such as 5 and 
  # displays top 5 countries with highest pollution . The vector "APordered"  is assigned the list of top pollution countries 
  
  APordered <- reactive({ 
    
    # Ordered list of top polluting countries
    All_Countries_P <- 
      na.omit(Air_Pollution) %>%
      group_by(Country) %>%
      summarise(Pollution=mean(Value)) %>%
      arrange(desc(Pollution)) 
    
    as.vector(
      unlist(
        top_n (All_Countries_P,n=input$APslider,wt=Pollution) %>% select(Country) ))  })
  
  vAP <- reactive({ # This code will prevent user from selection more than 15 and less than 2 countries in above code
    validate(
      need(input$APslider >=2 & input$APslider <= 15,
           "Please choose a number between 2 and 15") )
  })
  
  output$Air2 <- renderPlotly({ # This plot takes APordered vector and displays pollution trend in these countries
    
    vAP() # This line ensures user selects correct number
    Countries_AP <- APordered() # Passing list of the countries
    
    ggplotly(
      ggplot(na.omit(Air_Pollution) %>% filter(Country %in% Countries_AP & Year >= input$AllDate[1] & Year <= input$AllDate[2] ))+
        aes(x=Year,y=Value,col=Country)+
        geom_line(linewidth=1.3)+
        #Dynamic Name
        ggtitle(print(paste("Top",length(Countries_AP),"Countries with Highest Pollution")))+
        theme_bw()+
        scale_color_viridis(option = "D",discrete = TRUE))
    
  }) # Nepal, India and Qatar have the highest pollution 
  
  
  # Value box displaying highest pollution
  
  output$AP_Highest <- renderValueBox({
    
    AP_First <-  as.vector(unlist(AP_First)) # The top polluting country's name is saved in vector and later used for valuebox
    
    valueBox(print(paste(AP_First)), "Highest Pollution", icon = icon("arrow-trend-up"),
             color = "red")
    
  }) # Nepal has the highest pollution rate
  
  output$AP_Lowest <- renderValueBox({ # This valubox works simmilary to one above, the only difference is that is selects country with lowest pllution
    
    AP_Lowest <-  as.vector(unlist(AP_Lowest))
    
    valueBox(print(paste(AP_Lowest)), "Lowest Pollution", icon = icon("arrow-trend-down"),
             color = "green")
    
  }) # Finland has the lowest pollution rate
  
  
  
  ## Disasters frequency analysis 
  
  output$DF <- renderPlotly({ # This plot shows distribution of total number of disasters globally, the bars is subset by type of disasters
    
    ggplotly(
      ggplot(drop_na(Disasters) %>% filter(Indicator!="Total" & Year >= input$AllDate[1] & Year <= input$AllDate[2]))+
        aes(x=Year,y=Value,fill=Indicator)+
        geom_bar(stat="identity",position = "stack")+
        ggtitle("Global Frequency of Natural Disasters")+
        labs(y="Frequency")+
        theme_classic()+
        scale_fill_viridis(discrete=TRUE,option = "C")+ # colors 
        # The following code ensures correct number of years in displayed on x axis so they don't overlap 
        if (length(input$AllDate[1]:input$AllDate[2])>25) {scale_x_continuous (breaks = c(1980,1985,1990,1995,2000,2005,2010,2015,2020))
        } else {scale_x_continuous (breaks = c(input$AllDate[1]:input$AllDate[2]))})
    
  }) # Natural disasters are more frequent in recent years
  
  output$DF1.1 <- renderPlotly({ # This plot shows distribution number all of disasters in country selected by user
    
    ggplot(Disasters %>% filter(Country==input$DF_C1,Indicator!="Total" & Year >= input$AllDate[1] & Year <= input$AllDate[2]))+
      aes(x=Year,y=Value,fill=Indicator)+
      geom_bar(stat="identity",position = "stack")+
      ggtitle(paste("Observe the Distribution of Disasters in selected Country:",input$DF_C1))+
      scale_fill_viridis(discrete=TRUE,option = "C")+ # colors 
      theme_bw()
    
  }) # For example user may select United States and find out that for the country most frequent disaster is storm 
  
  
  output$DF2 <- renderPlotly({  # This plot shows distribution number total number of disasters in multiple countries selected by user
    # User has option to select box plot or barplot  
    if (input$DisC==1) { # Code for bat plot
      
      ggplot(Disasters %>% filter(Country==input$DF_C,Indicator=="Total" & Year >= input$AllDate[1] & Year <= input$AllDate[2]))+
        aes(x=Year,y=Value,fill=Country)+
        geom_bar(stat="identity",position = "dodge")+
        ggtitle(paste("Total Disasters Trend in Selected Countries:", paste(input$DF_C, collapse = ", ")))+
        theme_bw() 
    } # bar chart is a good way to observe trend in details 
    
    else if (input$DisC==2) {  # Code for boxplot
      
      plot_ly(data=Disasters[,-3] %>% filter(Country == input$DF_C),x= ~Value, type = "box",color=~Country) %>%
        layout(title = paste("Total Disasters Trend in Selected Countries:", paste(input$DF_C, collapse = ", "))) }
  }) # Here boxplot comes handy to compare trend in different countries till this point year - 2022
  
  # Table displaying in which countries most disasters occur
  output$DisastersTable <- renderDataTable( DTable )
  
  # United States, China and India have recorded the most natural disasters
  
  
  # table displaying how frequent each disaster is 
  output$DisastersTable2 <- renderDataTable(DTable2)
  
  # The most frequent natural disaster is Flood
  
  
  
  ## Forest Area
  
  output$Forest_Area <- renderPlotly({ # This forest are plot shows global trend of forest area
    # User has option to choose the indicator meaning user has option to observe the the trend in terms of share of forest are % or in terms of area
    
    if (input$FC_I=="Forest area") { # this plot shows trend in forest area
      
      ggplot(Forest_Area %>% filter(Country == "World" & Indicator == "Forest area" & Year >= input$AllDate[1] & Year <= input$AllDate[2]))+
        aes(x=Year, y=Value)+
        geom_area( fill="#69b3a2", alpha=0.1) +
        geom_line(color="#69b3a2", size=2) +
        geom_point(size=3, color="#69b3a2") +
        theme_bw()+
        ggtitle(paste("How is",paste(input$FC_I) ,"changing overtime globally?"))+
        # Ensuring correct number of years is displayed
        scale_x_continuous (breaks = c(1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 
                                       2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
                                       2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
                                       2016, 2017, 2018, 2019, 2020),limits = c(1992,2020))+
        
        scale_y_continuous(limits = c(4050000,4250000)) }
    
    else if (input$FC_I=="Share of forest area") { # this plot shows trend in percentage of land that forest occupies
      
      ggplot(Forest_Area %>% filter(Country == "World" & Indicator == "Share of forest area" & Year >= input$AllDate[1] & Year <= input$AllDate[2]))+
        aes(x=Year, y=Value)+
        geom_line(color="#69b3a2", size=2) +
        geom_point(size=3, color="#69b3a2") +
        theme_bw()+
        scale_y_continuous(breaks = c(30,31,32,32,33),limits = c(30,33))
      
    }             
    
  }) # in 3 decades forest area all over the world has decreased by 161829000 hectare - rate that is unsupportable!  
  
  
  output$Forest_Area2 <- renderPlotly({  # This plot shows trend in selected countries, user can select date and forest area or share of forest area
    ggplotly(
      ggplot(Forest_Area %>% filter(Country == input$FC_C2,Indicator == input$FC_I & Year >= input$AllDate[1] & Year <= input$AllDate[2]))+
        # The aesthetic function has argument text that ensures that when user moves cursor over the plot relevant information is displayed''
        aes(x = Year, y = reorder(Value,Value), colour = Country, group = Country,text = paste("Year:", Year, "<br>County:", Country, "<br>Value:", Value)) +
        geom_line(linewidth=1.1)+
        geom_point(size=1.5)+
        labs(y="Area")+
        # Ensuring plot looks nice
        scale_x_continuous (breaks = c(1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),limits = c(1992,2020))+
        theme_bw()+
        ggtitle(paste(input$FC_I , "Trend in", paste(input$FC_C2, collapse = ", ")))+
        labs(y=paste(input$FC_I))+
        # Better colors 
        scale_color_viridis(option = "C",discrete = TRUE),tooltip = c("text")) 
    
  })  # Forest area is increasing only in Europe and Asia
  
  
  ## Temperature Change Chart
  
  output$Temperature_Change <- renderPlot({ # This is smoothed line plot that displays temperature change trend in all countries and takes input from user about the date
    
    ggplot(Temperature_Change %>% filter(Year >= input$AllDate[1] & Year <= input$AllDate[2]))+
      aes(x = Year, y = Value, group = Country, fill = Country, col = Country) +
      geom_smooth() +
      theme_classic() +
      guides(fill = "none", col = "none", shape = "none")
    
  }) # Temperature is increasing globally by more than 1 degrees every year in most of the countries, the trend is unsustainable 
  
  output$Temperature_Change2 <- renderPlotly({ # This is line plot that displays temperature change trend in selected countries and takes input from user about the date and country
    
    ggplot(Temperature_Change %>% filter(Year >= input$AllDate[1] & Year <= input$AllDate[2], Country %in% input$TCC))+
      aes(x = Year, y = Value, col = Country) +
      geom_line(alpha=0.8,linewidth=1.5) +
      geom_point()+
      theme_bw()+
      scale_colour_viridis(option = "H",discrete = TRUE)+
      # The following code ensures correct number of years in displayed on x axis so they don't overlap 
      if (length(input$AllDate[1]:input$AllDate[2])>25) {scale_x_continuous (breaks = c(1990,1995,2000,2005,2010,2015,2020))
      } else {scale_x_continuous (breaks = c(input$AllDate[1]:input$AllDate[2]))}
    
    
  }) # Temperature is increasing in Georgia! 
  
  
  # The following valuebox shows which country is most affected by temperature change
  output$TC_top_1st <- renderValueBox({
    
    valueBox(
      print(paste(TC_top_1)),"Highest Temperature change", icon = icon("square-up-right"),
      color = "red"
    )
  }) # Andorra
  
  # The following valuebox shows which country is least affected by temperature change
  output$TC_bot_1st <- renderValueBox({
    
    valueBox(
      print(paste(TC_bot_1)),"Lowest Temperature change", icon = icon("square-up-left"),
      color = "green"
    )
  }) # Yemen
  
  # Overall the trens in temperature is alarming in most of the countries 
  
  
  
  ## CO2_Concentrations bar chart
  
  output$CO2_Concentrations <- renderPlotly({ # This plot Monthly atmospheric carbon dioxide concentrations globally 
    
    ggplot(CO2_Concentrations %>% filter(Year >= input$AllDate[1] & Year <= input$AllDate[2]))+
      aes(x=Date,y=Value)+
      geom_line(col="red",alpha=0.9)+
      labs(y="Concentration")+
      theme_bw()+
      # The title is dynamic and changes depending on years user has selected
      ggtitle(paste("Monthly atmospheric carbon dioxide concentrations from",input$AllDate[1],"to",input$AllDate[2]))
    
  }) # CO2_Concentration in increasing at stable rate in the atmosphere
  
  
  # CO2_Emissions by Countries
  
  output$CO2_Emissions <- renderPlotly({ # This plot displays trend of CO2 concentration in selected countries and date 
    ggplotly(
      ggplot(CO2_Emissions %>% filter(`Country Name`== input$CO2countries & Year >= input$AllDate[1] & Year <= input$AllDate[2]))+
        # The aesthetic function has argument text that ensures that when user moves cursor over the plot relevant information is displayed
        aes(y=Value,x=Year,roup = `Country Name`,fill=`Country Name`, text = paste("Year:", Year, "<br>County:", `Country Name`, "<br>Value:", Value))+
        geom_bar(stat="identity")+
        labs(y="Emissions")+
        theme_bw()+
        # The title is dynamic and changes depending on years user has selected
        ggtitle(paste('Emissions in Selcted Countries from',input$AllDate[1],"to",input$AllDate[2]))+
        scale_fill_viridis(option = "G",discrete = TRUE, end = 0.5)+
        # The following code ensures correct number of years in displayed on x axis so they dont overlap 
        if (length(input$AllDate[1]:input$AllDate[2])>25) {scale_x_continuous (breaks = c(1990,1995,2000,2005,2010,2015,2020))
        } else {scale_x_continuous (breaks = c(input$AllDate[1]:input$AllDate[2]))} , tooltip = c("text")) 
    
  })  # CO2 Emissions in Unites States is decreasing, in China it is increasing and in Russia in fluctuates
  
}