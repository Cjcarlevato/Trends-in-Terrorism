server <- function(input,output) {
  
  output$method_barplot <- renderPlotly({
    if (input$method_dataframeinput == "Hijackings") {
      p <- hj_freq %>%
        top_n(10, Freq) %>%
        ggplot(aes(x= reorder(Year, -Freq), y=Freq, fill = Year)) + geom_bar(stat = "identity") +
        xlab('Year') +
        ylab('Total Hijackings') +
        ggtitle('Top 10 Years for Deadly Terrorist Hijackings') 
        #ggplotly(p, tooltip="test")
    } else if (input$method_dataframeinput == "Armed Assaults")  { 
      p <- armed_assault_freq %>%
        top_n(10, Freq) %>%
        ggplot(aes(x= reorder(Year, -Freq), y=Freq, fill = Year)) + geom_bar(stat = "identity") + 
        xlab('Year') +
        ylab('Total Armed Assaults') +
        ggtitle('Top 10 Years for Deadly Terrorist Armed Assaults') 
    } else if (input$method_dataframeinput == "Assassinations")  { 
      p <- assassination_freq %>%
        top_n(10, Freq) %>%
        ggplot(aes(x= reorder(Year, -Freq), y=Freq, fill = Year)) + geom_bar(stat = "identity") + 
        xlab('Year') +
        ylab('Total Assassinations') +
        ggtitle('Top 10 Years for Deadly Terrorist Assassinations')
    } else if (input$method_dataframeinput == "Bombings")  { 
      p <- bomb_freq %>%
        top_n(10, Freq) %>%
        ggplot(aes(x= reorder(Year, -Freq), y=Freq, fill = Year)) + geom_bar(stat = "identity") + 
        xlab('Year') +
        ylab('Total Bombings') +
        ggtitle('Top 10 Years for Deadly Terrorist Bombings')
    } else if (input$method_dataframeinput == "Hostage Takings")  { 
      p <- hostage_freq %>% 
        top_n(10, Freq) %>%
        ggplot(aes(x= reorder(Year, -Freq), y=Freq, fill = Year)) + geom_bar(stat = "identity") + 
        xlab('Year') +
        ylab('Total Hostage Takings') +
        ggtitle('Top 10 Years for Deadly Terrorist Hostage Takings')
    } else if (input$method_dataframeinput == "Kidnappings")  { 
      p <- kidnapping_freq %>%
        top_n(10, Freq) %>%
        ggplot(aes(x= reorder(Year, -Freq), y=Freq, fill = Year)) + geom_bar(stat = "identity") + 
        xlab('Year') +
        ylab('Total Kidnappings') +
        ggtitle('Top 10 Years for Deadly Terrorist Kidnappings')
    } else if (input$method_dataframeinput == "Facility Attacks")  { 
      p <- facility_attack_freq %>%
        top_n(10, Freq) %>%
        ggplot(aes(x= reorder(Year, -Freq), y=Freq, fill = Year)) + geom_bar(stat = "identity") + 
        xlab('Year') +
        ylab('Total Facility Attacks') +
        ggtitle('Top 10 Years for Deadly Terrorist Facility Attacks')
    } else if (input$method_dataframeinput == "Unarmed Assaults")  { 
      p <- unarmed_assault_freq %>%
        top_n(10, Freq) %>%
        ggplot(aes(x= reorder(Year, -Freq), y=Freq, fill = Year)) + geom_bar(stat = "identity") + 
        xlab('Year') +
        ylab('Total Unarmed Assaults') +
        ggtitle('Top 10 Years for Deadly Terrorist Unarmed Assaults')
    } else if (input$method_dataframeinput == "Unknown Method")  { 
      p <- unknown_freq %>%
        top_n(10, Freq) %>%
        ggplot(aes(x= reorder(Year, -Freq), y=Freq, fill = Year)) + geom_bar(stat = "identity") + 
        xlab('Year') +
        ylab('Total Classification Attacks') +
        ggtitle('Top 10 Years for Deadly Unknown Method Terrorist Attacks')
    } else {
      print(input$method_dataframeinput)
    }
    p

  })
  
  output$method_lineplot <- renderPlotly({
    if(input$method_dataframeinput == "Hijackings") {
      p <- ggplot(hj_freq, aes(x= Year, y = Freq, group=1)) +
        geom_line(size=1, linetype= "solid", color="blue") +
        geom_point(color="black", size= 1) +
        xlab('Year') +
        ylab('Total Hijackings') +
        scale_x_discrete(breaks = seq(1970, 2017, by = 5)) +
        ggtitle('Frequency of Global Deadly Terrorist Hijackings from 1970 - 2017')
    } else if (input$method_dataframeinput == "Armed Assaults")  { 
      p <- ggplot(armed_assault_freq, aes(x= Year, y = Freq, group=1)) +
        geom_line(size=1, linetype= "solid", color="blue") +
        geom_point(color="black", size= 1) +
        xlab('Year') +
        ylab('Total Armed Assaults') +
        scale_x_discrete(breaks = seq(1970, 2017, by = 5)) +
        ggtitle('Frequency of Global Deadly Terrorist Armed Assaults from 1970 - 2017')
      } else if (input$method_dataframeinput == "Assassinations") { 
          p <- ggplot(assassination_freq, aes(x= Year, y = Freq, group=1)) +
            geom_line(size=1, linetype= "solid", color="blue") +
            geom_point(color="black", size= 1) +
            xlab('Year') +
            ylab('Total Bombings') +
            scale_x_discrete(breaks = seq(1970, 2017, by = 5)) +
            ggtitle('Frequency of Global Deadly Terrorist Assassinations from 1970 - 2017')
      } else if (input$method_dataframeinput == "Bombings") { 
            p <- ggplot(bomb_freq, aes(x= Year, y = Freq, group=1)) +
              geom_line(size=1, linetype= "solid", color="blue") +
              geom_point(color="black", size= 1) +
              xlab('Year') +
              ylab('Total Bombings') +
              scale_x_discrete(breaks = seq(1970, 2017, by = 5)) +
              ggtitle('Frequency of Global Deadly Terrorist Bombings from 1970 - 2017')
      } else if (input$method_dataframeinput == "Hostage Takings") { 
              p <- ggplot(hostage_freq, aes(x= Year, y = Freq, group=1)) +
                geom_line(size=1, linetype= "solid", color="blue") +
                geom_point(color="black", size= 1) +
                xlab('Year') +
                ylab('Total Hostage Takings') +
                scale_x_discrete(breaks = seq(1970, 2017, by = 5)) +
                ggtitle('Frequency of Global Deadly Terrorist Hostage Takings from 1970 - 2017')
      } else if (input$method_dataframeinput == "Kidnappings")  { 
                p <- ggplot(kidnapping_freq, aes(x= Year, y = Freq, group=1)) +
                geom_line(size=1, linetype= "solid", color="blue") +
                geom_point(color="black", size= 1) +
                xlab('Year') +
                ylab('Total Kidnappings') +
                scale_x_discrete(breaks = seq(1970, 2017, by = 5)) +
                ggtitle('Frequency of Global Deadly Terrorist Kidnappings from 1970 - 2017')
      } else if  (input$method_dataframeinput == "Facility Attacks"){ 
                  p <- ggplot(facility_attack_freq, aes(x= Year, y = Freq, group=1)) +
                    geom_line(size=1, linetype= "solid", color="blue") +
                    geom_point(color="black", size= 1) +
                    xlab('Year') +
                    ylab('Total Facility Attacks') +
                    scale_x_discrete(breaks = seq(1970, 2017, by = 5)) +
                    ggtitle('Frequency of Global Deadly Terrorist Facility Attacks from 1970 - 2017')
      } else if (input$method_dataframeinput == "Unarmed Assaults") { 
                  p <- ggplot(unarmed_assault_freq, aes(x= Year, y = Freq, group=1)) +
                      geom_line(size=1, linetype= "solid", color="blue") +
                      geom_point(color="black", size= 1) +
                      xlab('Year') +
                      ylab('Unarmed Assaults') +
                      scale_x_discrete(breaks = seq(1970, 2017, by = 5)) +
                      ggtitle('Frequency of Global Deadly Terrorist Unarmed Assaults from 1970 - 2017')
      } else if (input$method_dataframeinput == "Unknown Method")  { 
                      p <- ggplot(unknown_freq, aes(x= Year, y = Freq, group=1)) +
                        geom_line(size=1, linetype= "solid", color="blue") +
                        geom_point(color="black", size= 1) +
                        xlab('Year') +
                        ylab('Total Attacks with Unknown Method') +
                        scale_x_discrete(breaks = seq(1970, 2017, by = 5)) +
                        ggtitle('Frequency of Global Deadly Terrorist Unknown Attacks from 1970 - 2017')
                  
      } else {
      print(input$method_dataframeinput)
    }
    p
  })
  
  output$method_choropleth <- renderPlotly({
    g1 <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    if (input$method_choose_map == "Deaths") {
      p <- plot_geo(US_summary, locationmode = 'USA-states') %>%
        add_trace(
          z = ~log_kills, text = ~hover1, locations = ~State,
          color = ~log_kills, colors = 'Purples', showscale =FALSE
        ) %>%
        colorbar(title = "Logged Color Scale") %>%
        layout(
          title = 'Terrorism Deaths from 1970 to 2017 by State<br>(Hover for breakdown)',
          geo = g1)
    } else if (input$method_choose_map == "Injuries") {
      p <- plot_geo(US_summary, locationmode = 'USA-states') %>%
        add_trace(
          z = ~log_nwound, text = ~hover2, locations = ~State,
          color = ~log_nwound, colors = 'Greens', showscale =FALSE
        ) %>%
        colorbar(title = "Logged Color Scale") %>%
        layout(
          title = 'Total People Injured by Deadly Terror Attacks from 1970 to 2017 by State<br>(Hover for breakdown)',
          geo = g1)
      } else if (input$method_choose_map == "Hijackings") { 
        p <- plot_geo(US_summary, locationmode = 'USA-states') %>%
          add_trace(
            z = ~total_hijackings, text = ~hoverhijackings, locations = ~State,
            color = ~total_hijackings, colors = 'Oranges', showscale =FALSE
          ) %>%
          colorbar(title = "Total Incidents") %>%
          layout(
            title = 'Total Deadly Terrorist Attacks Classified as a Hijacking from 1970 to 2017 by State<br>(Hover for breakdown)',
            geo = g1)
      } else if (input$method_choose_map == "Armed Assaults") {
        p <- plot_geo(US_summary, locationmode = 'USA-states') %>%
          add_trace(
            z = ~total_armedassaults, text = ~hoverarmedassaults, locations = ~State,
            color = ~total_armedassaults, colors = 'Blues', showscale = FALSE
          ) %>%
          colorbar(title = "Total Incidents") %>%
          layout(
            title = 'Total Deadly Terrorist Attacks Classified as an Armed Assault from 1970 to 2017 by State<br>(Hover for breakdown)',
            geo = g1)
      } else if (input$method_choose_map == "Assassinations") {
        p <- plot_geo(US_summary, locationmode = 'USA-states') %>%
          add_trace(
            z = ~total_assassinations, text = ~hoverassassinations, locations = ~State,
            color = ~total_assassinations, colors = 'Purples', showscale = FALSE
          ) %>%
          colorbar(title = "Total Incidents") %>%
          layout(
            title = 'Total Deadly Terrorist Attacks Classified as an Assassination from 1970 to 2017 by State<br>(Hover for breakdown)',
            geo = g1)
      } else if (input$method_choose_map == "Bombings") {
        p <- plot_geo(US_summary, locationmode = 'USA-states') %>%
          add_trace(
            z = ~log_bombs, text = ~hoverbombs, locations = ~State, 
            color = ~log_bombs, colors = 'Greens', showscale =FALSE
          ) %>%
          colorbar(title = "Logged Color Scale") %>%
          layout(
            title = 'Total Deadly Terrorist Attacks Classified as a Bombing from 1970 to 2017 by State<br>(Hover for breakdown)',
            geo = g1)
      } else if (input$method_choose_map == "Hostage Takings") {
        p <- plot_geo(US_summary, locationmode = 'USA-states') %>%
          add_trace(
            z = ~total_hostages, text = ~hoverhostages, locations = ~State,
            color = ~total_hostages, colors = 'Oranges', showscale =FALSE
          ) %>%
          colorbar(title = "Total Incidents") %>%
          layout(
            title = 'Total Deadly Terrorist Attacks Classified as Hostage Taking from 1970 to 2017 by State<br>(Hover for breakdown)',
            geo = g1)
      } else if (input$method_choose_map == "Kidnappings") {
        p <- plot_geo(US_summary, locationmode = 'USA-states') %>%
          add_trace(
            z = ~total_kidnappings, text = ~hoverkidnappings, locations = ~State,
            color = ~total_kidnappings, colors = 'Blues', showscale =FALSE
          ) %>%
          colorbar(title = "Total Incidents") %>%
          layout(
            title = 'Total Deadly Terrorist Attacks Classified as a Kidnapping from 1970 to 2017 by State<br>(Hover for breakdown)',
            geo = g1)
      } else if (input$method_choose_map == "Facility Attacks") {
        p <- plot_geo(US_summary, locationmode = 'USA-states') %>%
          add_trace(
            z = ~log_facilityattacks, text = ~hoverfacilityattacks, locations = ~State,
            color = ~log_facilityattacks, colors = 'Purples', showscale =FALSE
          ) %>%
          colorbar(title = "Logged Color Scale") %>%
          layout(
            title = 'Total Deadly Terrorist Attacks Classified as a Facility Attack from 1970 to 2017 by State<br>(Hover for breakdown)',
            geo = g1)
      } else if (input$method_choose_map == "Unarmed Assaults") {
        p <- plot_geo(US_summary, locationmode = 'USA-states') %>%
          add_trace(
            z = ~total_unarmedassaults, text = ~hoverunarmedassaults, locations = ~State,
            color = ~total_unarmedassaults, colors = 'Greens', showscale =FALSE
          ) %>%
          colorbar(title = "Total Incidents") %>%
          layout(
            title = 'Total Deadly Terrorist Attacks Classified as an Unarmed Assault from 1970 to 2017 by State<br>(Hover for breakdown)',
            geo = g1)
      } else if (input$method_choose_map == "Unknown Method") {
        p <- plot_geo(US_summary, locationmode = 'USA-states') %>%
          add_trace(
            z = ~total_unknowns, text = ~hoverunknowns, locations = ~State,
            color = ~total_unknowns, colors = 'Oranges', showscale = FALSE
          ) %>%
          colorbar(title = "Total Incidents") %>%
          layout(
            title = 'Total Deadly Terrorist Attacks with Unknown Classification from 1970 to 2017 by State<br>(Hover for breakdown)',
            geo = g1)
      } else {  
      print(input$method_choose_map)
    }
    p
  })
  
  years <- eventReactive(input$select_year, {
    input$select_year
  })
  output$word_cloud <- renderWordcloud2({
    if (input$method_choose_cloud == "Nationality of Attacker") {
      terrorism_year_subset1 <- terrorism %>%
        dplyr::filter(iyear >= years()[1], iyear <= years()[2])
      usableText1 <- iconv(terrorism_year_subset1$natlty1_txt, "ASCII", "UTF-8", sub="")
      corpus1 <- Corpus(VectorSource(usableText1))
      tdm1 <-TermDocumentMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
      freq1 <- slam::row_sums(tdm1)
      cloud_final <- wordcloud2(data.frame(as.list(freq1)) %>% melt(variable.name = "word", value.name = "freq1"),
                 size = 1, color = "random-light", backgroundColor = "grey")
      
    } else if (input$method_choose_cloud == "City of Attack") {
    terrorism_year_subset2 <- terrorism %>%
      dplyr::filter(iyear >= years()[1], iyear <= years()[2])
   
     top_cities <- terrorism_year_subset2 %>% 
      group_by(city) %>%
      summarize(total=n()) %>%
      top_n(200, total) %>%
      select(city) %>%
      filter(city != "Unknown") %>%
      pull()
    
    terrorism_year_subset2 <- terrorism_year_subset2 %>%
      dplyr::filter(city %in% top_cities)
    usableText2 <- iconv(terrorism_year_subset2$city, "ASCII", "UTF-8", sub="")
    corpus2 <- Corpus(VectorSource(usableText2))
    tdm2 <-TermDocumentMatrix(corpus2, control=list(wordLengths=c(1,Inf)))
    freq2 <- slam::row_sums(tdm2)
    cloud_final <- wordcloud2(data.frame(as.list(freq2)) %>% melt(variable.name = "word", value.name = "freq2"),
               size = 1, color = "random-light", backgroundColor = "grey")
    } else {
    terrorism_year_subset3 <- terrorism %>%
      dplyr::filter(iyear >= years()[1], iyear <= years()[2])
   
     top_gname <- terrorism_year_subset3 %>% 
      group_by(gname) %>%
      summarize(total=n()) %>%
      ungroup() %>%
      top_n(50, total) %>%
      select(gname) %>%
      pull()
    terrorism_year_subset3 <- terrorism_year_subset3 %>%
      dplyr::filter(gname %in% top_gname)
    test <- terrorism_year_subset3 %>% group_by(gname) %>% summarise(n()) 
    usableText <- iconv(terrorism_year_subset3$gname, "ASCII", "UTF-8", sub="")
    corpus <- Corpus(VectorSource(usableText))
    tdm <-TermDocumentMatrix(corpus, control=list(wordLengths=c(1,Inf)))
    freq <- slam::row_sums(tdm)
    cloud_final <- wordcloud2(test, size = 5, color = "random-light", backgroundColor = "grey")
    # wordcloud2(data.frame(as.list(freq)) %>% melt(variable.name = "word", value.name = "freq"),
    #            size = 5, gridsize = 1, color = "random-light", backgroundColor = "grey")
    }
    cloud_final
  })
}