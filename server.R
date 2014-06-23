library(shiny)
library(ggplot2)
library(reshape)

base <- read.table('shiny_app.csv',header=T,sep='\t',stringsAsFactor=F, na.strings=c("","NA"))
base <- subset(base,base$precinct != 226)
base$time <- strptime(base$time, format="%d.%m.%Y %H:%M")
base <<- subset(base,!is.na(base$time))

base$region[base$region == "Автономна Республіка Крим"] = "ARC"
base$region[base$region == "Вінницька область"] = "Vinnitsa region"
base$region[base$region == "Волинська область"] = "Volyn region"
base$region[base$region == "Дніпропетровська область"] = "Dnipropetrovsk region"
base$region[base$region == "Донецька область"] = "Donetsk region"
base$region[base$region == "Житомирська область"] = "Zhytomyr region"
base$region[base$region == "Закарпатська область"] = "Transcarpathian region"
base$region[base$region == "Запорізька область"] = "Zaporizhia region"
base$region[base$region == "Івано-Франківська область"] = "Ivano-Frankivsk region"
base$region[base$region == "Київська область"] = "Kiev region"
base$region[base$region == "Кіровоградська область"] = "Kirovohrad Oblast"
base$region[base$region == "Луганська область"] = "Lugansk region"
base$region[base$region == "Львівська область"] = "Lviv region"
base$region[base$region == "м.Київ"] = "Kiev"
base$region[base$region == "м.Севастополь"] = "Sevastopol"
base$region[base$region == "Миколаївська область"] = "Nikolaev region"
base$region[base$region == "Одеська область"] = "Odessa region"
base$region[base$region == "Полтавська область"] = "Poltava region"
base$region[base$region == "Рівненська область"] = "Rivne region"
base$region[base$region == "Сумська область"] = "Sumy region"
base$region[base$region == "Тернопільська область"] = "Ternopil region"
base$region[base$region == "Харківська область"] = "Kharkiv region"
base$region[base$region == "Херсонська область"] = "Kherson region"
base$region[base$region == "Хмельницька область"] = "Khmelnytsky region"
base$region[base$region == "Черкаська область"] = "Cherkasy region"
base$region[base$region == "Чернівецька область"] = "Chernivtsi region"
base$region[base$region == "Чернігівська область"] = "Chernihiv region"


pagetext <<- data.frame(
  eng = c("Presidential Election 2014","Map","Scatterplot",
          "Histogram","Summaries","About","Select your language","Filter by precinct size",
          "Time of submission of protocol","Filters","From","to","Territory","Select region","# District",
          "All regions","ARC","Vinnitsa region","Volyn region","Dnipropetrovsk region","Donetsk region",
          "Zhytomyr region","Transcarpathian region","Zaporizhia region","Ivano-Frankivsk region",
          "Kiev region","Kirovohrad Oblast","Lugansk region","Lviv region","Nikolaev region","Odessa region",
          "Poltava region","Rivne region","Sumy region","Ternopil region","Kharkiv region",
          "Kherson region","Khmelnytsky region","Cherkasy region","Chernivtsi region","Chernihiv region","Kiev",
          "Sevastopol","Number of selected precints is", "Turnout","Voted at home (% of voters)",
          "Invalid ballots (% of votes)","Bohomolec O. V. %","Boyko Yu. A. %","Grynenko A. V. %",
          "Gritsenko A. S. %","Dobkin M. M. %","Klimenko A. I. %","Konovalyuk V. I. %","Kuzmin R. R. %",
          "Kuibida V. S. %","Lyashko O. V. %","Malomuzh M. G. %","Poroshenko P. O. %","Rabinovich V. Z. %",
          "Saranov V. G. %","Symonenko P. M. %","Tymoshenko Yu. V. %","Tigipko S. L. %",
          "Tyagnibok O. Ya. %","Tsushko V. P. %","Shkiriak S. N. %","Yarosh D. O. %","Time of protocol submission",
          "Disabled", "Main plot", "Secondary plot","Make plots","Submit", "Difference between", "and","Summary",
          "Number of precincts", "Please select the second variable","Instructions"),
  stringsAsFactors=F,
  row.names=c("title","tab1","tab2","tab3","tab4","tab5","lang","size","time","filter","from","to",
              "teritorry","region","district_num","reg_all", "reg_arc","reg_vin","reg_vol","reg_dp",
              "reg_don","reg_zht","reg_zkp","reg_zap","reg_ifr","reg_kie","reg_kir","reg_lug",
              "reg_lv","reg_myk","reg_ode","reg_pol","reg_riv","reg_sum","reg_ter","reg_har",
              "reg_her","reg_khm","reg_chk","reg_chr","reg_chn","reg_k","reg_s","prec_num","turn","home",
              "invalid","bogom","boyko","grin","gric","dobk","klym","konov","kuzm","kuyb","lyash","malom",
              "poros","rabin","saran","simon","timos","tigip","tyahn","cushk","shkir","yaros","subtim",
              "disabled","mainplot","secplot","makeplot","subm_but","diff","and","overall","dist_num",
              "sec_var","about"))

regions <<- unique(base$region)

ndistrict = 1:225

shinyServer(
  function(input, output,clientData, session) {

    # Locale definition
    output$title <- renderText({pagetext["title","eng"]})
    output$tab1 <- renderText({pagetext["tab1","eng"]})
    output$tab2 <- renderText({pagetext["tab2","eng"]})
    output$tab3 <- renderText({pagetext["tab3","eng"]})
    output$tab4 <- renderText({pagetext["tab4","eng"]})
    output$tab5 <- renderText({pagetext["tab5","eng"]})
    output$lang <- renderText({pagetext["lang","eng"]})
    output$filter <- renderText({pagetext["filter","eng"]})
    output$size <- renderText({pagetext["size","eng"]})
    output$time <- renderText({pagetext["time","eng"]})
    output$selectedTime <- renderText({paste(pagetext["from","eng"],
                                             strftime(as.POSIXct(input$timeRange[1], origin = "1960-01-02"),format="%H:%M (%d.%m)"),
                                             pagetext["to","eng"],
                                             strftime(as.POSIXct(input$timeRange[2], origin = "1960-01-02"),format="%H:%M (%d.%m)"))})
    output$teritorry <- renderText({pagetext["teritorry","eng"]})
    output$region <- renderText({pagetext["region","eng"]})
    output$reg_all <- renderText({pagetext["reg_all","eng"]})
    output$reg_arc <- renderText({pagetext["reg_arc","eng"]})
    output$reg_vin <- renderText({pagetext["reg_vin","eng"]})
    output$mainplot <- renderText({pagetext["mainplot","eng"]})
    output$secplot <- renderText({pagetext["secplot","eng"]})
    output$makeplot <- renderText({pagetext["makeplot","eng"]})
    output$district_num <- renderText({pagetext["district_num","eng"]})
    output$subm_but <- renderText({pagetext["subm_but","eng"]})
    output$about <- renderText({pagetext["about","eng"]})
    
     # Sliderange with precinct size
     output$precSizeMin <- renderText({min(base$voters_num,na.rm=T)})
     output$precSizeMax <- renderText({max(base$voters_num,na.rm=T)})
     
     # Sliderange with time
     output$timeMin <- renderText({min(time,na.rm=T)})
     output$timeMax <- renderText({max(time,na.rm=T)})
     
     # Input with regions
     observe({
       
       s_options <- list()
       s_options[[pagetext["reg_all","eng"]]] <- pagetext["reg_all","eng"]
       #s_options[[pagetext["reg_arc","eng"]]] <- pagetext["reg_arc","eng"]
       s_options[[pagetext["reg_vin","eng"]]] <- pagetext["reg_vin","eng"]
       s_options[[pagetext["reg_vol","eng"]]] <- pagetext["reg_vol","eng"]
       s_options[[pagetext["reg_dp","eng"]]] <- pagetext["reg_dp","eng"]
       s_options[[pagetext["reg_don","eng"]]] <- pagetext["reg_don","eng"]
       s_options[[pagetext["reg_zht","eng"]]] <- pagetext["reg_zht","eng"]
       s_options[[pagetext["reg_zkp","eng"]]] <- pagetext["reg_zkp","eng"]
       s_options[[pagetext["reg_zap","eng"]]] <- pagetext["reg_zap","eng"]
       s_options[[pagetext["reg_ifr","eng"]]] <- pagetext["reg_ifr","eng"]
       s_options[[pagetext["reg_kie","eng"]]] <- pagetext["reg_kie","eng"]
       s_options[[pagetext["reg_kir","eng"]]] <- pagetext["reg_kir","eng"]
       s_options[[pagetext["reg_lug","eng"]]] <- pagetext["reg_lug","eng"]
       s_options[[pagetext["reg_lv","eng"]]] <- pagetext["reg_lv","eng"]
       s_options[[pagetext["reg_myk","eng"]]] <- pagetext["reg_myk","eng"]
       s_options[[pagetext["reg_ode","eng"]]] <- pagetext["reg_ode","eng"]
       s_options[[pagetext["reg_pol","eng"]]] <- pagetext["reg_pol","eng"]
       s_options[[pagetext["reg_riv","eng"]]] <- pagetext["reg_riv","eng"]
       s_options[[pagetext["reg_sum","eng"]]] <- pagetext["reg_sum","eng"]
       s_options[[pagetext["reg_ter","eng"]]] <- pagetext["reg_ter","eng"]
       s_options[[pagetext["reg_har","eng"]]] <- pagetext["reg_har","eng"]
       s_options[[pagetext["reg_her","eng"]]] <- pagetext["reg_her","eng"]
       s_options[[pagetext["reg_khm","eng"]]] <- pagetext["reg_khm","eng"]
       s_options[[pagetext["reg_chk","eng"]]] <- pagetext["reg_chk","eng"]
       s_options[[pagetext["reg_chr","eng"]]] <- pagetext["reg_chr","eng"]
       s_options[[pagetext["reg_chn","eng"]]] <- pagetext["reg_chn","eng"]
       s_options[[pagetext["reg_k","eng"]]] <- pagetext["reg_k","eng"]
       s_options[[pagetext["reg_s","eng"]]] <- pagetext["reg_s","eng"]
       
       updateSelectInput(session, "inputRegion",                        
                         choices = s_options, selected = input$inputRegion)
       
      if (input$inputRegion %in% regions) { 
      updateSelectInput(session, "inputDistrict",
                        choices = min(base$precinct[base$region == input$inputRegion]):
                        max(base$precinct[base$region == input$inputRegion]),selected=input$inputDistrict)
      } else if (input$inputRegion == "All regions") {
        updateSelectInput(session, "inputDistrict",choices = 1:225,selected=input$inputDistrict)
      } 
      
      updateSelectInput(session, "inputRegion",                  
                        choices = s_options,
                        selected = input$inputRegion)
    
      s_options2 <- list()
      s_options2[[pagetext["turn","eng"]]] <- pagetext["turn","eng"]
      s_options2[[pagetext["home","eng"]]] <- pagetext["home","eng"]
      s_options2[[pagetext["invalid","eng"]]] <- pagetext["invalid","eng"]
      s_options2[[pagetext["bogom","eng"]]] <- pagetext["bogom","eng"]
      s_options2[[pagetext["boyko","eng"]]] <- pagetext["boyko","eng"]
      s_options2[[pagetext["grin","eng"]]] <- pagetext["grin","eng"]
      s_options2[[pagetext["gric","eng"]]] <- pagetext["gric","eng"]
      s_options2[[pagetext["dobk","eng"]]] <- pagetext["dobk","eng"]
      s_options2[[pagetext["klym","eng"]]] <- pagetext["klym","eng"]
      s_options2[[pagetext["konov","eng"]]] <- pagetext["konov","eng"]
      s_options2[[pagetext["kuzm","eng"]]] <- pagetext["kuzm","eng"]
      s_options2[[pagetext["kuyb","eng"]]] <- pagetext["kuyb","eng"]
      s_options2[[pagetext["lyash","eng"]]] <- pagetext["lyash","eng"]
      s_options2[[pagetext["malom","eng"]]] <- pagetext["malom","eng"]
      s_options2[[pagetext["poros","eng"]]] <- pagetext["poros","eng"]
      s_options2[[pagetext["rabin","eng"]]] <- pagetext["rabin","eng"]
      s_options2[[pagetext["saran","eng"]]] <- pagetext["saran","eng"]
      s_options2[[pagetext["simon","eng"]]] <- pagetext["simon","eng"]
      s_options2[[pagetext["timos","eng"]]] <- pagetext["timos","eng"]
      s_options2[[pagetext["tigip","eng"]]] <- pagetext["tigip","eng"]
      s_options2[[pagetext["tyahn","eng"]]] <- pagetext["tyahn","eng"]
      s_options2[[pagetext["cushk","eng"]]] <- pagetext["cushk","eng"]
      s_options2[[pagetext["shkir","eng"]]] <- pagetext["shkir","eng"]
      s_options2[[pagetext["yaros","eng"]]] <- pagetext["yaros","eng"]
      s_options2[[pagetext["disabled","eng"]]] <- pagetext["disabled","eng"]
      s_options2[[pagetext["subtim","eng"]]] <- pagetext["subtim","eng"]
      
      updateSelectInput(session, "plot1",                  
                        choices = s_options2[-25],
                        selected = input$plot1)
      
      updateSelectInput(session, "plot2",                  
                        choices = s_options2[4:25],
                        selected = input$plot2)
      
       if (input$plot1 %in% c("Time of submission of protocol")) {updateSelectInput(session,'plot2',selected="Disabled")}
#       
      plotdata <- subset(base,base$voters_num >= input$sizeRange[1] & base$voters_num <= input$sizeRange[2])
      plotdata <- subset(plotdata,as.numeric(plotdata$time) >= input$timeRange[1] & as.numeric(plotdata$time) <= input$timeRange[2])
      if (input$inputRegion %in% regions) {
        plotdata <- subset(plotdata,plotdata$region == input$inputRegion)
      }
      if (length(input$inputDistrict) != 0) {
        plotdata <- subset(plotdata,plotdata$precinct %in% input$inputDistrict)
      }
      output$rownums <- renderText({paste(pagetext["prec_num","eng"],nrow(plotdata))})

  output$plot <- renderPlot({
    plot_data <<- switch(input$plot1,
                         "Turnout" =plotdata$participated*100/plotdata$voters_num,
                         "Voted at home (% of voters)" =plotdata$home_bulletin*100/plotdata$participated,
                         "Invalid ballots (% of votes)" =plotdata$invalid*100/plotdata$participated,
                         "Bohomolec O. V. %" =plotdata$bogomolec*100/plotdata$participated,
                         "Boyko Yu. A. %" =plotdata$boyko*100/plotdata$participated,
                         "Grynenko A. V. %" =plotdata$grinenko*100/plotdata$participated,
                         "Gritsenko A. S. %" =plotdata$gricenko*100/plotdata$participated,
                         "Dobkin M. M. %" =plotdata$dobkin*100/plotdata$participated,
                         "Klimenko A. I. %" =plotdata$klimenko*100/plotdata$participated,
                         "Konovalyuk V. I. %" =plotdata$konovalenko*100/plotdata$participated,
                         "Kuzmin R. R. %" =plotdata$kuzmin*100/plotdata$participated,
                         "Kuibida V. S. %" =plotdata$kuibida*100/plotdata$participated,
                         "Lyashko O. V. %" =plotdata$lyashko*100/plotdata$participated,
                         "Malomuzh M. G. %" =plotdata$malomuzh*100/plotdata$participated,
                         "Poroshenko P. O. %" =plotdata$poroshenko*100/plotdata$participated,
                         "Rabinovich V. Z. %" =plotdata$rabinovich*100/plotdata$participated,
                         "Saranov V. G. %" =plotdata$saranov*100/plotdata$participated,
                         "Symonenko P. M. %" =plotdata$simonenko*100/plotdata$participated,
                         "Tymoshenko Yu. V. %" =plotdata$timoshenko*100/plotdata$participated,
                         "Tigipko S. L. %" =plotdata$tigipko*100/plotdata$participated,
                         "Tyagnibok O. Ya. %" =plotdata$tyahnibok*100/plotdata$participated,
                         "Tsushko V. P. %" =plotdata$cushko*100/plotdata$participated,
                         "Shkiriak S. N. %" =plotdata$shkiryak*100/plotdata$participated,
                         "Yarosh D. O. %" =plotdata$yarosh*100/plotdata$participated,
                         "Time of protocol submission" = as.numeric(plotdata$time))
    
    plot_data2 <<- rep(0,length(plot_data))

    if (input$plot2 == "Disabled") {
    text = pagetext[pagetext$eng == input$plot1,"eng"] }
    else {
      
    plot_data2 <<- switch(input$plot2,
                          "Turnout" =plotdata$participated*100/plotdata$voters_num,
                          "Voted at home (% of voters)" =plotdata$home_bulletin*100/plotdata$participated,
                          "Invalid ballots (% of votes)" =plotdata$invalid*100/plotdata$participated,
                          "Bohomolec O. V. %" =plotdata$bogomolec*100/plotdata$participated,
                          "Boyko Yu. A. %" =plotdata$boyko*100/plotdata$participated,
                          "Grynenko A. V. %" =plotdata$grinenko*100/plotdata$participated,
                          "Gritsenko A. S. %" =plotdata$gricenko*100/plotdata$participated,
                          "Dobkin M. M. %" =plotdata$dobkin*100/plotdata$participated,
                          "Klimenko A. I. %" =plotdata$klimenko*100/plotdata$participated,
                          "Konovalyuk V. I. %" =plotdata$konovalenko*100/plotdata$participated,
                          "Kuzmin R. R. %" =plotdata$kuzmin*100/plotdata$participated,
                          "Kuibida V. S. %" =plotdata$kuibida*100/plotdata$participated,
                          "Lyashko O. V. %" =plotdata$lyashko*100/plotdata$participated,
                          "Malomuzh M. G. %" =plotdata$malomuzh*100/plotdata$participated,
                          "Poroshenko P. O. %" =plotdata$poroshenko*100/plotdata$participated,
                          "Rabinovich V. Z. %" =plotdata$rabinovich*100/plotdata$participated,
                          "Saranov V. G. %" =plotdata$saranov*100/plotdata$participated,
                          "Symonenko P. M. %" =plotdata$simonenko*100/plotdata$participated,
                          "Tymoshenko Yu. V. %" =plotdata$timoshenko*100/plotdata$participated,
                          "Tigipko S. L. %" =plotdata$tigipko*100/plotdata$participated,
                          "Tyagnibok O. Ya. %" =plotdata$tyahnibok*100/plotdata$participated,
                          "Tsushko V. P. %" =plotdata$cushko*100/plotdata$participated,
                          "Shkiriak S. N. %" =plotdata$shkiryak*100/plotdata$participated,
                          "Yarosh D. O. %" =plotdata$yarosh*100/plotdata$participated,
                          "Time of protocol submission" = as.numeric(plotdata$time))
    
  text = paste(pagetext["diff","eng"],
               pagetext[pagetext$eng == input$plot1,"eng"],
               pagetext["and","eng"],
               pagetext[pagetext$eng == input$plot2,"eng"])
    }

                    ggplot(data=plotdata, aes(x=lon, y=lat,colour = plot_data-plot_data2),
                                          size=2, alpha=0.8)+ geom_point()+
                         scale_colour_gradient(low="blue",high="red",name=text) +
                         theme(legend.position="bottom") + coord_fixed() + 
                  xlim(22.16049, 40.13864) + ylim(44.4963,52.33833) 
}, height = 600)

output$hist <- renderPlot({
  plot_data <<- switch(input$plot1,
                       "Turnout" =plotdata$participated*100/plotdata$voters_num,
                       "Voted at home (% of voters)" =plotdata$home_bulletin*100/plotdata$participated,
                       "Invalid ballots (% of votes)" =plotdata$invalid*100/plotdata$participated,
                       "Bohomolec O. V. %" =plotdata$bogomolec*100/plotdata$participated,
                       "Boyko Yu. A. %" =plotdata$boyko*100/plotdata$participated,
                       "Grynenko A. V. %" =plotdata$grinenko*100/plotdata$participated,
                       "Gritsenko A. S. %" =plotdata$gricenko*100/plotdata$participated,
                       "Dobkin M. M. %" =plotdata$dobkin*100/plotdata$participated,
                       "Klimenko A. I. %" =plotdata$klimenko*100/plotdata$participated,
                       "Konovalyuk V. I. %" =plotdata$konovalenko*100/plotdata$participated,
                       "Kuzmin R. R. %" =plotdata$kuzmin*100/plotdata$participated,
                       "Kuibida V. S. %" =plotdata$kuibida*100/plotdata$participated,
                       "Lyashko O. V. %" =plotdata$lyashko*100/plotdata$participated,
                       "Malomuzh M. G. %" =plotdata$malomuzh*100/plotdata$participated,
                       "Poroshenko P. O. %" =plotdata$poroshenko*100/plotdata$participated,
                       "Rabinovich V. Z. %" =plotdata$rabinovich*100/plotdata$participated,
                       "Saranov V. G. %" =plotdata$saranov*100/plotdata$participated,
                       "Symonenko P. M. %" =plotdata$simonenko*100/plotdata$participated,
                       "Tymoshenko Yu. V. %" =plotdata$timoshenko*100/plotdata$participated,
                       "Tigipko S. L. %" =plotdata$tigipko*100/plotdata$participated,
                       "Tyagnibok O. Ya. %" =plotdata$tyahnibok*100/plotdata$participated,
                       "Tsushko V. P. %" =plotdata$cushko*100/plotdata$participated,
                       "Shkiriak S. N. %" =plotdata$shkiryak*100/plotdata$participated,
                       "Yarosh D. O. %" =plotdata$yarosh*100/plotdata$participated,
                       "Time of protocol submission" = as.numeric(plotdata$time))

    plot_data2 <<- switch(input$plot2,
                          "Turnout" =plotdata$participated*100/plotdata$voters_num,
                          "Voted at home (% of voters)" =plotdata$home_bulletin*100/plotdata$participated,
                          "Invalid ballots (% of votes)" =plotdata$invalid*100/plotdata$participated,
                          "Bohomolec O. V. %" =plotdata$bogomolec*100/plotdata$participated,
                          "Boyko Yu. A. %" =plotdata$boyko*100/plotdata$participated,
                          "Grynenko A. V. %" =plotdata$grinenko*100/plotdata$participated,
                          "Gritsenko A. S. %" =plotdata$gricenko*100/plotdata$participated,
                          "Dobkin M. M. %" =plotdata$dobkin*100/plotdata$participated,
                          "Klimenko A. I. %" =plotdata$klimenko*100/plotdata$participated,
                          "Konovalyuk V. I. %" =plotdata$konovalenko*100/plotdata$participated,
                          "Kuzmin R. R. %" =plotdata$kuzmin*100/plotdata$participated,
                          "Kuibida V. S. %" =plotdata$kuibida*100/plotdata$participated,
                          "Lyashko O. V. %" =plotdata$lyashko*100/plotdata$participated,
                          "Malomuzh M. G. %" =plotdata$malomuzh*100/plotdata$participated,
                          "Poroshenko P. O. %" =plotdata$poroshenko*100/plotdata$participated,
                          "Rabinovich V. Z. %" =plotdata$rabinovich*100/plotdata$participated,
                          "Saranov V. G. %" =plotdata$saranov*100/plotdata$participated,
                          "Symonenko P. M. %" =plotdata$simonenko*100/plotdata$participated,
                          "Tymoshenko Yu. V. %" =plotdata$timoshenko*100/plotdata$participated,
                          "Tigipko S. L. %" =plotdata$tigipko*100/plotdata$participated,
                          "Tyagnibok O. Ya. %" =plotdata$tyahnibok*100/plotdata$participated,
                          "Tsushko V. P. %" =plotdata$cushko*100/plotdata$participated,
                          "Shkiriak S. N. %" =plotdata$shkiryak*100/plotdata$participated,
                          "Yarosh D. O. %" =plotdata$yarosh*100/plotdata$participated,
                          "Time of protocol submission" = as.numeric(plotdata$time))
  
  if (input$plot2 == "Disabled") {
  ggplot(data=plotdata, aes(x=plot_data)) + 
    geom_histogram(aes(fill = ..count..)) + 
    xlab(pagetext[pagetext$eng == input$plot1,"eng"]) +
    ylab(pagetext["dist_num","eng"]) + theme(legend.position="none")
  } else {
    histdata = melt(cbind(plot_data,plot_data2))
    levels(histdata$X2) = c(pagetext[pagetext$eng == input$plot1,"eng"],pagetext[pagetext$eng == input$plot2,"eng"])
    ggplot(data=histdata, 
           aes(x=value)) + geom_histogram(aes(fill = ..count..),binwidth=1) +
      facet_grid(X2 ~ .) +
      xlab(pagetext[pagetext$eng == input$plot1,"eng"]) +
      ylab(pagetext["dist_num","eng"]) + theme(legend.position="none")
  }
  
}, height = 600)

output$scatter <- renderPlot({
  plot_data <<- switch(input$plot1,
                       "Turnout" =plotdata$participated*100/plotdata$voters_num,
                       "Voted at home (% of voters)" =plotdata$home_bulletin*100/plotdata$participated,
                       "Invalid ballots (% of votes)" =plotdata$invalid*100/plotdata$participated,
                       "Bohomolec O. V. %" =plotdata$bogomolec*100/plotdata$participated,
                       "Boyko Yu. A. %" =plotdata$boyko*100/plotdata$participated,
                       "Grynenko A. V. %" =plotdata$grinenko*100/plotdata$participated,
                       "Gritsenko A. S. %" =plotdata$gricenko*100/plotdata$participated,
                       "Dobkin M. M. %" =plotdata$dobkin*100/plotdata$participated,
                       "Klimenko A. I. %" =plotdata$klimenko*100/plotdata$participated,
                       "Konovalyuk V. I. %" =plotdata$konovalenko*100/plotdata$participated,
                       "Kuzmin R. R. %" =plotdata$kuzmin*100/plotdata$participated,
                       "Kuibida V. S. %" =plotdata$kuibida*100/plotdata$participated,
                       "Lyashko O. V. %" =plotdata$lyashko*100/plotdata$participated,
                       "Malomuzh M. G. %" =plotdata$malomuzh*100/plotdata$participated,
                       "Poroshenko P. O. %" =plotdata$poroshenko*100/plotdata$participated,
                       "Rabinovich V. Z. %" =plotdata$rabinovich*100/plotdata$participated,
                       "Saranov V. G. %" =plotdata$saranov*100/plotdata$participated,
                       "Symonenko P. M. %" =plotdata$simonenko*100/plotdata$participated,
                       "Tymoshenko Yu. V. %" =plotdata$timoshenko*100/plotdata$participated,
                       "Tigipko S. L. %" =plotdata$tigipko*100/plotdata$participated,
                       "Tyagnibok O. Ya. %" =plotdata$tyahnibok*100/plotdata$participated,
                       "Tsushko V. P. %" =plotdata$cushko*100/plotdata$participated,
                       "Shkiriak S. N. %" =plotdata$shkiryak*100/plotdata$participated,
                       "Yarosh D. O. %" =plotdata$yarosh*100/plotdata$participated,
                       "Time of protocol submission" = as.numeric(plotdata$time))
  
  plot_data2 <<- switch(input$plot2,
                        "Turnout" =plotdata$participated*100/plotdata$voters_num,
                        "Voted at home (% of voters)" =plotdata$home_bulletin*100/plotdata$participated,
                        "Invalid ballots (% of votes)" =plotdata$invalid*100/plotdata$participated,
                        "Bohomolec O. V. %" =plotdata$bogomolec*100/plotdata$participated,
                        "Boyko Yu. A. %" =plotdata$boyko*100/plotdata$participated,
                        "Grynenko A. V. %" =plotdata$grinenko*100/plotdata$participated,
                        "Gritsenko A. S. %" =plotdata$gricenko*100/plotdata$participated,
                        "Dobkin M. M. %" =plotdata$dobkin*100/plotdata$participated,
                        "Klimenko A. I. %" =plotdata$klimenko*100/plotdata$participated,
                        "Konovalyuk V. I. %" =plotdata$konovalenko*100/plotdata$participated,
                        "Kuzmin R. R. %" =plotdata$kuzmin*100/plotdata$participated,
                        "Kuibida V. S. %" =plotdata$kuibida*100/plotdata$participated,
                        "Lyashko O. V. %" =plotdata$lyashko*100/plotdata$participated,
                        "Malomuzh M. G. %" =plotdata$malomuzh*100/plotdata$participated,
                        "Poroshenko P. O. %" =plotdata$poroshenko*100/plotdata$participated,
                        "Rabinovich V. Z. %" =plotdata$rabinovich*100/plotdata$participated,
                        "Saranov V. G. %" =plotdata$saranov*100/plotdata$participated,
                        "Symonenko P. M. %" =plotdata$simonenko*100/plotdata$participated,
                        "Tymoshenko Yu. V. %" =plotdata$timoshenko*100/plotdata$participated,
                        "Tigipko S. L. %" =plotdata$tigipko*100/plotdata$participated,
                        "Tyagnibok O. Ya. %" =plotdata$tyahnibok*100/plotdata$participated,
                        "Tsushko V. P. %" =plotdata$cushko*100/plotdata$participated,
                        "Shkiriak S. N. %" =plotdata$shkiryak*100/plotdata$participated,
                        "Yarosh D. O. %" =plotdata$yarosh*100/plotdata$participated,
                        "Time of protocol submission" = as.numeric(plotdata$time))
  
  if (input$plot2 != "Disabled") {
    scatter = as.data.frame(cbind(plot_data,plot_data2))    
    names(scatter) = c("X","Y")
    ggplot(data=scatter, aes(x=X,y=Y)) +
      geom_point(shape=19) + 
      geom_smooth(method=lm, se=FALSE) +
      xlab(pagetext[pagetext$eng == input$plot1,"eng"]) +
      ylab(pagetext[pagetext$eng == input$plot2,"eng"])
  } else {
    library(ggplot2)
    text = paste(pagetext["sec_var","eng"])
    ggplot() + 
      annotate("text", x = 4, y = 25, size=8, label = text) + 
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())
  }  
}, height = 600)

output$sumtable1 <- renderDataTable({
  a<-aggregate(plotdata[,c(8,15:37)],list(plotdata$precinct),sum)
  names(a) = pagetext[c("district_num","district_num","turn","invalid","bogom","boyko","grin","gric","dobk","klym",
                        "konov","kuzm","kuyb","lyash","malom", "poros","rabin","saran","simon","timos","tigip",
                        "tyahn","cushk","shkir","yaros"),"eng"]
  names(a) = gsub("\\( "," ",(gsub("%","", names(a))))
  b<-apply(a[2:25],2,sum)*100/apply(a[2:24],2,sum)[1]
  winners <- names(sort(b[4:25],decreasing=T))[1:5]
  b[1] = apply(a[2:24],2,sum)[2]/apply(a[2:24],2,sum)[1]
  b=format(b,nsmall=2,digits=0, scientific=F)
  finaltable = rbind(a[-2],c(b))
  finaltable = cbind(finaltable[c(1,3,4)],finaltable[winners])
  finaltable
}, options = list(bSortClasses = FALSE,bFilter = FALSE,iDisplayLength = 10))

})

}
)

