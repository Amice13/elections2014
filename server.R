library(shiny)
library(ggplot2)
library(reshape)


base <- read.table('shiny_app.csv',header=T,sep='\t',stringsAsFactor=F, na.strings=c("","NA"))
base <- subset(base,base$precinct != 226)
base$time <- strptime(base$time, format="%d.%m.%Y %H:%M")
base <<- subset(base,!is.na(base$time))

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
  rus = c("Выборы Президента 2014","Карта","Диаграмма рассеяния",
          "Гистограмма","Итоги","Автор", "Выберите ваш язык","Размер избирательных участков",
          "Время составления протокола","Фильтры", "С", "до","Территория","Область","Округ","Все регионы",
          "Автономная Республика Крым","Винницкая область","Волынская область","Днепропетровская область",
          "Донецкая область","Житомирская область","Закарпатская область","Запорожская область",
          "Ивано-Франковская область","Киевская область","Кировоградская область","Луганская область",
          "Львовская область","Николаевская область","Одесская область","Полтавская область","Ровенская область",
          "Сумская область","Тернопольская область","Харьковская область","Херсонская область","Хмельницкая область",
          "Черкасская область","Черновицкая область","Черниговская область","г.Киев","г.Севастополь", 
          "Количество выделенных избирательных участков составляет","Явка", 
          "Голосование на дому (% от проголосовавших)","Недействительные бюллетени (% от проголосовавших)",
          "Богомолец О.В. %","Бойко Ю. А. %","Гриненко А. В. %","Гриценко А. С. %","Добкин М. М. %",
          "Клименко А. И. %","Коновалюк В. И. %","Кузьмин Р.Р. %","Куйбида В. С. %","Ляшко О. В. %",
          "Маломуж М.Г. %","Порошенко П. А. %","Рабинович В. С. %","Саранов В.Г. %","Симоненко П. Н. %",
          "Тимошенко Ю. В. %","Тигипко С. Л. %","Тягнибок О. Я. %","Цушко В.П. %","Шкиряк С. Н. %",
          "Ярош Д. А. %","Время принятия протоколов","Не доступен","Основной график","Сравнительный график",
          "Составление графиков","Построить график", "Разница между", "и","Итого","Количество избирательных участков",
          "Пожалуйста, выберите вторую переменную","О приложении"),
  ukr = c("Вибори Президента 2014","Мапа", "Точкова діаграма",
          "Гістограма","Підсумкові таблиці","Автор", "Оберіть вашу мову","Розмір виборчих дільниць",
          "Час складання протоколу","Фільтри","З", "до","Територія","Область","Округ","Всі регіони",
          "Автономна Республіка Крим","Вінницька область","Волинська область","Дніпропетровська область",
          "Донецька область","Житомирська область","Закарпатська область","Запорізька область",
          "Івано-Франківська область","Київська область","Кіровоградська область","Луганська область",
          "Львівська область","Миколаївська область","Одеська область","Полтавська область",
          "Рівненська область","Сумська область","Тернопільська область","Харківська область",
          "Херсонська область","Хмельницька область","Черкаська область","Чернівецька область",
          "Чернігівська область","м.Київ","м.Севастополь", "Кількість обраних виборчих дільниць складає", 
          "Явка","Голосовуння на дому (% від проголосувавших)","Недійсні бюлетені (% від проголосувавших)",
          "Богомолець О. В. %","Бойко Ю. А. %","Гриненко А. В. %","Гриценко А. С. %","Добкін М. М. %",
          "Клименко О. І. % ","Коновалюк В. І. %","Кузьмін Р. Р. %","Куйбіда В. С. %","Ляшко О. В. %",
          "Маломуж М. Г. %","Порошенко П. О. %","Рабінович В. З. %","Саранов В. Г. %","Симоненко П. М. %",
          "Тимошенко Ю. В. %","Тігіпко С. Л. %","Тягнибок О. Я. %","Цушко В. П. %","% Шкіряк З. Н.",
          "Ярош Д. А. %","Час прийняття протоколів","Не доступний","Основний графік","Порівняльний графік",
          "Складення графіків","Побудувати графік", "Різниця між", "та","Всього","Кількість виборчих дільниць",
          "Будь ласка, оберіть другу змінну","Про додаток"),
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
    output$title <- renderText({pagetext["title",input$lang]})
    output$tab1 <- renderText({pagetext["tab1",input$lang]})
    output$tab2 <- renderText({pagetext["tab2",input$lang]})
    output$tab3 <- renderText({pagetext["tab3",input$lang]})
    output$tab4 <- renderText({pagetext["tab4",input$lang]})
    output$tab5 <- renderText({pagetext["tab5",input$lang]})
    output$lang <- renderText({pagetext["lang",input$lang]})
    output$filter <- renderText({pagetext["filter",input$lang]})
    output$size <- renderText({pagetext["size",input$lang]})
    output$time <- renderText({pagetext["time",input$lang]})
    output$selectedTime <- renderText({paste(pagetext["from",input$lang],
                                             strftime(as.POSIXct(input$timeRange[1], origin = "1960-01-02"),format="%H:%M (%d.%m)"),
                                             pagetext["to",input$lang],
                                             strftime(as.POSIXct(input$timeRange[2], origin = "1960-01-02"),format="%H:%M (%d.%m)"))})
    output$teritorry <- renderText({pagetext["teritorry",input$lang]})
    output$region <- renderText({pagetext["region",input$lang]})
    output$reg_all <- renderText({pagetext["reg_all",input$lang]})
    output$reg_arc <- renderText({pagetext["reg_arc",input$lang]})
    output$reg_vin <- renderText({pagetext["reg_vin",input$lang]})
    output$mainplot <- renderText({pagetext["mainplot",input$lang]})
    output$secplot <- renderText({pagetext["secplot",input$lang]})
    output$makeplot <- renderText({pagetext["makeplot",input$lang]})
    output$district_num <- renderText({pagetext["district_num",input$lang]})
    output$subm_but <- renderText({pagetext["subm_but",input$lang]})
    output$about <- renderText({pagetext["about",input$lang]})
    
    # Sliderange with precinct size
    output$precSizeMin <- renderText({min(base$voters_num,na.rm=T)})
    output$precSizeMax <- renderText({max(base$voters_num,na.rm=T)})
    
    # Sliderange with time
    output$timeMin <- renderText({min(time,na.rm=T)})
    output$timeMax <- renderText({max(time,na.rm=T)})
    
    # Input with regions
    observe({
      
      s_options <- list()
      s_options[[pagetext["reg_all",input$lang]]] <- pagetext["reg_all","ukr"]
#      s_options[[pagetext["reg_arc",input$lang]]] <- pagetext["reg_arc","ukr"]
      s_options[[pagetext["reg_vin",input$lang]]] <- pagetext["reg_vin","ukr"]
      s_options[[pagetext["reg_vol",input$lang]]] <- pagetext["reg_vol","ukr"]
      s_options[[pagetext["reg_dp",input$lang]]] <- pagetext["reg_dp","ukr"]
      s_options[[pagetext["reg_don",input$lang]]] <- pagetext["reg_don","ukr"]
      s_options[[pagetext["reg_zht",input$lang]]] <- pagetext["reg_zht","ukr"]
      s_options[[pagetext["reg_zkp",input$lang]]] <- pagetext["reg_zkp","ukr"]
      s_options[[pagetext["reg_zap",input$lang]]] <- pagetext["reg_zap","ukr"]
      s_options[[pagetext["reg_ifr",input$lang]]] <- pagetext["reg_ifr","ukr"]
      s_options[[pagetext["reg_kie",input$lang]]] <- pagetext["reg_kie","ukr"]
      s_options[[pagetext["reg_kir",input$lang]]] <- pagetext["reg_kir","ukr"]
      s_options[[pagetext["reg_lug",input$lang]]] <- pagetext["reg_lug","ukr"]
      s_options[[pagetext["reg_lv",input$lang]]] <- pagetext["reg_lv","ukr"]
      s_options[[pagetext["reg_myk",input$lang]]] <- pagetext["reg_myk","ukr"]
      s_options[[pagetext["reg_ode",input$lang]]] <- pagetext["reg_ode","ukr"]
      s_options[[pagetext["reg_pol",input$lang]]] <- pagetext["reg_pol","ukr"]
      s_options[[pagetext["reg_riv",input$lang]]] <- pagetext["reg_riv","ukr"]
      s_options[[pagetext["reg_sum",input$lang]]] <- pagetext["reg_sum","ukr"]
      s_options[[pagetext["reg_ter",input$lang]]] <- pagetext["reg_ter","ukr"]
      s_options[[pagetext["reg_har",input$lang]]] <- pagetext["reg_har","ukr"]
      s_options[[pagetext["reg_her",input$lang]]] <- pagetext["reg_her","ukr"]
      s_options[[pagetext["reg_khm",input$lang]]] <- pagetext["reg_khm","ukr"]
      s_options[[pagetext["reg_chk",input$lang]]] <- pagetext["reg_chk","ukr"]
      s_options[[pagetext["reg_chr",input$lang]]] <- pagetext["reg_chr","ukr"]
      s_options[[pagetext["reg_chn",input$lang]]] <- pagetext["reg_chn","ukr"]
      s_options[[pagetext["reg_k",input$lang]]] <- pagetext["reg_k","ukr"]
      s_options[[pagetext["reg_s",input$lang]]] <- pagetext["reg_s","ukr"]
      
      updateSelectInput(session, "inputRegion",                        
                        choices = s_options,
                        selected = input$inputRegion)
      
      if (input$inputRegion %in% regions) { 
      updateSelectInput(session, "inputDistrict",
                        choices = min(base$precinct[base$region == input$inputRegion]):
                        max(base$precinct[base$region == input$inputRegion]),selected=input$inputDistrict)
      } else if (input$inputRegion == "Всі регіони") {
        updateSelectInput(session, "inputDistrict",choices = 1:225,selected=input$inputDistrict)
      } 
      
      updateSelectInput(session, "inputRegion",                  
                        choices = s_options,
                        selected = input$inputRegion)
    
      s_options2 <- list()
      s_options2[[pagetext["turn",input$lang]]] <- pagetext["turn","ukr"]
      s_options2[[pagetext["home",input$lang]]] <- pagetext["home","ukr"]
      s_options2[[pagetext["invalid",input$lang]]] <- pagetext["invalid","ukr"]
      s_options2[[pagetext["bogom",input$lang]]] <- pagetext["bogom","ukr"]
      s_options2[[pagetext["boyko",input$lang]]] <- pagetext["boyko","ukr"]
      s_options2[[pagetext["grin",input$lang]]] <- pagetext["grin","ukr"]
      s_options2[[pagetext["gric",input$lang]]] <- pagetext["gric","ukr"]
      s_options2[[pagetext["dobk",input$lang]]] <- pagetext["dobk","ukr"]
      s_options2[[pagetext["klym",input$lang]]] <- pagetext["klym","ukr"]
      s_options2[[pagetext["konov",input$lang]]] <- pagetext["konov","ukr"]
      s_options2[[pagetext["kuzm",input$lang]]] <- pagetext["kuzm","ukr"]
      s_options2[[pagetext["kuyb",input$lang]]] <- pagetext["kuyb","ukr"]
      s_options2[[pagetext["lyash",input$lang]]] <- pagetext["lyash","ukr"]
      s_options2[[pagetext["malom",input$lang]]] <- pagetext["malom","ukr"]
      s_options2[[pagetext["poros",input$lang]]] <- pagetext["poros","ukr"]
      s_options2[[pagetext["rabin",input$lang]]] <- pagetext["rabin","ukr"]
      s_options2[[pagetext["saran",input$lang]]] <- pagetext["saran","ukr"]
      s_options2[[pagetext["simon",input$lang]]] <- pagetext["simon","ukr"]
      s_options2[[pagetext["timos",input$lang]]] <- pagetext["timos","ukr"]
      s_options2[[pagetext["tigip",input$lang]]] <- pagetext["tigip","ukr"]
      s_options2[[pagetext["tyahn",input$lang]]] <- pagetext["tyahn","ukr"]
      s_options2[[pagetext["cushk",input$lang]]] <- pagetext["cushk","ukr"]
      s_options2[[pagetext["shkir",input$lang]]] <- pagetext["shkir","ukr"]
      s_options2[[pagetext["yaros",input$lang]]] <- pagetext["yaros","ukr"]
      s_options2[[pagetext["disabled",input$lang]]] <- pagetext["disabled","ukr"]
      s_options2[[pagetext["subtim",input$lang]]] <- pagetext["subtim","ukr"]
      
      updateSelectInput(session, "plot1",                  
                        choices = s_options2[-25],
                        selected = input$plot1)
      
      updateSelectInput(session, "plot2",                  
                        choices = s_options2[4:25],
                        selected = input$plot2)
      
      if (input$plot1 %in% c("Час прийняття протоколів")) {updateSelectInput(session,'plot2',selected="Не доступний")}
      
      plotdata <- subset(base,base$voters_num >= input$sizeRange[1] & base$voters_num <= input$sizeRange[2])
      plotdata <- subset(plotdata,as.numeric(plotdata$time) >= input$timeRange[1] & as.numeric(plotdata$time) <= input$timeRange[2])
      if (input$inputRegion %in% regions) {
        plotdata <- subset(plotdata,plotdata$region == input$inputRegion)
      }
      if (length(input$inputDistrict) != 0) {
        plotdata <- subset(plotdata,plotdata$precinct %in% input$inputDistrict)
      }
      output$rownums <- renderText({paste(pagetext["prec_num",input$lang],nrow(plotdata))})

  output$plot <- renderPlot({
    plot_data <<- switch(input$plot1,
                            "Явка" = plotdata$participated*100/plotdata$voters_num,
                            "Голосовуння на дому (% від проголосувавших)" = plotdata$home_bulletin*100/plotdata$participated,
                            "Недійсні бюлетені (% від проголосувавших)" = plotdata$invalid*100/plotdata$participated,
                            "Богомолець О. В. %" = plotdata$bogomolec*100/plotdata$participated,
                            "Бойко Ю. А. %" = plotdata$boyko*100/plotdata$participated,
                            "Гриненко А. В. %" = plotdata$grinenko*100/plotdata$participated,
                            "Гриценко А. С. %"= plotdata$gricenko*100/plotdata$participated,
                            "Добкін М. М. %"= plotdata$dobkin*100/plotdata$participated,
                            "Клименко О. І. % "= plotdata$klimenko*100/plotdata$participated,
                            "Коновалюк В. І. %"= plotdata$konovalenko*100/plotdata$participated,
                            "Кузьмін Р. Р. %"= plotdata$kuzmin*100/plotdata$participated,
                            "Куйбіда В. С. %"= plotdata$kuibida*100/plotdata$participated,
                            "Ляшко О. В. %"= plotdata$lyashko*100/plotdata$participated,
                            "Маломуж М. Г. %"= plotdata$malomuzh*100/plotdata$participated,
                            "Порошенко П. О. %"= plotdata$poroshenko*100/plotdata$participated,
                            "Рабінович В. З. %"= plotdata$rabinovich*100/plotdata$participated,
                            "Саранов В. Г. %"= plotdata$saranov*100/plotdata$participated,
                            "Симоненко П. М. %"= plotdata$simonenko*100/plotdata$participated,
                            "Тимошенко Ю. В. %"= plotdata$timoshenko*100/plotdata$participated,
                            "Тігіпко С. Л. %"= plotdata$tigipko*100/plotdata$participated,
                            "Тягнибок О. Я. %"= plotdata$tyahnibok*100/plotdata$participated,
                            "Цушко В. П. %"= plotdata$cushko*100/plotdata$participated,
                            "% Шкіряк З. Н."= plotdata$shkiryak*100/plotdata$participated,
                            "Ярош Д. А. %"= plotdata$yarosh*100/plotdata$participated,
                            "Час прийняття протоколів"= as.numeric(plotdata$time))
    plot_data2 <<- rep(0,length(plot_data))

    if (input$plot2 == "Не доступний") {
    text = pagetext[pagetext$ukr == input$plot1,input$lang] }
    else {
      
    plot_data2 <<- switch(input$plot2,
                       "Явка" = plotdata$participated*100/plotdata$voters_num,
                       "Голосовуння на дому (% від проголосувавших)" = plotdata$home_bulletin*100/plotdata$participated,
                       "Недійсні бюлетені (% від проголосувавших)" = plotdata$invalid*100/plotdata$participated,
                       "Богомолець О. В. %" = plotdata$bogomolec*100/plotdata$participated,
                       "Бойко Ю. А. %" = plotdata$boyko*100/plotdata$participated,
                       "Гриненко А. В. %" = plotdata$grinenko*100/plotdata$participated,
                       "Гриценко А. С. %"= plotdata$gricenko*100/plotdata$participated,
                       "Добкін М. М. %"= plotdata$dobkin*100/plotdata$participated,
                       "Клименко О. І. % "= plotdata$klimenko*100/plotdata$participated,
                       "Коновалюк В. І. %"= plotdata$konovaluk*100/plotdata$participated,
                       "Кузьмін Р. Р. %"= plotdata$kuzmin*100/plotdata$participated,
                       "Куйбіда В. С. %"= plotdata$kuibida*100/plotdata$participated,
                       "Ляшко О. В. %"= plotdata$lyashko*100/plotdata$participated,
                       "Маломуж М. Г. %"= plotdata$malomuzh*100/plotdata$participated,
                       "Порошенко П. О. %"= plotdata$poroshenko*100/plotdata$participated,
                       "Рабінович В. З. %"= plotdata$rabinovich*100/plotdata$participated,
                       "Саранов В. Г. %"= plotdata$saranov*100/plotdata$participated,
                       "Симоненко П. М. %"= plotdata$simonenko*100/plotdata$participated,
                       "Тимошенко Ю. В. %"= plotdata$timoshenko*100/plotdata$participated,
                       "Тігіпко С. Л. %"= plotdata$tigipko*100/plotdata$participated,
                       "Тягнибок О. Я. %"= plotdata$tyahnibok*100/plotdata$participated,
                       "Цушко В. П. %"= plotdata$cushko*100/plotdata$participated,
                       "% Шкіряк З. Н."= plotdata$shkiryak*100/plotdata$participated,
                       "Ярош Д. А. %"= plotdata$yarosh*100/plotdata$participated,
                       "Час прийняття протоколів"= as.numeric(plotdata$time))
  text = paste(pagetext["diff",input$lang],
               pagetext[pagetext$ukr == input$plot1,input$lang],
               pagetext["and",input$lang],
               pagetext[pagetext$ukr == input$plot2,input$lang])
    }

                    ggplot(data=plotdata, aes(x=lon, y=lat,colour = plot_data-plot_data2),
                                          size=2, alpha=0.8)+ geom_point()+
                         scale_colour_gradient(low="blue",high="red",name=text) +
                         theme(legend.position="bottom") + coord_fixed() + 
                  xlim(22.16049, 40.13864) + ylim(44.4963,52.33833) 
}, height = 600)

output$hist <- renderPlot({
  plot_data <<- switch(input$plot1,
                       "Явка" = plotdata$participated*100/plotdata$voters_num,
                       "Голосовуння на дому (% від проголосувавших)" = plotdata$home_bulletin*100/plotdata$participated,
                       "Недійсні бюлетені (% від проголосувавших)" = plotdata$invalid*100/plotdata$participated,
                       "Богомолець О. В. %" = plotdata$bogomolec*100/plotdata$participated,
                       "Бойко Ю. А. %" = plotdata$boyko*100/plotdata$participated,
                       "Гриненко А. В. %" = plotdata$grinenko*100/plotdata$participated,
                       "Гриценко А. С. %"= plotdata$gricenko*100/plotdata$participated,
                       "Добкін М. М. %"= plotdata$dobkin*100/plotdata$participated,
                       "Клименко О. І. % "= plotdata$klimenko*100/plotdata$participated,
                       "Коновалюк В. І. %"= plotdata$konovalenko*100/plotdata$participated,
                       "Кузьмін Р. Р. %"= plotdata$kuzmin*100/plotdata$participated,
                       "Куйбіда В. С. %"= plotdata$kuibida*100/plotdata$participated,
                       "Ляшко О. В. %"= plotdata$lyashko*100/plotdata$participated,
                       "Маломуж М. Г. %"= plotdata$malomuzh*100/plotdata$participated,
                       "Порошенко П. О. %"= plotdata$poroshenko*100/plotdata$participated,
                       "Рабінович В. З. %"= plotdata$rabinovich*100/plotdata$participated,
                       "Саранов В. Г. %"= plotdata$saranov*100/plotdata$participated,
                       "Симоненко П. М. %"= plotdata$simonenko*100/plotdata$participated,
                       "Тимошенко Ю. В. %"= plotdata$timoshenko*100/plotdata$participated,
                       "Тігіпко С. Л. %"= plotdata$tigipko*100/plotdata$participated,
                       "Тягнибок О. Я. %"= plotdata$tyahnibok*100/plotdata$participated,
                       "Цушко В. П. %"= plotdata$cushko*100/plotdata$participated,
                       "% Шкіряк З. Н."= plotdata$shkiryak*100/plotdata$participated,
                       "Ярош Д. А. %"= plotdata$yarosh*100/plotdata$participated,
                       "Час прийняття протоколів"= as.numeric(plotdata$time))

    plot_data2 <<- switch(input$plot2,
                          "Явка" = plotdata$participated*100/plotdata$voters_num,
                          "Голосовуння на дому (% від проголосувавших)" = plotdata$home_bulletin*100/plotdata$participated,
                          "Недійсні бюлетені (% від проголосувавших)" = plotdata$invalid*100/plotdata$participated,
                          "Богомолець О. В. %" = plotdata$bogomolec*100/plotdata$participated,
                          "Бойко Ю. А. %" = plotdata$boyko*100/plotdata$participated,
                          "Гриненко А. В. %" = plotdata$grinenko*100/plotdata$participated,
                          "Гриценко А. С. %"= plotdata$gricenko*100/plotdata$participated,
                          "Добкін М. М. %"= plotdata$dobkin*100/plotdata$participated,
                          "Клименко О. І. % "= plotdata$klimenko*100/plotdata$participated,
                          "Коновалюк В. І. %"= plotdata$konovaluk*100/plotdata$participated,
                          "Кузьмін Р. Р. %"= plotdata$kuzmin*100/plotdata$participated,
                          "Куйбіда В. С. %"= plotdata$kuibida*100/plotdata$participated,
                          "Ляшко О. В. %"= plotdata$lyashko*100/plotdata$participated,
                          "Маломуж М. Г. %"= plotdata$malomuzh*100/plotdata$participated,
                          "Порошенко П. О. %"= plotdata$poroshenko*100/plotdata$participated,
                          "Рабінович В. З. %"= plotdata$rabinovich*100/plotdata$participated,
                          "Саранов В. Г. %"= plotdata$saranov*100/plotdata$participated,
                          "Симоненко П. М. %"= plotdata$simonenko*100/plotdata$participated,
                          "Тимошенко Ю. В. %"= plotdata$timoshenko*100/plotdata$participated,
                          "Тігіпко С. Л. %"= plotdata$tigipko*100/plotdata$participated,
                          "Тягнибок О. Я. %"= plotdata$tyahnibok*100/plotdata$participated,
                          "Цушко В. П. %"= plotdata$cushko*100/plotdata$participated,
                          "% Шкіряк З. Н."= plotdata$shkiryak*100/plotdata$participated,
                          "Ярош Д. А. %"= plotdata$yarosh*100/plotdata$participated,
                          "Час прийняття протоколів"= as.numeric(plotdata$time))

  if (input$plot2 == "Не доступний") {
  ggplot(data=plotdata, aes(x=plot_data)) + 
    geom_histogram(aes(fill = ..count..)) + 
    xlab(pagetext[pagetext$ukr == input$plot1,input$lang]) +
    ylab(pagetext["dist_num",input$lang]) + theme(legend.position="none")
  } else {
    histdata = melt(cbind(plot_data,plot_data2))
    levels(histdata$X2) = c(pagetext[pagetext$ukr == input$plot1,input$lang],pagetext[pagetext$ukr == input$plot2,input$lang])
    ggplot(data=histdata, 
           aes(x=value)) + geom_histogram(aes(fill = ..count..),binwidth=1) +
      facet_grid(X2 ~ .) +
      xlab(pagetext[pagetext$ukr == input$plot1,input$lang]) +
      ylab(pagetext["dist_num",input$lang]) + theme(legend.position="none")
  }
  
}, height = 600)

output$scatter <- renderPlot({
  plot_data <<- switch(input$plot1,
                       "Явка" = plotdata$participated*100/plotdata$voters_num,
                       "Голосовуння на дому (% від проголосувавших)" = plotdata$home_bulletin*100/plotdata$participated,
                       "Недійсні бюлетені (% від проголосувавших)" = plotdata$invalid*100/plotdata$participated,
                       "Богомолець О. В. %" = plotdata$bogomolec*100/plotdata$participated,
                       "Бойко Ю. А. %" = plotdata$boyko*100/plotdata$participated,
                       "Гриненко А. В. %" = plotdata$grinenko*100/plotdata$participated,
                       "Гриценко А. С. %"= plotdata$gricenko*100/plotdata$participated,
                       "Добкін М. М. %"= plotdata$dobkin*100/plotdata$participated,
                       "Клименко О. І. % "= plotdata$klimenko*100/plotdata$participated,
                       "Коновалюк В. І. %"= plotdata$konovalenko*100/plotdata$participated,
                       "Кузьмін Р. Р. %"= plotdata$kuzmin*100/plotdata$participated,
                       "Куйбіда В. С. %"= plotdata$kuibida*100/plotdata$participated,
                       "Ляшко О. В. %"= plotdata$lyashko*100/plotdata$participated,
                       "Маломуж М. Г. %"= plotdata$malomuzh*100/plotdata$participated,
                       "Порошенко П. О. %"= plotdata$poroshenko*100/plotdata$participated,
                       "Рабінович В. З. %"= plotdata$rabinovich*100/plotdata$participated,
                       "Саранов В. Г. %"= plotdata$saranov*100/plotdata$participated,
                       "Симоненко П. М. %"= plotdata$simonenko*100/plotdata$participated,
                       "Тимошенко Ю. В. %"= plotdata$timoshenko*100/plotdata$participated,
                       "Тігіпко С. Л. %"= plotdata$tigipko*100/plotdata$participated,
                       "Тягнибок О. Я. %"= plotdata$tyahnibok*100/plotdata$participated,
                       "Цушко В. П. %"= plotdata$cushko*100/plotdata$participated,
                       "% Шкіряк З. Н."= plotdata$shkiryak*100/plotdata$participated,
                       "Ярош Д. А. %"= plotdata$yarosh*100/plotdata$participated,
                       "Час прийняття протоколів"= as.numeric(plotdata$time))
  
  plot_data2 <<- switch(input$plot2,
                        "Явка" = plotdata$participated*100/plotdata$voters_num,
                        "Голосовуння на дому (% від проголосувавших)" = plotdata$home_bulletin*100/plotdata$participated,
                        "Недійсні бюлетені (% від проголосувавших)" = plotdata$invalid*100/plotdata$participated,
                        "Богомолець О. В. %" = plotdata$bogomolec*100/plotdata$participated,
                        "Бойко Ю. А. %" = plotdata$boyko*100/plotdata$participated,
                        "Гриненко А. В. %" = plotdata$grinenko*100/plotdata$participated,
                        "Гриценко А. С. %"= plotdata$gricenko*100/plotdata$participated,
                        "Добкін М. М. %"= plotdata$dobkin*100/plotdata$participated,
                        "Клименко О. І. % "= plotdata$klimenko*100/plotdata$participated,
                        "Коновалюк В. І. %"= plotdata$konovaluk*100/plotdata$participated,
                        "Кузьмін Р. Р. %"= plotdata$kuzmin*100/plotdata$participated,
                        "Куйбіда В. С. %"= plotdata$kuibida*100/plotdata$participated,
                        "Ляшко О. В. %"= plotdata$lyashko*100/plotdata$participated,
                        "Маломуж М. Г. %"= plotdata$malomuzh*100/plotdata$participated,
                        "Порошенко П. О. %"= plotdata$poroshenko*100/plotdata$participated,
                        "Рабінович В. З. %"= plotdata$rabinovich*100/plotdata$participated,
                        "Саранов В. Г. %"= plotdata$saranov*100/plotdata$participated,
                        "Симоненко П. М. %"= plotdata$simonenko*100/plotdata$participated,
                        "Тимошенко Ю. В. %"= plotdata$timoshenko*100/plotdata$participated,
                        "Тігіпко С. Л. %"= plotdata$tigipko*100/plotdata$participated,
                        "Тягнибок О. Я. %"= plotdata$tyahnibok*100/plotdata$participated,
                        "Цушко В. П. %"= plotdata$cushko*100/plotdata$participated,
                        "% Шкіряк З. Н."= plotdata$shkiryak*100/plotdata$participated,
                        "Ярош Д. А. %"= plotdata$yarosh*100/plotdata$participated,
                        "Час прийняття протоколів"= as.numeric(plotdata$time))
  
  if (input$plot2 != "Не доступний") {
    scatter = as.data.frame(cbind(plot_data,plot_data2))    
    names(scatter) = c("X","Y")
    ggplot(data=scatter, aes(x=X,y=Y)) +
      geom_point(shape=19) + 
      geom_smooth(method=lm, se=FALSE) +
      xlab(pagetext[pagetext$ukr == input$plot1,input$lang]) +
      ylab(pagetext[pagetext$ukr == input$plot2,input$lang])
  } else {
    library(ggplot2)
    text = paste(pagetext["sec_var",input$lang])
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
                        "tyahn","cushk","shkir","yaros"),input$lang]
  names(a) = gsub("\\( "," ",(gsub("%","", names(a))))
  b<-apply(a[2:25],2,sum)*100/apply(a[2:24],2,sum)[2]
  winners <- names(sort(b[4:25],decreasing=T))[1:5]
  b[2] = apply(a[2:24],2,sum)[2]/apply(a[2:24],2,sum)[1]
  b=format(b,digits=1, scientific=F)
  finaltable = rbind(a[-2],c(b))
  finaltable = cbind(finaltable[c(1,3,4)],finaltable[winners])
  finaltable
}, options = list(bSortClasses = FALSE,bFilter = FALSE,iDisplayLength = 10))

})

  }
)


gsub("%","",pagetext[c("district_num","district_num","turn","invalid","bogom","boyko","grin","gric","dobk","klym",
                       "konov","kuzm","kuyb","lyash","malom", "poros","rabin","saran","simon","timos","tigip",
                       "tyahn","cushk","shkir","yaros"),"ukr"])
