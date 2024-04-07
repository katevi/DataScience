library(shiny)
library(tidyverse)
library(ggcorrplot)
library(shinythemes)
library(DT)

# Load review data
diamonds_data <- read.csv("data/diamonds.csv")

# Define UI for application
ui <- fluidPage(theme = shinytheme("flatly"),
  
  # CSS
  tags$head(
    tags$style(HTML("
      body { background-color: #f2efe9; }
      .container-fluid { background-color: #fff; width: 1100px; padding: 60px; }
      .topimg { width: 120px; display: block; margin: 0px auto 40px auto; }
      .title { text-align: center; }
      .toprow { margin: 60px 0px; padding: 30px; background-color: #fae8bb; }
      .filters { margin: 0px auto; }
      .shiny-input-container { width:100% !important; }
      .table { padding: 30px; margin-top: 30px; }
      .leaflet-top { z-index:999 !important; }
      "))
  ),
  
  # Top image
  img(class = "topimg", src = "https://www.freepnglogos.com/uploads/diamond-png/diamond-icon-download-icons-3.png"),
  
  # Application title
  h1("Diamond price prediction", class = "title"),
  h4("Автор работы"),
  p("Винник Екатерина Петровна, 22.М04"),
  
  h4("Формулировка задачи"),
  "По предоставленному датасету алмазов необходимо определить, зависит ли цена алмаза от остальных его характеристик",
  "и построить модель, показывающую, на сколько изменится цена алмаза при изменении какой-либо его характеристики.",
  br(),
  br(),
  
  h4("Описание данных"),
  "Датасет из алмазов состоит из 53490 наблюдений и 10 признаков. Признаки имеют следующую семантику:",
  HTML("<ul><li>x, y, z --- характеристики размеров алмаза (в миллиметрах)</li>"),
  HTML("<li>price --- цена алмаза</li>"),
  HTML("<li>сarat --- карат алмаза</li>"),
  HTML("<li>clarity --- прозрачность алмаза, может быть: (I1 (худший), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (лучший))</li>"),
  HTML("<li>cut --- огранка алмаза, может быть: (Fair, Good, Very Good, Premium, Ideal)</li>"),
  HTML("<li>color --- цвет алмаза, от J (лучший цвет) до D (худший цвет)</li>"),
  HTML("<li>table --- ширина вершины алмаза относительно самого широкого места</li>"),
  HTML("<li>depth --- глубина алмаза, вычисляется по формуле z / mean(x, y) = 2 * z / (x + y) </li></ul>"),
  
  "Ниже представлен фрагмент датасета, который можно фильтровать по значениям двух признаков: Cut и Color.",
  "Значения признаков можно выбрать в фильтрах.",
  
  fluidRow(class = "toprow",
           fluidRow (class = "filters",
                     
                     column(6,
                            # Style Menu
                            selectInput("cut", "Cut",  c("Fair",
                                                         "Good",
                                                         "Very Good",
                                                         "Premium",
                                                         "Dial") %>% 
                                          append("All"))
                     ),
                     
                     column(6,
                            # Country menu
                            selectInput("color", "Color", c("D",
                                                            "E",
                                                            "F",
                                                            "G",
                                                            "H",
                                                            "I",
                                                            "J") %>% 
                                          append("All") %>% # Add "All" option
                                          sort()) # Sort options alphabetically
                            
                     )
           )
  ),
  
  
  fluidRow (class = "table", dataTableOutput("table")),
  
  h2("Разведочный анализ данных"),
  br(),
  h3("Корреляционный анализ"),
  p("Рассмотрим корреляции признаков алмаза с помощью матриц корреляций."),
  
  fluidRow (
    
    column(6, class = "bar",
           # Bar Chart
           plotOutput("corrPlot1")
    ),
    column(6, class = "bar",
           # Bar Chart
           plotOutput("corrPlot2")
    )
    
  ),
  
  br(),
  p("Из матрицы корреляций видно, что:"),
  HTML("<ul><li>признаки x, y, z высоко коррелируют от признаком карат</li>"),
  HTML("<li>признаки x, y, z, карат высоко коррелируют с признаком price</li></ul>"),
  
  p("Из знания предметной области известно, что карат зависит от объема алмаза --- то есть, от его параметров x, y, z."),
  p("Следовательно, можем опустить признаки x, y, z при дальнейшем исследовании."),
  br(),
  
  p("Корреляционный анализ показал, что переменные x, y, z и carat сильно коррелируют друг с другом. Проверим это с помощью теста."),
  p("В фильтре ниже можно выбрать два численных признака и проверить гипотезу об их корреляции друг с другом."),
  
  fluidRow (class = "filters",
            
            column(6,
                   # Style Menu
                   selectInput("feature1", "Feature 1",  c("price",
                                                        "carat",
                                                        "table",
                                                        "depth",
                                                        "x",
                                                        "y",
                                                        "z"))
            ),
            column(6,
                   # Style Menu
                   selectInput("feature2", "Feature 2",  c("price",
                                                        "carat",
                                                        "table",
                                                        "depth",
                                                        "x",
                                                        "y",
                                                        "z"))
            ),
  ),
  
  fluidRow (
    column(12,verbatimTextOutput("corrTest"))
  ),
  
  p("С помощью фильтра выше были проверены следующие гипотезы:"),
  HTML("<ul><li>Нулевая гипотеза: коэффициент корреляции между переменными carat и price равен нулю. 
       Статистика критерия равна 551.41 (53938 степеней свободы), p-value < 0.05. 
       Таким образом, гипотеза о равенстве нулю коэффициента корреляции не принимается на выбранном уровне значимости.</li>"),
  HTML("<li>Нулевая гипотеза: коэффициент корреляции между переменными table и price равен нулю. 
       Статистика критерия равна 29.768 (53938 степеней свободы), p-value < 0.05. 
       Таким образом, гипотеза о равенстве нулю коэффициента корреляции не принимается на выбранном уровне значимости.</li>"),
  HTML("<li>Нулевая гипотеза: коэффициент корреляции между переменными depth и price равен нулю. 
       Статистика критерия равна -2.473 (53938 степеней свободы), p-value < 0.05. 
       Таким образом, гипотеза о равенстве нулю коэффициента корреляции не принимается на выбранном уровне значимости.</li>"),
  HTML("<li>Нулевая гипотеза: коэффициент корреляции между переменными x и price равен нулю. 
       Статистика критерия равна 440.16 (53938 степеней свободы), p-value < 0.05. 
       Таким образом, гипотеза о равенстве нулю коэффициента корреляции не принимается на выбранном уровне значимости.</li>"),
  HTML("<li>Нулевая гипотеза: коэффициент корреляции между переменными y и price равен нулю. 
       Статистика критерия равна 401.14 (53938 степеней свободы), p-value < 0.05. 
       Таким образом, гипотеза о равенстве нулю коэффициента корреляции не принимается на выбранном уровне значимости.</li>"),
  HTML("<li>Нулевая гипотеза: коэффициент корреляции между переменными z и price равен нулю. 
       Статистика критерия равна 393.6 (53938 степеней свободы), p-value < 0.05. 
       Таким образом, гипотеза о равенстве нулю коэффициента корреляции не принимается на выбранном уровне значимости.</li></ul>"),
  
  h3("Анализ распределений признаков с помощью гистограмм"),
  p("Гистограммы значений каждого из признаков можно посмотреть, выбрав признак в фильтре."),
  fluidRow (class = "filters",
            
            column(6,
                   # Style Menu
                   selectInput("feature", "Feature",  c("price",
                                                        "carat",
                                                        "table",
                                                        "cut",
                                                        "clarity",
                                                        "depth",
                                                        "x",
                                                        "y",
                                                        "z"))
            ),
  ),
  
  fluidRow (
    column(12, class = "hist",
           # Hist Chart
           plotOutput("histPlot")
    ),
  ),
  
  p("При изучении гистограмм значений признаков видно, что:"),
  HTML("<ul><li>большинство алмазов (более 20000) имеют идеальную огранку, то есть, Cut = Ideal</li>"),
  HTML("<li>большинство алмазов имеют среднюю прозрачность --- то есть, Sl1, или VS2 (более 10000 в каждом случае)</li>"),
  HTML("<li>значения характеристики глубина алмаза (depth) нормально распределены</li>"),
  HTML("<li>большинство алмазов имеют значение характеристики x от 3 до 9 мм</li>"),
  HTML("<li>большинство алмазов имеют значение характеристики y от до 10 мм</li>"),
  HTML("<li>большинство алмазов имеют значение характеристики z от до 10 мм</li>"),
  HTML("<li>у распределения цен алмазов тяжелый хвост, большинство алмазов стоят до 5000$, но также есть алмазы со стоимостью больше 10000$, 15000$</li></ul>"),
  
  h3("Попарный статистический вывод (t-критерий) с интерпретацией"),
  
  p("В датасете имеются три категориальных признака -- Cut, Color и Clarity (огранка, цвет и прозрачность), 
    которые еще не были исследованы."),
  p("Исследуем, как значения этих признаков соотносятся с ценой алмаза --- проверим, совпадают ли средние значения цен 
    при разных значениях признаков Cut, Color, Clarity."),
  br(),
  
  p("Ниже представлен фильтр, в котором можно выбрать категориальный признак cut, color или clarity, для которого будет проведен попарный статистический вывод с интерпретацией."),
  
  fluidRow (class = "filters",
            column(6,
                   # Style Menu
                   selectInput("categoricalFeature", "categoricalFeature",  c("cut",
                                                        "clarity",
                                                        "color"))
            ),
  ),
  fluidRow (
    column(12,verbatimTextOutput("pairwiseCriteria"))
  ),
  fluidRow (
    column(12,plotOutput("boxPlot"))
  ),
  
  h4("Проверка гипотезы о зависимости цены алмаза от типа огранки"),
  p("Нулевая гипотеза: среднее значение цены (price) одинаково для любого цвета (cut) алмаза."),
  p("Из результатов t-критерия видно, что разница между средним значением цены для алмазов 
    с цветом `Good` и `Very Good`, а также для алмазов с типом огранки `Premium` и `Fair` не является статистически значимой."),
  p("Это же подтверждается соответствующим ящиком с усами -- 
    из него видно, что средние значения цены для этих пар типов огранки, близки."),
  
  h4("Проверка гипотезы о зависимости цены алмаза от вида цвета алмаза"),
  p("Нулевая гипотеза: среднее значение цены (price) одинаково для любого типа огранки (color) алмаза."),
  p("Из результатов t-критерия видно, что разница между средним значением цены для алмазов 
    цвета D и E, не является статистически значимой."),
  p("Это же подтверждается соответствующим ящиком с усами -- 
    из него видно, что средние значения цены для алмазов цветов D, E, близки."),
  
  h4("Проверка гипотезы о зависимости цены алмаза от степени прозрачности алмаза"),
  p("Нулевая гипотеза: среднее значение цены (price) одинаково для любого типа прозрачности (clarity) алмаза."),
  p("Из результатов t-критерия видно, что разница между средним значением цены для алмазов 
    со степенями прозрачности I1 и SI1, I1 и VS1, I1 и VS2, VS1 и VS2, VS2 и SI1, не является статистически значимой."),
  p("Это же подтверждается соответствующим ящиком с усами -- 
    из него видно, что средние значения цены для алмазов цветов D, E, близки."),
  
  
  h2("Построение модели прогнозирования цены алмаза на основании остальных его характеристик"),
  
  p("Так как в процессе анализа было установлено, что переменные x, y, z коррелируют с переменной carat с коэффиентом 1, можем отбросить их при построении модели."),
  p("Таким образом, будем использовать признаки carat, table, depth, cut, clarity, color при обучении модели."),
  
  h3("Построение модели"),
  p("Попробуем спрогнозировать цену алмаза с помощью модели линейной регрессии."),
  fluidRow (
    column(12,verbatimTextOutput("summary"))
  ),
  p("Оценим качество подгонки модели. Посчитаем коэффициент детерминации:"),
  fluidRow (
    column(12,verbatimTextOutput("estimation"))
  ),
  p("Так как коэффициент детерминации близок к 1, подогнанная регрессия объясняет большую часть вариабельности ключевой переменной цены алмаза."),
  p("Кроме того, p–значение для F–критерия равно 0, таким образом гипотеза о равенстве нулю всех коэффициентов при признаках не принимается на уровне значимости 0.05, регрессия значима."),
  h3("Анализ остатков регрессии"),
  h4("Линейность взаимосвязи"),
  fluidRow (
    column(12, plotOutput("residualFittedPlot"))
  ),
  p("С помощью графика о зависимости между подогнанными значениями и остатками регрессии, представленного выше, 
    можно оценить истинность предположения о линейности взаимосвязи. Видно, что на графике в небольшом количестве присутствуют выбросы. "),
  p("Для рассматриваемой модели выбросами является примерно следующее количество наблюдений:"),
  fluidRow(
    column(12, verbatimTextOutput("residualsList"))
  ),
  p("Для более полного анализа выбросов, используем расстояние Кука (Cook’s distance), которое отражает, 
    насколько сильно изменится предсказание при удалении каждого наблюдения. Обычно выбросами считаются те наблюдения, 
    для которых расстояние Кука больше, чем 4/n, или 4/53490 в нашем случае."),
  fluidRow (
    column(12, plotOutput("residualLeveragePlot1"))
  ),
  fluidRow (
    column(12, plotOutput("residualLeveragePlot2"))
  ),
  h4("Нормальность остатков регрессии"),
  p("Применим критерий Лиллиефорса для проверки на нормальность из пакета nortest."),
  fluidRow (
    column(12,verbatimTextOutput("normalityTest"))
  ),
  fluidRow (
    column(12, plotOutput("qqPlot"))
  ),
  h4("Однородность дисперсии остатков"),
  p("Оценим гомоскедастичность при помощи статистических критериев и графически."),
  fluidRow (
    column(12,verbatimTextOutput("ncvTest"))
  ),
  fluidRow (
    column(12, plotOutput("scaleLocationPlot"))
  ),
  p("В нашем случае p-значение для критерия меньше 0.05, и потому гипотеза об однородности дисперсии не принимается, хотя графически не наблюдается значительных отклонений. 
  Так как размер выборки большой, требование гомоскедастичности не является жестким."),
  p("В целом, простая линейная регрессия хорошо описывает интересующий признак, цену алмаза: значительных отклонений от предпосылок 
    модели не наблюдается, регрессия в целом значима, описывает 89% вариабельности признака."),
  h4("Качество модели без выбросов"),
  fluidRow(
    column(12, verbatimTextOutput("woResiduals"))
  ),
  p("Если отсеять из выборки выбросы с помощью расстояния Кука, построенная модель будет описывать 92% вариабельности признака."),
  
  h2("Калькулятор цены алмаза в зависимости от его характеристик"),
  
  fluidRow(
    column(4, selectInput('calcCut',label = h4('Cut'),
                          choices = list("Fair" = "Fair", "Good" = "Good",
                                         "Very Good" = "Very Good", "Premium" = "Premium",
                                         "Ideal" = "Ideal"))),
    column(4, selectInput("calcColor", label = h4("Color"), 
                          choices = list("D" = "D", "E" = "E",
                                         "F" = "F", "G" ="G",
                                         "H" = "H", "I" = "I",
                                         "J" = "J"))),
    column(4, selectInput("calcClarity", label = h4("Clarity"), 
                          choices = list("I1" = "I1", "SI2" = "SI2",
                                         "SI1" = "SI1", "VS2" = "VS2", "VS1" = "VS1",
                                         "VVS2" = "VVS2", "VVS1" = "VVS1", "IF" = "IF" )))
  ),
  fluidRow(
    column(4, numericInput('calcCarat',label=h4("Carats"),step=0.01,value=0.2)),
    column(4, numericInput('calcTable',label=h4("Table"),step=0.01,value=43)),
    column(4, numericInput('calcDepth',label=h4("Depth"),step=0.01,value=2)),
  ),
  h4("Прогнозируемая цена алмаза:"),
  fluidRow(
    column(12, textOutput("calculate"))
  ),
  
  h3("Корректность модели"),
  h4("Проверка гипотез"),
  p("С помощью представленного выше калькулятора можно проверить, что следующие зависимости цены алмаза, приведенные в описании датасета, сохранились:"),
  HTML("<ul><li> Цена алмаза зависит от типа его цвета --- наименьшая цена достигается при цвете J, наивысшая при цвете D (лучшем цвете алмаза согласно описанию датасета). </li>"),
  HTML("<li> Цена алмаза зависит от качества его огранки -- наименьшая цена достигается при огранке Fair, наивысшая при огранке Ideal (лучшей огранке алмаза согласно описанию датасета). </li>"),
  HTML("<li> Цена алмаза растет с увеличением карата.</li>"),
  HTML("<li> Прозрачность алмаза сильно влияет на его цену, наименьшая цена достигается при степени прозрачности I1 (худшей), наивысшая при степени прозрачности IF (лучшей).</li>"),
  HTML("<li> Переменные table, depth, не сильно влияют на цену алмаза (что совпадает с результатам корреляционного анализа).</li></ul>"),
)

# Define server logic
server <- function(input, output) {
  output$histPlot <- renderPlot({
    diamonds_data %>% select(input$feature)  %>% tidyr::pivot_longer(1:1, values_to="value", names_to="names") %>% 
      ggplot() + geom_bar(aes(x = value), bins = 10, colour = "black", fill = "white") + facet_wrap(~ names, scales = "free", ncol=2)
  })
  
  # Create corr plot 1 
  output$corrPlot1 <- renderPlot( {
    diamonds_data_numeric <- diamonds_data %>% keep(is.numeric)
    corr_scaled <- round(cor(diamonds_data_numeric),1)
    ggcorrplot(corr_scaled, hc.order = TRUE, 
               type = "lower", 
               lab = TRUE, 
               lab_size = 3, 
               method="circle", 
               colors = c("tomato2", "white", "springgreen3"), 
               title="Correlogram of Diamonds Data", 
               ggtheme=theme_bw)
  })
  
  # Create corr plot 2 
  output$corrPlot2 <- renderPlot( {
    diamonds_data_numeric <- diamonds_data %>%  keep(is.numeric)
    corrplot::corrplot(cor(diamonds_data_numeric, method="pearson"))
  })
  
  # Create data table
  output$table <- renderDataTable({
    
    # Filter data based on selected Style
    if (input$cut != "All") {
      diamonds_data <- filter(diamonds_data, cut == input$cut)
    }
    
    # Filter data based on selected Country
    if (input$color != "All") {
      diamonds_data <- filter(diamonds_data, color == input$color)
    }
    
    diamonds_data
    
  })
  
  output$corrTest <- renderPrint({
    cor.test(diamonds_data[,input$feature1], diamonds_data[,input$feature2])
  })
  output$pairwiseCriteria <- renderPrint({
    pairwise.t.test(diamonds_data$price, diamonds_data[, input$categoricalFeature])
  })
  output$boxPlot <- renderPlot({
    p <- ggplot(diamonds_data, aes(y=`price`, fill = as.factor(diamonds_data[,input$categoricalFeature]))) + geom_boxplot()
    p
  })
  
  rmodel_1 <- lm(log(price)~carat + cut + color + clarity + depth + table, data=diamonds_data)
  output$summary <- renderPrint( {
    summary(rmodel_1)
  })
  
  output$estimation <- renderPrint({
    round(summary(rmodel_1)$adj.r.squared,2)
  })
  
  # нормальность остатков регрессии
  output$normalityTest <- renderPrint( {
    nortest::lillie.test(rstandard(rmodel_1))
  })
  
  # линейность взаимосвязи
  output$residualFittedPlot <- renderPlot({
    p1 <- ggplot(rmodel_1, aes(.fitted, .resid))+ geom_point() + stat_smooth(method="gam") + geom_hline(yintercept=0, col="red", linetype="dashed") + labs(x="Подогнанные значения (fitted)", y="Остатки регрессии (residuals)", title = "Residual vs Fitted Plot")+theme_bw()
    p1 
  })
  
  output$residualsList <- renderPrint({
    length(which(as.numeric(cooks.distance(rmodel_1)) > 4 / 53490))
  })
  
  output$residualLeveragePlot1 <- renderPlot({
    plot(rmodel_1, 5)
  })
  
  output$residualLeveragePlot2 <- renderPlot({
    p5 <- ggplot(rmodel_1, aes(.hat, .stdresid)) + 
      geom_point(aes(size=.cooksd), na.rm=TRUE) + 
      stat_smooth(method="gam", na.rm=TRUE) + 
      labs(x="Leverage", y="Standardized Residuals", title = "Residual vs Leverage Plot") + 
      scale_size_continuous("Cook's Distance", range=c(1,5)) + 
      theme_bw()+theme(legend.position="bottom")
    
    p5
  })
  
  output$qqPlot <- renderPlot( {
    p2 <- ggplot(rmodel_1, aes(qqnorm(.stdresid)[[1]], .stdresid)) + 
      geom_point(na.rm = TRUE) + 
      geom_abline() + 
      labs(x = "Нормальное распределение", y = "Стандартизованные остатки", title = "График Q-Q с нормальным распределением") + 
      theme_bw()
    
    p2
  })
  
  # оценка дисперсии остатков
  output$nvcTest <- renderPrint({
    car::ncvTest(rmodel_1)
  })
  
  output$scaleLocationPlot <- renderPlot({
    ggplot(rmodel_1, aes(.fitted, sqrt(abs(.stdresid)))) + 
      geom_point(na.rm=TRUE) + 
      stat_smooth(method="gam", na.rm = TRUE) + 
      labs(x="Fitted Value", y = expression(sqrt("|Standardized residuals|")), title = "Scale-Location plot") + 
      theme_bw()
  })
  
  inx <- ifelse(cooks.distance(rmodel_1) < 4/53490, "keep","delete")
  rmodel_1_mod <- lm(log(price)~carat + cut + color + clarity + depth + table, data=diamonds_data[inx=='keep',])
  
  # модель без выбросов
  output$woResiduals <- renderPrint({
    #создадим индекс, который определяет соответствие наблюдения эпирическому правилу о выбросах
    summary(rmodel_1_mod)
  })
  
  # калькулятор
  output$calculate <- renderText({
    pred <- predict(rmodel_1_mod, newdata = data.frame(carat = input$calcCarat,
                                              cut = input$calcCut,
                                              color = input$calcColor,
                                              clarity = input$calcClarity,
                                              table = input$calcTable,
                                              depth = input$calcDepth))
    res <- paste(round(exp(pred), digits = 2), "$")
    res
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
