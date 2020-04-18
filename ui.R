library(shiny)
library(shinyWidgets)
library(plotly)

fluidPage(
  titlePanel("新冠病毒传播与病床容量模拟"),
  hr(),
  p(div(HTML("免责声明：此模拟仅用于科研和教学，不作为决策工具， 我们对新冠肺炎（COVID-19）传播细节了解十分有限，这一简单模型有较大局限. 汉化自 https://github.com/alsnhll/SEIR_COVID19，版权归属Github @alsnhll  使用 <a href=https://creativecommons.org/licenses/by-sa/4.0/> Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0) License </a>"))),
  
  
  sidebarLayout(
    
    sidebarPanel(
      
      fluidRow(
        column(width=6,
               
               setSliderColor(c(rep("#b2df8a", 3)), sliderId=c(8,9,10)),
               h4(div(HTML("<em>临床参数...</em>"))),
               sliderInput("IncubPeriod", "潜伏期", 0, 20, 5, step=0.5, post = " 天"),
               sliderInput("DurMildInf", "症状轻微时长", 0, 20, 6, step=1, post = " 天"),
               sliderInput("FracSevere", "重症比例", 0, 100, 15, step=1, pre="%"),
               sliderInput("DurHosp", "重症住院时长", 0, 10, 6, step=1, post = " 天"),
               sliderInput("FracCritical", "危重症比例",0, 20, 5, step=1, pre="%"),
               sliderInput("TimeICUDeath", " 危重症住院时长 (ICU)", 0, 30, 8, step=1, post = " 天"),
               sliderInput("ProbDeath", " 危重症死亡率", 0, 100, 40, step=1, pre="%"),
               htmlOutput("CFR"),
               br(),
               #hr()
               
               ),
        column(width=6,
               
               h4(div(HTML("<em>设定传染速率...</em>"))),
               sliderInput("b1", div(HTML("轻症")), 0, 3, 0.5, step=0.01, post="每天"),
               sliderInput("b2", div(HTML("重症")),0, 3, 0.1, step=0.01, post="每天"),
               sliderInput("b3", div(HTML("危重症")),0, 3, 0.1, step=0.01, post="每天"),
               radioButtons("AllowSeason", "传播是否有季节性?",
                            choices = list("是" = "Yes","否" = "No"),inline=TRUE, selected="No"),
               conditionalPanel(
                 condition="input.AllowSeason == 'Yes'",
                 sliderInput("seas.amp", "季节性的强度", 0, 100, 0, step=10, pre="%"),
                 sliderInput("seas.phase", "传播高峰是第 ", -365, 365, 0, step=1, post = " 天"),
               ),
               radioButtons("AllowAsym", "是否有无症状感染者?",
                            choices = list("是" = "Yes","否" = "No"),inline=TRUE,selected="No"),
               conditionalPanel(
                 condition="input.AllowAsym == 'Yes'",
                 sliderInput("FracAsym", "无症状感染者比例", 0, 100, 30, step=1, pre="%"),
                 sliderInput("DurAsym", "无症状时长", 1, 20, 6, step=1, post = " 天"),
                 sliderInput("b0", div(HTML("无症状感染者传染速率")), 0, 3, 0.5, step=0.02, post="/天"),
               ),
                      radioButtons("AllowPresym", "是否有无症状前传染?",
                                   choices = list("是" = "Yes","否" = "No"),inline=TRUE, selected="No"),
               conditionalPanel(
                 condition="input.AllowPresym == 'Yes'",
                 sliderInput("PresymPeriod", "出现症状几天前可以开始传播", 0, 3, 2, step=0.5, post = " days"), #Make reactive
                 sliderInput("be", div(HTML("无症状前传播率")),0, 3, 0.5, step=0.02, post="/天"),
               ),
               hr(),

        )
      ),
      h4(div(HTML("<em>模拟参数...</em>"))),
      #sliderInput("LogN", div(HTML("Total population size (log10)")), 1, 9, 3, step=0.1),
      #htmlOutput("N"),
      column(width=5,
             numericInput("N", div(HTML("人口总数:")), value=1000, max=10^10, min=1000, step=1000)
      ),
      column(width=5,
             numericInput("InitInf","最初感染者:",value = 1, min = 1, step = 1)
      ),
      #br(),
      sliderInput("Tmax", div(HTML("天数")),0, 1000, 300, step=10, post=" 天"),
      actionButton("reset", "全部重置"),    
      width=5
    ),
    
    mainPanel(
      
      #p(div(HTML("Test")))
      navbarPage("输出:",
                 
                 tabPanel("传播",
                          fluidPage(
                            fluidRow(
                              
                              h3("预测感染者（以临床亚型分类）"),
                              p(HTML("新冠肺炎在无措施下的传播模拟：")),
                              
                              plotlyOutput("plot0"),
                              br(),
                              br(),
                              column(width=6,
                                     radioButtons("yscale", "Y 轴尺度",
                                                  choices = list("线性" = "linear","Log10" = "log"),inline=TRUE)
                                     ),
                              column(width=6,
                                     radioButtons("PlotCombine", "不按临床分型?",
                                                  choices = list("是" = "Yes","否" = "No"),inline=TRUE, selected="No")
                              ),
                              br(),
                              p(HTML("<b>指南:</b> 本图绘制了所有感染病例，治愈病例，疑似病例和死亡病例数随时间的变。 感染者首先经过潜伏期，之后进无症状，无传播性期，而后进入无症状可传播期，最后发病并按照临床分为轻症，重症和危重症， 具体设置参见模型描述（英文）。人口总数，出使条件和传播率可以调节，默认参数来源于参考文献 （见Sources）。可以通过点击重置来重置所有参数，可以将鼠标放置在图上看到数字，双击图例可以只看某条曲线，也可以放大。"))
                            )
                          )
                 ),
                 
                 tabPanel("减缓",
                          fluidPage(
                            fluidRow(
                              h3("采取措施减缓传播"),
                              p(HTML("模拟新冠病毒在采取隔离措施条件下的传播")),
                              plotlyOutput("plotInt"),
                              br(),
                              br(),
                              radioButtons("yscaleInt", "Y轴尺度:",
                                           choices = list("线性" = "linear","Log10" = "log"),inline=TRUE),
                              wellPanel(
                                h4(div(HTML("<em>减缓参数...</em>"))),
                                selectInput("VarShowInt",
                                            label = "选择参数:",
                                            choices = c("疑似 (S)"="S", "暴露 (E)"="E", "轻症 (I1)"="I1", "重症 (I2)"="I2", "危重症 (I3)"="I3", "治愈 (R)"="R", "Dead (D)"="D", "所有感染者 (E + all I)"="Inf","有症状者 (I1+I2+I3)"="Cases","入院患者 (I2+I3)"="Hosp"),
                                            selected = c("Cases")
                                ),
                                column(width=6,
                                         numericInput("Tint","减缓开始时间 (days):",value = 0, min = 0, step = 10)
                                         ),
                                  column(width=6,
                                         numericInput("Tend","减缓结束时间 (days):",value = 300, min = 0, step = 10)
                                         ),
                                p(HTML("<b>采取措施: 减缓传播, </b> 可以使社交距离或社区隔离 (无症状感染者) 或者更好条件的隔离和看护(对于更严重的感染者). 只有可以传播的临床亚型可以通过这一措施减缓传播")),
                                sliderInput("s1", "轻症传染力减缓比例 ", 0, 100, 30, pre="%",step=1, animate=TRUE),
                                sliderInput("s2", "重症传染力减缓比例", 0, 100, 0, pre="%",step=1, animate=TRUE),
                                sliderInput("s3", "危重症传染力减缓比例", 0, 100, 0, pre="%",step=1, animate=TRUE),
                                conditionalPanel(
                                  condition="input.AllowAsym == 'Yes' || input.AllowPresym == 'Yes' ",
                                  sliderInput("s0", "无症状/症状前感染传染力减缓比例 ", 0, 100, 0, pre="%",step=1, animate=TRUE),
                                ),
                                radioButtons("RoundOne", "四舍五入到整数?",
                                             choices = list("是" = "True","否" = "False"),inline=TRUE),
                              ),
                              p(HTML("<b>指南:</b> 本图绘制了所有感染病例，治愈病例，疑似病例和死亡病例数在有/无减缓措施的情况下随时间的变化。 感染者首先经过潜伏期，之后进无症状，无传播性期，而后进入无症状可传播期，最后发病并按照临床分为轻症，重症和危重症， 具体设置参见模型描述（英文）。"))
                            )
                          )
                 ),
                 
                 tabPanel("医疗系统容量",
                          fluidPage(
                            fluidRow(
                              h3("COVID-19 病例 vs 医疗系统容量"),
                              p(HTML("模拟COVID-19 需求与医疗系统容量的对比。重症患者需要住院而危重症患者常常需要重症监护资源（ICU）以及机械呼吸辅助系统")),
                              plotlyOutput("plotCap"),
                              br(),
                              br(),
                              radioButtons("yscaleCap", "Y 轴尺度:",
                                           choices = list("线性" = "linear","Log10" = "log"),inline=TRUE),
                              wellPanel(
                                h4(div(HTML("<em>卫生系统容量...</em>"))),
                                p(HTML(" 默认参数是美国的情况参见Sources")),
                                #Sliders for hospital capacity are reactive, since they take in default values from a file, so they are defined in the server file.  
                                fluidRow(
                                  p(HTML(" <b> 全部床位: </b>")),
                                  column(width = 6,
                                         uiOutput("HospBedper")
                                  ),
                                  column(width = 6,
                                         uiOutput("HospBedOcc")
                                  ),
                                  p(HTML(" <b> ICU beds: </b>")),
                                  column(width = 6,
                                         uiOutput("ICUBedper")
                                  ),
                                  column(width = 6,
                                         uiOutput("ICUBedOcc")
                                  ),
                                  column(width = 12,
                                         uiOutput("IncFluOcc")
                                  ),
                                  p(HTML(" <b> 机械通气呼吸机: </b>")),
                                  column(width = 4,
                                         uiOutput("ConvVentCap")
                                  ),
                                  column(width = 4,
                                         uiOutput("ContVentCap")
                                  ),
                                  column(width = 4,
                                         uiOutput("CrisisVentCap")
                                  )
                                ),
                                ),
                              wellPanel(
                                h4(div(HTML("<em>措施参数...</em>"))),
                                selectInput("VarShowCap",
                                            label = "选择显示的变量:",
                                            choices = c("危重症 (I3) vs ICU "="I3bed", "危重症 (I3) vs 机械通气呼吸机"="I3mv", "重症和危重症 (I2+I3) vs 床位"="Hosp", "所有有症状患者 (I1+I2+I3) vs 床位"="CasesCap"),
                                            selected = c("Hosp")
                                ),
                                column(width=6,
                                       numericInput("TintC","减缓措施开始时间:",value = 0, min = 0, step = 10)
                                ),
                                column(width=6,
                                       numericInput("TendC","减缓措施结束时间:",value = 300, min = 0, step = 10)
                                ),
                                p(HTML("<b>采取措施: 减缓传播, </b> 可以使社交距离或社区隔离 (无症状感染者) 或者更好条件的隔离和看护(对于更严重的感染者). 只有可以传播的临床亚型可以通过这一措施减缓传播")),
                                sliderInput("s1C", "轻症传染力减缓比例 ", 0, 100, 30, pre="%",step=1, animate=TRUE),
                                sliderInput("s2C", "重症传染力减缓比例 ", 0, 100, 0, pre="%",step=1, animate=TRUE),
                                sliderInput("s3C", "危重症传染力减缓比例 ", 0, 100, 0, pre="%",step=1, animate=TRUE),
                                conditionalPanel(
                                  condition="input.AllowAsym == 'Yes' || input.AllowPresym == 'Yes' ",
                                  sliderInput("s0C", "无症状/症状前感染传染力减缓比例  ", 0, 100, 0, pre="%",step=1, animate=TRUE),
                                ),
                                radioButtons("RoundOneCap", "四舍五入到整数?",
                                             choices = list("是" = "True","否" = "False"), inline=TRUE),
                              ),
                              p(HTML("<b>指南:</b> 本图绘制了所有感染病例，治愈病例，疑似病例和死亡病例数在有/无减缓措施的情况下随时间的变化。 感染者首先经过潜伏期，之后进无症状，无传播性期，而后进入无症状可传播期，最后发病并按照临床分为轻症，重症和危重症， 具体设置参见模型描述（英文）。"))
                            )
                          )
                 ),

                 tabPanel("Model", br(),
                          fluidRow(column(12,
                                          withMathJax(),
                                          h2("Model Description"),
                                          plotOutput("plot4", height=200),
                                          includeMarkdown("SEIR.Rmd"),
                                          #h3("Equations"),
                                          br(),
                                          h2("Output"),
                                          h3("Rate parameters of dynamic model"),
                                          p(HTML("These parameters can be changed using the sliders in the other tabs. The values in this table represent the current values chosen via the sliders. Note that the transmission rates chosen by the sliders are always scaled by \\(N\\), so that \\(\\beta*N\\) is constant as \\(N\\) changes.")),
                                          tableOutput("ParameterTable"),br(),
                                          h3("Ratios of cases during early growth phase"),
                                          p(HTML("These values are calculated based on the current model parameters")),
                                          tableOutput("RatioTable"),br(),
                          ))),
                 
                 tabPanel("Sources",
                          fluidPage(
                            br(),
                            uiOutput("parameterDesc")
                          )),
                 # tabPanel("Output",
                 #          fluidPage(
                 #            br(),
                 #            h3("Rate parameters of dynamic model"),
                 #            p(HTML("These parameters can be changed using the sliders in the other tabs. The values in this table represent the current values chosen via the sliders. Note that the transmission rates chosen by the sliders are always scaled by \\(N\\), so that \\(\\beta*N\\) is constant as \\(N\\) changes.")),
                 #            tableOutput("ParameterTable"),br(),br(),
                 #          )),
                 tabPanel("Tutorial",
                          fluidPage(
                            br(),
                            uiOutput("Tutorial")
                            #includeMarkdown("Tutorial.Rmd")
                          )),
                 
                 tabPanel("About",
                          fluidPage(
                            br(),
                            includeMarkdown("About.Rmd")
                          ))
                 
      ),
      width=7
    )
    
  )
  
)

