#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(rvest)
library(ggplot2)
library(lubridate)
library(scales)
library(DT)
library(geobr)
library(sf)
library(dplyr)
library(car)
library(chron)
library(TTR)
library(curl)


if (format(as.POSIXct(Sys.time(),format="%H:%M:%S"),"%H:%M") > "21:00"){
        url <- "https://brasil.io/dataset/covid19/caso_full/?format=csv"
        covid <- read.csv2(file=url, encoding = "UTF-8")
        write.csv(covid, file = covid19.csv, encoding = "UTF-8")
        covid$date <- as.Date(covid$date)
}else{
        covid <- read.csv("covid19.csv", encoding = "UTF-8")
        covid$date <- as.Date(covid$date)
} 

#Novos casos
teste <- covid %>%
        filter(place_type == "state") %>%
        group_by(date) %>%
        summarise(n = sum(last_available_confirmed),
                  rate = mean(last_available_death_rate)) %>%
        arrange(desc(date)) %>%
        mutate(new = -1*(c(0, diff(n))))

novos_24h <- teste[1,2] - teste[2,2]
data <- teste[1,1]
#Óbitos
obts <- covid %>%
        filter(place_type == "state") %>%
        group_by(date) %>%
        summarise(nobit = sum(last_available_deaths)) %>%
        arrange(desc(date)) 

tot_obts <- obts[1,2]
obts_24h <- obts[1,2] - obts[2,2]
options(scipen = 999)
fig1 <- ggplot(teste, aes(x = date, y = n))+
        geom_line(col = "blue", size = 2) +
        xlab("Data da Notificação")+
        ylab("Número de Casos Acumulados")+
        scale_x_date(breaks = pretty_breaks(7))+
        theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                         size = 14),
              axis.text.y = element_text(face="bold", color="darkgray", 
                                         size = 14))+
        theme(axis.title = element_text(face = "bold", color = "black", size = 18))

fig2 <- ggplot(obts, aes(x = date, y = nobit))+
        geom_line(col = "red", size = 2) +
        xlab("Data da Notificação")+
        ylab("Número de Óbitos Acumulados")+
        scale_x_date(breaks = pretty_breaks(7))+
        theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                         size = 14),
              axis.text.y = element_text(face="bold", color="darkgray", 
                                         size = 14))+
        theme(axis.title = element_text(face = "bold", color = "black", size = 18))

fig_new_casos <- ggplot(obts, aes(x = date, y = diff_n))+
        geom_col(fill = "red", size = 1) +
        xlab("Data da Notificação")+
        ylab("Novos Óbitos por dia")+
        scale_x_date(breaks = pretty_breaks(7))+
        theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                         size = 14),
              axis.text.y = element_text(face="bold", color="darkgray", 
                                         size = 14))+
        theme(axis.title = element_text(face = "bold", color = "black", size = 18))
#Mapas
cas_states <- covid %>%
        filter(date == Sys.Date() - 1) %>%
        group_by(state) %>%
        summarise(n = sum(last_available_confirmed))


states <- read_state(year = 2014)
cas_states$state
# joind the databases
fin_cas_states <-left_join(states, cas_states, by = c("abbrev_state" = "state"))


no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

fig3 <- ggplot() +
        geom_sf(data = fin_cas_states, aes(fill=n), color= NA, size=.15) +
        labs(subtitle="Número de casos Acumulados", size=8) +
        scale_fill_distiller(palette = "Greens", name="", limits = c(0, 500000)) +
        theme_minimal()+
        no_axis

obt_states <- covid %>%
        filter(date == Sys.Date() - 1) %>%
        group_by(state) %>%
        summarise(n = sum(last_available_deaths))
fin_obt_states <-left_join(states, obt_states, by = c("abbrev_state" = "state"))

fig4 <- ggplot() +
        geom_sf(data = fin_obt_states, aes(fill=n), color= NA, size=.15) +
        labs(subtitle="Número de Óbitos Acumulados", size=8) +
        scale_fill_distiller(palette = "Reds", name="", limits = c(0, 25000)) +
        theme_minimal()+
        no_axis

abrev_state <- states$abbrev_state  



# Define UI for application that draws a histogram
## ui.R ##
sidebar <- dashboardSidebar(
        sidebarMenu(
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Mapas", tabName = "mapas", icon = icon("globe-americas")),
                menuItem("Filtrar por Estado", tabName = "estado", icon = icon("map-marker-alt")),
                menuItem("Filtrar por Cidade", tabName = "cidade", icon = icon("map-pin")),
                #menuItem("Análise de Sentimento", tabName = "twitter", icon = icon("twitter")),
                menuItem("Dados", tabName = "dados", icon = icon("code")),
                menuItem("Termos de Uso", tabName = "disclaimer", icon = icon("exclamation-triangle")),
                menuItem("buymeacoffee", href = "https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=GGPVNE6PE8PFG&source=url", icon = icon("coffee")),
                menuItem("Colabore", href = "https://github.com/marcelomedre/covid_br", icon = icon("github")),
                menuItem("Autor", href = "https://www.linkedin.com/in/marcelo-medre-nobrega-a35a4a48/", icon = icon("linkedin"))
        )
)

body <- dashboardBody(
        tabItems(
                tabItem(tabName = "dashboard",
                        fluidRow(
                        imageOutput("Covid19"),
                        h5(verbatimTextOutput("foo")),
                        valueBoxOutput("box1", width = 6),
                        valueBoxOutput("box2", width = 6),
                        valueBoxOutput("box3", width = 6),
                        valueBoxOutput("box4", width = 6),
                        valueBoxOutput("box20", width = 6),
                        valueBoxOutput("box21", width = 6)
                        ,
                        tabBox(title = "Evolução do número de casos",
                            id = "tabset1",
                            width = 12,
                            tabPanel("Linear",
                            plotOutput("fig1")
                            ),
                            tabPanel("Log", "Log",
                                     plotOutput("fig1log")
                            )),
                        br(),
                        tabBox(title = "Evolução do número de Óbitos",
                               id = "tabset2",
                               width = 12,
                               tabPanel("Linear",
                                        plotOutput("fig2")
                               ),
                               tabPanel("Log", "Log",
                                        plotOutput("fig2log")
                               )),
                        box(title = "Novos Casos por dia",
                            status = "warning",
                            solidHeader = TRUE,
                            width = 12,
                            plotOutput("fig_new_cases"),
                            h4("Média móvel de 4 dias")
                        ),
                        box(title = "Novos Óbitos por dia",
                            status = "danger",
                            solidHeader = TRUE,
                            width = 12,
                            plotOutput("fig20")
                        ),
                        box(title = "Evolução dos Casos utilizando Modelo Logístico",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 12,
                            plotOutput("fig_mod")
                        )
                        )
                ),
                tabItem(tabName = "mapas",
                        fluidRow(
                                box(title = "Número de Casos Acumulados",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = 12,
                                    plotOutput("fig3")
                                ),
                                box(title = "Número de Óbitos Acumulados",
                                    status = "danger",
                                    solidHeader = TRUE,
                                    width = 12,
                                    plotOutput("fig4")
                                )
                                )
                ),
                # tab ESTADOS ------------------------------------------------
                tabItem(tabName = "estado",
                        fluidRow(
                                box(title = "Escolha o Estado",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = 12,
                                    selectInput("estado", label = "Escolha um Estado:", choices = c("RO", "AC", "AM",
                                                                                                    "RR", "PA", "AP",
                                                                                                    "TO", "MA", "PI",
                                                                                                    "CE", "RN", "PB",
                                                                                                    "PE", "AL", "SE",
                                                                                                    "BA", "MG", "ES",
                                                                                                    "RJ", "SP", "PR",
                                                                                                    "SC", "RS", "MS", 
                                                                                                    "MT", "GO", "DF"))
                                ),
                                valueBoxOutput("box5", width = 6),
                                valueBoxOutput("box6", width = 6),
                                valueBoxOutput("box7", width = 6),
                                valueBoxOutput("box8", width = 6),
                                valueBoxOutput("box13", width = 6),
                                valueBoxOutput("box14", width = 6)
                                ,
                                tabBox(title = "Evolução do número de casos",
                                       id = "tabset3",
                                       width = 12,
                                       tabPanel("Linear",
                                                plotOutput("fig5")
                                       ),
                                       tabPanel("Log", "Log",
                                                plotOutput("fig5log")
                                       )),
                                tabBox(title = "Evolução do número de óbitos",
                                       id = "tabset4",
                                       width = 12,
                                       tabPanel("Linear",
                                                plotOutput("fig6")
                                       ),
                                       tabPanel("Log", "Log",
                                                plotOutput("fig6log")
                                       )),
                                box(title = "Novos Casos por dia",
                                    status = "warning",
                                    solidHeader = TRUE,
                                    width = 12,
                                    plotOutput("fig13")
                                ),
                                box(title = "Novos Óbitos por dia",
                                    status = "danger",
                                    solidHeader = TRUE,
                                    width = 12,
                                    plotOutput("fig21")
                                ),
                                box(title = "Número de Casos por 100 Mil habitantes",
                                    status = "warning",
                                    solidHeader = TRUE,
                                    width = 12,
                                    plotOutput("fig14")
                                ),
                                box(title = "Evolução dos Casos utilizando Modelo Logístico",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = 12,
                                    plotOutput("fig_mod_est")
                                )
                        )
                ), # tab ESTADOS --end----------------------------------------------
                # tab Cidades ------------------------------------------------
                tabItem(tabName = "cidade",
                        fluidRow(
                                box(title = "Escolha a Cidade",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = 12,
                                    textInput("cidade", label = "Digite o Nome da Cidade:", value = "Ibiporã")
                                ),
                                valueBoxOutput("box9", width = 6),
                                valueBoxOutput("box10", width = 6),
                                valueBoxOutput("box11", width = 6),
                                valueBoxOutput("box12", width = 6),
                                valueBoxOutput("box15", width = 6),
                                valueBoxOutput("box16", width = 6)
                                ,
                                tabBox(title = "Evolução do número de casos",
                                       id = "tabset3",
                                       width = 12,
                                       tabPanel("Linear",
                                                plotOutput("fig7")
                                       ),
                                       tabPanel("Log", "Log",
                                                plotOutput("fig7log")
                                       )),
                                tabBox(title = "Evolução do número de óbitos",
                                       id = "tabset4",
                                       width = 12,
                                       tabPanel("Linear",
                                                plotOutput("fig8")
                                       ),
                                       tabPanel("Log", "Log",
                                                plotOutput("fig8log")
                                       )),
                                box(title = "Novos Casos por dia",
                                    status = "warning",
                                    solidHeader = TRUE,
                                    width = 12,
                                    plotOutput("fig9")
                                ),
                                box(title = "Novos Óbitos por dia",
                                    status = "danger",
                                    solidHeader = TRUE,
                                    width = 12,
                                    plotOutput("fig22")
                                    #),
                               # ),
                        #        box(title = "Evolução dos Casos utilizando Modelo Logístico",
                        #            status = "primary",
                        #            solidHeader = TRUE,
                        #            width = 12,
                        #            plotOutput("fig_mod_cid")
                                )
                        )
                ),
                # Cidades end--------------------------------------------
                tabItem(tabName = "twitter",
                        fluidRow(
                                h2("Análise de Sentimentos de Mensagens do twitter"),
                                br(),
                                h3("Número de tweets 1k"),
                                br(),
                                box(title = "Palavras com Sentimento Positivo",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = 12,
                                    plotOutput("fig30")
                                )
                        )),
                tabItem(tabName = "dados",
                        fluidRow(
                                h2("Sobre os dados utilizados no APP"),
                                br(),
                                h4("Os dados são baixados diretamente do API disponibilizado"),
                                h4("no site https://brasil.io/home/"),
                                br(),
                                dataTableOutput("data")
                )),
                tabItem(tabName = "disclaimer",
                        h2("Termos de Uso"),
                        br(),
                        h4("As informações contidas neste website são de caráter meramente informativo."),
                        h4("Este App está em constante atualização")
                )
        )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
        dashboardHeader(title = "Dashboard da COVID-19 no Brasil",
                        titleWidth = 1200),
        sidebar,
        body
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        
        output$Covid19 <- renderImage({
                # A temp file to save the output.
                # This file will be removed later by renderImage
                # Generate the PNG
                png("1_o71LSwXo9Xoz5af5oZ23-A.png", width = 1200, height = 600)
        })
        #creating the valueBoxOutput content
        output$box1 <- renderValueBox({
                valueBox(
                        formatC(teste[1,2], format="d", big.mark='.')
                        ,paste('Casos Confirmados:')
                        ,icon = icon("stats",lib='glyphicon')
                        ,color = "blue")  
        })
        
        output$box2 <- renderValueBox({
                valueBox(
                        formatC(novos_24h, format="d", big.mark='.')
                        ,paste('Novas notificações em 24h:')
                        ,icon = icon("stats",lib='glyphicon')
                        ,color = "aqua")  
        })
        
        output$box3 <- renderValueBox({
                valueBox(
                        formatC(tot_obts, format="d", big.mark='.')
                        ,paste('Total de Óbitos:')
                        ,icon = icon("stats",lib='glyphicon')
                        ,color = "red")  
        })

        output$box4 <- renderValueBox({
                valueBox(
                        formatC(obts_24h, format="d", big.mark='.')
                        ,paste('Novos Óbitos em 24h:')
                        ,icon = icon("stats",lib='glyphicon')
                        ,color = "yellow")  
        })
        output$box20 <- renderValueBox({
                valueBox(
                        formatC(round((teste$rate[1]*100),2), format="g", big.mark=',')
                        ,paste('Taxa de Letalidade (%):')
                        ,icon = icon("skull",lib="font-awesome")
                        ,color = "red")  
        })
        output$box21 <- renderValueBox({
                valueBox(
                        formatC((length(covid$date)), format="d", big.mark='.')
                        ,paste('Número de dados avaliados:')
                        ,icon = icon("file-csv",lib="font-awesome")
                        ,color = "green")  
        })
        
        #Box estados----------------------------------------------------------
        output$box5 <- renderValueBox({
                st_choice <- covid %>%
                        filter(place_type == "state") %>%
                        filter(state == input$estado) %>%
                        group_by(date) %>%
                        summarise(n = sum(last_available_confirmed)) %>%
                        arrange(desc(date))
                valueBox(
                        formatC(st_choice[1,2], format="d", big.mark='.')
                        ,paste('Casos Confirmados:')
                        ,icon = icon("stats",lib='glyphicon')
                        ,color = "blue")  
        })
        
        output$box6 <- renderValueBox({
                st_choice <- covid %>%
                        filter(place_type == "state") %>%
                        filter(state == input$estado) %>%
                        group_by(date) %>%
                        summarise(n = sum(last_available_confirmed)) %>%
                        arrange(desc(date))
                valueBox(
                        formatC((st_choice[1,2] - st_choice[2,2]), format="d", big.mark='.')
                        ,paste('Novas notificações em 24h:')
                        ,icon = icon("stats",lib='glyphicon')
                        ,color = "aqua")  
        })
        
        output$box7 <- renderValueBox({
                st_ob_choice <- covid %>%
                        filter(place_type == "state") %>%
                        filter(state == input$estado) %>%
                        group_by(date) %>%
                        summarise(n = sum(last_available_deaths)) %>%
                        arrange(desc(date))
                valueBox(
                        formatC(st_ob_choice[1,2], format="d", big.mark='.')
                        ,paste('Total de Óbitos:')
                        ,icon = icon("stats",lib='glyphicon')
                        ,color = "red")  
        })
        
        output$box8 <- renderValueBox({
                st_ob_choice <- covid %>%
                        filter(place_type == "state") %>%
                        filter(state == input$estado) %>%
                        group_by(date) %>%
                        summarise(n = sum(last_available_deaths)) %>%
                        arrange(desc(date))
                valueBox(
                        formatC((st_ob_choice[1,2] - st_ob_choice[2,2]), format="d", big.mark='.')
                        ,paste('Novos Óbitos em 24h:')
                        ,icon = icon("stats",lib='glyphicon')
                        ,color = "yellow")  
        })
        output$box13 <- renderValueBox({
                death_rate_st <- covid %>%
                        filter(place_type == "state") %>%
                        filter(state == input$estado) %>%
                        arrange(desc(date))
                
                valueBox(
                        formatC(round(death_rate_st$last_available_death_rate[1]*100,2), format="g", big.mark=',')
                        ,paste('Taxa de Letalidade (%):')
                        ,icon = icon("skull",lib="font-awesome")
                        ,color = "red")  
        })
        output$box14 <- renderValueBox({
                perc_pop <- covid %>%
                        filter(place_type == "state") %>%
                        filter(state == input$estado) %>%
                        arrange(desc(date))
                
                valueBox(
                        formatC(round(perc_pop$last_available_confirmed_per_100k_inhabitants[1],2), format="d", big.mark='.')
                        ,paste('Número de casos por 100 mil habitantes:')
                        ,icon = icon("stats",lib= "glyphicon")
                        ,color = "yellow")  
        })
        #Box estados----------------------------------------------------------
        #Box Cidades START------------------------------------------------------
        output$box9 <- renderValueBox({
                cit_choice <- covid %>%
                        filter(place_type == "city") %>%
                        filter(city == input$cidade) %>%
                        group_by(date) %>%
                        summarise(n = sum(last_available_confirmed)) %>%
                        arrange(desc(date))
                valueBox(
                        formatC(cit_choice[1,2], format="d", big.mark='.')
                        ,paste('Casos Confirmados:')
                        ,icon = icon("stats",lib='glyphicon')
                        ,color = "blue")  
        })
        
        output$box10 <- renderValueBox({
                cit_choice <- covid %>%
                        filter(place_type == "city") %>%
                        filter(city == input$cidade) %>%
                        group_by(date) %>%
                        summarise(n = sum(last_available_confirmed)) %>%
                        arrange(desc(date))
                valueBox(
                        formatC((cit_choice[1,2] - cit_choice[2,2]), format="d", big.mark='.')
                        ,paste('Novas notificações em 24h:')
                        ,icon = icon("stats",lib='glyphicon')
                        ,color = "aqua")  
        })
        
        output$box11 <- renderValueBox({
                cit_obt_choice <- covid %>%
                        filter(place_type == "city") %>%
                        filter(city == input$cidade) %>%
                        group_by(date) %>%
                        summarise(n = sum(last_available_deaths)) %>%
                        arrange(desc(date))
                valueBox(
                        formatC(cit_obt_choice[1,2], format="d", big.mark='.')
                        ,paste('Total de Óbitos:')
                        ,icon = icon("stats",lib='glyphicon')
                        ,color = "red")  
        })
        
        output$box12 <- renderValueBox({
                cit_obt_choice <- covid %>%
                        filter(place_type == "city") %>%
                        filter(city == input$cidade) %>%
                        group_by(date) %>%
                        summarise(n = sum(last_available_deaths)) %>%
                        arrange(desc(date))
                valueBox(
                        formatC((cit_obt_choice[1,2] - cit_obt_choice[2,2]), format="d", big.mark='.')
                        ,paste('Novos Óbitos em 24h:')
                        ,icon = icon("stats",lib='glyphicon')
                        ,color = "yellow")  
        })
        output$box15 <- renderValueBox({
                death_rate_cit <- covid %>%
                        filter(place_type == "city") %>%
                        filter(city == input$cidade) %>%
                        arrange(desc(date))
                
                valueBox(
                        formatC(round(death_rate_cit$last_available_death_rate[1]*100,2), format="g", digits = 2, big.mark=',')
                        ,paste('Taxa de Letalidade(%):')
                        ,icon = icon("skull",lib="font-awesome")
                        ,color = "red")  
        })
        output$box16 <- renderValueBox({
                perc_pop_cit <- covid %>%
                        filter(place_type == "city") %>%
                        filter(city == input$cidade) %>%
                        arrange(desc(date))
                
                valueBox(
                        formatC(round(perc_pop_cit$last_available_confirmed_per_100k_inhabitants[1],2), format="d", big.mark='.')
                        ,paste('Número de casos por 100 mil habitantes:')
                        ,icon = icon("stats",lib= "glyphicon")
                        ,color = "yellow")  
        })
        #Box cidades END-------------------------------------------------------
        output$foo <- renderText({
                paste0("Última atualização: ",  Sys.Date())
        })
        
        output$fig1 <- renderPlot(
                fig1
        )
        output$fig1log <- renderPlot({
                ggplot(teste, aes(x = date, y = n))+
                        geom_line(col = "blue", size = 2) +
                        scale_y_log10()+
                        xlab("Data da Notificação")+
                        ylab("Número de Casos Acumulados")+
                        scale_x_date(breaks = pretty_breaks(7))+
                        theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                         size = 14),
                              axis.text.y = element_text(face="bold", color="darkgray", 
                                                         size = 14))+
                        theme(axis.title = element_text(face = "bold", color = "black", size = 18))
        })
        
        output$fig2 <- renderPlot(
                fig2
        )
        output$fig2log <- renderPlot(
                ggplot(obts, aes(x = date, y = nobit))+
                        geom_line(col = "red", size = 2) +
                        scale_y_log10()+
                        xlab("Data da Notificação")+
                        ylab("Número de Óbitos Acumulados")+
                        scale_x_date(breaks = pretty_breaks(7))+
                        theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                         size = 14),
                              axis.text.y = element_text(face="bold", color="darkgray", 
                                                         size = 14))+
                        theme(axis.title = element_text(face = "bold", color = "black", size = 18))
                
        )
        
        output$fig4 <- renderPlot(
                fig4
        )
        
        output$fig3 <- renderPlot(
                fig3
        )
        output$fig_new_cases <- renderPlot({
                
                new_cases <- covid %>%
                        filter(place_type == "state") %>%
                        group_by(date) %>%
                        summarise(n = sum(last_available_confirmed)) %>%
                        arrange(date) %>%
                        mutate(new = (c(0, diff(n))))
                mm4 <- SMA(new_cases$new,n = 4)
                
                ggplot(new_cases, aes(x = date, y = new))+
                        geom_col(col = "darkgray", fill = "gray", size = 1) +
                        xlab("Data da Notificação")+
                        ylab("Novos Casos por dia")+
                        scale_x_date(breaks = pretty_breaks(7))+
                        geom_line(mapping = aes(x = new_cases$date, y = mm4), color = "black", size = 1.0)+
                        theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                         size = 14),
                              axis.text.y = element_text(face="bold", color="darkgray", 
                                                         size = 14))+
                        theme(axis.title = element_text(face = "bold", color = "black", size = 18))
                        
                        
                        
        })
        output$fig20 <- renderPlot({
                
                new_obti <- covid %>%
                        filter(place_type == "state") %>%
                        group_by(date) %>%
                        summarise(n = sum(last_available_deaths)) %>%
                        arrange(date) %>%
                        mutate(new = (c(0, diff(n))))
                
                mm4o <- SMA(new_obti$new,n = 4)
                
                ggplot(new_obti, aes(x = date, y = new))+
                        geom_col(col = "darkgray", fill = "gray", size = 1) +
                        xlab("Data da Notificação")+
                        ylab("Novos Casos por dia")+
                        scale_x_date(breaks = pretty_breaks(7))+
                        geom_line(mapping = aes(x = new_obti$date, y = mm4o), color = "black", size = 1.0)+
                        theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                         size = 14),
                              axis.text.y = element_text(face="bold", color="darkgray", 
                                                         size = 14))+
                        theme(axis.title = element_text(face = "bold", color = "black", size = 18))

        })
        
        output$fig_mod <- renderPlot({
                mod <- covid %>%
                        filter(place_type == "state") %>%
                        group_by(date) %>%
                        summarise(n = sum(last_available_confirmed)) %>%
                        arrange(date)
                Day <- 1:(length(mod$n))
                N <- 211623513 # population of Brazil
                
                coef(lm(logit(n/100) ~ Day,data = mod))
                
                logit <- nls(n ~ c/(1+exp(-(a+b*Day))),
                             start=list(c = 210000,
                                        a =-10.0599783,
                                        b = 0.2987),
                             data = mod,
                             trace = TRUE)
                
                #summary(logit)
                
                
                #set parameters
                c = coef(logit)[1]
                a = coef(logit)[2]
                b = coef(logit)[3]
                x <- c(min(Day):max(Day+50)) #construct a range of x values bounded by the data
                y <- c/(1+exp(-(a+b*x))) #predicted mass
                predict<-data.frame(x,y) #create the prediction data frame#And add a nice plot (I cheated and added the awesome inset jpg in another program)
                dates <- seq(as.Date(mod$date[1]), by = "days", length = length(x))
                
                cores <- c("Mod.Logístico" = "black", "Casos Reportados" = "red") 
                
                ggplot()+
                        geom_line(predict, mapping = aes(x = dates, y = y),  colour = "red", size=1)+
                        geom_point(mod, mapping = aes(x = date,  y = n), colour = "black", pch = 1, size = 3)+
                        labs(x = "Data", y = "Número de Infectados", color = "")+
                        theme(legend.position ="bottom")+
                        scale_x_date(breaks = pretty_breaks(7))+
                        theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                         size = 14),
                              axis.text.y = element_text(face="bold", color="darkgray", 
                                                         size = 14))+
                        theme(axis.title = element_text(face = "bold", color = "black", size = 18))
                
        })
        
        # Figura estados
        output$fig5 <- renderPlot({
                st_choice <- covid %>%
                        filter(place_type == "state") %>%
                        filter(state == input$estado) %>%
                        group_by(date) %>%
                        summarise(n = sum(last_available_confirmed))
                
                ggplot(st_choice, aes(x = date, y = n))+
                        geom_line(col = "blue", size = 2) +
                        xlab("Data da Notificação")+
                        ylab("Número de Casos Acumulados")+
                        scale_x_date(breaks = pretty_breaks(7))+
                        theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                         size = 14),
                              axis.text.y = element_text(face="bold", color="darkgray", 
                                                         size = 14))+
                        theme(axis.title = element_text(face = "bold", color = "black", size = 18))
        })
        output$fig5log <- renderPlot({
                st_choice <- covid %>%
                        filter(place_type == "state") %>%
                        filter(state == input$estado) %>%
                        group_by(date) %>%
                        summarise(n = sum(last_available_confirmed))
                
                ggplot(st_choice, aes(x = date, y = n))+
                        geom_line(col = "blue", size = 2) +
                        scale_y_log10()+
                        xlab("Data da Notificação")+
                        ylab("Número de Casos Acumulados")+
                        scale_x_date(breaks = pretty_breaks(7))+
                        theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                         size = 14),
                              axis.text.y = element_text(face="bold", color="darkgray", 
                                                         size = 14))+
                        theme(axis.title = element_text(face = "bold", color = "black", size = 18))
        })
                output$fig6 <- renderPlot({
                #óbitos
                st_ob_choice <- covid %>%
                        filter(place_type == "state") %>%
                        filter(state == input$estado) %>%
                        group_by(date) %>%
                        summarise(n = sum(last_available_deaths))
                ggplot(st_ob_choice, aes(x = date, y = n))+
                        geom_point(col = "blue", size = 2) +
                        geom_line(col = "blue", size = 1) +
                        xlab("Data da Notificação")+
                        ylab("Número de Casos Acumulados")+
                        scale_x_date(breaks = pretty_breaks(7))+
                        theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                         size = 14),
                              axis.text.y = element_text(face="bold", color="darkgray", 
                                                         size = 14))+
                        theme(axis.title = element_text(face = "bold", color = "black", size = 18))
        })
                output$fig6log <- renderPlot({
                        #óbitos
                        st_ob_choice <- covid %>%
                                filter(place_type == "state") %>%
                                filter(state == input$estado) %>%
                                group_by(date) %>%
                                summarise(n = sum(last_available_deaths))
                        ggplot(st_ob_choice, aes(x = date, y = n))+
                                geom_line(col = "red", size = 2) +
                                scale_y_log10()+
                                xlab("Data da Notificação")+
                                ylab("Número de Casos Acumulados")+
                                scale_x_date(breaks = pretty_breaks(7))+
                                theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                                 size = 14),
                                      axis.text.y = element_text(face="bold", color="darkgray", 
                                                                 size = 14))+
                                theme(axis.title = element_text(face = "bold", color = "black", size = 18))
                })
                output$fig13 <- renderPlot({
                        #óbitos
                        new_cases_st <- covid %>%
                                filter(place_type == "state") %>%
                                filter(state == input$estado) %>%
                                group_by(date) %>%
                                summarise(n = sum(last_available_confirmed)) %>%
                                arrange(date) %>%
                                mutate(new = (c(0, diff(n))))
                                
                        mm4_st <- SMA(new_cases_st$new,n = 4)
                        
                        ggplot(new_cases_st, aes(x = date, y = new))+
                                geom_col(col = "darkgray", fill = "gray", size = 1) +
                                xlab("Data da Notificação")+
                                ylab("Novos Casos por dia")+
                                scale_x_date(breaks = pretty_breaks(7))+
                                geom_line(mapping = aes(x = new_cases_st$date, y = mm4_st), color = "black", size = 1)+
                                theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                                 size = 14),
                                      axis.text.y = element_text(face="bold", color="darkgray", 
                                                                 size = 14))+
                                theme(axis.title = element_text(face = "bold", color = "black", size = 18))
                })
                output$fig14 <- renderPlot({
                        #óbitos
                        perc_pop_fig <- covid %>%
                                filter(place_type == "state") %>%
                                filter(state == input$estado) %>%
                                arrange(desc(date))
                        
                      ggplot(perc_pop_fig, aes(x = date, y = last_available_confirmed_per_100k_inhabitants))+
                                geom_point(col = "black", size = 2) +
                                geom_line(col = "black", size = 1) +
                                xlab("Data da Notificação")+
                                ylab("Número de Casos por 100 mil habitantes")+
                                scale_x_date(breaks = pretty_breaks(7))+
                                theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                                 size = 14),
                                      axis.text.y = element_text(face="bold", color="darkgray", 
                                                                 size = 14))+
                                theme(axis.title = element_text(face = "bold", color = "black", size = 18))
                })
                
                output$fig21 <- renderPlot({
                        new_st_ob_choice <- covid %>%
                                filter(place_type == "state") %>%
                                filter(state == input$estado) %>%
                                group_by(date) %>%
                                summarise(n = sum(last_available_deaths)) %>%
                                arrange(date) %>%
                                mutate(new = (c(0, diff(n))))
                        
                        mm4o_st <- SMA(new_st_ob_choice$new,n = 4)
                        
                        ggplot(new_st_ob_choice, aes(x = date, y = new))+
                                geom_col(col = "darkgray", fill = "gray", size = 1) +
                                xlab("Data da Notificação")+
                                ylab("Novos Casos por dia")+
                                scale_x_date(breaks = pretty_breaks(7))+
                                geom_line(mapping = aes(x = new_st_ob_choice$date, y = mm4o_st), color = "black", size = 1.0)+
                                theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                                 size = 14),
                                      axis.text.y = element_text(face="bold", color="darkgray", 
                                                                 size = 14))+
                                theme(axis.title = element_text(face = "bold", color = "black", size = 18))
                })
                
                output$fig_mod_est <- renderPlot({
                        mod_est <-  covid %>%
                                filter(place_type == "state") %>%
                                filter(state == input$estado) %>%
                                group_by(date) %>%
                                summarise(n = sum(last_available_confirmed))
                        
                        Day <- 1:(length(mod_est$n))
                        N <- 211623513 # population of Brazil
                        
                        coef(lm(logit(n/100) ~ Day,data = mod_est))
                        
                        logit1 <- nls(n ~ c/(1+exp(-(a+b*Day))),
                                     start=list(c = 21000,
                                                a = coef(lm(logit(n/100) ~ Day,data = mod_est))[1],
                                                b = coef(lm(logit(n/100) ~ Day,data = mod_est))[2]),
                                     data = mod_est,
                                     trace = TRUE)
                        
                        #summary(logit)
                        
                        
                        #set parameters
                        c = coef(logit1)[1]
                        a = coef(logit1)[2]
                        b = coef(logit1)[3]
                        x <- c(min(Day):max(Day+30)) #construct a range of x values bounded by the data
                        y <- c/(1+exp(-(a+b*x))) #predicted mass
                        predict<-data.frame(x,y) #create the prediction data frame#And add a nice plot (I cheated and added the awesome inset jpg in another program)
                        dates <- seq(as.Date(mod_est$date[1]), by = "days", length = length(x))
                        
                        cores <- c("Mod.Logístico" = "black", "Casos Reportados" = "red") 
                        
                        ggplot()+
                                geom_line(predict, mapping = aes(x = dates, y = y),  colour = "red", size=1)+
                                geom_point(mod_est, mapping = aes(x = date,  y = n), colour = "black", pch = 1, size = 3)+
                                labs(x = "Data", y = "Número de Infectados", color = "")+
                                theme(legend.position ="bottom")+
                                scale_x_date(breaks = pretty_breaks(7))+
                                theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                                 size = 14),
                                      axis.text.y = element_text(face="bold", color="darkgray", 
                                                                 size = 14))+
                                theme(axis.title = element_text(face = "bold", color = "black", size = 18))
                        
                })
                # Figura cidades
                output$fig7 <- renderPlot({
                        cit_choice <- covid %>%
                                filter(place_type == "city") %>%
                                filter(city == input$cidade) %>%
                                group_by(date) %>%
                                summarise(n = sum(last_available_confirmed)) %>%
                                arrange(desc(date))
                        
                        ggplot(cit_choice, aes(x = date, y = n))+
                                geom_point(col = "blue", size = 2) +
                                geom_line(col = "blue", size = 1) +
                                xlab("Data da Notificação")+
                                ylab("Número de Casos Acumulados")+
                                scale_x_date(breaks = pretty_breaks(7))+
                                theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                                 size = 14),
                                      axis.text.y = element_text(face="bold", color="darkgray", 
                                                                 size = 14))+
                                theme(axis.title = element_text(face = "bold", color = "black", size = 18))
                })
                output$fig7log <- renderPlot({
                        cit_choice <- covid %>%
                                filter(place_type == "city") %>%
                                filter(city == input$cidade) %>%
                                group_by(date) %>%
                                summarise(n = sum(last_available_confirmed)) %>%
                                arrange(desc(date))
                        
                        ggplot(cit_choice, aes(x = date, y = n))+
                                geom_line(col = "blue", size = 2) +
                                scale_y_log10()+
                                xlab("Data da Notificação")+
                                ylab("Número de Casos Acumulados")+
                                scale_x_date(breaks = pretty_breaks(7))+
                                theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                                 size = 14),
                                      axis.text.y = element_text(face="bold", color="darkgray", 
                                                                 size = 14))+
                                theme(axis.title = element_text(face = "bold", color = "black", size = 18))
                })
                output$fig8 <- renderPlot({
                        #óbitos
                        cit_obt_choice <- covid %>%
                                filter(place_type == "city") %>%
                                filter(city == input$cidade) %>%
                                group_by(date) %>%
                                summarise(n = sum(last_available_deaths)) %>%
                                arrange(desc(date))
                        ggplot(cit_obt_choice, aes(x = date, y = n))+
                                geom_point(col = "blue", size = 2) +
                                geom_line(col = "blue", size = 1) +
                                xlab("Data da Notificação")+
                                ylab("Número de Casos Acumulados")+
                                scale_x_date(breaks = pretty_breaks(7))+
                                theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                                 size = 14),
                                      axis.text.y = element_text(face="bold", color="darkgray", 
                                                                 size = 14))+
                                theme(axis.title = element_text(face = "bold", color = "black", size = 18))
                })
                output$fig8log <- renderPlot({
                        #óbitos
                        cit_obt_choice <- covid %>%
                                filter(place_type == "city") %>%
                                filter(city == input$cidade) %>%
                                group_by(date) %>%
                                summarise(n = sum(last_available_deaths)) %>%
                                arrange(desc(date))
                        ggplot(cit_obt_choice, aes(x = date, y = n))+
                                geom_line(col = "red", size = 2) +
                                scale_y_log10()+
                                xlab("Data da Notificação")+
                                ylab("Número de Casos Acumulados")+
                                scale_x_date(breaks = pretty_breaks(7))+
                                theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                                 size = 14),
                                      axis.text.y = element_text(face="bold", color="darkgray", 
                                                                 size = 14))+
                                theme(axis.title = element_text(face = "bold", color = "black", size = 18))
                })
                output$fig9 <- renderPlot({
                        #óbitos
                        new_cases_cid <- covid %>%
                                filter(place_type == "city") %>%
                                filter(city == input$cidade) %>%
                                group_by(date) %>%
                                summarise(n = sum(last_available_confirmed)) %>%
                                arrange(date) %>%
                                mutate(new = (c(0, diff(n))))
                        
                        mm4_cid <- SMA(new_cases_cid$new,n = 4)
                        
                        ggplot(new_cases_cid, aes(x = date, y = new))+
                                geom_col(col = "darkgray", fill = "gray", size = 1) +
                                xlab("Data da Notificação")+
                                ylab("Novos Casos por dia")+
                                scale_x_date(breaks = pretty_breaks(7))+
                                geom_line(mapping = aes(x = new_cases_cid$date, y = mm4_cid), color = "black", size = 1)+
                                theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                                 size = 14),
                                      axis.text.y = element_text(face="bold", color="darkgray", 
                                                                 size = 14))+
                                theme(axis.title = element_text(face = "bold", color = "black", size = 18))
                })
                output$fig22 <- renderPlot({
                        new_ct_ob_choice <- covid %>%
                                filter(place_type == "city") %>%
                                filter(city == input$cidade) %>%
                                group_by(date) %>%
                                summarise(n = sum(last_available_deaths)) %>%
                                arrange(date) %>%
                                mutate(new = (c(0, diff(n))))
                        
                        mm4o_ct <- SMA(new_ct_ob_choice$new,n = 4)
                        
                        ggplot(new_ct_ob_choice, aes(x = date, y = new))+
                                geom_col(col = "darkgray", fill = "gray", size = 1) +
                                xlab("Data da Notificação")+
                                ylab("Novos Casos por dia")+
                                scale_x_date(breaks = pretty_breaks(7))+
                                geom_line(mapping = aes(x = new_ct_ob_choice$date, y = mm4o_ct), color = "black", size = 1.0)+
                                theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                                 size = 14),
                                      axis.text.y = element_text(face="bold", color="darkgray", 
                                                                 size = 14))+
                                theme(axis.title = element_text(face = "bold", color = "black", size = 18))
                })
                output$fig_mod_cid <- renderPlot({
                        mod_cid <-  covid %>%
                                filter(place_type == "city") %>%
                                filter(city == input$cidade) %>%
                                group_by(date) %>%
                                summarise(n = sum(last_available_confirmed)) %>%
                                arrange(desc(date))
                        
                        Day <- 1:(length(mod_cid$n))
                        N <- 211623513 # population of Brazil
                        
                        coef(lm(logit(n/100) ~ Day,data = mod_cid))
                        
                        logit2 <- nls(n ~ c/(1+exp(-(a+b*Day))),
                                     start=list(c = 2100000,
                                                a = coef(lm(logit(n/100) ~ Day,data = mod_cid))[1],
                                                b = coef(lm(logit(n/100) ~ Day,data = mod_cid))[2]),
                                     data = mod_cid,
                                     trace = TRUE)
                        
                        #summary(logit)
                        
                        
                        #set parameters
                        c = coef(logit2)[1]
                        a = coef(logit2)[2]
                        b = coef(logit2)[3]
                        x <- c(min(Day):max(Day+30)) #construct a range of x values bounded by the data
                        y <- c/(1+exp(-(a+b*x))) #predicted mass
                        predict<-data.frame(x,y) #create the prediction data frame#And add a nice plot (I cheated and added the awesome inset jpg in another program)
                        dates <- seq(as.Date(mod_cid$date[1]), by = "days", length = length(x))
                        
                        cores <- c("Mod.Logístico" = "black", "Casos Reportados" = "red") 
                        
                        ggplot()+
                                geom_line(predict, mapping = aes(x = dates, y = y),  colour = "red", size=1)+
                                geom_point(mod_cid, mapping = aes(x = date,  y = n), colour = "black", pch = 1, size = 3)+
                                labs(x = "Data", y = "Número de Infectados", color = "")+
                                theme(legend.position ="bottom")+
                                scale_x_date(breaks = pretty_breaks(7))+
                                theme(axis.text.x = element_text(face="bold", color="darkgray", 
                                                                 size = 14),
                                      axis.text.y = element_text(face="bold", color="darkgray", 
                                                                 size = 14))+
                                theme(axis.title = element_text(face = "bold", color = "black", size = 18))
                })
                
                output$fig30 <- renderPlot({
                        #wordcloud
                        sentiment_counts <- attributes(pol_words)$counts
                        sentiment_counts[polarity > 0,]
                        ## Positive Words
                        with(
                                sentiment_counts[polarity > 0,],
                                wordcloud(words = words, freq = n, min.freq = 1,
                                          max.words = 200, random.order = F, rot.per = 0.35,
                                          colors = brewer.pal(8, "Dark2"), scale = c(4.5, .75)
                                )
                        )
                        
                })

        output$data <- renderDataTable({
                datatable(covid, extensions = "Buttons",
                              options = list(lengthChange = FALSE,
                                             dom = "Blfrtip",
                                             buttons = c("copy", "csv")))
        })
}

# Run the application 
shinyApp(ui = ui, server = server)

