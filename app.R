library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(ggplot2)
library(tidyverse)

# UI ----
ui <- page_sidebar(
  
  title = "Données de manchots",
  
  theme = bs_theme(preset = "darkly"),
  
  sidebar = sidebar("Choisir l'espèce et l'année",
                    selectInput(
                      "espece",
                      tags$strong("Espèce"),
                      choices = list("Adelie", "Gentoo", "Chinstrap"),
                      selected = 1),
                    checkboxGroupInput(
                      "annee",
                      tags$strong("Année"),
                      choices = list("2007", "2008", "2009")#attention en mettant des guillemets ici les valeurs numériques sont enregistrées comme du texte, ne pas mettre de guillemets si on veut des valeurs numériques
                    )),
  card(
      card_header(tags$strong("Tableau de bord")),
      "Pésentation de données de manchots en Antartique",
    layout_columns(
      card(tags$span("Longueur de l'aile selon la masse corporelle", plotOutput("aile_masse_graph"), inline = TRUE)),
        card("Masse corporelle selon le sexe", plotOutput("masse_sexe_graph"))
    ),
      card(
          DT::DTOutput("tableau")
          ))
)

# Serveur logic ----
server <- function(input, output) {
  output$masse_sexe_graph <- renderPlot({ penguins %>%
      mutate(sex = recode(sex, "female" = "femelle", "male"   = "mâle")) %>%
      filter(!is.na(body_mass), !is.na(sex)) %>%
      ggplot(aes(x = sex, y = body_mass, color = sex)) +
      stat_summary(fun = mean, geom = "point", size = 3) +
      stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
      facet_wrap(~ year) +
      scale_color_manual(values = c("femelle" = "#74ADD1", "mâle"   = "#2B83BA")) +
      labs(title = "Masse corporelle selon le sexe",
           x = NULL,
           y = "Masse corporelle (g)",
           color = "Sexe") +
      theme_minimal() +
      theme(axis.text.x = element_blank())})
  
  output$aile_masse_graph <- renderPlot({penguins %>%
      filter(!is.na(flipper_len),!is.na(body_mass)) %>%
      ggplot(aes(x = body_mass, y = flipper_len)) +
      geom_point(color = "#74ADD1", alpha = 0.7) + 
      geom_smooth(method = "lm", se = FALSE, color = "#2B83BA", linewidth = 1) +
      labs(title = "Longueur de l'aile selon la masse corporelle",
           x = "Masse corporelle (g)",
           y = "Longueur de l'aile (mm)" ) +
      theme_minimal()})
  
  output$tableau <- DT::renderDT({penguins})
  }

# Appeler shinyapp ----
shinyApp(ui = ui, server = server)
