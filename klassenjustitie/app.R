# Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, modelsummary, ggplot2, ggeffects, performance,
               scales, here, markdown, rlang, install = TRUE, update = FALSE)

# Define UI for app
ui <- fluidPage(
  titlePanel("Klassenjustitie?"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("independent_var", "Ik ben geïnteresseerd in:",
                  choices = c("Nationale origine" = "victim_ethnicity", "Gender" = "victim_gender")),
      checkboxGroupInput("covariates", "Ik wil statistisch controleren voor:",
                         choices = c("Gender" = "victim_gender", 
                                     "Nationale origine" = "victim_ethnicity", 
                                     "Leeftijd" = "victim_age", 
                                     "Beroep" = "victim_job", 
                                     "Jaar van vonnis" = "year")),
      actionButton("update", "Bereken en visualiseer")
    ),
    
    mainPanel(
      plotOutput("regression_output"),
      uiOutput("markdown_output")
    )
  )
)

lastInput <- reactiveValues(independent_var = NULL, covariates = NULL)

# Define server logic
server <- function(input, output) {
  
  # Load
  inti <- readr::read_csv("https://raw.githubusercontent.com/IntiDC/klassenjustitie/main/data.csv")
  
  # Wrangle
  inti <-
    inti %>%
    mutate(year = year(dmy(date)),
           victim_gender = case_when(
             victim_gender == "V" ~ "Vrouw",
             victim_gender == "M" ~ "Man"
           ),
           victim_ethnicity = case_when(
             victim_ethnicity == "B" ~ "Belg",
             victim_ethnicity == "NB" ~ "Niet-Belg"
           ),
           victim_job = relevel(as_factor(victim_job), ref = "Kind"),
           victim_gender = relevel(as_factor(victim_gender), ref = "Man"),
           victim_ethnicity = relevel(as_factor(victim_ethnicity), ref = "Belg")
    )
  
  # Wrangling and data processing function
  process_data <- function(independent_var, covariates) {
    model <<- if (length(covariates) == 0) {
      lm(as.formula(paste("log(compensation) ~", independent_var)), data = inti)
    } else {
      lm(as.formula(paste("log(compensation) ~", independent_var, "+", paste(covariates, collapse = "+"))), data = inti)
    }
    
    predictions_95 <- ggpredict(model = model,
                                terms = independent_var,
                                ci.lvl = .95,
                                back.transform = TRUE,
                                vcov.type = "HC3")
    predictions_90 <- ggpredict(model = model,
                                terms = independent_var,
                                ci.lvl = .90,
                                back.transform = TRUE,
                                vcov.type = "HC3")
    predictions_80 <- ggpredict(model = model,
                                terms = independent_var,
                                ci.lvl = .80,
                                back.transform = TRUE,
                                vcov.type = "HC3")
    
    list(model = model, predictions_95 = predictions_95, predictions_90 = predictions_90, predictions_80 = predictions_80)
  }
  
  observeEvent(input$update, {
    lastInput$independent_var <- input$independent_var
    lastInput$covariates <- input$covariates
  })
  
  # Check in the reactive expression whether the values have changed
  results <- reactive({
    # Only run the calculations if the inputs have changed
    if (identical(lastInput$independent_var, input$independent_var) && identical(lastInput$covariates, input$covariates)) {
      return()
    }
    
    process_data(input$independent_var, input$covariates)
  })
  
  results <- eventReactive(input$update, {
    # Store the current inputs in variables
    current_independent_var <- input$independent_var
    current_covariates <- input$covariates
    
    # Call the data processing function with the stored inputs
    process_data(current_independent_var, current_covariates)
  })
  
  # Generate regression results on response
  output$regression_output <- renderPlot({
    # Get the results
    res <- results()
    
    predictions <-
      res$predictions_95 %>%
      rename(conf_l_95 = conf.low,
             conf_h_95 = conf.high) %>%
      cbind(res$predictions_90 %>% select(conf.low, conf.high)) %>%
      rename(conf_l_90 = conf.low,
             conf_h_90 = conf.high) %>%
      cbind(res$predictions_80 %>% select(conf.low, conf.high)) %>%
      rename(conf_l_80 = conf.low,
             conf_h_80 = conf.high)
    
    # Visualise
    
    ## Parameters
    family <- "Helvetica"
    title.size <- 14
    text.size <- 12
    caption.size <- 9
    nudge.x <- .05
    text.col <- "black"
    
    x_axis_label <- reactive({
      switch(lastInput$independent_var,
             "victim_gender" = "Gender",
             "victim_ethnicity" = "Nationale origine")
    })
    
    ## ggplot
    p <- ggplot(predictions,
                aes(x = x,
                    y = predicted,
                    colour = x)) +
      geom_errorbar(aes(ymin = conf_l_95, ymax = conf_h_95),
                    width = 0,
                    linewidth = .75) +
      geom_errorbar(aes(ymin = conf_l_90, ymax = conf_h_90),
                    width = 0,
                    linewidth = 1.25) +
      geom_errorbar(aes(ymin = conf_l_80, ymax = conf_h_80),
                    width = 0,
                    linewidth = 1.75) +
      geom_point(size = 5,
                 colour = "white") +
      geom_point(size = 4) +
      geom_jitter(data = inti,
                  aes(x = !!rlang::sym(lastInput$independent_var),
                      y = compensation,
                      colour = !!rlang::sym(lastInput$independent_var)),
                  size = 2,
                  alpha = .2,
                  height = 0,
                  width = .1) +
      geom_text(aes(label = number(round(predicted), prefix = "€")),
                hjust = 0,
                nudge_x = nudge.x) +
      geom_text(aes(y = conf_l_95,
                    label = number(round(conf_l_95), prefix = "€")),
                hjust = 0,
                nudge_x = nudge.x) +
      geom_text(aes(y = conf_h_95,
                    label = number(round(conf_h_95), prefix = "€")),
                hjust = 0,
                nudge_x = nudge.x) +
      geom_label(aes(y = 7*10^5, x = 1.5,
                     label = paste0("Verklaarde variantie: ",
                                    percent(performance::r2(model)$R2_adjusted,
                                            accuracy = .01))),
                 hjust = 0,
                 nudge_x = nudge.x,
                 colour = "gray20") +
      scale_y_log10(labels = number_format(prefix = "€"),
                    breaks = breaks_log(n = 8, base = 10)) +
      scale_colour_viridis_d(begin = .3, end = .7) + 
      theme_minimal() +
      theme(plot.title = element_text(
        family = family,
        size = title.size,
        hjust = .5,
        margin = margin(t = 0, r = 0, b = 10, l = 0),
        colour = text.col),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
          colour = "gray90", linewidth = 0.5),
        panel.grid.major.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(
          family = family,
          size = text.size,
          colour = text.col),
        axis.title.y = element_text(
          family = family,
          margin = margin(t = 0, r = 10, b = 0, l = 0),
          angle = 90,
          colour = text.col,
          size = text.size,
          vjust = .5,
          hjust = 0.5),
        axis.title.x = element_text(
          family = family,
          angle = 0,
          margin = margin(t = 10, r = 0, b = 10/2, l =0),
          colour = text.col,
          size = text.size),
        plot.caption = element_text(
          size = caption.size,
          hjust = 0,
          colour = text.col),
        plot.caption.position = "panel",
        plot.title.position = "plot",
        panel.spacing = unit(10, "points"),
        legend.position = "none") +
      labs(title = str_wrap("Gemiddelde totale compensatie voor schade bij een overlijden naar demografische subgroep", 
                            width = 70),
           x = x_axis_label(),
           y = "Totale compensatie (log-schaal)")
    
    p
    
  })
  
  output$markdown_output <- renderUI({
    HTML(
      markdownToHTML(
        text = "**Data.** Data afkomstig van https://klassenjustitie.be/, beheerd door Inti De Ceukelaire.
           De ruwe data kunnen geraadpleegd worden via [GitHub](https://github.com/IntiDC/klassenjustitie),
           waar tevens aanpassingen aan de dataset kunnen worden aangevraagd.
           \n\n**Beschrijving.** De grote, niet-transparante punten geven de gemiddelde, voorspelde waarden weer voor de totale compensatie
           voor morele en materiële schade bij een overlijden naar demografische subgroep.
           De bekomen waarden kunnen desgewenst statistisch 'gecontroleerd' worden voor andere variabelen.
           De weergegeven minima en maxima voor elke subgroep vormen een interval en
           geven de betrouwbaarheid van de schatting weer.
           Hoe groter de spreiding (of hoe groter het verschil tussen de minima en maxima),
           hoe minder betrouwbaar schatting.
           Wanneer de intervallen tussen subgroepen overlappen, is het verschil niet statistisch significant
           op basis van gangbare normen binnen de sociale wetenschappen.
           De kleine, semi-transparante punten visualiseren (de spreiding van) de onderliggende datapunten;
           elk punt is hier een apart vonnis.
           Op de figuur is aangeduid in welke mate de variabelen die in de analyse werden meegenomen
           de variatie in totale compensatie tussen vonnissen kunnen verklaren (i.e. een waarde tussen
           0%, het minimum, en 100%, het maximum).
           \n\n**Intuïtie.** De figuur geeft slechts de samenhang weer tussen de compensatie en
           de groepsvariabelen en is op basis van bovenstaande analyse niet causaal te interpreteren.
           Andere niet-opgenomen karakteristieken, zoals de kostwinnerstatus van het slachtoffer of
           de juridische voorgeschiedenis van de dader, kunnen mogelijk (beter) verklaren waarom de waarden
           tussen subgroepen afwijken. Daarnaast kan het aselecte of niet-willekeurige karakter van
           de steekproeftrekking van vonnissen voor een algemeen vertekend beeld zorgen.
           Minder goed gedocumenteerde of gemediatiseerde vonnissen werden bijvoorbeeld vooralsnog
           niet opgenomen in de dataset.
           \n\n**Technische details.** De afhankelijke variabele in het lineair regressiemodel
           (gebruik makend van de kleinste-kwadratenmethode) waarop deze figuur gebaseerd is,
           is het natuurlijk logaritme van de totale compensatie.
           De voorspelde waarden voor de log-getransformeerde responses werden teruggetransformeerd
           naar de oorspronkelijke responsschaal in de visualisatie.
           De onafhankelijke variabelen verschillen naargelang de input van de gebruiker.
           In de berekening van de voorspelde waarden worden de controlevariabelen constant gehouden
           op hun gemiddelde of de referentiecategorie (typisch de meest voorkomende waarde).
           De dikke (dunnere) ((dunste)) lijn representeert het 80% (90%) ((95%)) betrouwbaarheidsinterval.
           De standaardfouten uit het regressiemodel werden gecorrigeerd voor heteroskedasticiteit
           gebruik makend van de robuuste HC3-schatter ([Long & Ervin, 2000](https://doi.org/10.2307/2685594)).
           De gerapporteerde verklaarde variantie is de variantie gecorrigeerd voor het aantal
           opgenomen predictoren in het regressiemodel.",
        fragment.only = TRUE
      )
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)