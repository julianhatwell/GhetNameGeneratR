library(shiny)
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("Fantasy Name Generator"),
    
    sidebarPanel(
      h4('Set the parameters below and generate your perfect fantasy name. Great for everything from aspiring pop stars to RPG characters.'),
      numericInput('phonLength', 'Enter the number of syllables/phonemes:', 3),
      radioButtons('gen', 'Gender',
                   c("Female" = "f",
                     "Male" = "m",
                     "Unisex" = "u")),
      sliderInput("rnd", "Randomness", 0, 100, 10)
      ),
    mainPanel(
      h3('Your fantasy name is:'),
      verbatimTextOutput("ghetName")
    )
  )
)