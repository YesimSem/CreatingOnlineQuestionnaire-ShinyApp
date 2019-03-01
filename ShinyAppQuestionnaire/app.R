library(shiny)
#setup questions
{
  #intro
  intro <- "Please respond the questions by clicking on the option that suits you the most. Now, click on the next button to start the survey."
  
  #create question objects
  q1.obj <- list("Imagine that your partner wants to move in with you. Your reaction would be:", c("OMG! No way!", "Well, we can discuss that", "Of course, I never understood what took him so long to propose that anyways"), "")
  q2.obj <- list("Imagine that your partner left his/her toothbrush in your bathroom. Your reaction would be:", c("Are we even in a relationship? Who does s/he think s/he is?", "Ok, I will tolerate that for now", "YEY, s/he wants to be in a relationship!"), "")
  q3.obj <- list("Imagine that your partner wants to introduce you to his/her parents. Your reaction would be:", c("How will I escape from this? I need an excuse immediately!", "Ok, I will meet them.", "OMG, I have been waiting for this, YAY!"), "")
  q4.obj <- list("Imagine that your partner texts you every morning and says: good morning, honey!. Your reaction would be:", c("Does s/he think that s/he is my morning alarm?", "Ok, I will try to respond to those messages", "S/he is so cute!!"), "")
  q5.obj <- list("Imagine that your partner wants to see you every day. Your reaction would be:", c("I need to live my own life! I cannot stand that :/", "I will try to explain that I have other duties too, but if I have time, why not", "Absolutely! That is what I want too, I must have found my soul mate!"), "")
  q6.obj <- list("Imagine that your partner wants you to share your day with him/her. Your reaction would be:", c("Are you my mother?", "Sure, I would not share every single detail but I would talk about my day.", "Sharing is caring! I would share even if s/he did not ask me to."), "")
  q7.obj <- list("How would you spend time with your partner?", c("Adrenalin sports", "Eating out in a fancy restaurant", "At home, cuddling:=)"), "")
  q8.obj <- list("Is your partner one of your best friends?", c("No, not a chance", "A friend for sure but I would not say a best friend", "Absolutely, s/he is my bestie!"), "")
  q9.obj <- list("How important is romanticism for you in relationships?", c("Not at all", "I would like to experience it but it is not an absolute necessity", "The most important element of the relationship!"), "")
  q10.obj <- list(" Would you like to travel without your partner?", c("It is a must!", "I would not but if I have to, then I will do it", "Nope, never"), "")
  
  names(q1.obj) <- c("Question", "Answers", "Response")
  names(q2.obj) <- c("Question", "Answers", "Response")
  names(q3.obj) <- c("Question", "Answers", "Response")
  names(q4.obj) <- c("Question", "Answers", "Response")
  names(q5.obj) <- c("Question", "Answers", "Response")
  names(q6.obj) <- c("Question", "Answers", "Response")
  names(q7.obj) <- c("Question", "Answers", "Response")
  names(q8.obj) <- c("Question", "Answers", "Response")
  names(q9.obj) <- c("Question", "Answers", "Response")
  names(q10.obj) <- c("Question", "Answers", "Response")
  
  #Create question objects list
  question.objs <- list(q1.obj, q2.obj, q3.obj, q4.obj, q5.obj, q6.obj, q7.obj, q8.obj, q9.obj, q10.obj)
  
  #Randomize question objects
  randomized <- sample(question.objs, 10, replace = F)
}

#helpers
{
  randomize.answers <- function(question) {
    answers <- question$Answers
    randomized.answers <- sample(answers, 3, replace = F)
    return(randomized.answers)
  }
  
  get.score <- function() {
    score <- 0
    for (question in randomized) {
      if (question$Response == question$Answers[1])
        score <- score + 1
      else if (question$Response == question$Answers[2])
        score <- score + 2
      else if (question$Response == question$Answers[3])
        score <- score + 3
    }
    
    print(paste("Score:", score))
    #switch(score <= 15 ="Need some flame!", score > 15 & score <= 25 = "Just the right amount;)", score > 25 = "You are burning!" )
    if (score <= 15)
      return("Need some flame!")
    if (score > 15 & score <= 25)
      return("Just the right amount;)")
    if (score > 25)
      return("You are burning!")
  }
}

update.question <- function(input, output, session) {
  if (q.index > 0 & q.index < length(randomized)) {
    randomized[[q.index]]$Response <<- input$response
  }
  q.index <<- q.index + 1
  if (q.index > 0 & q.index <= length(randomized)) {
    q <- randomized[[q.index]]
    answers <- sample(q$Answers, 3, replace = F)
    updateRadioButtons(session = session, inputId = "response", choices = c(answers[[1]], answers[[2]], answers[[3]]))
    output$question <- renderText(q$Question)
  }
  else if (q.index > length(randomized))
    output$result <- renderText(get.score())
}

q.index <- 0

#shiny
{
  ui <- fluidPage(titlePanel("How warm are you in your relationships?"), 
                  #textOutput("question") = use this if you are okay with default black color
                  span(textOutput("question"), style="color:red"),
                  conditionalPanel(condition = "output.question != 'Please respond the questions by clicking on the option that suits you the most. Now, click on the next button to start the survey.'",
                                   textOutput("blank"),
                                   radioButtons(inputId = "response", "Response:", choices = c("a", "b", "c"), width = 500),
                                   textOutput("blank2")),
                  actionButton(inputId = "nxt", "Next"),
                  textOutput("result")
  )
  
  server <- function(input, output, session) {
    output$question <- renderText(intro)
    observeEvent(input$nxt, update.question(input, output, session))
    session$onSessionEnded(stopApp)
  }
  
  shinyApp(server = server, ui = ui)
}

