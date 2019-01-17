library(shiny)
library(lpSolveAPI)
ui <- fluidPage(navbarPage("Final Shiny Project", tabPanel("Results",(mainPanel(
  h3('The LP formulation below is created in R based on the user inputs and is solved using the 
     lpSolveAPI package'))))),
  verbatimTextOutput("model"),
  tabPanel("Optimization of 2v-2c equation",
           sidebarPanel(
             h3('Please enter variable,constraint values'),
             numericInput('Bound1','Bound 1', 6, min = 0, max = 99999, step = 1),
             numericInput('Bound2','Bound 2', 45, min = 0, max = 99999, step = 1),
             numericInput('Bound3','Bound 3', 45, min = 0, max = 99999, step = 1),
             numericInput('Variable1','Objective Variable 1', 1, min = 0, max = 99999, step = 1),
             numericInput('Variable2','Objective Variable 2', 1, min = 0, max = 99999, step = 1),
             numericInput('Variable3','Objective Variable 3', 1, min = 0, max = 99999, step = 1),
             numericInput('Row1x1','Constraint1:Variable 1',1, min = 0, max = 99999, step = 1),
             numericInput('Row1x2','Constraint1:Variable 2',1, min = 0, max = 99999, step = 1),
             numericInput('Row1x3','Constraint1:Variable 3',1, min = 0, max = 99999, step = 1),
             numericInput('Row21','Constraint2:Variable 1',1, min = 0, max = 99999, step = 1),
             numericInput('Row22','Constraint2:Variable 2',1, min = 0, max = 99999, step = 1),
             numericInput('Row23','Constraint2:Variable 3',1, min = 0, max = 99999, step = 1),
             numericInput('Row31','Constraint3:Variable 1',1, min = 0, max = 99999, step = 1),
             numericInput('Row32','Constraint3:Variable 2',1, min = 0, max = 99999, step = 1),
             numericInput('Row33','Constraint3:Variable 3',1, min = 0, max = 99999, step = 1),
             submitButton('Submit')),
           
           
           
           mainPanel(
             h2('Results of optimization'),
             #h4('Varable 1 Selected is'),verbatimTextOutput("outvariable1"),
             #h4('Varable 2 Selected is'),verbatimTextOutput("outvariable2"),
             #h4('Varable 3 Selected is'),verbatimTextOutput("outvariable3"),
             
             
             #h4('Row1 x1 is'),verbatimTextOutput("outrow1x1"),
             #h4('Row1 x2 is'),verbatimTextOutput("outrow1x2"),
             #h4('Row1 x3 is'),verbatimTextOutput("outrow1x3"),
             
             #h4('Row2 x1 is'),verbatimTextOutput("outrow21"),
             #h4('Row2 x2 is'),verbatimTextOutput("outrow22"),
             #h4('Row2 x3 is'),verbatimTextOutput("outrow23"),
             
             #h4('Row3 x1 is'),verbatimTextOutput("outrow31"),
             #h4('Row3 x2 is'),verbatimTextOutput("outrow32"),
             #h4('Row3 x3 is'),verbatimTextOutput("outrow33"),
             
             #h4('Bound x1 is'),verbatimTextOutput("outBound1"),
             #h4('Bound x2 is'),verbatimTextOutput("outBound2"),
             #h4('Bound x3 is'),verbatimTextOutput("outBound3"),
             h4('Constraints for the given equation is'),verbatimTextOutput('constraints'),
             
             h4('Variables for the following equation is '),verbatimTextOutput("variables"),
             h4('The Optimized values for the given equation is '),verbatimTextOutput("objective"))),
  tabPanel("Uses of Optimization",
           mainPanel(
             
             h3('Limits:'),
             
             h4('For user convinency the limits are set between   1 to 99999 for bounds,objective and constraint variables '),
             h3('Example: Using Wyndor Glass Co. problem ' ),
             h4('Maximizing 3x1 + 5x2 + 0x3' ),
             h4('ST' ),
             h4(' 1x1 + 0x2 + 0x3 <=4' ),
             h4(' 0x1 + 2x2 + 0x3 <=12' ),
             h4(' 3x1 + 2x2 + 0x3 <=18' ),
             h4('x1,x2,x3 >=0' ),
             h4('Where: x 1,x2,X3 = Products produced from each plant' ),
             h4('Product 1 = Glass door ' ),
             h4('Product 2 = Wooden framed window ' ),
             h4('       X1, X2 =   # of batches of Product 1 and 2 produced per week' ),
             h4('       Bounds = Production time/week,Hours' ),
             h4('       Constraint 1,2,3  = Different Plants ' ),
             
        
             h4('       Z= Profit($,000) / Week '),
             h3('Aim:'),
             
             h4('Trying to maximize the profit by producing approprite mix of products.'),
             
             h4('Thefore company Wyndor Glass co. can produce $36,000 per week by allocating
                Prudction Time of 12,2,18 hours for the 3 plants (respectivly) and making a profit of $2,000 and 
                $6,000 per batch for product 1 and 2') )))
server <- function(input, output) {
  lprec <- make.lp(3, 3)
  invisible(lp.control(lprec, sense = "max"))
  set.objfn(lprec, c(8, 5, 10))
  set.constr.value(lprec, rhs = c(6,45,20), constraints=seq(1:3))
  set.constr.type(lprec, c(rep("<=", 3)))
  set.row(lprec, 1, c(1, 1,1))
  set.row(lprec, 2, c(9,5,8))
  set.row(lprec, 3, c(20,12,9))
  name.lp(lprec, "Maximize the given Function")
  dimnames(lprec) <- list(c("Constraint 1","Constraint 2","Constraint 3"), c("Variable1","Variable2","Variable3"))
  
  
  output$outvariable1 <- renderPrint({input$Variable1})
  output$outvariable2 <- renderPrint({input$Variable2})
  output$outvariable3 <- renderPrint({input$Variable3})
  output$outrow1x1 <- renderPrint({input$Row1x1})
  output$outrow1x2 <- renderPrint({input$Row1x2})
  output$outrow1x3 <- renderPrint({input$Row1x3})
  output$outrow21 <- renderPrint({input$Row21})
  output$outrow22 <- renderPrint({input$Row22})
  output$outrow23 <- renderPrint({input$Row23})
  output$outrow31 <- renderPrint({input$Row31})
  output$outrow32 <- renderPrint({input$Row32})
  output$outrow33 <- renderPrint({input$Row33})
  output$outBound1 <- renderPrint({input$Bound1})
  output$outBound2 <- renderPrint({input$Bound2})
  output$outBound3 <- renderPrint({input$Bound3})
  
  
  
  output$objective <- renderText({
    set.objfn(lprec,c(input$Variable1,input$Variable2,input$Variable3))
    set.row(lprec, 1, c(input$Row1x1,input$Row1x2,input$Row1x3))
    set.row(lprec, 2, c(input$Row21,input$Row22,input$Row23))
    set.row(lprec, 3, c(input$Row31,input$Row32,input$Row33))
    set.constr.value(lprec, rhs = c(input$Bound1,input$Bound2,input$Bound3), constraints=seq(1:3))
    solve(lprec)
    get.objective(lprec)
  })
  
  output$constraints <- renderText({
    set.objfn(lprec,c(input$Variable1,input$Variable2,input$Variable3))
    set.row(lprec, 1, c(input$Row1x1,input$Row1x2,input$Row1x3))
    set.row(lprec, 2, c(input$Row21,input$Row22,input$Row23))
    set.row(lprec, 3, c(input$Row31,input$Row32,input$Row33))
    set.constr.value(lprec, rhs = c(input$Bound1,input$Bound2,input$Bound3), constraints=seq(1:3))
    solve(lprec)
    get.constraints(lprec)
  })

  output$variables <- renderText({
    set.objfn(lprec,c(input$Variable1,input$Variable2,input$Variable3))
    set.row(lprec, 1, c(input$Row1x1,input$Row1x2,input$Row1x3))
    set.row(lprec, 2, c(input$Row21,input$Row22,input$Row23))
    set.row(lprec, 3, c(input$Row31,input$Row32,input$Row33))
    set.constr.value(lprec, rhs = c(input$Bound1,input$Bound2,input$Bound3), constraints=seq(1:3))
    solve(lprec)
    get.variables(lprec)
  })  
  
  output$model <- renderPrint({
    set.objfn(lprec,c(input$Variable1,input$Variable2,input$Variable3))
    set.row(lprec, 1, c(input$Row1x1,input$Row1x2,input$Row1x3))
    set.row(lprec, 2, c(input$Row21,input$Row22,input$Row23))
    set.row(lprec, 3, c(input$Row31,input$Row32,input$Row33))
    set.constr.value(lprec, rhs = c(input$Bound1,input$Bound2,input$Bound3), constraints=seq(1:3))
    solve(lprec)
    get.objective(lprec)
    solve(lprec)
    print(lprec)
  })
  
}  
shinyApp(ui, server)
