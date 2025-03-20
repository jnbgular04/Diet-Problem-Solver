library(shiny)
library(readxl)
library(MASS)
library(DT)
library(bslib)

setwd("C:/Users/jazmi/Documents/ACADEMICS/CMSC 150/Proj")
source("GularEx05Code.R")

#Global Variables
NutriT <- read_excel("NutritionTable.xlsx")
choices <- setNames(seq_along(NutriT$Foods), NutriT$Foods)
Costs_Matrix <- NutriT[,2]
rownames(Costs_Matrix) <- NutriT$Foods 

#explain seq_along
ui <- navbarPage(
  #TTTLE
  div(
    "Diet Problem Solver",
    class = "custom-header-text"),
  #Inline CSS
  tags$head(
      tags$link(href="https://fonts.googleapis.com/css2?family=Kanit:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap",
                rel="stylesheet"),
      
      tags$style(HTML("
      .nav-tabs > li > a {
        font-family: 'Kanit', sans-serif;
        font-size: 22px;
        font-weight: bold;
        color: #8EB486;
        background-color: #f5f5f5;
        border-radius: 5px;
      }
      .nav-tabs > li > a[data-value='Results']:hover {
        background-color: #ddd;
      }
       body {
          background-image: url('Background.png');
          background-size: cover;
          background-position: center;
          background-attachment: fixed;
        }
        .custom-sidebar {
          background-color: #FDF7F4;
          border: 1px solid #ddd;
          padding: 20px;
          border-radius: 10px;
        }
        .custom-header-text {
          font-family : 'Kanit';
          color: #a7c98c;
          font-size: 25px;
          font-weight: bold;
        }
        .custom-font{
          font-family : 'Kanit';
        }
        .custom-tab-style{
          padding: 20px; 
          height: 700px;
          border: 1px solid #ddd;
          border-radius: 10px; 
          background-color: #fafff3;
        }
        .custom-card-style{
          padding: 20px; 
          border: 1px solid #ddd;
          border-radius: 10px; 
          background-color: #fafff3;
        }
        .custom-solutions-style{
          padding: 20px; 
          height: 400px;
          border: 1px solid #ddd;
          border-radius: 10px; 
          background-color: #fafff3;
        }
        .dataTable {
          font-family: 'Kanit';
        }
        .center {
          margin: auto;
          width: 50%;
          border: 3px solid green;
          padding: 10px;
          font-family : 'Kanit';
        }
        #solved_feasible_output {
          font-family : 'Kanit', Light 300;
          font-size: 20px; 
          color: #c0e2a5; 
          font-weight: bold; 
          text-align: center;
        }
        .custom-about-container {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 50vh;  # Ensures the container takes up the full height of the viewport
        overflow-y: auto;
        box-sizing: border-box; 
      }
        .about-style {
        padding: 20px;
        border: 1px solid #ddd;
        border-radius: 10px;
        background-color: #fafff3;
        box-sizing: border-box; 
        width: 900px;  
        overflow-y: auto;
        }
       .custom-text {
        font-family : 'Kanit', Light 300;
        font-size: 20px; 
        color: #c0e2a5; 
        text-align: left;
      }
      "))
  ),
  
  #Instructions Panel
  tabPanel(
      div(
        "About",
        class = "custom-header-text"),
      
      card(
        class = "custom-about-container",
        card(
          class = "about-style",
          h3("Diet Problem Solver",class = "custom-header-text"),
          h1("The Diet Problem Solver is a program that aims to find the cheapest 
             and most nutritious combination of foods that will satisfy all the daily 
             nutritional requirements of an individual. The problem is formulated as a 
             linear program where the objective is to minimize cost and meet constraints 
             but will still satisfy the nutritional needs.", class = "custom-text"),
          h3("Instructions: ",class = "custom-header-text"),
          h1("Navigate towards the Calculations Tab to start. 
             Choose atleast two foods in the left bar menu, and 
             click submit once you are done. The results will then show up on the page!"
             ,class = "custom-text")
        )
      )
  ),
  
  #Calculation Panel
  tabPanel( 
    div(
      "Calculations",
      class = "custom-header-text"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        style = "background-color: #fafff3",
        div(
          #Action button to submit
          actionButton("submit", label = "Submit",class="center"),
 
          checkboxInput('all',"Select All/None", value = TRUE), 
          
          #Checkbox Group for choices
          checkboxGroupInput(inputId ='foodChoices', 
                             label = h3("Choices for Food",
                              class = "custom-header-text"), 
                             choices),
          
          class ="custom-font",
        ),
      ), #End of Side Bar Panel
      mainPanel(
        width = 9,
        conditionalPanel(
          condition = "output.dataVisible", # Condition for showing the table
          
          tabsetPanel(
            tabPanel("Results", 
                     card(
                       class = "custom-card-style",
                       h3("The Optimized Menu :", class = "custom-header-text"),
                       textOutput("solved_feasible_output"),
                       h3("The Solution and Cost Breakdown by Food :", class = "custom-header-text"),
                       DTOutput("solved_tableau")
                     )
            ),
            
            tabPanel("Initial Tableau",
                     card(
                       class = "custom-tab-style",
                       h3("Here is the Initial Tableau",class = "custom-header-text"),
                       DTOutput("initial_tableau"),
                       DTOutput("initial_basicsolutions")
                     ),
                     card(
                       class = "custom-solutions-style",
                       h3("Here are the initial basic solutions",class = "custom-header-text"),
                       DTOutput("initial_basicsolutions")
                     )
            ),
            
            tabPanel("Tableaus and Solutions", 
                     #CARD HOLDS THE ITERATIONS
                     card(
                       selectInput("iteration", "Select Iteration:", choices = NULL,), # Dropdown for iterations
                       class = "custom-tab-style",
                         h3("Tableaus per Iteration",class = "custom-header-text"),
                         DTOutput("iteration_table"), 
                     ),
                     card(
                       class = "custom-solutions-style",
                       h3("Basic Solutions per Iteration",class = "custom-header-text"),
                       DTOutput("basicsolutions") 
                     )
            )
          )
          
        ) #End of Conditional Panel
      ) # Main panel for displaying outputs
    )
  )

)

server <- function(input, output, session) {
  #set reactive values
  available_iterations <- reactiveVal(NULL)
  data_visible <- reactiveVal(FALSE)
  solved_feasible <- reactiveVal(NULL)
  final_tab <- reactiveVal(NULL)
  final_sols <- reactiveVal(NULL)
  list_tabs <- reactiveVal(NULL)
  list_sols <- reactiveVal(NULL)
  total_iterations <- reactiveVal(NULL)
  sol_table <- reactiveVal(NULL)
  initial_tableau <- reactiveVal(NULL)
  initial_basicsols <- reactiveVal(NULL)
  
  selected_iteration <- reactive({
    input$iteration
  })
  
  selected_foods <- reactive({ #reactive updates as the data is changed
    foods <- c()  # create an empty vector
    foods <- c(foods, input$foodChoices)  # update the current current selection
    foods  # Return the vector
  })
  
  #source: https://www.youtube.com/watch?v=PNzNDq1_uKQ
  observe({
    # if input$all is TRUE (basically a SELECT ALL option), all choices will be selected
    # if input$all is FALSE (basically a NONE option), none of the choices will be selected
    
    updateCheckboxGroupInput(
      session, 'foodChoices', choices = choices,
      selected = if(input$all) choices
    )

  })
  
  output$value <- renderPrint({
    selected_foods() # Print the reactive vector
  })
  
  # observeEvent ensures the calculation is done once the submit button is clicked
  observeEvent(input$submit, {
    selected_foods <- as.numeric(selected_foods()) # Vector of choices is a vector that holds the submitted food choices by the user
    
    if (length(selected_foods) == 0 ||length(selected_foods) == 1 || all(is.na(selected_foods))) {
      showModal(modalDialog(
        title = "ENGK! ERROR!",
        "Please select at least two food items.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()  # Exit the function if no foods are selected
    }
    
    # Setting Up
    mat <- as.matrix(NutriT[selected_foods(), ]) # Create a matrix from inputs
    foodnames <- mat[, 1]  
    rownames(mat) <- foodnames                 # Reset name of matrix columns
    mat <- mat[, -1]                           # Remove the first column, which holds the food names
    mat <- apply(mat, c(1, 2), trimws)         # Get rid of white spaces [nagerror when i first tested kaya i did these]
    mat <- apply(mat, c(1, 2), as.numeric)     # Convert to numeric
    
    mat <- t(mat)                              # Transpose
    
    obj_func <- (mat[1,]) #get vector of obj function
    
    #Get the Initial Tableau for nutrition constraints
    iniTab <- mat[2:nrow(mat),] #starts at 2 since column one holds names
    
    #Setup for Nutrient Constraints
    nutrientMax <- iniTab
    nutrientMin <- iniTab
    
    #List Nutrient Constraints
    nutrientminConstraints <- c(2000,0,0,0,0,25,50,5000,50,800,10)
    nutrientmaxConstraints <- c(2250,300, 65, 2400, 300,100,100,50000,20000,1600,30)
    
    nutrientMax  <- cbind(nutrientMax,nutrientmaxConstraints)
    nutrientMax  <- nutrientMax*(-1) #multiply Max by -1 
    nutrientMin <- cbind(nutrientMin,nutrientminConstraints)
    
    #Add to a Tableau
    tempfinalTab <- rbind(nutrientMax,nutrientMin)
    
    #Setup for Serving Constraints
    diagonal <- diag(1,length(selected_foods))
    #https://www.tutorialspoint.com/how-to-create-a-vector-with-repeated-values-in-r
    servings_min <-rep(0,length(selected_foods))
    servings_max <-rep(-10,length(selected_foods))
    
    servings_min_eq <- cbind(diagonal,servings_min)
    servings_max_eq <- cbind((-1*diagonal),servings_max)
    
    finalServings <- rbind(servings_min_eq,servings_max_eq)
    
    #Add the constraints for the servings
    FinalTab <- rbind(tempfinalTab,finalServings)
    obj_func <- c(obj_func,0) #Add a 0 to the objective function
    FinalTab <- rbind(FinalTab,obj_func) #add back objective function
    
    transposedFT <- t(FinalTab)
    transposedFT[nrow(transposedFT),] <- transposedFT[nrow(transposedFT),]* -1 #multiply by -1 the constraints
    
    obj_func <- transposedFT[,ncol(transposedFT)] # Take away obj function
    transposedFT <- transposedFT[,-ncol(transposedFT)] # Get the new transposedtable
    slackVars <- diag(1,nrow(transposedFT)) # Create a diagnonal for the slack values
    colnames(slackVars) <- c(foodnames,"Z")
    FtransposedFT <- cbind(transposedFT,slackVars) # Bind the diagonal
    FtransposedFT <- cbind(FtransposedFT,obj_func) # Bind the two
    
    initial_tableau(FtransposedFT) #set the initial tableau reactiveVal as the transposedFT
    
    #Calculate Simplex
    results <- Simplex(FtransposedFT,FALSE)

    #Extract vector of solution data
    solution_data <- results$basic_solutions[,(ncol(results$basic_solutions)- length(selected_foods)):(ncol(results$basic_solutions)- 1)] #extract basic solution
    new_data_vec <- solution_data[solution_data != 0] #gets only the non zeros
    valid_foods <- names(new_data_vec)
    
    # Extract rows with specific row names
    valid_foods_costs <- Costs_Matrix[valid_foods,]
    solution_cost <- new_data_vec*valid_foods_costs
    # Now create the solution table with two columns: non-zero solutions and their corresponding costs
    solution_table <- data.frame(Servings = round(new_data_vec,digits = 2), Cost = round(solution_cost,digits = 2))

    
    if(results$feasible){ #THE GIVEN IS FEASIBLE
      solved_feasible(paste("The cost of this optimal diet is $", round(results$Z, 2), " per day.")) #UPDATES
      final_tab(results$final_tableu)
      final_sols(results$basic_solutions)
      list_tabs(results$iteration_tab) 
      list_sols(results$iteration_sols)
      total_iterations(results$iterations)
      sol_table(solution_table)
      initial_basicsols(results$initial_basicsolutions)
      
      
      available_iterations(c(1:results$iterations))  
      updateSelectInput(session, "iteration", choices = available_iterations())
      
    }else{
      solved_feasible("The problem is infeasible. You can trace where you went wrong in the next pages.")
      final_tab(results$final_tableu)
      final_sols(results$basic_solutions)
      list_tabs(results$iteration_tab) 
      list_sols(results$iteration_sols)
      total_iterations(results$iterations)
      initial_basicsols(results$initial_basicsolutions)
      sol_table(NULL)
      
      available_iterations(c(1:(results$iterations - 1)))  
      updateSelectInput(session, "iteration", choices = available_iterations())
    }
    
    data_visible(TRUE)
    
  }) #end of observed event
  
  output$initial_tableau <- renderDT({ 
    initial_tableau()
  }) 
  
  output$initial_basicsolutions <- renderDT({
    sols <- initial_basicsols()
    
    initial_basicsols <- as.data.frame(t(sols))
    datatable(
      initial_basicsols, 
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  output$solved_feasible_output <- renderText({ 
    solved_feasible()
  }) 
  
  output$solved_tableau <- renderDT({ 
    if(!is.null(sol_table())){
        sol_table()
    }
  }) 
  
  output$iteration_table <- renderDT({
    req(data_visible())  # Only render the table if it's set to TRUE
    iterations_tab <- list_tabs()

    selected_iteration_index <- as.numeric(selected_iteration()) #ensures that it is numeric so it can work in indexing

    datatable(
        iterations_tab[[selected_iteration_index]],  # Access the data for the selected iteration
        options = list(pageLength = 5, scrollX = TRUE)
      )
  })
  
  output$basicsolutions <- renderDT({
    req(data_visible())  # Only render the table if it's set to TRUE
    solutions_list <- list_sols()
    
    selected_iteration_index <- as.numeric(selected_iteration()) # ensures that it is numeric so it can work in indexing
    
    # Access the selected vector from the list
    selected_solution <- solutions_list[[selected_iteration_index]]
    sol_names <- names(selected_solution)

    selected_solution <- as.data.frame(t(selected_solution))
   
    colnames(selected_solution) <- sol_names
    
    datatable(
      selected_solution,  # Access the data for the selected iteration
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  # Expose the reactive value to the conditionalPanel
  output$dataVisible <- reactive({
    data_visible()
  })
  outputOptions(output, "dataVisible", suspendWhenHidden = FALSE)
  
}

shinyApp(ui, server)
