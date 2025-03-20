# Gular, Jazmine
# CMSC 150 AB3L
# Exer 05
# This program implements the simplex method in R
# Creation Date : 10/18/2024

library(MASS)
Simplex <- function(tableu,isMax){ #tableu is a matrix and isMax = boolean value
  if(!is.matrix(tableu)) {
    stop("You did not input a matrix") #make sure input is a matrix
  }
    
  is_feasible = TRUE
  max_iterations= 1000
  iteration = 0
  tab <- tableu
  negnum = -1 
  
  # Create initial basic solutions (taken from code below)
    initial_basicsolutions <- vector()
    for (i in 1:(ncol(tab)-1)){
      count = 0 #non zero count
      row_index = 0
      
      for (j in 1:nrow(tab)) {
        if (tab[j, i] != 0) { # if it is not 0, add the count
          count = count + 1
          if (tab[j, i] == 1) { # if it is 1, save the index at where it is 1
            row_index = j 
          }
        }
      }
      
      if ((count == 1) && row_index > 0){
        initial_basicsolutions[i] <- tab[row_index, ncol(tab)] / tab[row_index, i]
      }else{ #if there is only one non-zero
        initial_basicsolutions[i] <- 0
      }
    }
    
    bsnames <- colnames(tab)
    bsnames <- bsnames[-c(ncol(tab))]
    names(initial_basicsolutions)<- bsnames

  iteration_tab <- list()  # Use a list to store the tables (matrices)
  iteration_sols <- list() # Use a list to store the solutions (vectors)
  
  while(negnum < 0){ # Stops if there are no negative numbers in the last row
    iteration = iteration + 1 #increment iteration
    
    if (iteration > max_iterations) {
      is_feasible = FALSE
      break
    }
    pivcol = 0
    col = 0
    
    # Find column with most negative value
    pivcol <- which.min(tab[nrow(tab), 1:(ncol(tab) - 1)]) 
    
    
    # Set initial pivot element
    pivel = Inf
    pivrow = -1
    
    for (i in 1:(nrow(tab) - 1)) {
      if (tab[i, pivcol] > 0) {
        temp <- tab[i, ncol(tab)] / tab[i, pivcol]
        if (temp < pivel) {
          pivel = temp
          pivrow = i
        }
      }
    }
    
    if(pivel == Inf){ #walang pivot element na hanap
      is_feasible = FALSE
      break
    }
    
    #Implement Gauss Jordan
    tab[pivrow ,] = tab[pivrow,]/tab[pivrow ,pivcol] #normalize pivot row
    normalizedrow <- tab [pivrow,]
    #clearing of pivot column
    
    #Get pivot row
    for(i in 1:nrow(tab)){
      if (i == pivrow){
        next #continue in R
      }
      tab[i,] = tab[i,] - ( (normalizedrow) * tab[i,pivcol])
    }
    
    # Create Basic Solutions
    basicsolutions <- vector()
    for (i in 1:(ncol(tab)-1)){
      count = 0 #non zero count
      row_index = 0
      
      for (j in 1:nrow(tab)) {
        if (tab[j, i] != 0) { # if it is not 0, add the count
          count = count + 1
          if (tab[j, i] == 1) { # if it is 1, save the index at where it is 1
            row_index = j 
          }
        }
      }
      
      if ((count == 1) && row_index > 0){
        basicsolutions[i] <- tab[row_index, ncol(tab)] / tab[row_index, i]
      }else{ #if there is only one non-zero
        basicsolutions[i] <- 0
      }
    }
    
    bsnames <- colnames(tab)
    bsnames <- bsnames[-c(ncol(tab))]
    names(basicsolutions)<- bsnames
    # Store tableau and basic solutions into seperate lists
    iteration_tab[[iteration]] <- tab
    iteration_sols[[iteration]] <- basicsolutions
    
    # Update negative numbers in the lowest row
    negnum <- min(tab[nrow(tab), 1:ncol(tab)-1]) 
    
  }#end of while loop
  
  if (isMax == TRUE){ # Maximization
    
    Z = basicsolutions[ncol(tab)-1]
    basicSolution <- matrix(basicsolutions,nrow=1,ncol=ncol(tab)-1,byrow=FALSE)
    
    # Column names for matrix
    bsnames <- colnames(tab)
    
    # Remove last element source: https://www.geeksforgeeks.org/how-to-remove-specific-elements-from-vector-in-r/
    bsnames <- bsnames[-c(ncol(tab))]
    colnames(basicSolution)<- bsnames
    finalTableau <- tab
    
  }else if (isMax == FALSE){ # Minimization
    basicSolution <- tab[nrow(tab),1:(ncol(tab)-1),drop=FALSE] # Cut the basic sol into a 1 row matrix
    basicSolution[nrow(basicSolution),ncol(basicSolution)] <- tab[nrow(tab),ncol(tab)] # Replace the last element with the solution
    Z = as.numeric(basicSolution[1,ncol(basicSolution)]) # Get Z, and turn into as numeric as to remove the dimname
    
    bsnames <- colnames(tab)
    bsnames <- bsnames[-c(ncol(tab))]
    colnames(basicSolution)<- bsnames
    
    finalTableau <- tab=
  }
  
  finallist <- list(initial_basicsolutions = initial_basicsolutions,final_tableu = finalTableau, basic_solutions=basicSolution, 
                    Z = Z, feasible = is_feasible, iterations = iteration, 
                    iteration_tab = iteration_tab, iteration_sols = iteration_sols )
  
  return(finallist)
}

iniTab <- matrix(c(
  1,7,1,0,0,14,
  2,6,0,1,0,20,
  -4,-20,0,0,1,0
), nrow =3, byrow = TRUE
)

var_names<-c("s1","s2","x1","x2","z","solution")
colnames(iniTab)<- var_names

TEST <- Simplex(iniTab,FALSE)
