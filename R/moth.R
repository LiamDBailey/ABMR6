#' R6 class representing an individual moth
#' @export
#' @examples
#' #Create a world in which the moth lives
#' example_world <- world$new()
#' 
#' #Create new moth object
#' example_moth <- moth$new(world = example_world)
#' 
#' #Moth has empty fitness initially
#' example_moth$fitness
#' 
#' #Compute fitness and then print it
#' example_moth$computefitness()
#' example_moth$fitness

moth <- R6::R6Class(classname = "moth",
  public = list(
    
    ## Attributes #######################
    
    #' @field colour Integer. Colour of the moth. Can be either 0 (white) or 1 (black)
    colour = NA,
    
    #' @field mutation_rate Probability. Probability of mutation in genes that control colour.
    mutation_rate = NA,
    
    #' @field world R6 object of class world. World in which moth lives.
    world = NA,
    
    #' @field fitness Numeric. Fitness of individual in current world. Ranges between 0 and 1.
    fitness = NA,
    
    ## Methods #######################
    
    #Always need a constructor (called 'initializer' in R6)
    
    #' @description
    #' Initialize new R6 object of class 'moth'. 
    #'
    #' @param mutation_rate Probability. Probability of mutation in genes that control colour.
    #' @param world R6 object of class world. World in which moth lives.
    #'
    #' @return An R6 object of class 'moth'

    initialize = function(mutation_rate = 1e-6, world) {
      
      self$mutation_rate <- mutation_rate
      self$world         <- world
      #Colour of moth is assigned randomly on initialization
      self$colour        <- round(runif(1, min = 0, max = 1))
      
    },
    
    #' @description
    #' Change moth colour
    #' 
    #' Moth colour will change with a probability defind by attribute
    #' `mutation_rate`
    
    mutation = function() {
      if (runif(n = 1) < self$mutation_rate) {
        self$colour <- abs(self$colour - 1)
      }
    },
    
    #' @description
    #' Compute fitness of individual in current environment.
    #' 
    #' Compare colour of moth to the colour of the world and update attribute
    #' `fitness` accordingly.
    computefitness = function() {
      self$fitness <- abs(self$world$colour + self$colour - 1)*0.5 + 0.5
    }
  )
)

# if (example) {
#   my_world <- World$new()
#   my_butterfly <- Butterfly$new(mutation_rate = 1e-1, world = my_world)
#   my_world$colour
#   my_butterfly$colour
#   my_butterfly$computefitness()
#   my_butterfly$fitness
#   my_world$colour <- 1
#   my_butterfly$computefitness()
#   my_butterfly$fitness
#   my_butterfly$colour <- 1
#   my_butterfly$computefitness()
#   my_butterfly$fitness
#   
#   my_world <- World$new(period = 10)
#   my_butterfly <- Butterfly$new(mutation_rate = 1e-1, world = my_world)
#   plot(NULL, ylim = c(0, 1), xlim = c(1, 100), xlab = "time", ylab = "fitness", las = 1)
#   for (t in 1:100) {
#     my_world$moveforward()
#     abline(v = t, col = my_world$colour, lwd = 5)
#     my_butterfly$mutation()
#     my_butterfly$computefitness()
#     points(t, my_butterfly$fitness, col = 1 - my_butterfly$colour, bg = my_butterfly$colour, pch = 21)
#   }
# }

