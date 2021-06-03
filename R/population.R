#' R6 class representing population of all moths in the world
#' @export
#' @examples
#'  set.seed(2L)
#'  my_world <- world$new()
#'  my_population <- population$new(N = 4, world = my_world)
#'  my_population$individuals[[1]]$colour <- 1
#'  my_population$computefitness()
#'  print(unlist(lapply(my_population$individuals, function(i) i$colour)))
#'  my_population$reproduction()
#'  print(unlist(lapply(my_population$individuals, function(i) i$colour)))

population <- R6::R6Class(classname = "population",
  public = list(
    
    ## Attributes #######################
    
    #' @field individuals List. List of R6 objects of class 'moth'.
    individuals = NA,

    ## Methods #######################
    
    #Always need a constructor (called 'initializer' in R6)
    
    #' @description
    #' Initialize new R6 object of class 'population'.
    #'
    #' @param N Integer. Number of individual moths in the population.
    #' @param mutation_rate Probability. Probability that moths can change colour in a timestep.
    #' @param world R6 object of class world. World in which moths live.
    #'
    #' @return An R6 object of class 'population'
    
    initialize = function(N = 100, mutation_rate = 1e-6, world) {
      self$individuals <- list()
      for (i in 1:N) {
        self$individuals[[i]] <- moth$new(mutation_rate, world)
      }
    },
    
    #' @description
    #' Compute fitness of all moths.
    computefitness = function() {
      for (i in 1:length(self$individuals)) {
        self$individuals[[i]]$computefitness()
      }
    },
    
    #' @description
    #' Change colour of all moths.
    mutation = function() {
      for (i in 1:length(self$individuals)) {
        self$individuals[[i]]$mutation()
      }
    },
    
    #' @description
    #' Moths reproduce with a probability defined by their fitness in the environment.
    reproduction = function() {
      N <- length(self$individuals)
      fitnesses <- unlist(lapply(self$individuals, function(ind) ind$fitness))
      id_reproductor <- sample(1:N, size = N, prob = fitnesses, replace = TRUE)
      
      method <- 2
      if (method == 1) {
        ## Easy
        parents <- self$individuals
        for (i in 1:N) {
          self$individuals[[i]] <- parents[[id_reproductor[i]]]$clone(deep = FALSE)
        }
      }
      
      if (method == 2) {
        ## Optimised: we do not touch the one that make one offspring
        whodies <- setdiff(1:N, id_reproductor)
        howmanyoffspring <- table(id_reproductor) 
        whomultiparent <- as.numeric(names(howmanyoffspring)[howmanyoffspring > 1])
        whereoffspring <- rep(whomultiparent, howmanyoffspring[howmanyoffspring > 1] - 1)
        for (i in 1:length(whodies)) {
          self$individuals[[whodies[i]]] <- self$individuals[[whereoffspring[i]]]$clone(deep = FALSE)
        }
      }
    },
    
    #' @description
    #' Run all demographic methods.
    #' 
    #' Compute fitness, mutation and reproduction for all moths.
    generation = function(){
      self$computefitness()
      self$mutation()
      self$reproduction()
    }
  )
)

# if (example) {
#   set.seed(2L)
#   my_world <- World$new()
#   my_population <- Population$new(N = 4, world = my_world)
#   my_population$individuals[[1]]$colour <- 1
#   my_population$computefitness()
#   print(unlist(lapply(my_population$individuals, function(i) i$colour)))
#   my_population$reproduction()
#   print(unlist(lapply(my_population$individuals, function(i) i$colour)))
# }