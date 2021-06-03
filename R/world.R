#' R6 class representing the world in which the moths live.
#' @export
#' @examples
#' my_world <- world$new(period = 50)
#' 
#' 
#' plot(NULL, ylim = c(0, 1), xlim = c(1, 1000), xlab = "time", ylab = "", axes = FALSE)
#' axis(1)
#' 
#' #Move the world forward 1000 time steps
#' #World will change colour every 50
#' for (t in 1:1000) {
#'   my_world$moveforward()
#'   abline(v = t, col = my_world$colour, lwd = 5)
#' }

world <- R6::R6Class(classname = "world",
                 public = list(
                   ## Attributes #######################
                   
                   #' @field time Integer. Number of time steps that have passed in the world.
                   time = 1,
                   
                   #' @field colour Integer. Colour of the world. Can be either 0 (white) or 1 (black).
                   colour = 0,
                   
                   #' @field period Integer. Number of time steps after which colour of world changes.
                   period = NA,
                   
                   ## Methods #######################
                   
                   #Always need a constructor (called 'initializer' in R6)
                   
                   #' @description
                   #' Initialize new R6 object of class 'world'. 
                   #'
                   #' @param period Integer. Number of time steps after which colour of world changes.
                   #'
                   #' @return An R6 object of class 'world'
                   initialize = function(period = 100){
                     self$period <- period
                   },
                   
                   #' @description
                   #' Change colour of the world.
                   #' 
                   #' World will change colour after a set period.
                   
                   computecolour = function() {
                     relative_time <- self$time %% (2*self$period) ## check that, it seems to lead to small initial period sometimes
                     if (self$period == Inf) relative_time <- self$time
                     self$colour <- as.numeric(relative_time < self$period)
                   },
                   
                   #' @description
                   #' Advance time by 1.
                   #'
                   #' Time will move forward by 1, after which we check to see
                   #' if the colour of the world will change.
                   moveforward = function(){ 
                     self$time <- self$time + 1
                     self$computecolour()  ## one function can call another
                   }
                 )
)

# if (example) {
  # my_world <- World$new(period = 50)
  # print(my_world)
  # 
  # plot(NULL, ylim = c(0, 1), xlim = c(1, 1000), xlab = "time", ylab = "", axes = FALSE)
  # axis(1)
  # for (t in 1:1000) {
  #   my_world$moveforward()
  #   abline(v = t, col = my_world$colour, lwd = 5)
  # }
  # print(my_world)
# }
