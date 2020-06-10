VersionOneCredentials<-R6::R6Class("VersionOneCredentials",
                             inherit = NymbulCredentials,
                             private = list(authToken = NULL),

                             public = list(
                               initialize = function(authUsername = NA,
                                                     authPassword = NA,
                                                     authToken = NA){

                                 private$authUsername <- authUsername
                                 private$authPassword <- authPassword
                                 private$authToken <- authToken
                                 private$tool<-"VERSION_ONE"

                               }

                             )

)
