SonarqubeCredentials<-R6::R6Class("SonarqubeCredentials",
                    inherit = NymbulCredentials,
                    private=list(
                      authHeaderName = NULL,
                      authPublicKey = NULL,
                      authPrivateKey = NULL,
                      authPassPhrase = NULL),
                      
                    public = list(
                    initialize = function(authUsername = NA,
                                          authPassword = NA,
                                          authToken = NA){
                      
                      private$authUsername <- authUsername
                      private$authPassword <- authPassword
                      private$authToken <- authToken
                      private$tool<-"SONARQUBE"
                      
                      
                    }
                    
          
              )
                    
)