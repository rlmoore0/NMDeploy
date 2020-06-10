NymbulCredentials<-R6::R6Class("NymbulCredentials",

  private = list(
    tool = NULL,
    # authCaPath = NULL, #cert aut,hority bundle path
    # authSSLCert = NULL,
    # authCertType = NULL,
    # authCaInfo = NULL, #peer certificate blundle file
    # authPassPhrase = NULL,
    authUsername = NULL,
    authPassword = NULL,
    authToken = NULL),

  public = list(
    initialize = function(tool = NA,
                          # authPassPhrase =NA,
                          # authCaPath = NA, #cert authority bundle path
                          # authSSLCert = NA,
                          # authCertType = NA,
                          # authCaInfo = NA, #peer certificate blundle file
                          authUsername = NA,
                          authPassword = NA,
                          authToken = NA){

      private$tool <- tool
      # private$authPassPhrase <- authPassPhrase
      # private$authCaPath <- authCaPath #cert aut,hority bundle path
      # private$authSSLCert <- authSSLCert
      # private$authCertType <- authCertType
      # private$authCaInfo <- authCaInfo #peer certificate blundle file
      private$authUsername <- authUsername
      private$authPassword <- authPassword
      private$authToken <- authToken

    },
    SetUsername = function(value){

      private$authUsername<-value

      invisible(self)

      # keyring::key_set_with_value(paste0(private$tool,"_PASSWORD"),
      #                             username = private$authUsername,
      #                             password = value)
    },
    SetPassword = function(value){


      keyring::key_set_with_value(paste0(private$tool,"_PASSWORD"),
                                  username = private$authUsername,
                                  password = value)


    },
    SetToken = function(value){


      keyring::key_set_with_value(paste0(private$tool,"_PERSONAL_TOKEN"),
                                  username = private$authUsername,
                                  password = value)

    },
    GetPassword = function(){

      keyring::key_get(paste0(private$tool,"_PASSWORD"),
                       username = private$authUsername)


    },
    GetToken = function(){

      keyring::key_get(paste0(private$tool,"_PERSONAL_TOKEN"),
                       username = private$authUsername)

    },
    CheckPassword = function(){

      dplyr::filter(keyring::key_list(), service==paste0(private$tool, "_PASSWORD"),
             username==private$authUsername)%>%
        nrow()->keyringFromList

      isPasswordInKeyring<-isTRUE(keyringFromList>0)


      return(isPasswordInKeyring)

    },
    CheckToken = function(){


      dplyr::filter(keyring::key_list(), service==paste0(private$tool, "_PERSONAL_TOKEN"),
             username==private$authUsername)%>%nrow()->keyringFromList

      isTokenInKeyring<-isTRUE(keyringFromList>0)


      return(isTokenInKeyring)

    },
    DeletePassword = function(){

      keyring::key_delete(paste0(private$tool,"_PASSWORD"),
                          username = private$authUsername)


    },
    DeleteToken = function(){

      keyring::key_delete(paste0(private$tool,"_PERSONAL_TOKEN"),
                       username = private$authUsername)


    }

  )
)




