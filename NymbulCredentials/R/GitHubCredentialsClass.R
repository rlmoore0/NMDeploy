GitHubCredentials<-R6::R6Class("GitHubCredentials",
                         inherit = NymbulCredentials,
                         private = list(authPrivateKey = NULL,
                                        authPublicKey = NULL,
                                        authPassPhrase = NULL),

                         public = list(
                         initialize = function(authUsername = NA,
                                               authPassword = NA,
                                               authToken = NA,
                                               privateKey = NA,
                                               publicKey = NA,
                                               privateKeyPassPhrase = NA){

                           private$authUsername <- authUsername
                           private$authPassword <- authPassword
                           private$authToken <- authToken
                           private$authPrivateKey <- privateKey
                           private$authPublicKey <- publicKey
                           private$authPassPhrase <- privateKeyPassPhrase
                           private$tool<-"GITHUB"

                         },
                         SetPublicKey = function(value){


                           keyring::key_set_with_value(paste0(private$tool,"_PUBLIC_KEY"),
                                                       username = private$authUsername,
                                                       password = value)

                         },
                         GetPublicKey = function(){

                           keyring::key_get(paste0(private$tool,"_PUBLIC_KEY"),
                                            username = private$authUsername)


                         },
                         SetPrivateKey = function(value){


                           keyring::key_set_with_value(paste0(private$tool,"_PRIVATE_KEY"),
                                                       username = private$authUsername,
                                                       password = value)

                         },
                         GetPrivateKey = function(){

                           keyring::key_get(paste0(private$tool,"_PRIVATE_KEY"),
                                            username = private$authUsername)


                         },
                         SetPassPhrase = function(value){

                           keyring::key_set_with_value(paste0(private$tool,"_PRIVATE_KEY_PASSPHRASE"),
                                                       username = private$authUsername,
                                                       password = value)

                         },
                         GetPassPhrase = function(){

                           keyring::key_get(paste0(private$tool,"_PRIVATE_KEY_PASSPHRASE"),
                                            username = private$authUsername)

                         },
                         CheckPrivateKey = function(){

                           dplyr::filter(keyring::key_list(), service==paste0(private$tool, "_PRIVATE_KEY"),
                                  username==private$authUsername)%>%nrow()->keyringFromList

                           isPrivateKeyInKeyring<-isTRUE(keyringFromList>0)

                           return(isPrivateKeyInKeyring)

                         },
                         CheckPublicKey = function(){

                           dplyr::filter(keyring::key_list(), service==paste0(private$tool, "_PUBLIC_KEY"),
                                  username==private$authUsername)%>%nrow()->keyringFromList

                           isPublicKeyInKeyring<-isTRUE(keyringFromList>0)

                           return(isPublicKeyInKeyring)

                         },
                         CheckPassPhrase = function(){

                           dplyr::filter(keyring::key_list(), service==paste0(private$tool, "_PRIVATE_KEY_PASSPHRASE"),
                                         username==private$authUsername)%>%nrow()->keyringFromList

                           isPassPhraseInKeyring<-isTRUE(keyringFromList>0)

                           return(isPassPhraseInKeyring)

                         },
                         DeletePublicKey = function(){

                           keyring::key_delete(paste0(private$tool,"_PUBLIC_KEY"),
                                               username = private$authUsername)

                         },
                         DeletePrivateKey = function(){

                           keyring::key_delete(paste0(private$tool,"_PRIVATE_KEY"),
                                               username = private$authUsername)

                         }

            )

)
