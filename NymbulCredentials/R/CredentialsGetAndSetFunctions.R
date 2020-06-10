#' Fucntion to set authentication parameters to NymbulCredentials object
#'
#' @param toolName Name of tool in use from Jira, GitHub, SonarQube, VersionOne, GitLab
#' @param configParameters List of configuration parameters taken from Rcmdr Nymbul Mineral interface
#'
#' @return Does not return anything since we are intersted only in its side effects
#' @export
#'
SetAuthenticationParametersToKeyring<-function(toolName, configParameters){

  checkmate::assertChoice(toolName, c("Jira", "GitHub", "SonarQube", "VersionOne", "GitLab"))
  checkmate::assertList(configParameters)

  # stopifnot(!stringi::stri_isempty(configParameters[[1]]$authUsername),
  #           "There is no username in the configuration file, please input an username")

  if(toolName == "Jira"){

    jiraCredentials <- JiraCredentials$new(authUsername = configParameters[[1]]$authUsername)

    if(!stringi::stri_isempty(configParameters[[1]]$authPassword)){

      jiraCredentials$SetPassword(value = configParameters[[1]]$authPassword)

    }

    if(!stringi::stri_isempty(configParameters[[1]]$authToken)){

      jiraCredentials$SetToken(value = configParameters[[1]]$authToken)

    }


  } else if(toolName == "GitHub"){


    gitHubCredentials <- GitHubCredentials$new(authUsername = configParameters[[1]]$authUsername)

    if(!stringi::stri_isempty(configParameters[[1]]$authPassword)){

      gitHubCredentials$SetPassword(value = configParameters[[1]]$authPassword)

    }

    if(!stringi::stri_isempty(configParameters[[1]]$authToken)){

      gitHubCredentials$SetToken(value = configParameters[[1]]$authToken)

    }

    if(!stringi::stri_isempty(configParameters[[1]]$authPublicKey)){

      gitHubCredentials$SetPublicKey(value = configParameters[[1]]$authPublicKey)

    }

    if(!stringi::stri_isempty(configParameters[[1]]$authPrivateKey)){

      gitHubCredentials$SetPrivateKey(value = configParameters[[1]]$authPrivateKey)

    }

    if(!stringi::stri_isempty(configParameters[[1]]$authPassPhrase)){

      gitHubCredentials$SetPassPhrase(value = configParameters[[1]]$authPassPhrase)

    }

  } else if(toolName == "SonarQube"){


    sonarqubeCredentials <- SonarqubeCredentials$new(authUsername = configParameters[[1]]$authUsername)

    if(!stringi::stri_isempty(configParameters[[1]]$authPassword)){

      sonarqubeCredentials$SetPassword(value = configParameters[[1]]$authPassword)

    }

    if(!stringi::stri_isempty(configParameters[[1]]$authToken)){

      sonarqubeCredentials$SetToken(value = configParameters[[1]]$authToken)

    }
  } else if(toolName == "VersionOne"){

    versionOneCredentials <- VersionOneCredentials$new(authUsername = configParameters[[1]]$authUsername)

    if(!stringi::stri_isempty(configParameters[[1]]$authPassword)){

      versionOneCredentials$SetPassword(value = configParameters[[1]]$authPassword)

    }

    if(!stringi::stri_isempty(configParameters[[1]]$authToken)){

      versionOneCredentials$SetToken(value = configParameters[[1]]$authToken)

    }

  } else if(toolName == "GitLab"){

    gitLabCredentials <- GitLabCredentials$new(authUsername = configParameters[[1]]$authUsername)

    if(!stringi::stri_isempty(configParameters[[1]]$authPassword)){

      gitLabCredentials$SetPassword(value = configParameters[[1]]$authPassword)

    }

    if(!stringi::stri_isempty(configParameters[[1]]$authToken)){

      gitLabCredentials$SetToken(value = configParameters[[1]]$authToken)

    }

    if(!stringi::stri_isempty(configParameters[[1]]$authPublicKey)){

      gitLabCredentials$SetPublicKey(value = configParameters[[1]]$authPublicKey)

    }

    if(!stringi::stri_isempty(configParameters[[1]]$authPrivateKey)){

      gitLabCredentials$SetPrivateKey(value = configParameters[[1]]$authPrivateKey)

    }

    if(!stringi::stri_isempty(configParameters[[1]]$authPassPhrase)){

      gitLabCredentials$SetPassPhrase(value = configParameters[[1]]$authPassPhrase)

    }

  }

}

#' Functions to get credentials from a NymbulCredentials object
#'
#' @param toolName Name of tool in use from Jira, GitHub, SonarQube, VersionOne, GitLab
#' @param configList List from configuration file
#' @param calledFromInterface Logical value indicating if the function is called within the
#'                            Rcmdr Nymbul Mineral interface
#'
#' @return List of credentials
#' @export
#'
GetAuthenticationParametersFromKeyring<-function(toolName,
                                                 configList,
                                                 calledFromInterface=TRUE){

  checkmate::assertChoice(toolName,
                          c("Jira",
                            "GitHub",
                            "SonarQube",
                            "VersionOne",
                            "GitLab"))

  checkmate::assertList(configList)
  checkmate::assertLogical(calledFromInterface)

  if(isTRUE(calledFromInterface)){

  credentialList<-list("authUsername" = configList[[1]]$authUsername,
                       "authPassword" = "",
                       "authToken" = "",
                       "authPrivateKey" = "",
                       "authPublicKey" = "",
                       "authPassPhrase" = "")

  } else {

    credentialList<-list("authUsername" = configList$authUsername,
                         "authPassword" = "",
                         "authToken" = "",
                         "authPrivateKey" = "",
                         "authPublicKey" = "",
                         "authPassPhrase" = "")

  }


  if(toolName == "Jira"){

    jiraCredentials <- JiraCredentials$new(authUsername = credentialList$authUsername)

    if(jiraCredentials$CheckPassword()){

      credentialList$authPassword <- jiraCredentials$GetPassword()

    }


    if(jiraCredentials$CheckToken()){

      credentialList$authToken <- jiraCredentials$GetToken()

    }


  } else if(toolName == "GitHub"){


    gitHubCredentials <- GitHubCredentials$new(authUsername = credentialList$authUsername)

    if(gitHubCredentials$CheckPassword()){

      credentialList$authPassword <- gitHubCredentials$GetPassword()

    }


    if(gitHubCredentials$CheckToken()){

      credentialList$authToken <- gitHubCredentials$GetToken()

    }

    if(gitHubCredentials$CheckPublicKey()){

      credentialList$authPublicKey <- gitHubCredentials$GetPublicKey()

    }

    if(gitHubCredentials$CheckPrivateKey()){

      credentialList$authPrivateKey <- gitHubCredentials$GetPrivateKey()

    }

    if(gitHubCredentials$CheckPassPhrase()){

      credentialList$authPassPhrase <- gitHubCredentials$GetPassPhrase()

    }


  } else if(toolName == "SonarQube"){


    sonarqubeCredentials <- SonarqubeCredentials$new(authUsername = credentialList$authUsername)

    if(sonarqubeCredentials$CheckPassword()){

      credentialList$authPassword <- sonarqubeCredentials$GetPassword()

    }

    if(sonarqubeCredentials$CheckToken()){

      credentialList$authToken <- sonarqubeCredentials$GetToken()

    }

  } else if(toolName == "VersionOne"){


    versionOneCredentials <- VersionOneCredentials$new(authUsername = credentialList$authUsername)

    if(versionOneCredentials$CheckPassword()){

      credentialList$authPassword <- versionOneCredentials$GetPassword()

    }


    if(versionOneCredentials$CheckToken()){

      credentialList$authToken <- versionOneCredentials$GetToken()

    }

  } else if(toolName == "GitLab"){
     #have to check if GitLabsAPI was substituted by GitAPI

    gitLabCredentials <- GitLabCredentials$new(authUsername = credentialList$authUsername)


    if(gitLabCredentials$CheckPassword()){

      credentialList$authPassword <- gitLabCredentials$GetPassword()

    }


    if(gitLabCredentials$CheckToken()){

      credentialList$authToken <- gitLabCredentials$GetToken()

    }

    if(gitLabCredentials$CheckPublicKey()){

      credentialList$authPublicKey <- gitLabCredentials$GetPublicKey()

    }

    if(gitLabCredentials$CheckPrivateKey()){

      credentialList$authPrivateKey <- gitLabCredentials$GetPrivateKey()

    }

    if(gitLabCredentials$CheckPassPhrase()){

      credentialList$authPassPhrase <- gitLabCredentials$GetPassPhrase()

    }

  }

  return(credentialList)

}

#' Removes Credential Info from a Configuration variable
#'
#' @param config Configuration object
#'
#' @return Configuration object without
#' @export
#'
RemoveCredentialsFromConfig<-function(config){

  tryCatch({

    configNoCredentials<-rlist::list.remove(config,
                                          c("authPassword",
                                            "authToken",
                                            "authPrivateKey",
                                            "authPublicKey",
                                            "authPassPhrase"))

    retValue<-configNoCredentials

  }, error = function(err){

    if ("package:Rcmdr" %in% search()) {
      Rcmdr::RcmdrTkmessageBox(
        err,
        icon = c("error"),
        type = c("ok"),
        title = "Configuration error")
      retValue <- FALSE
    } else {
      print(err)
      retValue <- FALSE
    }

  })

  return(retValue)

}
