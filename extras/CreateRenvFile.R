OhdsiRTools::createRenvLockFile(rootPackage = "Covid19VaccineAesiIncidenceRate",
                                additionalRequiredPackages = c('keyring', "DatabaseConnector","dplyr","lubridate","purrr","ROhdsiWebApi",
                                                               "SqlRender","tidyr"))
renv::activate()
