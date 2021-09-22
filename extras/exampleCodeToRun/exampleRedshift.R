################################################################################
# VARIABLES - please change
################################################################################
# The folder where the study intermediate and result files will be written:
outputFolder <- "D:/studyResults/Covid19VaccineAesiIncidenceRate"
databaseToRun <- 'truven_ccae'
minCellCount <- 5

############### Note this is a custom script, a version of CodeToRun.R that may not work for everyone ##############
############### Please use CodeToRun.R as it a more generic version  ###############################################
library(Covid19VaccineAesiIncidenceRate)
library(magrittr)

# create output directory if it doesnt exist
if (!dir.exists(outputFolder)) {
  dir.create(outputFolder, showWarnings = FALSE, recursive = TRUE)
}

################################################################################
# WORK
################################################################################

# the next set of lines shows how one site uses ROhdsiWebApi to get cdmSources configuration
# then parses it to collect the connectionSpecification information into
# one object called cdmSources
ROhdsiWebApi::authorizeWebApi(Sys.getenv('baseUrl'), "windows") # Windows authentication - if security enabled using windows authentication

cdmSources <- ROhdsiWebApi::getCdmSources(baseUrl = Sys.getenv('baseUrl')) %>%
  dplyr::mutate(baseUrl = Sys.getenv('baseUrl'),
                dbms = 'redshift',
                sourceDialect = 'redshift',
                port = 5439,
                version = .data$sourceKey %>% substr(., nchar(.) - 3, nchar(.)) %>% as.integer(),
                database = .data$sourceKey %>% substr(., 5, nchar(.) - 6)) %>%
  dplyr::group_by(.data$database) %>%
  dplyr::arrange(dplyr::desc(.data$version)) %>%
  dplyr::mutate(sequence = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(.data$database, .data$sequence) %>%
  dplyr::mutate(server = tolower(paste0(Sys.getenv("serverRoot"),"/", .data$database)))

# the cdm sources is then used to populate connectionSpecifications object
connectionSpecifications <- cdmSources %>%
  dplyr::filter(sequence == 1) %>%
  dplyr::filter(database == !!databaseToRun)

# Details for connecting to the server:
connectionDetails <-
  DatabaseConnector::createConnectionDetails(
    dbms = connectionSpecifications$dbms,
    server = connectionSpecifications$server,
    user = keyring::key_get(service = 'OHDSI_USER'),
    password =  keyring::key_get(service = 'OHDSI_PASSWORD'),
    port = connectionSpecifications$port
  )

# The name of the database schema where the CDM data can be found:
cdmDatabaseSchema <- connectionSpecifications$cdmDatabaseSchema

# The name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <-
  paste0("scratch_", keyring::key_get(service = 'OHDSI_USER'))
# cohortDatabaseSchema <-
#   paste0("scratch_rao_", connectionSpecifications$database)

# Some meta-information that will be used by the export function:
databaseId <- connectionSpecifications$database

# this function calls details of the data source.

getDataSourceInformation <-
  function(connectionDetails,
           cdmDatabaseSchema,
           vocabDatabaseSchema,
           databaseId) {
    sqlCdmDataSource <- "select * from @cdmDatabaseSchema.cdm_source;"
    sqlVocabularyVersion <-
      "select * from @vocabDatabaseSchema.vocabulary where vocabulary_id = 'None';"
    etlVersionNumber <-
      "select * from @cdmDatabaseSchema._version;"
    sourceInfo <- list(cdmSourceName = databaseId,
                       sourceDescription = databaseId)
    
    if (!is.null(connectionDetails)) {
      connection <-
        DatabaseConnector::connect(connectionDetails = connectionDetails)
    } else {
      return(NULL)
    }
    
    vocabulary <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sqlVocabularyVersion,
        vocabDatabaseSchema = vocabDatabaseSchema,
        snakeCaseToCamelCase = TRUE
      ) %>%
      dplyr::tibble() %>%
      dplyr::rename(vocabularyVersion = .data$vocabularyVersion)
    
    tryCatch(
      expr = {
        cdmDataSource <-
          DatabaseConnector::renderTranslateQuerySql(
            connection = connection,
            sql = sqlCdmDataSource,
            cdmDatabaseSchema = cdmDatabaseSchema,
            snakeCaseToCamelCase = TRUE
          )
        if (nrow(cdmDataSource) == 0) {
          warning("cdmDataSource table has no data")
          return(sourceInfo)
        } else {
          cdmDataSource <- cdmDataSource %>%
            dplyr::tibble() %>%
            dplyr::rename(vocabularyVersionCdm = .data$vocabularyVersion)
        }
        sourceInfo$sourceDescription <- ""
        if ("sourceDescription" %in% colnames(cdmDataSource)) {
          sourceInfo$sourceDescription <- paste0(sourceInfo$sourceDescription,
                                                 " ",
                                                 cdmDataSource$sourceDescription) %>%
            stringr::str_trim()
        }
        if ("cdmSourceName" %in% colnames(cdmDataSource)) {
          sourceInfo$cdmSourceName <- cdmDataSource$cdmSourceName
        } else {
          warning("cdmSourceName column not found in cdmDataSource table")
        }
        if ("cdmEtlReference" %in% colnames(cdmDataSource)) {
          sourceInfo$sourceDescription <- paste0(
            sourceInfo$sourceDescription,
            " ETL Reference: ",
            cdmDataSource$cdmEtlReference
          ) %>%
            stringr::str_trim()
        } else {
          warning("cdmEtlReference column not found in cdmDataSource table")
          sourceInfo$sourceDescription <-
            paste0(sourceInfo$sourceDescription,
                   " ETL Reference: None") %>%
            stringr::str_trim()
        }
        if ("cdmReleaseDate" %in% colnames(cdmDataSource)) {
          sourceInfo$sourceDescription <- paste0(
            sourceInfo$sourceDescription,
            " CDM release date: ",
            as.character(cdmDataSource$cdmReleaseDate)
          ) %>%
            stringr::str_trim()
        } else {
          warning("cdmReleaseDate column not found in cdmDataSource table")
          sourceInfo$sourceDescription <-
            paste0(sourceInfo$sourceDescription,
                   " CDM release date: None") %>%
            stringr::str_trim()
        }
        if ("sourceReleaseDate" %in% colnames(cdmDataSource)) {
          sourceInfo$sourceDescription <- paste0(
            sourceInfo$sourceDescription,
            " Source release date: ",
            as.character(cdmDataSource$sourceReleaseDate)
          ) %>%
            stringr::str_trim()
        } else {
          warning("sourceReleaseDate column not found in cdmDataSource table")
          sourceInfo$sourceDescription <-
            paste0(sourceInfo$sourceDescription,
                   " Source release date: None") %>%
            stringr::str_trim()
        }
        if ("sourceDocumentationReference" %in% colnames(cdmDataSource)) {
          sourceInfo$sourceDescription <- paste0(
            sourceInfo$sourceDescription,
            " Source Documentation Reference: ",
            as.character(cdmDataSource$sourceDocumentationReference)
          ) %>%
            stringr::str_trim()
        } else {
          warning("sourceDocumentationReference column not found in cdmDataSource table")
          sourceInfo$sourceDescription <-
            paste0(sourceInfo$sourceDescription,
                   " Source Documentation Reference: None") %>%
            stringr::str_trim()
        }
      },
      error = function(...) {
        return(sourceInfo)
      }
    )
    
    version <- dplyr::tibble()
    tryCatch(
      expr = {
        version <-
          DatabaseConnector::renderTranslateQuerySql(
            connection = connection,
            sql = etlVersionNumber,
            cdmDatabaseSchema = cdmDatabaseSchema,
            snakeCaseToCamelCase = TRUE
          ) %>%
          dplyr::tibble() %>%
          dplyr::mutate(rn = dplyr::row_number()) %>%
          dplyr::filter(.data$rn == 1) %>%
          dplyr::select(-.data$rn)
      },
      error = function(...) {
        warning("_version table not found")
        return(NULL)
      }
    )
    
    if (nrow(version) == 1) {
      if (all('versionId' %in% colnames(version),
              'versionDate' %in% colnames(version))) {
        cdmDataSource <- cdmDataSource %>%
          dplyr::mutate(
            cdmSourceAbbreviation = paste0(
              .data$cdmSourceAbbreviation,
              " (v",
              version$versionId,
              " ",
              as.character(version$versionDate),
              ")"
            )
          ) %>%
          dplyr::mutate(
            cdmSourceName = paste0(
              .data$cdmSourceName,
              " (v",
              version$versionId,
              " ",
              as.character(version$versionDate),
              ")"
            )
          ) %>%
          dplyr::mutate(
            sourceDescription = paste0(
              .data$sourceDescription,
              " (v",
              version$versionId,
              " ",
              as.character(version$versionDate),
              ")"
            )
          )
      }
    }
    
    DatabaseConnector::disconnect(connection = connection)
    return(if (nrow(cdmDataSource) > 0) {
      tidyr::crossing(cdmDataSource, vocabulary, version) %>%
        dplyr::mutate(databaseDescription =
                        .data$sourceDescription)
    } else {
      warning("cdmDataSource table has no data")
      vocabulary
    })
  }

dataSourceInformation <- getDataSourceInformation(connectionDetails = connectionDetails,
                                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                                  vocabDatabaseSchema = connectionSpecifications$vocabDatabaseSchema, 
                                                  databaseId = databaseId)

if (all(!is.null(dataSourceInformation),
        nrow(dataSourceInformation) == 1,
        nchar(dataSourceInformation$versionId) > 0)) {
  databaseId <- paste0(databaseId, "_v",dataSourceInformation$versionId, "_", as.character(dataSourceInformation$versionDate))
}

databaseName <- dataSourceInformation$cdmSourceName
databaseDescription <- dataSourceInformation$sourceDescription

Covid19VaccineAesiIncidenceRate::runStudy(connectionDetails = connectionDetails,
                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                          cohortDatabaseSchema = cohortDatabaseSchema,
                                          cohortTablePrefix = "ir",
                                          exportFolder = file.path(outputFolder, databaseId),
                                          databaseId = databaseId,
                                          databaseName = databaseName,
                                          databaseDescription = databaseDescription,
                                          incremental = TRUE,
                                          minCellCount = minCellCount)
