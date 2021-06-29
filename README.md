Calculating the background rates of adverse events of special interest (AESI) for the COVID vaccines
=============

<img src="https://img.shields.io/badge/Study%20Status-Repo%20Created-lightgray.svg" alt="Study Status: Design Finalized">

- Analytics use case(s): Characterization
- Study type: Clinical Application
- Tags: COVID-19
- Study lead: OHDSI Covid Vaccine Safety Workgroup
- Study lead forums tag: **[[Lead tag]](https://teams.microsoft.com/l/team/19%3a327b4d1d7cf54151b4f8e474f7a4292c%40thread.tacv2/conversations?groupId=d0a6ad98-f9dd-4137-9fc2-b483b0f31952&tenantId=a30f0094-9120-4aab-ba4c-e5509023b2d5)**
- Study start date: **June 16th 2021**
- Study end date: **July 30th 2021**
- Protocol: **[[Vaccine_safety_sensitivity_protocol_BEST_v2]](https://github.com/ohdsi-studies/Covid19VaccineAesiIncidenceRate/blob/master/extras/Protocol/Vaccine_safety_sensitivity_protocol_BEST_v2.1_06292021.docx)**
- Publications: **-**
- Results explorer: **-**

Requirements
============

- A database in [Common Data Model version 5](https://github.com/OHDSI/CommonDataModel) in one of these platforms: SQL Server, Oracle, PostgreSQL, IBM Netezza, Apache Impala, Amazon RedShift, Google BigQuery, or Microsoft APS.
- R version 4.0.0 or newer
- On Windows: [RTools](http://cran.r-project.org/bin/windows/Rtools/)
- [Java](http://java.com)
- 25 GB of free disk space

How to run
==========
1. Follow [these instructions](https://ohdsi.github.io/Hades/rSetup.html) for setting up your R environment, including RTools and Java. 

2. Open your study package in RStudio. Use the following code to install all the dependencies:

	```r
	renv::restore()
	```

3. In RStudio, select 'Build' then 'Install and Restart' to build the package.

3. Once installed, you can execute the study by modifying and using the code below. For your convenience, this code is also provided under `extras/CodeToRun.R`:

	```r
	library(Covid19VaccineAesiIncidenceRate)
	
	# Optional: specify where the temporary files (used by the Andromeda package) will be created:
	options(andromedaTempFolder = "s:/andromedaTemp")
	
	# Maximum number of cores to be used:
	maxCores <- parallel::detectCores()
	
	# The folder where the study intermediate and result files will be written:
	outputFolder <- "c:/Covid19VaccineAesiIncidenceRate"
	
	# Details for connecting to the server:
	# See ?DatabaseConnector::createConnectionDetails for help
	connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
									server = "some.server.com/ohdsi",
									user = "joe",
									password = "secret")
	
	# The name of the database schema where the CDM data can be found:
	cdmDatabaseSchema <- "cdm_synpuf"
	
	# The name of the database schema and table where the study-specific cohorts will be instantiated:
	cohortDatabaseSchema <- "scratch.dbo"
	cohortTable <- "my_study_cohorts"
	
	# Some meta-information that will be used by the export function:
	databaseId <- "Synpuf"
	databaseName <- "Medicare Claims Synthetic Public Use Files (SynPUFs)"
	databaseDescription <- "Medicare Claims Synthetic Public Use Files (SynPUFs) were created to allow interested parties to gain familiarity using Medicare claims data while protecting beneficiary privacy. These files are intended to promote development of software and applications that utilize files in this format, train researchers on the use and complexities of Centers for Medicare and Medicaid Services (CMS) claims, and support safe data mining innovations. The SynPUFs were created by combining randomized information from multiple unique beneficiaries and changing variable values. This randomization and combining of beneficiary information ensures privacy of health information."
	
	# For some database platforms (e.g. Oracle): define a schema that can be used to emulate temp tables:
	options(sqlRenderTempEmulationSchema = NULL)
	
	Covid19VaccineAesiIncidenceRate::runStudy(connectionDetails = connectionDetails,
											  cdmDatabaseSchema = cdmDatabaseSchema,
												cohortDatabaseSchema = cohortDatabaseSchema,
												cohortTablePrefix = "ir",
												exportFolder = outputFolder,
												databaseId = databaseId,
												databaseName = databaseName,
												databaseDescription = databaseDescription,
												incremental = TRUE)
	```

4. Upload the file ```export/Results_<DatabaseId>.zip``` in the output folder to the study coordinator:

	```r
	uploadResults(outputFolder, privateKeyFileName = "<file>", userName = "<name>")
	```
	
	Where ```<file>``` and ```<name<``` are the credentials provided to you personally by the study coordinator.
		

License
=======
The Covid19VaccineAesiIncidenceRate package is licensed under Apache License 2.0

Development
===========
Covid19VaccineAesiIncidenceRate was developed in ATLAS and R Studio.

### Development status

Unknown