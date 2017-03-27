# Upload data packages for Space Time heterogeneity community dynamics

library(datapack)
library(EML)
library(rmarkdown)
library(stringr)
library(dataone)
library(uuid)

# Set up repository access
cn <- CNode("DEV2")
mn <- getMNode(cn, "urn:node:mnDevUCSB1")
d1c <- new("D1Client", cn=cn, mn=mn)


# leave as example, everything from the sev-comm-dyn metadata generation
# process_dp1 <- function() {
# 
#     # Create a DataPackage to hold all of the objects
#     dp <- new("DataPackage")
#     dataDir <- getwd()
#     eml_file <- sprintf("%s/dp1-metadata.xml", dataDir)
#     title <- "Sevilleta LTER Metstation number 49, precipition, daily raw and gap filled from 1992 - 2015"
#     pubDate <- "2017"
#     file_ext <- "dp1"
#     keywords <- c("meteorology", "precipitation")
# 
#     eml <- create_eml(title, pubDate, file_ext, keywords, dataDir)
#     eml <- add_people_eml(eml, file_ext)
# 
#     # Create a DataObject to hold the script file and add it to the EML file
#     file_name <- "Met_gap_fill.R"
#     file_description <- "R script that fills in data gaps in meteorlogical data."
#     file_path <- sprintf("%s/%s", dataDir, file_name)
#     progObj <- new("DataObject", format="application/R", filename=file_path,
#                    mediaType="text/x-rsrc", suggestedFilename=file_name)
#     eml <- add_entity_eml_new(eml, file_name, file_description, file_path, progObj@sysmeta@identifier, cn@endpoint)
# 
#     # Document and add the first data file to the package
#     file_name <- "Met_all_excel.csv"
#     file_description <- "Sevilleta Meteorological data"
#     file_path <- sprintf("%s/%s", dataDir, file_name)
#     do1 <- new("DataObject", format="text/csv", filename=file_path,
#                    mediaType="text/csv", suggestedFilename=file_name)
#     eml <- add_entity_eml_new(eml, file_name, file_description, file_path, do1@sysmeta@identifier, cn@endpoint)
# 
#     # Document and add the derived data file to the package
#     file_name <- "Met_all.csv"
#     file_description <- "Sevilleta Meteorological data, with gaps filled"
#     file_path <- sprintf("%s/%s", dataDir, file_name)
#     do2 <- new("DataObject", format="text/csv", filename=file_path,
#                mediaType="text/csv", suggestedFilename=file_name)
#     eml <- add_entity_eml_new(eml, file_name, file_description, file_path, do2@sysmeta@identifier, cn@endpoint)
#     # Save this DataObject identifier, so that dp3 can read it
#     writeLines(getIdentifier(do2), file.path(tempdir, "Met_all.csv.identifier"))
# 
#     # Set the package identifier
#     eml_id <- paste0("urn:uuid:", uuid::UUIDgenerate())
#     eml@packageId <- new("xml_attribute", eml_id)
#     eml@system <- new("xml_attribute", "knb")
# 
#     # Validate the eml and write it to disk
#     eml_validate(eml)
#     write_eml(eml, eml_file)
# 
#     # add the data objects and EML to the DataPackage
#     eml_object <- new("DataObject", format="eml://ecoinformatics.org/eml-2.1.1", filename=eml_file,
#                       mediaType="text/xml", suggestedFilename=basename(eml_file))
#     dp <- addData(dp, do1, mo=eml_object)
#     dp <- addData(dp, progObj, mo=eml_object)
#     dp <- addData(dp, do2, mo=eml_object)
# 
#     # Add the DataONE PIDs for the Servilleta source meteorological data sets.
#     # The package has to be uploaded to the DataONE production  environment for these
#     # PIDs to be locateable for the provenance icons in metacatui.
#     # Note: Use these PIDs instead of 'sev1.file1', etc, when uploading to the production DataONE env.
#     #sources <- c("https://pasta.lternet.edu/package/data/eml/knb-lter-sev/1/13/6fd544ecb52935cbca5c891f2546defa",
#     #             "https://pasta.lternet.edu/package/data/eml/knb-lter-sev/1/13/e7345d4d4bf781ae59f073aa50431d9d",
#     #             "https://pasta.lternet.edu/package/data/eml/knb-lter-sev/1/13/65d65d6f9e62ca17123c90192ab5c43d",
#     #             "https://pasta.lternet.edu/package/data/eml/knb-lter-sev/1/13/83a08679fb84f7c15c3a94a3680f361d",
#     #             "https://pasta.lternet.edu/package/data/eml/knb-lter-sev/1/13/d2813a036a980b9cb99d7ab58671659e")
# 
#     # Add provenance information about the derived objects
#     dp <- insertDerivation(dp,
#                            sources=list("sev.1.file1", "sev.1.file2", "sev.1.file3", "sev.1.file4", "sev.1.file5"),
#                            derivations=list(do1@sysmeta@identifier))
#     dp <- insertDerivation(dp, do1, progObj, do2)
# 
#     # Upload package to the repository
#     resourceMapId <- uploadDataPackage(d1c, dp, replicate=FALSE, public=TRUE, quiet=F, resolveURI=paste0(d1c@cn@endpoint, "/resolve"))
#     message("    EML ID: ", eml_object@sysmeta@identifier)
#     message("Package ID: ", resourceMapId)
# }
# 
# process_dp2 <- function(){
# 
#     # Create a DataPackage to hold all of the objects
#     dp <- new("DataPackage")
#     dataDir <- getwd()
#     eml_file <- sprintf("%s/dp2-metadata.xml", dataDir)
#     title <- "Sevilleta species list with life form designations"
#     pubDate <- "2017"
#     file_ext <- "dp2"
#     keywords <- c("species names", "taxonomy", "life forms", "metabolism")
# 
#     eml <- create_eml(title, pubDate, file_ext, keywords, dataDir)
#     eml <- add_people_eml(eml, file_ext)
# 
#     # Document and add the first data file to the package
#     file_name <- "Sevilleta_species_list.csv"
#     file_description <- "Sevilleta species list with life form designations"
#     file_path <- sprintf("%s/%s", dataDir, file_name)
#     do1 <- new("DataObject", format="text/csv", filename=file_path,
#                mediaType="text/csv", suggestedFilename=file_name)
#     eml <- add_entity_eml_new(eml, file_name, file_description, file_path, do1@sysmeta@identifier, cn@endpoint)
# 
#     # Set the package identifier
#     eml_id <- paste0("urn:uuid:", uuid::UUIDgenerate())
#     eml@packageId <- new("xml_attribute", eml_id)
#     eml@system <- new("xml_attribute", "knb")
# 
#     # Validate the eml and write it to disk
#     eml_validate(eml)
#     write_eml(eml, eml_file)
# 
# 
# }
# 
# 
# 
# process_dp3 <- function() {
# 
#     # Create a DataPackage to hold all of the objects
#     dp <- new("DataPackage")
#     sources <- list()
#     derivations <- list()
#     dataDir <- getwd()
#     eml_file <- sprintf("%s/dp3-metadata.xml", dataDir)
#     title <- "Data analysis and graphs for publication"
#     pubDate <- "2017"
#     file_ext <- "dp3"
#     keywords <- c("species names", "taxonomy", "life forms", "metabolism")
# 
#     eml <- create_eml(title, pubDate, file_ext, keywords, dataDir)
#     eml <- add_people_eml(eml, file_ext)
# 
#     # Add input files to the package
#     # filename read by veg_analysis.R: sev182_nppcorewebbiomass_20150816.csv
#     # Corresponding DataONE filename: sev182_anpp_20150814.txt
#     # Don't create entries in the EML for this input files as it is external to the current package and
#     # will serve to link this package to the source package that includes/produced the file.
#     sources[[length(sources)+1]] <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sev/182/244946/73ad386bfbcfcd218631e495ae571ddb"
# 
#     # filename read by veg_analysis.R: sev129_nppcorequadrat_20161214.csv
#     # Corresponsing DataONE filename: 	sev129_nppcorequadrat_20160718.csv
#     sources[[length(sources)+1]] <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sev/129/258320/458251ae0484fc154a5140c294b5598c"
# 
#     # Sevilleta species list
#     sources[[length(sources)+1]] <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sev/51/186795/c03899b16eb743ff8d05eebc1f259ea3"
# 
#     # Sevilleta rodent trap data
#     # filename read by veg_analysis.R: rodentdata2016_rodentdataall.csv, rodentdata2016_weightedaverages.csv
#     # Corresponding DataONE filename: sev008_rodentpopns_20160701.csv
#     sources[[length(source)+1]] <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sev/8/297976/d70c7027949ca1d8ae053eb10300dc0e"
# 
#     # Met_all.csv, producted by first workflow step
#     # get the DataONE PID for this dataset, after it is uploaded
#     idFile <- file.path(tempdir(), "Met_all.csv.identifier")
#     if(file.exists(idFile)) {
#         metPid <- readLines(idFile)
#         sources[[length(sources)+1]] <- metPid
#     } else {
#         stop("Unable to determine DataONE PID of file \"Met_all.csv\" from package dp1")
#     }
# 
#     # Document and add the first file to the package
#     file_name <- "MSDS.jpg"
#     file_description <- "Ordination of mice feeding guilds at two sites"
#     file_path <- sprintf("%s/%s", dataDir, file_name)
#     do1 <- new("DataObject", format="image/jpg", filename=file_path,
#                mediaType="image/jpg", suggestedFilename=file_name)
#     derivations[[length(derivations)+1]] <- do1
#     eml <- add_entity_eml_new(eml, file_name, file_description, file_path, do1@sysmeta@identifier, cn@endpoint)
# 
#     # Document and add the second file to the package
#     file_name <- "Noble_BlackGramaCore.png"
#     file_description <- "Precipitation, biomass, NPP, and rodent abundance at black grama site"
#     file_path <- sprintf("%s/%s", dataDir, file_name)
#     do1 <- new("DataObject", format="image/png", filename=file_path,
#                mediaType="image/png", suggestedFilename=file_name)
#     derivations[[length(derivations)+1]] <- do1
#     eml <- add_entity_eml_new(eml, file_name, file_description, file_path, do1@sysmeta@identifier, cn@endpoint)
# 
#     # Document and add the third file to the package
#     file_name <- "Noble_CreosoteCore.png"
#     file_description <- "Precipitation, biomass, NPP, and rodent abundance at creosote site"
#     file_path <- sprintf("%s/%s", dataDir, file_name)
#     do1 <- new("DataObject", format="image/png", filename=file_path,
#                mediaType="image/png", suggestedFilename=file_name)
#     derivations[[length(derivations)+1]] <- do1
#     eml <- add_entity_eml_new(eml, file_name, file_description, file_path, do1@sysmeta@identifier, cn@endpoint)
# 
#     # Create a DataObject to hold the script file and add it to the EML file
#     file_name <- "veg_analysis.R"
#     file_description <- "R script that manipulates input data, runs analysis and generates graphs"
#     file_path <- sprintf("%s/%s", dataDir, file_name)
#     progObj <- new("DataObject", format="application/R", filename=file_path,
#                    mediaType="text/x-rsrc", suggestedFilename=file_name)
#     eml <- add_entity_eml_new(eml, file_name, file_description, file_path, progObj@sysmeta@identifier, cn@endpoint)
# 
#     # Set the package identifier
#     eml_id <- paste0("urn:uuid:", uuid::UUIDgenerate())
#     eml@packageId <- new("xml_attribute", eml_id)
#     eml@system <- new("xml_attribute", "knb")
# 
#     # Validate the eml and write it to disk
#     eml_validate(eml)
#     write_eml(eml, eml_file)
# 
#     # Add provenance information about the derived objects
#     dp <- insertDerivation(dp,
#                            sources=sources,
#                            program=progObj,
#                            derivations=derivations)
# 
#     # Upload package to the repository
#     resourceMapId <- uploadDataPackage(d1c, dp, replicate=FALSE, public=TRUE, quiet=F, resolveURI=paste0(d1c@cn@endpoint, "/resolve"))
#     message("    EML ID: ", eml_object@sysmeta@identifier)
#     message("Package ID: ", resourceMapId)
# 
# }


# start metadata and data package generation for heterogeneity paper

process_dp1 <- function() {

    # Create a DataPackage to hold all of the objects
    dp <- new("DataPackage")
    dataDir <- paste(getwd(), "metadata", sep = "/")
    setwd(dataDir)
    eml_file <- sprintf("%s/dp1-metadata.xml", dataDir)
    title <- "Compiled long-term community composition datasets of primary producers and consumers in both freshwater and terrestrial communities"
    pubDate <- "2017"
    file_ext <- "dp1"
    keywords <- c("species names", "populations", "communities", "community", "heterogeneity", "temporal", "spatial")

    eml <- create_eml(title, pubDate, file_ext, keywords, dataDir)
    eml <- add_people_eml(eml, file_ext)
    eml <- add_geocoverage_eml(eml)
    eml <- add_provenance_eml(eml)

    # Document and add the first data file to the package
    file_name <- "relative_cover_nceas_and_converge_12012015_long.csv"
    file_description <- "Long-tem community composition data"
    file_path <- sprintf("%s/%s", dataDir, file_name)
    do1 <- new("DataObject", format="text/csv", filename=file_path,
               mediaType="text/csv", suggestedFilename=file_name)
    eml <- add_entity_eml(eml, file_name, file_description, file_path, do1@sysmeta@identifier, cn@endpoint)

    # Document and add the second data file to the package
    file_name <- "siteinfo_key.csv"
    file_description <- "detailed information for each sampling site"
    file_path <- sprintf("%s/%s", dataDir, file_name)
    do2 <- new("DataObject", format="text/csv", filename=file_path,
               mediaType="text/csv", suggestedFilename=file_name)
    eml <- add_entity_eml(eml, file_name, file_description, file_path, do2@sysmeta@identifier, cn@endpoint)

    
    # Create a DataObject to hold the script file and add it to the EML file
    file_name <- "convertToMatrix.R"
    file_description <- "R script that reformats the relative_cover_NCEAS_and_converge_12012015_long.csv into a matrix style wide table with species as columns headings"
    file_path <- sprintf("%s/%s", dataDir, file_name)
    progObj <- new("DataObject", format="application/R", filename=file_path,
                   mediaType="text/x-rsrc", suggestedFilename=file_name)
    eml <- add_entity_eml(eml, file_name, file_description, file_path, progObj@sysmeta@identifier, cn@endpoint)

    
    # Set the package identifier
    eml_id <- paste0("urn:uuid:", uuid::UUIDgenerate())
    eml@packageId <- new("xml_attribute", eml_id)
    eml@system <- new("xml_attribute", "knb")

    # Validate the eml and write it to disk
    eml_validate(eml)
    write_eml(eml, eml_file)



}

process_dp2 <- function() {
  
  # Create a DataPackage to hold all of the objects
  dp <- new("DataPackage")
  dataDir <- paste(getwd(), "metadata", sep = "/")
  setwd(dataDir)
  eml_file <- sprintf("%s/dp2-metadata.xml", dataDir)
  title <- "Temporal heterogeneity increases with spatial heterogeneity in ecological communities"
  pubDate <- "2017"
  file_ext <- "dp2"
  keywords <- c("species names", "populations", "communities", "community", "heterogeneity", "temporal", "spatial")
  
  eml <- create_eml(title, pubDate, file_ext, keywords, dataDir)
  eml <- add_people_eml(eml, file_ext)
  eml <- add_geocoverage_eml(eml)
  
  # Document and add the data file to the package
  file_name <- "SpatioTemp_testStatistics_20170208.csv"
  file_description <- "Community heterogeneity dataset"
  file_path <- sprintf("%s/%s", dataDir, file_name)
  do1 <- new("DataObject", format="text/csv", filename=file_path,
             mediaType="text/csv", suggestedFilename=file_name)
  eml <- add_entity_eml(eml, file_name, file_description, file_path, do1@sysmeta@identifier, cn@endpoint)
  
  # Create a DataObject to hold the script file and add it to the EML file
  file_name <- "CoDyn_Step0_BrayCurtisFuntions.R"
  file_description <- "Rate of community change over successive time intervals"
  file_path <- sprintf("%s/%s", dataDir, file_name)
  progObj1 <- new("DataObject", format="application/R", filename=file_path,
                 mediaType="text/x-rsrc", suggestedFilename=file_name)
  eml <- add_entity_eml(eml, file_name, file_description, file_path, progObj1@sysmeta@identifier, cn@endpoint)
  
  # Create a DataObject to hold the script file and add it to the EML file
  file_name <- "CoDyn_Step1_TestStatistics.R"
  file_description <- "Generation of test statistics for subsequent analysis and visualization"
  file_path <- sprintf("%s/%s", dataDir, file_name)
  progObj2 <- new("DataObject", format="application/R", filename=file_path,
                 mediaType="text/x-rsrc", suggestedFilename=file_name)
  eml <- add_entity_eml(eml, file_name, file_description, file_path, progObj2@sysmeta@identifier, cn@endpoint)

  # Create a DataObject to hold the script file and add it to the EML file
  file_name <- "CoDyn_Step2_AnalysesVisuals.R"
  file_description <- "Analysis and visualization"
  file_path <- sprintf("%s/%s", dataDir, file_name)
  progObj3 <- new("DataObject", format="application/R", filename=file_path,
                  mediaType="text/x-rsrc", suggestedFilename=file_name)
  eml <- add_entity_eml(eml, file_name, file_description, file_path, progObj3@sysmeta@identifier, cn@endpoint)
  
  # Document and add the figure 1 to the package
  file_name <- "Fig1.jpg"
  file_description <- "Relationship between spatial heterogeneity and temporal heterogeneity in aquatic (blue) and terrestrial (green) systems."
  file_path <- sprintf("%s/%s", dataDir, file_name)
  do2 <- new("DataObject", format="image/jpg", filename=file_path,
             mediaType="image/png", suggestedFilename=file_name)
  eml <- add_entity_eml(eml, file_name, file_description, file_path, do2@sysmeta@identifier, cn@endpoint)
  
  # Document and add the figure 2 to the package
  file_name <- "Fig2.jpg"
  file_description <- "Effects of spatial heterogeneity and (left) experimental or (right) biological predictors on temporal heterogeneity. "
  file_path <- sprintf("%s/%s", dataDir, file_name)
  do3 <- new("DataObject", format="image/jpg", filename=file_path,
             mediaType="image/png", suggestedFilename=file_name)
  eml <- add_entity_eml(eml, file_name, file_description, file_path, do3@sysmeta@identifier, cn@endpoint)
  
  # Document and add the figure 3 to the package
  file_name <- "Fig3.jpg"
  file_description <- "Relationship between spatial heterogeneity and temporal heterogeneity by lifespan and over time."
  file_path <- sprintf("%s/%s", dataDir, file_name)
  do4 <- new("DataObject", format="image/jpg", filename=file_path,
             mediaType="image/png", suggestedFilename=file_name)
  eml <- add_entity_eml(eml, file_name, file_description, file_path, do4@sysmeta@identifier, cn@endpoint)
  
  # Set the package identifier
  eml_id <- paste0("urn:uuid:", uuid::UUIDgenerate())
  eml@packageId <- new("xml_attribute", eml_id)
  eml@system <- new("xml_attribute", "knb")
  
  # Validate the eml and write it to disk
  eml_validate(eml)
  write_eml(eml, eml_file)
  

}

create_eml <- function(title, pubDate, file_ext, keywords, dataDir){
    eml <- new("eml")

    abstract_file <- paste(dataDir, paste("abstract", file_ext, ".txt", sep = ""), sep = "/")

    abstract <- as(set_TextType(abstract_file), "abstract")

    #start the dataset EML
    dataset <- new("dataset",
                   title = title,
                   pubDate = pubDate,
                   abstract = abstract)


    #add keywords
    keywordSet <- c(new("keywordSet",
                        keyword = keywords))

    dataset@keywordSet <- new("ListOfkeywordSet", c(keywordSet))

    #intellectual Rights
    dataset@intellectualRights <- as(set_TextType("intellectualRights.txt"), "intellectualRights")


    #add methods
    method_file <- paste(dataDir, paste("method", file_ext, ".txt", sep = ""), sep = "/")
    methods <- set_methods(method_file)

    dataset@methods <- methods

    #add coverage
    begindate <- "1943"
    enddate <- "2015"
    coverage <- set_coverage(begin = begindate, end = enddate)

    dataset@coverage <- coverage

    eml@dataset <- dataset
    return(eml)
}

add_people_eml <- function(eml, file_ext){


    people_file <- paste(dataDir, paste("people", file_ext, ".csv", sep = ""), sep = "/")

    #read csv file with person information (givenName, surName, organization,  electronicMailAddress, userId)
    personinfo <- read.csv(people_file, header = TRUE, sep = ",", colClasses = "character")

    #subset personinfo for creators
    creatorinfo <- subset(personinfo, role == "creator")

    #run each row through the helper function to set creators
    eml@dataset@creator <- as(lapply(1:dim(creatorinfo)[1], function(i)
        set_creator(creatorinfo[i,])),
        "ListOfcreator")

    #subset personinfo for contacts
    contactinfo <- subset(personinfo, role == "contact")

    #run each row through the helper function to set creators
    eml@dataset@contact <- as(lapply(1:dim(contactinfo)[1], function(i)
        set_contact(contactinfo[i,])),
        "ListOfcontact")

    return(eml)


}

set_creator <- function(personinforow){

    individualName <- new("individualName",
                          givenName = personinforow[,"givenName"],
                          surName = personinforow[,"surName"])

    creator <- new("creator",
                   individualName = individualName,
                   organizationName = personinforow[,"organizationName"])

    if(nchar(personinforow[,"electronicMailAddress"]) > 0){

        email <- new("electronicMailAddress")
        email@.Data <- personinforow[,"electronicMailAddress"]
        creator@electronicMailAddress <- new("ListOfelectronicMailAddress", c(email))
    }

    if(nchar(personinforow[,"userId"]) > 0){

        userId <- new("userId")
        userId@directory <- new("xml_attribute", "http://orcid.org")
        userId@.Data <- personinforow[,"userId"]
        creator@userId <- new("ListOfuserId", c(userId))
    }
    return(creator)
}

set_contact <- function(personinforow){

    individualName <- new("individualName",
                          givenName = personinforow[,"givenName"],
                          surName = personinforow[,"surName"])

    contact <- new("contact",
                   individualName = individualName,
                   organizationName = personinforow[,"organizationName"])

    if(nchar(personinforow[,"electronicMailAddress"]) > 0){

        email <- new("electronicMailAddress")
        email@.Data <- personinforow[,"electronicMailAddress"]
        contact@electronicMailAddress <- new("ListOfelectronicMailAddress", c(email))
    }

    if(nchar(personinforow[,"userId"]) > 0){

        userId <- new("userId")
        userId@directory <- new("xml_attribute", "http://orcid.org")
        userId@.Data <- personinforow[,"userId"]
        contact@userId <- new("ListOfuserId", c(userId))
    }
    return(contact)
}

add_geocoverage_eml <- function(eml){

  geo_file <- paste(dataDir, "geo_coverage.csv", sep = "/")
  geoinfo <- read.csv(geo_file,header = TRUE, sep = ",", colClasses = "character", quote = "\"")

  #run each row through the helper function to set geoCoverages
  eml@dataset@coverage@geographicCoverage <- as(lapply(1:dim(geoinfo)[1], function(i)
    set_geocoverage(geoinfo[i,])), "ListOfgeographicCoverage")
  
  return(eml)
  
}

set_geocoverage <- function(geoinforow){
  
  geographicCoverage <- new("geographicCoverage",
                            geographicDescription = geoinforow[,"description"])
  geographicCoverage@boundingCoordinates <- new("boundingCoordinates",
                                                westBoundingCoordinate = geoinforow[,"west"],
                                                eastBoundingCoordinate = geoinforow[,"east"],
                                                northBoundingCoordinate = geoinforow[,"north"],
                                                southBoundingCoordinate = geoinforow[,"south"])
  return(geographicCoverage)
}

add_provenance_eml <- function(eml){
  
  provenance_file <- paste(dataDir, "input_datasets.csv", sep = "/")
  provenanceinfo <- read.csv(provenance_file, header = TRUE, sep = ",", colClasses = "character", quote = "\"")
  
  #get the original method steps
  current_method_steps <- eml@dataset@methods@methodStep
  #run each row through the helper function to set the provenance information
  eml@dataset@methods@methodStep <- as(lapply(1:dim(provenanceinfo)[1], function(i)
    set_provenance(provenanceinfo[i,])), "ListOfmethodStep")
  
  #add the original methodSteps back in
  eml@dataset@methods@methodStep[length(eml@dataset@methods@methodStep)+1]<- current_method_steps
  
  return(eml)
}

set_provenance <- function(provenancerow){
  
  methodStep <- new("methodStep")
  text1 <- "Source Dataset"
  text2 <- "This dataset is not publicly available, the data were obtained from authors of the paper"
  text3 <- provenancerow[,"dataset_citation"]
  para1 <- new("para", text1)
  para2 <- new("para", text2)
  para3 <- new("para", text3)
  description <- new("description")

  if(provenancerow[,"dataset_url"] == "unpublished"){
    
    description@para <- c(para1, para2, para3)
  }
  
  else {
    
    description@para <- c(para1, para3)

  }
  methodStep@description <- description
 
  return(methodStep)
}

add_entity_eml <- function(eml, entity_name, entity_description, file_path, identifier, resolve_uri) {

    if (stringr::str_detect(file_path, '.*.csv$')){
        file_name <- basename(file_path)
        path_only <- dirname(file_path)

        #define custom units
        unitTypefile_name <- paste(file_name, "unitType.csv", sep = "")
        if(file.exists(paste(path_only, unitTypefile_name, sep = "/"))){
            unitType <- read.csv(paste(path_only, unitTypefile_name, sep = "/"), header = TRUE, sep = ",", quote = "\"", as.is = TRUE, na.strings = "")
            custom_unitsfile_name <- paste(file_name, "custom_units.csv", sep = "")
            custom_units <- read.csv(paste(path_only, custom_unitsfile_name, sep = "/"), header = TRUE, sep = ",", quote = "\"", as.is = TRUE, na.strings = "")
            unitsList <- set_unitList(custom_units, unitType)
            additionalMetadata <- as(unitsList, "additionalMetadata")
            eml@additionalMetadata <- c(additionalMetadata)
        }

        #read the attributes file back in with all new entries
        attrmetafile_name <- paste(file_name,"meta.csv", sep = "")
        attributes <- read.csv(paste(path_only, attrmetafile_name, sep = "/"), header = TRUE, sep = ",", quote = "\"", as.is = TRUE, na.strings = "")

        # get the column classes into a vector as required by the set_attribute function
        col_classes <- attributes[,"columnClasses"]

        #take out that column again
        attributes$columnClasses <- NULL

        factor_meta <- paste(path_only, paste(file_name, "factor.csv", sep = ""), sep = "/")

        if(file.exists(factor_meta)){
            factors <- read.csv(factor_meta, header = TRUE, sep = ",", quote = "\"", as.is = TRUE)
            #create the attributeList element
            attributeList <- set_attributes(attributes, col_classes = col_classes, factors = factors)

        }else{

            # create the attributeList element without factors
            attributeList <- set_attributes(attributes, col_classes = col_classes)
        }

        #physical parameter for standard Microsoft csv file
        resolve_url <- paste(resolve_uri, identifier, sep="/")
        

        physical <- set_physical(file_name,
                                 numHeaderLines = "1",
                                 recordDelimiter = "\\r\\n",
                                 quoteCharacter = "\"",
                                 url = resolve_url)


        #pull together information for the dataTable

        dataTable <- new("dataTable",
                         entityName = entity_name,
                         entityDescription = entity_description,
                         physical = physical,
                         attributeList = attributeList)

        current_dataTables <- eml@dataset@dataTable
        current_dataTables[[length(current_dataTables)+1]] <- dataTable
        eml@dataset@dataTable <- current_dataTables


        return(eml)

    }else{

        if (stringr::str_detect(file_path, '.*.R$')){
            entityType <- "text/x-rsrc"
            formatName <- "application/R"
        }else if(stringr::str_detect(file_path, '.*.png$')){
            entityType <- "image/png"
            formatName <- "image/png"
        }else if(stringr::str_detect(file_path, '.*.jpg$')){
            entityType <- "image/jpg"
            formatName <- "image/jpg"
        }


        other_entity <- new("otherEntity")
        other_entity@entityName <- entity_name
        other_entity@entityDescription <- entity_description
        other_entity@entityType <- entityType
        resolve_url <- paste(resolve_uri, identifier, sep="/")
        online <- new("online")
        online@url <- new("url", resolve_url)
        dist <- new("distribution")
        dist@online <- online
        phys <- new("physical")
        phys@objectName <- basename(file_path)
        phys@dataFormat@externallyDefinedFormat@formatName <- formatName
        phys@distribution <- c(dist)
        other_entity@physical <- c(phys)
        current_other_entities <- eml@dataset@otherEntity
        current_other_entities[[length(current_other_entities)+1]] <- other_entity
        eml@dataset@otherEntity <- current_other_entities
        return(eml)
    }
}

