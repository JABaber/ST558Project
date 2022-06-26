#knitr::opts_chunk$set(echo = TRUE)
#rmarkdown::render('README.Rmd', 
                  #output_format = "github_document",
                  #output_options = list(
                    #html_preview = FALSE
                  #)
#)
library(jsonlite)
library(tidyverse)
library(forcats)

# Define getPokemon function, can be done with vector of strings (names) or ID 
# numbers, comes from pokemon endpoint
getPokemon <- function(pokemon = NULL, id = NULL){
  # If both pokemon and id arguments provided, throw this error
  if(!is.null(pokemon) & !is.null(id)){
    stop("Only can provide a vector of pokemon names or ids, not both")
  }
  # If a pokemon argument was provided
  if(!is.null(pokemon)){
    # Check if the pokemon argument is a character type
    if(!is.character(pokemon)){
      # If not a character type, show this error message
      stop("Pokemon name must be provided as a string/character vector")
    }
    # Part of URL that doesn't change
    baseURL <- "https://pokeapi.co/api/v2/pokemon/"
    # Part of URL that is based on argument
    name <- pokemon
    # Paste the two together into one string
    fullURL <- paste0(baseURL, name)
    # Read in the JSON file from the API with the full URL string
    pokeinfo <- fromJSON(fullURL)
  }
  # If an id argument was provided
  if(!is.null(id)){
    # Check if the id argument was numeric
    if(!is.numeric(id)){
      # If it wasn't numeric, show this error message
      stop("ID number must be a numeric value/vector")
    }
    # Part of URL that doesn't change
    baseURL <- "https://pokeapi.co/api/v2/pokemon/"
    # Part of URL that is based on argument
    pokedexnum <- id
    # Paste the two together into one string
    fullURL <- paste0(baseURL, pokedexnum)
    # Read in the JSON file from the API with the full URL string
    pokeinfo <- fromJSON(fullURL)
  }
  # Return the info on the pokemon
  return(pokeinfo)
}

# Define getPokeInfo function, can be done with strings (names) or id numbers,
# comes from pokemon-species endpoint
getPokeInfo <- function(pokemon = NULL, id = NULL){
  # Check if both pokemon and id arguments were passed, if so, throw this error 
  # message
  if(!is.null(pokemon) & !is.null(id)){
    stop("Only can provide a vector of pokemon names or ids, not both")
  }
  # If a pokemon argument was provided
  if(!is.null(pokemon)){
    # Check if the pokemon argument is a character type
    if(!is.character(pokemon)){
      # If not a character type, show this error message
      stop("Pokemon name must be provided as a string/character vector")
    }
    # Part of URL that doesn't change
    baseURL <- "https://pokeapi.co/api/v2/pokemon-species/"
    # Part of URL that is based on argument
    name <- pokemon
    # Paste the two together into one string
    fullURL <- paste0(baseURL, pokemon)
    # Read in the JSON file from the API with the full URL string
    pokemoninfo <- fromJSON(fullURL)
  }
  # If an id argument was provided
  if(!is.null(id)){
    # Check if the id argument was numeric
    if(!is.numeric(id)){
      # If it wasn't numeric, show this error message
      stop("ID number must be a numeric value/vector")
    }
    # Part of URL that doesn't change
    baseURL <- "https://pokeapi.co/api/v2/pokemon-species/"
    # Part of URL that is based on argument
    pokedexnum <- id
    # Paste the two together into one string
    fullURL <- paste0(baseURL, pokedexnum)
    # Read in the JSON file from the API with the full URL string
    pokemoninfo <- fromJSON(fullURL)
  }
  # Return the info on the pokemon
  return(pokemoninfo)
}

# Function that combines the data from the pokemon endpoint and the 
# pokemon-species endpoint, can pass it a vector of strings (names) or id 
# numbers and optional argument vars which will select only specified variables 
# from each pokemon
fullPokeInfo <- function(pokemon = NULL, id = NULL, vars = all()){
  # Checks if both a pokemon and id argument were passed, throws this error if so
  if(!is.null(pokemon) & !is.null(id)){
    stop("Only can provide a vector of pokemon names or ids, not both")
  }
  # If a pokemon argument was provided
  if(!is.null(pokemon)){
    # Check if the pokemon argument is a character type
    if(!is.character(pokemon)){
      # If not a character type, show this error message
      stop("Pokemon name must be provided as a string/character vector")
    }
    # Initiate empty lists for some potential looping
    basepoke <- list()
    speciespoke <- list()
    fullpoke <- list()
    # If we received a vector of pokemon names, we can loop through them
    for(i in pokemon){
      # Grab the data from the pokemon endpoint
      basepoke[[i]] <- getPokemon(pokemon = i)
      # Grab the data from the pokemon-species endpoint
      speciespoke[[i]] <- getPokeInfo(pokemon = i)
      # Combine the two into one list
      fullpoke[[i]] <- c(basepoke[[i]], speciespoke[[i]])
    }
    # Subset the full list into just the variables specified with vars
    fullpoke2 <- lapply(fullpoke, function(x)x[vars])
    # Return the list with the combined data for each pokemon
    return(fullpoke2)
  }
  # If an id argument was provided
  if(!is.null(id)){
    # Check if the id argument was numeric
    if(!is.numeric(id)){
      # If it wasn't numeric, show this error message
      stop("ID number must be a numeric value or vector")
    }
    # Initialize empty lists for some potential looping
    idpokemon <- list()
    idspecies <- list()
    idfull <- list()
    # If multiple id numbers were given to id argument, we can loop through them
    # Pretty sure this is where the id variable gets weird.  Couldn't figure out
    # How to make it more flexible.
    for(i in id){
      # Grab the data from the pokemon endpoint
      idpokemon[[i]] <- getPokemon(id = i)
      # Grab the data from the pokemon-species endpoint
      idspecies[[i]] <- getPokeInfo(id = i)
      # Combine the two into one list
      idfull[[i]] <- c(idpokemon[[i]], idspecies[[i]])
    }
    # Subset the full list into just the variables specified with vars
    idfull2 <- lapply(idfull, function(x)x[vars])
    # Return the list with the combined data for each pokemon
    return(idfull2)
  }
}

# Define getBerry function, can be done with a vector of strings (names) or id 
# numbers
getBerry <- function(berry = NULL, id = NULL){
  # Check if both berry and id arguments passed, if so, need to throw this error 
  # message
  if(!is.null(berry) & !is.null(id)){
    stop("Only can provide a vector of berry names or ids, not both")
  }
  # If a berry argument was provided
  if(!is.null(berry)){
    # Check if the berry argument is a character type
    if(!is.character(berry)){
      # If not a character type, show this error message
      stop("Berry name must be provided as a string")
    }
    # Part of URL that doesn't change
    baseURL <- "https://pokeapi.co/api/v2/berry/"
    # Part of URL that is based on argument
    name <- berry
    # Paste the two together into one string
    fullURL <- paste0(baseURL, name)
    # Read in the JSON file from the API with the full URL string
    berryinfo <- fromJSON(fullURL)
  }
  # If an id argument was provided
  if(!is.null(id)){
    # Check if the id argument was numeric
    if(!is.numeric(id)){
      # If it wasn't numeric, show this error message
      stop("ID number must be a numeric value")
    }
    # Part of URL that doesn't change
    baseURL <- "https://pokeapi.co/api/v2/berry/"
    # Part of URL that is based on argument
    berrynum <- id
    # Paste the two together into one string
    fullURL <- paste0(baseURL, berrynum)
    # Read in the JSON file from the API with the full URL string
    berryinfo <- fromJSON(fullURL)
  }
  # Return the info on the berry
  return(berryinfo)
}

# Now we can customize a function that will allow us to get the info for a vector 
# of berry names or id numbers.  Also we have an optional vars argument to pass a 
# vector of variable names to keep, with all variables being the default
berryInfo <- function(berry = NULL, id = NULL, vars = all()){
  # Check if both berry and id arguments were passed, if so, need to throw this 
  # error message
  if(!is.null(berry) & !is.null(id)){
    stop("Only can provide a vector of berry names or ids, not both")
  }
  # If a berry argument was provided
  if(!is.null(berry)){
    # Check if the berry argument is a character type
    if(!is.character(berry)){
      # If not a character type, show this error message
      stop("Berry name must be provided as a string")
    }
    # Initialize an empty list
    berryinfo <- list()
    # Add the berry info for each requested berry to our berryinfo list
    for(i in berry){
      berryinfo[[i]] <- getBerry(berry = i)
    }
    # Subset each iteration of our berryinfo list to contain the vars specified
    berrylist <- lapply(berryinfo, function(x)x[vars])
    # Return the list
    return(berrylist)
  }
  # If an id argument was provided
  if(!is.null(id)){
    # Check if the id argument was numeric
    if(!is.numeric(id)){
      # If it wasn't numeric, show this error message
      stop("ID number must be a numeric value or vector")
    }
    # Initialize an empty list
    idberry <- list()
    # Add the berry info for each requested berry to our idberry list
    for(i in id){
      idberry[[i]] <- getBerry(id = i)
    }
    # Subset each iteration of our idberry list to contain the vars specified
    idlist <- lapply(idberry, function(x)x[vars])
    # Return the list
    return(idlist)
  }
}

# Create a function that will convert one of our output lists to a tibble.  I 
# can't put this in the functions above because some of the objects that are 
# returned in the list won't fit into a data frame with other objects.  We 
# return several data frames, values, vectors, list, etc. so we have to parse 
# it all separately
convertToTibble <- function(list){
  # Create a data frame from a matrix created from a list
  df = as.data.frame(matrix(unlist(list), nrow = length(list), byrow = T))
  # Convert data frame to tibble so it will be compliant with tidyverse functions
  tibble = tibble(df)
  # Return the tibble
  return(tibble)
}

# Abilities are stored in a data frame, so I need a function to help convert to a 
# tibble
getAbilities <- function(list){
  # Subset to just the abilities for each pokemon in list
  abilities <- lapply(list, function(x)x["abilities"]$abilities)
  # Initialize empty vectors, some pokemon may have 3 possible abilities
  ability1 <- vector()
  ability2 <- vector()
  ability3 <- vector()
  # Loop through each pokemon's abilities list
  for(i in 1:length(abilities)){
    # If there is not a 1st ability say "none", else return ability name to 
    # ability1
    ability1[i] <- ifelse(is.na(abilities[[i]]$ability$name[1]), "none", 
                          abilities[[i]]$ability$name[1])
    # If there is not a 2nd ability say "none", else return ability name to 
    # ability2
    ability2[i] <- ifelse(is.na(abilities[[i]]$ability$name[2]), "none", 
                          abilities[[i]]$ability$name[2])
    # If there is not a 3rd ability say "none", else return ability name to 
    # ability3
    ability3[i] <- ifelse(is.na(abilities[[i]]$ability$name[3]), "none", 
                          abilities[[i]]$ability$name[3])
  }
  # Return a tibble of the three ability columns
  return(tibble(ability1, ability2, ability3))
}

# Since stats is also stored in a list, it will take some work to get out what I 
# want
getStats <- function(list){
  # Subset to just the stats list for each pokemon in list
  stats <- lapply(list, function(x)x["stats"]$stats)
  # Every pokemon has six stats, initialize lists for them
  hp <- vector()
  attack <- vector()
  defense <- vector()
  special_attack <- vector()
  special_defense <- vector()
  speed <- vector()
  # Loop through each pokemon's stats list
  for(i in 1:length(stats)){
    # Append vectors with the corresponding stat from base_stat
    hp[i] <- stats[[i]]$base_stat[1]
    attack[i] <- stats[[i]]$base_stat[2]
    defense[i] <- stats[[i]]$base_stat[3]
    special_attack[i] <- stats[[i]]$base_stat[4]
    special_defense[i] <- stats[[i]]$base_stat[5]
    speed[i] <- stats[[i]]$base_stat[6]
  }
  # Return a tibble of stats for each pokemon
  return(tibble(hp, attack, defense, special_attack, special_defense, speed))
}

# Last but not least, we need types
getTypes <- function(list){
  # Subset to just the types list for each pokemon in list
  types <- lapply(list, function(x)x["types"]$types)
  # Every pokemon has one or two types, intialize empty lists
  type1 <- vector()
  type2 <- vector()
  # Loop through each pokemon's types list
  for(i in 1:length(types)){
    # If there is not a 1st type, return "none", otherwise return the type name
    type1[i] <- ifelse(is.na(types[[i]]$type$name[1]), "none", 
                       types[[i]]$type$name[1])
    # If there is not a 2nd type, return "none", otherwise return the type name
    type2[i] <- ifelse(is.na(types[[i]]$type$name[2]), "none", 
                       types[[i]]$type$name[2])
  }
  # Return tibble of types
  return(tibble(type1, type2))
}

# Firmness is stored in a list, need a function to parse this data
getFirmness <- function(list){
  # Subset to just the firmness list for each berry in list
  firm <- lapply(list, function(x)x["firmness"]$firmness)
  # Initialize firmness vector
  firmness <- vector()
  # Loop through each berry's firmness list
  for(i in 1:length(firm)){
    # Append the "name" value of firmness onto the vector for each berry
    firmness[i] <- firm[[i]]$name
  }
  # Return tibble of types
  return(tibble(firmness))
}

# Natural Gift Type is stored in a list, need a function to parse this data
getNatGiftType <- function(list){
  # Subset to just the natural_gift_type list for each berry in list
  giftType <- lapply(list, function(x)x["natural_gift_type"]$natural_gift_type)
  # Initialize natural_gift_type vector
  natural_gift_type <- vector()
  # Loop through each berry's giftType list
  for(i in 1:length(giftType)){
    # Append the "name" value of giftType onto the vector for each berry
    natural_gift_type[i] <- giftType[[i]]$name
  }
  # Return tibble of types
  return(tibble(natural_gift_type))
}

# Since flavors is also stored in a list, it will take some work to get out what I 
# want
getFlavors <- function(list){
  # Subset to just the flavors list for each berry in list
  flavors <- lapply(list, function(x)x["flavors"]$flavors)
  # Every berry has five flavors, initialize lists for them
  spicy <- vector()
  dry <- vector()
  sweet <- vector()
  bitter <- vector()
  sour <- vector()
  # Loop through each berry's flavors list
  for(i in 1:length(flavors)){
    # Append vectors with the corresponding stat from potency
    spicy[i] <- flavors[[i]]$potency[1]
    dry[i] <- flavors[[i]]$potency[2]
    sweet[i] <- flavors[[i]]$potency[3]
    bitter[i] <- flavors[[i]]$potency[4]
    sour[i] <- flavors[[i]]$potency[5]
  }
  # Return a tibble of flavor values for each berry
  return(tibble(spicy, dry, sweet, bitter, sour))
}

# item name is stored in a list, need a function to parse this data
getItem <- function(list){
  # Subset to just the item list for each berry in list
  items <- lapply(list, function(x)x["item"]$item)
  # Initialize item_name vector
  item_name <- vector()
  # Loop through each berry's items list
  for(i in 1:length(items)){
    # Append the "name" value of items onto the vector for each berry
    item_name[i] <- items[[i]]$name
  }
  # Return tibble of item names
  return(tibble(item_name))
}

# Function that will parse a pokemon query into a tibble 
pokeTibble <- function(pokemon = NULL, id = NULL, vars){
  # Get a list from fullPokeInfo()
  pokelist <- fullPokeInfo(pokemon = pokemon, id = id, vars = vars)
  # These variables are data frames in the list, need to have use helper functions 
  # to get them usable
  dfs <- c("types", "abilities", "stats")
  # These variables are not data frames and can be easily put into a tibble
  nondfs <- c("name", "id", "height", "weight", "capture_rate", "base_experience", 
              "base_happiness", "is_baby", "is_legendary", "is_mythical", 
              "is_default", "order", "hatch_counter")
  # These variables are either pretty useless, too large, contain urls, or are 
  # hard to work with and give me errors
  badvars <- c("forms", "game_indices", "held_items", "location_area_encounters", 
               "moves", "past_types", "species", "sprites", "color", "egg_groups", 
               "evolution_chain", "flavor_text_entries", "form_descriptions", 
               "has_gender_differences", "gender_rate", "evolves_from_species", 
               "forms_switchable", "genera", "generation", "growth_rate", 
               "habitat", "names", "pal_park_encounters", "pokedex_numbers", 
               "shape", "varities")
  # These variables are numeric, need to be explicitly coerced into numeric 
  # variables later
  numericvars <- c("height", "weight", "capture_rate", "base_experience", 
                   "base_happiness", "order", "hatch_counter")
  # These variables are logical, need to be explicitly coerced into logical 
  # variables later
  logicalvars <- c("is_baby", "is_legendary", "is_mythical", "is_default")
  # First I want to throw an error if a "badvar" was passed in the vars argument,
  # since they can't go in a tibble
  for(i in badvars){
    if(i %in% vars){
      stop(paste0(i, " is not a valid variable, please remove.  Valid variables 
                  include name, id, height, weight, capture_rate, base_experience, 
                  base_happiness, is_baby, is_legendary, is_mythical, is_default, 
                  order, evolves_from_species, forms_switchable, gender_rate, 
                  has_gender_differences, hatch_counter, types, abilities, and 
                  stats"))
    }
  }
  # Next, initialize an empty vector for variables
  nondfvars = c()
  # For every nondf variable
  for(i in nondfs){
    # Check if it was passed in the vars argument
    if(i %in% vars){
      # If so, append it to the nondfvars vector
      nondfvars[i] <- i
    }
  }
  # Now that we have a vector nondf vars that were passed, we can subset each 
  # pokemon's list and call it values
  values <- lapply(pokelist, function(x)x[nondfvars])
  # Convert to a tibble
  valuestibble <- convertToTibble(values)
  # Rename the columns
  colnames(valuestibble) <- nondfvars
  # Next we need to consider our variables that are stuck in data frames, 
  # initialize an empty vector named dfvars
  dfvars = c()
  # For every df variable
  for(i in dfs){
    # Check if it was passed in the vars argument
    if(i %in% vars){
      # If so, append it to the dfvars vector
      dfvars[i] <- i
    }
  }
  # Check if "types" was a variable passed, if so, create a types tibble using 
  # getTypes() and append it to the valuestibble
  if("types" %in% dfvars){
    types <- getTypes(pokelist)
    valuestibble <- tibble(valuestibble, types)
  }
  # Check if "abilities" was a variable passed, if so, create an abilities tibble 
  # using getAbilities() and append it to the valuestibble
  if("abilities" %in% dfvars){
    abilities <- getAbilities(pokelist)
    valuestibble <- tibble(valuestibble, abilities)
  }
  # Check if "stats" was a variable passed, if so, create a stats tibble using 
  # getStats() and append it to the valuestibble
  if("stats" %in% dfvars){
    stats <- getStats(pokelist)
    valuestibble <- tibble(valuestibble, stats)
  }
  # Now we need to convert numeric variables into integer values if possible
  for(i in colnames(valuestibble)){
    # If a column name is in our numericvars vector
    if(i %in% numericvars){
      # Replace the values in that column with the integer value of it
      valuestibble[[i]] <- as.integer(valuestibble[[i]])
    }
    # If a column name is in our logicalvars vector
    if(i %in% logicalvars){
      # Replace the values in that column with the logical value of it
      valuestibble[[i]] <- as.logical(valuestibble[[i]])
    }
  }
  # Return our tibble
  return(valuestibble)
}

# Similar function but for berries, can take in berry or id and a vector of 
# variables
berryTibble <- function(berry = NULL, id = NULL, vars){
  # Get a list of berry info from the berryInfo() function
  berrylist <- berryInfo(berry = berry, id = id, vars = vars)
  # These are the variables that require extra parsing because they are in 
  # list/data frames
  dfs <- c("flavors", "natural_gift_type", "firmness", "item")
  # These variables do no require extra parsing
  nondfs <- c("name", "id", "growth_time", "max_harvest", "natural_gift_power", 
              "size", "smoothness", "soil_dryness")
  # These variables are supposed to be numeric, will have to convert them to 
  # numeric later
  numericvars <- c("growth_time", "max_harvest", "natural_gift_power", "size", 
                   "smoothness", "soil_dryness")
  # Initialize an empty vector to hold nondf variables that were passed through 
  # vars argument
  nondfvars = c()
  # For every variable in nondfs
  for(i in nondfs){
    # Check if it was passed through vars argument
    if(i %in% vars){
      # If so, append that variable to the nondfvars vector
      nondfvars[i] <- i
    }
  }
  # Subset each berry list to contain the nondf vars variables
  values <- lapply(berrylist, function(x)x[nondfvars])
  # Convert to a tibble
  valuestibble <- convertToTibble(values)
  # Rename the columns
  colnames(valuestibble) <- nondfvars
  # Initialize an empty bector to hold df variables that were passed through vars 
  # argument
  dfvars = c()
  # For every variable in dfs
  for(i in dfs){
    # Check if it was passed through vars argument
    if(i %in% vars){
      # If so, append that variable to the dfvars vector
      dfvars[i] <- i
    }
  }
  # If "flavors" was in the vars argument, get the flavors tibble using getFlavors, 
  # then combine that tibble with the valuestibble
  if("flavors" %in% dfvars){
    flavors <- getFlavors(berrylist)
    valuestibble <- tibble(valuestibble, flavors)
  }
  # If "natural_gift_type" was in the vars argument, get the tibble using 
  # getNatGiftType, then combine that tibble with the valuestibble
  if("natural_gift_type" %in% dfvars){
    natural_gift_type <- getNatGiftType(berrylist)
    valuestibble <- tibble(valuestibble, natural_gift_type)
  }
  # If "firmness" was in the vars argument, get the flavors tibble using 
  # getFirmness, then combine that tibble with the valuestibble
  if("firmness" %in% dfvars){
    firmness <- getFirmness(berrylist)
    valuestibble <- tibble(valuestibble, firmness)
  }
  # If "item" was in the vars argument, get the item tibble using getItem, 
  # then combine that tibble with the valuestibble
  if("item" %in% dfvars){
    item <- getItem(berrylist)
    valuestibble <- tibble(valuestibble, item)
  }
  # Convert the numeric variables to numeric, check each column name in the tibble
  for(i in colnames(valuestibble)){
    # If the column name is in numericvars
    if(i %in% numericvars){
      # Convert that column to integer values
      valuestibble[[i]] <- as.integer(valuestibble[[i]])
    }
  }
  # Return the tibble
  return(valuestibble)
}

# Lastly, we can wrap everything together to produce nice tibbles for either 
# pokemon or berry queries
queryAPI <- function(pokemon = NULL, pokeid = NULL, pokevars = NULL, berry = NULL, 
                     berryid = NULL, berryvars = NULL){
  # If only pokemon and pokevars arguments were passed
  if(!is.null(pokemon) & !is.null(pokevars) & is.null(c(pokeid, berry, berryid, 
                                                        berryvars))){
    # Create a pokeTibble from the pokemon and pokevars arguments and return it
    poketibble <- pokeTibble(pokemon = pokemon, vars = pokevars)
    return(poketibble)
  }
  # Else if only pokeid and pokevars arguments were passed
  else if(!is.null(pokeid) & !is.null(pokevars) & is.null(c(pokemon, berry, 
                                                            berryid, berryvars))){
    # Create a pokeTibble from the pokeid and pokevars arguments and return it
    poketibble <- pokeTibble(id = pokeid, vars = pokevars)
    return(poketibble)
  }
  # Else if only berry and berryvars arguments were passed
  else if(!is.null(berry) & !is.null(berryvars) & is.null(c(pokemon, pokeid, 
                                                            pokevars, berryid))){
    # Create a berryTibble from the berry and berryvars arguments and return it
    berrytibble <- berryTibble(berry = berry, vars = berryvars)
    return(berrytibble)
  }
  # Else if only berryid and berryvars arguments were passed
  else if(!is.null(berryid) & !is.null(berryvars) & is.null(c(pokemon, pokeid, 
                                                              pokevars, berry))){
    # Create a berryTibble the berryid and berryvars arguments and return it
    berrytibble <- berryTibble(id = berryid, vars = berryvars)
    return(berrytibble)
  }
  # If some other combination of arguments was passed, it is invalid, return this 
  # error message
  else{
    stop("Must provide either names and variables or ids and variables for 
         pokemon or for berries, not both!")
  }
}

# These are the variables I am going to be working with in my EDA for pokemon and 
# berries
myPokeVars <- c("name", "id", "types", "abilities", "height", "weight", "is_baby", 
                "is_legendary", "is_mythical", "capture_rate", "stats")
myBerryVars <- c("name", "id", "growth_time", "max_harvest", "natural_gift_power", 
                 "size", "smoothness", "soil_dryness", "flavors", 
                 "natural_gift_type", "firmness", "item")

# Test that it works for pokemon and pokevars combination
queryAPI(pokemon = c("pikachu", "mewtwo", "greninja", "igglybuff", "rayquaza"), 
         pokevars = myPokeVars)

# Test that it works for pokeid and pokevars combination
queryAPI(pokeid = c(1:10), pokevars = myPokeVars)

# Test that it works for berry and berryvars combination
queryAPI(berry = c("cheri", "rawst", "chesto"), berryvars = myBerryVars)

# Test that it works for berryid and berryvars combination
queryAPI(berryid = 1:64, berryvars = myBerryVars)

# These are the variables I am going to be working with in my EDA for pokemon and 
# berries
myPokeVars <- c("name", "id", "types", "abilities", "height", "weight", "is_baby", 
                "is_legendary", "is_mythical", "capture_rate", "stats")
myBerryVars <- c("name", "id", "growth_time", "max_harvest", "natural_gift_power", 
                 "size", "smoothness", "soil_dryness", "flavors", 
                 "natural_gift_type", "firmness", "item")

# Generate data set for first four generations of pokemon (My childhood)
myPokeData <- queryAPI(pokeid = 1:493, pokevars = myPokeVars)

# Generate data set for all berries (there are 64 total)
myBerryData <- queryAPI(berryid = 1:64, berryvars = myBerryVars)

# Convert id to numeric just for the purpose of subsetting
idnums <- as.integer(myPokeData$id)
# Bin based on the id variables using ifelse() and save as a vector named 
# generation.  These values came from bulbapedia.
generation <- ifelse(idnums <= 151, "1",
                     ifelse(idnums > 151 & idnums <= 251 , "2",
                            ifelse(idnums > 251 & idnums <= 386, "3",
                                   ifelse(idnums > 386 & idnums <= 493, "4",
                                          ifelse(idnums > 493 & idnums <= 649, "5",
                                                 ifelse(idnums > 649 & idnums <= 721, "6",
                                                        ifelse(idnums > 721 & idnums <= 809, "7",
                                                               ifelse(idnums > 809 & idnums <= 905, "8", "None"))))))))
# Append the generation vector onto the myPokeData tibble
myPokeData <- myPokeData %>% mutate(generation = generation)
# Print the first few observations with just three columns to check
head(myPokeData) %>% select(name, id, generation)

# Print the last few observations with just three columns to check
tail(myPokeData) %>% select(name, id, generation)

# Counts of legendaries and non-legendaries
table(myPokeData$is_legendary)

# Counts of type 1s
table(myPokeData$type1)

# Counts of type 2s
table(myPokeData$type2)

# Counts of type 1s and type 2s
table(myPokeData$type1, myPokeData$type2)

# Counts of Fire type 1s against generations
table(myPokeData$type1 == "fire", myPokeData$generation)

# Counts of Fire type 2s against generations
table(myPokeData$type2 == "fire", myPokeData$generation)

# One-way Contingency Table of Natural Gift Types
table(myBerryData$natural_gift_type)

# One-way contingency table of Natural Gift Power
table(myBerryData$natural_gift_power)

# Two-way contingency table of natural gift power and firmness
table(myBerryData$natural_gift_power, myBerryData$firmness)

# Five number summary of hp across all four generations
summary(myPokeData$hp)

# Get numerical summaries for capture rates of pokemon across each generation
myPokeData %>% 
  group_by(generation) %>% 
  summarize(mean = mean(capture_rate), sd = sd(capture_rate), 
            min = min(capture_rate), max = max(capture_rate))

# Get numerical summaries for capture rates of pokemon based on whether or not 
# they are legendary
myPokeData %>%
  group_by(is_legendary) %>%
  summarize(mean = mean(capture_rate), sd = sd(capture_rate), 
            min = min(capture_rate), max = max(capture_rate))

# Get numerical summaries for attack stat of pokemon based on whether or not they 
# are a baby pokemon
myPokeData %>%
  group_by(is_baby) %>%
  summarize(mean = mean(attack), sd = sd(attack), min = min(attack), 
            max = max(attack))

# Get numerical summaries for growth times grouped by Natural Gift powers
myBerryData %>%
  group_by(natural_gift_power) %>%
  summarize(mean = mean(growth_time), sd = sd(growth_time), min = min(growth_time), 
            max = max(growth_time))

# Get numerical summaries for size grouped by firmness
myBerryData %>%
  group_by(firmness) %>%
  summarize(mean = mean(size), sd = sd(size), min = min(size), max = max(size))

# Scatterplot of Special Attack vs. Attack, colored by Type 1.  I gave custom 
# labels and title.  Also change the opacity and size of the point and the size 
# of the legend.
ggplot(myPokeData, aes(x = attack, y = special_attack, color = type1)) + 
  geom_point(alpha = 0.5, size = 2) + theme(legend.key.size = unit(0.5, "cm")) + 
  labs(x = "Attack", y = "Special Attack", 
       title = "Attack vs. Special Attack Colored by Type 1 (Gens 1 Thru 4)") +
  scale_color_discrete("Type 1")

# I needed to call in this fct_rev function to put the legendary pokemon on top, 
# since they were on the bottom at first and I didn't like that.
library(forcats)
# Create a bar plot of total pokemon counts by generation.  Also filled bars based 
# on whether or not pokemon were legendary.  This allows us to see the total amount 
# of legendary pokemon as well.  Added custom labels, title, legend, and colors 
# with labs() and scale_fill_manual()
ggplot(myPokeData, aes(x = generation)) + 
  geom_bar(aes(fill = forcats::fct_rev(as.factor(is_legendary)))) + 
  labs(x = "Generation", y = "Total Number of Pokemon", 
       title = "Total Number Of Pokemon and Legendaries by Generation") +
  scale_fill_manual(breaks = c(FALSE, TRUE), values = c("navy", "maroon"), 
                    name = " ", labels = c("Is Legendary", "Is Not Legendary"))

# Subset the data into just gens 1 and 2
gens1and2 <- myPokeData %>% filter(generation < 3)
# Create two histograms, one for each generation, with 25 bins for the speed stat. 
# Overlay a Density plot on each of them.  Customize labels, title, legend, and 
# colors.
ggplot(gens1and2, aes(x = speed)) + 
  geom_histogram(bins = 25, aes(y = ..density.., fill = generation)) +
  geom_density(aes(fill = generation), alpha = 0.6, position = "stack") + 
  labs(x = "Speed", y = "Density", 
       title = "Histogram and Kernel Smoother of Speed for Generations 1 and 2") +
  scale_fill_manual(name = "Generation", breaks = c("1", "2"), 
                    values = c("red", "orange"))

# Create a boxplot for size for each value of natural gift power, since it's 
# technically continuous, the colors will be a gradient. Made custom legend, 
# labels, and title.
ggplot(myBerryData, aes(x = as.factor(as.character(natural_gift_power)), y = size, 
                        fill = natural_gift_power)) + geom_boxplot() +
  labs(x = "Natural Gift Power", y = "Size", 
       title = "Boxplots for Size at Each Level of Natural Gift Power") +
  scale_fill_continuous(name = "Natural Gift Power")

# Created a scatterplot of growth time vs. size, colored by natural gift type. 
# Customized labels and title to make it look nice, also changed the size of 
# legends and points, as well as opacity of points.
ggplot(myBerryData, aes(x = growth_time, y = size, color = natural_gift_type)) + 
  geom_point(alpha = 0.5, size = 2) + theme(legend.key.size = unit(0.5, "cm")) + 
  labs(x = "Growth Time", y = "Size", 
       title = "Growth Time vs. Size, Colored by Natural Gift Type") +
  scale_color_discrete("Natural Gift Type")