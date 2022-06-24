Pokemon API Vignette
================
Josh Baber
6/23/2022

``` r
# Packages
library(jsonlite)
library(tidyverse)
```

``` r
# render function, doesn't eval just here to copy/paste into console
rmarkdown::render('README.Rmd', 
                  output_format = "github_document",
                  output_options = list(
                    html_preview = FALSE
                    )
                  )
```

``` r
# Define getPokemon function, can be done with vector of strings (names) or ID numbers, comes from pokemon endpoint
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
```

``` r
# Define getPokeInfo function, can be done with strings (names) or id numbers, comes from pokemon-species endpoint
getPokeInfo <- function(pokemon = NULL, id = NULL){
  # Check if both pokemon and id arguments were passed, if so, throw this error message
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
```

``` r
# Function that combines the data from the pokemon endpoint and the pokemon-species endpoint,
# can pass it a vector of strings (names) or id numbers
fullPokeInfo <- function(pokemon = NULL, id = NULL){
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
    # Return the list with the combined data for each pokemon
    return(fullpoke)
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
    for(i in id){
      # Grab the data from the pokemon endpoint
      idpokemon[[i]] <- getPokemon(id = i)
      # Grab the data from the pokemon-species endpoint
      idspecies[[i]] <- getPokeInfo(id = i)
      # Combine the two into one list
      idfull[[i]] <- c(idpokemon[[i]], idspecies[[i]])
    }
    # Return the list with the combined data for each pokemon
    return(idfull)
  }
}
```

``` r
# This is the final pokemon function, where we can get pokemon info from both endpoints in a nice combined list
# Also, we can specify what variables we want for each pokemon with the vars argument, which by default will return all variables
pokeList <- function(pokemon = NULL, id = NULL, vars = all()){
  # Initialize an empty list
  pokelist = list()
  # Check if both pokemon and id arguments were passed, if so, throw this error message
  if(!is.null(pokemon) & !is.null(id)){
    stop("Only can provide a vector of pokemon names or ids, not both")
  }
  # If a pokemon argument was passed
  if(!is.null(pokemon)){
    # Check that it was a character type
    if(!is.character(pokemon)){
      stop("Pokemon name must be a string/character vector")
    }
    # Grab the full pokemon info for each value passed, and put it in our pokelist
    for(i in pokemon){
      pokelist[i] <- fullPokeInfo(pokemon = i)
    }
  }
  # If an id argument was passed
  if(!is.null(id)){
    # Check that it was a numeric type
    if(!is.numeric(id)){
      stop("ID number must be a numeric value/vector")
    }
    # Grab the full pokemon info for each value passed, and put it in our pokelist
    for(i in id){
    pokelist[i] <- fullPokeInfo(id = i)
    }
  }
  # Now we can apply a subset of variables for element on our list based on the optional vars argument
  pokelist2 <- lapply(pokelist, function(x)x[vars])
  # Return the list
  return(pokelist2)
}
```

``` r
# Define getBerry function, can be done with a vector of strings (names) or id numbers
getBerry <- function(berry = NULL, id = NULL){
  # Check if both berry and id arguments passed, if so, need to throw this error message
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
  # Return the info on the pokemon
  return(berryinfo)
}
```

``` r
# Now we can customize a function that will allow us to get the info for a vector of berry names or id numbers,
# Also we have an optional vars argument to pass a vector of variable names to keep, with all variables being the default
berryInfo <- function(berry = NULL, id = NULL, vars = all()){
  # Check if both berry and id arguments were passed, if so, need to throw this error message
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
```

``` r
mons <- pokeList(pokemon = c("mewtwo", "garchomp", "froslass"), vars = c("name", "height"))
as.data.frame(matrix(unlist(mons), nrow = length(mons), byrow = T))
```

    ##         V1 V2
    ## 1   mewtwo 20
    ## 2 garchomp 19
    ## 3 froslass 13

``` r
squirtle <- pokeList(pokemon = "squirtle")
```

<https://bulbapedia.bulbagarden.net/wiki/Berry>

``` r
berrySizes <- berryInfo(id = 1:64, vars = c("name", "size"))
berrydf <- as.data.frame(matrix(unlist(berrySizes), nrow = length(berrySizes), byrow = T))
colnames(berrydf) <- c("name", "size")
berrydf
```

    ##      name size
    ## 1   cheri   20
    ## 2  chesto   80
    ## 3   pecha   40
    ## 4   rawst   32
    ## 5  aspear   50
    ## 6   leppa   28
    ## 7    oran   35
    ## 8  persim   47
    ## 9     lum   34
    ## 10 sitrus   95
    ## 11   figy  100
    ## 12   wiki  115
    ## 13   mago  126
    ## 14  aguav   64
    ## 15 iapapa  223
    ## 16   razz  120
    ## 17   bluk  108
    ## 18  nanab   77
    ## 19 wepear   74
    ## 20  pinap   80
    ## 21  pomeg  135
    ## 22 kelpsy  150
    ## 23 qualot  110
    ## 24 hondew  162
    ## 25  grepa  149
    ## 26 tamato  200
    ## 27  cornn   75
    ## 28 magost  140
    ## 29 rabuta  226
    ## 30  nomel  285
    ## 31 spelon  133
    ## 32 pamtre  244
    ## 33 watmel  250
    ## 34  durin  280
    ## 35  belue  300
    ## 36   occa   90
    ## 37 passho   33
    ## 38  wacan  250
    ## 39  rindo  156
    ## 40  yache  135
    ## 41 chople   77
    ## 42  kebia   90
    ## 43  shuca   42
    ## 44   coba  278
    ## 45 payapa  252
    ## 46  tanga   42
    ## 47 charti   28
    ## 48  kasib  144
    ## 49  haban   23
    ## 50 colbur   39
    ## 51 babiri  265
    ## 52 chilan   34
    ## 53 liechi  111
    ## 54 ganlon   33
    ## 55  salac   95
    ## 56 petaya  237
    ## 57 apicot   75
    ## 58 lansat   97
    ## 59  starf  153
    ## 60 enigma  155
    ## 61  micle   41
    ## 62 custap  267
    ## 63 jaboca   33
    ## 64  rowap   52
