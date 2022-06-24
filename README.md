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
# Define getPokemon function, can be done with string or id (pokedex) number
getPokemon <- function(pokemon = NULL, id = NULL){
  if(!is.null(pokemon) & !is.null(id)){
    stop("Only can provide a vector of pokemon names or ids, not both")
  }
  # If a pokemon argument was provided
  if(!is.null(pokemon)){
    # Check if the pokemon argument is a string
    if(!is.character(pokemon)){
      # If not a string, show this error message
      stop("Pokemon name must be provided as a string")
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
      stop("ID number must be a numeric value")
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
# Define getPokeInfo function, can be done with string or id number
getPokeInfo <- function(pokemon = NULL, id = NULL){
  if(!is.null(pokemon) & !is.null(id)){
    stop("Only can provide a vector of pokemon names or ids, not both")
  }
  # If a pokemon argument was provided
  if(!is.null(pokemon)){
    # Check if the pokemon argument is a string
    if(!is.character(pokemon)){
      # If not a string, show this error message
      stop("Pokemon name must be provided as a string")
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
      stop("ID number must be a numeric value")
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
fullPokeInfo <- function(pokemon = NULL, id = NULL){
  if(!is.null(pokemon) & !is.null(id)){
    stop("Only can provide a vector of pokemon names or ids, not both")
  }
  # If a pokemon argument was provided
  if(!is.null(pokemon)){
    # Check if the pokemon argument is a string
    if(!is.character(pokemon)){
      # If not a string, show this error message
      stop("Pokemon name must be provided as a string")
    }
    basepoke <- list()
    speciespoke <- list()
    fullpoke <- list()
    for(i in pokemon){
      basepoke[[i]] <- getPokemon(pokemon = i)
      speciespoke[[i]] <- getPokeInfo(pokemon = i)
      fullpoke[[i]] <- c(basepoke[[i]], speciespoke[[i]])
    }
    return(fullpoke)
  }
  # If an id argument was provided
  if(!is.null(id)){
    # Check if the id argument was numeric
    if(!is.numeric(id)){
      # If it wasn't numeric, show this error message
      stop("ID number must be a numeric value or vector")
    }
    idpokemon <- list()
    idspecies <- list()
    idfull <- list()
    for(i in id){
      idpokemon[[i]] <- getPokemon(id = i)
      idspecies[[i]] <- getPokeInfo(id = i)
      idfull[[i]] <- c(idpokemon[[i]], idspecies[[i]])
    }
    return(idfull)
  }
}
```

``` r
pokeList <- function(pokemon = NULL, id = NULL, vars = all()){
  pokelist = list()
  if(!is.null(pokemon) & !is.null(id)){
    stop("Only can provide a vector of pokemon names or ids, not both")
  }
  if(!is.null(pokemon)){
    if(!is.character(pokemon)){
      stop("Pokemon name must be a string")
    }
    for(i in pokemon){
      pokelist[i] <- fullPokeInfo(pokemon = i)
    }
  }
  if(!is.null(id)){
    if(!is.numeric(id)){
      stop("ID number must be a numeric value")
    }
    for(i in id){
    pokelist[i] <- fullPokeInfo(id = i)
    }
  }
  pokelist2 <- lapply(pokelist, function(x)x[vars])
  return(pokelist2)
}
```

``` r
# Define getBerry function, can be done with string or id number
getBerry <- function(berry = NULL, id = NULL){
  if(!is.null(berry) & !is.null(id)){
    stop("Only can provide a vector of berry names or ids, not both")
  }
  # If a berry argument was provided
  if(!is.null(berry)){
    # Check if the berry argument is a string
    if(!is.character(berry)){
      # If not a string, show this error message
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
    if(!is.null(berry)){
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
mons <- pokeList(pokemon = c("mewtwo", "garchomp", "froslass"), vars = c("name", "height"))
```

``` r
squirtle <- pokeList(pokemon = "squirtle")
```

``` r
cheri <- getBerry(berry = "cheri")
```

<https://bulbapedia.bulbagarden.net/wiki/Berry>

``` r
# Initalize a list that
allberries <- list()
# Query the API for information on each of the original 151 pokemon, and store the information in our poke list
for(i in 1:64){
  allberries[[i]] <- getBerry(id = i)
}
```
