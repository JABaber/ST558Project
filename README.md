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
getPokemon <- function(pokemon = FALSE, id = FALSE){
  # If a pokemon argument was provided
  if(pokemon != FALSE){
    # Check if the pokemon argument is a string
    if(is.character(pokemon) == FALSE){
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
  if(id != FALSE){
    # Check if the id argument was numeric
    if(is.numeric(id) == FALSE){
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
# Define getBerry function, can be done with string or id number
getBerry <- function(berry = FALSE, id = FALSE){
  # If a berry argument was provided
  if(berry != FALSE){
    # Check if the berry argument is a string
    if(is.character(berry) == FALSE){
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
  if(id != FALSE){
    # Check if the id argument was numeric
    if(is.numeric(id) == FALSE){
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
# Initalize a list that
poke <- list()
# Query the API for information on each of the original 151 pokemon, and store the information in our poke list
for(i in 1:151){
  poke[[i]] <- getPokemon(id = i)
}
```

``` r
# Create a names vector
names <- vector()
# For each of the 151 pokemon, grab the name variable from that entry and put it in our names vector
for(i in 1:151){
  names[i] <- poke[[i]]$name
}
# Create a height vector
height <- vector()
# For each of the 150 pokemon, grab the height variable from that entry and put it in our height vector
for(i in 1:151){
  height[i] <- poke[[i]]$height
}
# Create a data frame that has the heights for each of the first 150 pokemon and print it.
heights <- data.frame(names, height)
heights
```

    ##          names height
    ## 1    bulbasaur      7
    ## 2      ivysaur     10
    ## 3     venusaur     20
    ## 4   charmander      6
    ## 5   charmeleon     11
    ## 6    charizard     17
    ## 7     squirtle      5
    ## 8    wartortle     10
    ## 9    blastoise     16
    ## 10    caterpie      3
    ## 11     metapod      7
    ## 12  butterfree     11
    ## 13      weedle      3
    ## 14      kakuna      6
    ## 15    beedrill     10
    ## 16      pidgey      3
    ## 17   pidgeotto     11
    ## 18     pidgeot     15
    ## 19     rattata      3
    ## 20    raticate      7
    ## 21     spearow      3
    ## 22      fearow     12
    ## 23       ekans     20
    ## 24       arbok     35
    ## 25     pikachu      4
    ## 26      raichu      8
    ## 27   sandshrew      6
    ## 28   sandslash     10
    ## 29   nidoran-f      4
    ## 30    nidorina      8
    ## 31   nidoqueen     13
    ## 32   nidoran-m      5
    ## 33    nidorino      9
    ## 34    nidoking     14
    ## 35    clefairy      6
    ## 36    clefable     13
    ## 37      vulpix      6
    ## 38   ninetales     11
    ## 39  jigglypuff      5
    ## 40  wigglytuff     10
    ## 41       zubat      8
    ## 42      golbat     16
    ## 43      oddish      5
    ## 44       gloom      8
    ## 45   vileplume     12
    ## 46       paras      3
    ## 47    parasect     10
    ## 48     venonat     10
    ## 49    venomoth     15
    ## 50     diglett      2
    ## 51     dugtrio      7
    ## 52      meowth      4
    ## 53     persian     10
    ## 54     psyduck      8
    ## 55     golduck     17
    ## 56      mankey      5
    ## 57    primeape     10
    ## 58   growlithe      7
    ## 59    arcanine     19
    ## 60     poliwag      6
    ## 61   poliwhirl     10
    ## 62   poliwrath     13
    ## 63        abra      9
    ## 64     kadabra     13
    ## 65    alakazam     15
    ## 66      machop      8
    ## 67     machoke     15
    ## 68     machamp     16
    ## 69  bellsprout      7
    ## 70  weepinbell     10
    ## 71  victreebel     17
    ## 72   tentacool      9
    ## 73  tentacruel     16
    ## 74     geodude      4
    ## 75    graveler     10
    ## 76       golem     14
    ## 77      ponyta     10
    ## 78    rapidash     17
    ## 79    slowpoke     12
    ## 80     slowbro     16
    ## 81   magnemite      3
    ## 82    magneton     10
    ## 83   farfetchd      8
    ## 84       doduo     14
    ## 85      dodrio     18
    ## 86        seel     11
    ## 87     dewgong     17
    ## 88      grimer      9
    ## 89         muk     12
    ## 90    shellder      3
    ## 91    cloyster     15
    ## 92      gastly     13
    ## 93     haunter     16
    ## 94      gengar     15
    ## 95        onix     88
    ## 96     drowzee     10
    ## 97       hypno     16
    ## 98      krabby      4
    ## 99     kingler     13
    ## 100    voltorb      5
    ## 101  electrode     12
    ## 102  exeggcute      4
    ## 103  exeggutor     20
    ## 104     cubone      4
    ## 105    marowak     10
    ## 106  hitmonlee     15
    ## 107 hitmonchan     14
    ## 108  lickitung     12
    ## 109    koffing      6
    ## 110    weezing     12
    ## 111    rhyhorn     10
    ## 112     rhydon     19
    ## 113    chansey     11
    ## 114    tangela     10
    ## 115 kangaskhan     22
    ## 116     horsea      4
    ## 117     seadra     12
    ## 118    goldeen      6
    ## 119    seaking     13
    ## 120     staryu      8
    ## 121    starmie     11
    ## 122    mr-mime     13
    ## 123    scyther     15
    ## 124       jynx     14
    ## 125 electabuzz     11
    ## 126     magmar     13
    ## 127     pinsir     15
    ## 128     tauros     14
    ## 129   magikarp      9
    ## 130   gyarados     65
    ## 131     lapras     25
    ## 132      ditto      3
    ## 133      eevee      3
    ## 134   vaporeon     10
    ## 135    jolteon      8
    ## 136    flareon      9
    ## 137    porygon      8
    ## 138    omanyte      4
    ## 139    omastar     10
    ## 140     kabuto      5
    ## 141   kabutops     13
    ## 142 aerodactyl     18
    ## 143    snorlax     21
    ## 144   articuno     17
    ## 145     zapdos     16
    ## 146    moltres     20
    ## 147    dratini     18
    ## 148  dragonair     40
    ## 149  dragonite     22
    ## 150     mewtwo     20
    ## 151        mew      4

``` r
cheri <- getBerry(berry = "cheri")
```

``` r
cheri$natural_gift_type$name
```

    ## [1] "fire"

<https://bulbapedia.bulbagarden.net/wiki/Berry>

``` r
# Initalize a list that
allberries <- list()
# Query the API for information on each of the original 151 pokemon, and store the information in our poke list
for(i in 1:64){
  allberries[[i]] <- getBerry(id = i)
}
```

``` r
# Define getPokeInfo function, can be done with string or id number
getPokeInfo <- function(pokemon = FALSE, id = FALSE){
  # If a pokemon argument was provided
  if(pokemon != FALSE){
    # Check if the pokemon argument is a string
    if(is.character(pokemon) == FALSE){
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
  if(id != FALSE){
    # Check if the id argument was numeric
    if(is.numeric(id) == FALSE){
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
# Initalize a list that
morePokeInfo <- list()
# Query the API for information on each of the original 151 pokemon, and store the information in our poke list
for(i in 1:151){
  morePokeInfo[[i]] <- getPokeInfo(id = i)[c("capture_rate", "is_baby", "is_legendary", "is_mythical")]
}
```

``` r
fullPoke <- list()
for(i in 1:151){
  fullPoke[[i]] <- c(poke[[i]], morePokeInfo[[i]])
}
```
