# little-servant-elm-demo
A little project to demonstrate using Servant with Elm
=======

## Server

Most of the server code can be found in app/ 

The Servant routes are defined in Routes.hs. 
The routes are implemented in Lib.hs.

## Frontend

The Elm code is found in frontend/

## Code generator

The code to generate the Elm code is found in generate/Main.hs.

This creates a seperate executable that uses the type definitions found in src/ to generate the Elm types. It places the generated code in frontend/src/Generated.


