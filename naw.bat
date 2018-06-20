:: naw.bat

:: Control Batch File for the 'nesrea_awareness' RStudio Project
@ECHO OFF

SETLOCAL ENABLEEXTENSIONS

SET this=%~n0
SET app=--shiny

:: Launch the Shiny App
IF "%app%"=="--shiny" (
    Rscript.exe --vanilla src/launch.R
)
