# R Data MVP Project

This repository is a skeleton project for R MVP development.  

The included example-project shows how to build a basic Shiny app within an R package, as well as generating documentation and managing external dependencies.

## Getting Started

First, you need to have the command 'make' and  Docker setup on your machine. 

Make allows us to automate building commands and Docker will provide a standard environment so that we guarantee the same behavior on Windows, MacOS, or running in the cloud.

Clone this repository onto your development machine. 
```
git clone git@github.com/bain/rstats-data-mvp
```

Next, inside the newly created folder you can run 
```bash
make build
```
This will run through the build process and create a new docker with our base project inside.

We can test that everything worked correctly by running the following command to get into the R REPL
```
docker run -it rstats-data-mvp R
```
If you see an interactive R shell which reports its version, then everything is working.  
You can play around with basic R commands and finish by typing `quit()`

## Installing Dependencies

Typically packages are installed in R using the install.packages() function. Packages that are hosted on [CRAN](https://cran.r-project.org/) are recommended as there is a high standard required of packages to be hosted on CRAN.
This example project manages R dependencies through the package structure. Dependencies are listed under the `Imports:` header of the DESCRIPTION file. Dependencies can be added to this section in order to 
include them in the package. 

## Run app
To run the app
```
make shiny
```
and it will be served at localhost:8787.

## Coding
When you update configuration you must rerun the `make build` command but when you update code it will be immediate because the docker mounts the folder so all code is shared in realtime.
To see code changes updated in realtime first run
```
make dev
```
and you should be given a shell inside the docker in the directory /var/app.  If you `ls` you should see the files which are part of the project. 

Tests can be run using the devtools packages as follows:
```
Rscript -e "devtools::test()"
```

You can launch the shiny app with this command
```
Rscript -e "testpackage::runExample()"
```
The app will then be available at localhost:8787 in your browser



