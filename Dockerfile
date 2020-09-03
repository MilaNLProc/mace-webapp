###############################################################################
# main
###############################################################################

FROM rocker/shiny:4.0.2 as main

RUN apt-get update -y && \
    apt-get install -y libssl-dev libxml2-dev libgit2-dev

RUN Rscript -e "install.packages('devtools', repos = 'https://cloud.r-project.org')"

WORKDIR /opt/app

COPY DESCRIPTION .

# NOTE: This step takes ~ 10 mins but it only re-runs when the
# contents of the "DESCRIPTION" file changes.
RUN Rscript -e "devtools::install_deps('/opt/app')" && \
    Rscript -e "devtools::check('/opt/app', error_on = 'error')"

COPY . .

RUN Rscript -e "devtools::install('/opt/app')"

EXPOSE 8787

CMD Rscript -e "testpackage::runExample()"

###############################################################################
# test
###############################################################################

FROM main as test

RUN Rscript -e "devtools::test()"
