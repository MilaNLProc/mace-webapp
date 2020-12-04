.PHONY: help build dev integration-test push
.DEFAULT_GOAL := help

# Docker image build info
PROJECT:=mace-webapp
BUILD_TAG?=first

ALL_IMAGES:=src

help:
# http://marmelab.com/blog/2016/02/29/auto-documented-makefile.html
	@echo "R starter project"
	@echo "====================="
	@echo "Replace % with a directory name (e.g., make build/rstats-example)"
	@echo
	@grep -E '^[a-zA-Z0-9_%/-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

########################################################
## Local development
########################################################

build: ## Build the latest image
	docker build --target main --progress=plain -t $(PROJECT):${BUILD_TAG} .

test: ## Run tests
	docker build --target test --progress=plain -t $(PROJECT):test .

exec: DARGS?=-v $(PWD):/opt/app
exec: ## Exec into the container
	docker run -it --rm $(DARGS) $(PROJECT):${BUILD_TAG} bash

shiny: DARGS?=-v $(PWD):/opt/app -p 8787:8787
shiny: ## Run shiny
	docker run -d --name maceapp_container -it --rm $(DARGS) $(PROJECT):${BUILD_TAG}
