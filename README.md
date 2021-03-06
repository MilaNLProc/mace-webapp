# R Data MVP Project

This repository is a skeleton project for R MVP development.  

The included example-project shows how to build a basic Shiny app within an R package, as well as generating documentation and managing external dependencies.

## Installing Docker on CentOS 7 With Yum
Installing from Docker repositories using the yum command is the easiest and most popular method. For more info, follow [this](https://phoenixnap.com/kb/how-to-install-docker-centos-7) guide (basically, copy-pasted from there).

### Step 1: Update Docker Package Database
In a terminal window, type:

	sudo yum check-update

Allow the operation to complete.

### Step 2: Install the Dependencies
The next step is to download the dependencies required for installing Docker.

Type in the following command:

	sudo yum install -y yum-utils device-mapper-persistent-data lvm2

The –y switch indicates to the yum installer to answer “yes” to any prompts that may come up. The yum-utils switch adds the yum-config-manager. Docker uses a device mapper storage driver, and the device-mapper-persistent-data and lvm2 packages are required for it to run correctly.

### Step 3: Add the Docker Repository to CentOS
To install the edge or test versions of Docker, you need to add the Docker CE stable repository to your system. To do so, run the command:

	sudo yum-config-manager --add-repo https://download.docker.com/linux/centos/docker-ce.repo

### Step 4: Install Docker On CentOS Using Yum
With everything set, you can finally move on to installing Docker on CentOS 7 by running:

	sudo yum install docker

The system should begin the installation. Once it finishes, it will notify you the installation is complete and which version of Docker is now running on your system.

### Step: 5 Manage Docker Service
Although you have installed Docker on CentOS, the service is still not running.

To start the service, enable it to run at startup. Run the following commands in the order listed below.

Start Docker:

	sudo systemctl start docker

Enable Docker:

	sudo systemctl enable docker

Check the status of the service with:

	sudo systemctl status docker

### Bonus. Install a Specific Version of Docker on CentOS
To install a specific version of Docker, start by listing the available releases.

Type the following in your terminal window:

	yum list docker-ce --showduplicates | sort –r

The system should give you a list of different versions from the repositories you have enabled above.

Install the selected Docker version with the command:

	sudo yum install docker-ce-<VERSION STRING>

### Remove docker
To remove docker run

	sudo yum remove docker docker-common docker-selinux docker-engine 

### Fix: Docker got permission denied issue
Run the code below to fix the permission to access docker.socker issue

	sudo chmod 666 /var/run/docker.sock

### Run Docker container in the background
To run a Docker container in the background, use the use `-d=true` or just `-d` option.

To list all containers, run the following command (default shows just running).

	docker ps -a

In addition, to reattach to a detached container, use docker attach command.

	docker attach --name mycontainer

or 

	docker attach container_id

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



# MACE (Multi-Annotator Competence Estimation)

When evaluating redundant annotations (like those from Amazon's MechanicalTurk), we usually want to 
1. aggregate annotations to recover the most likely answer
2. find out which annotators are trustworthy
3. evaluate item and task difficulty

MACE solves all of these problems, by learning competence estimates for each annotators and computing the most likely answer based on those competences.


## 1 Usage

(lines starting with '$' denote command line input)

### Shell script:

`$./MACE [options] <CSV input file>`
			
### Java

`$java -jar MACE.jar [options] <CSV input file>`

If you have trouble running the shell script, you might want to modify the script by adding your classpath and/or minimum and maximum heap space information.
	
MACE runs Variational Bayes EM training by default. If you would like vanilla EM training, set --em 


### Options:
```
	--controls <FILE>:	supply a file with annotated control items. Each line corresponds to one item,
				so the number of lines MUST match the input CSV file.
				The control items serve as semi-supervised input. Controls usually improve accuracy.

	--alpha <FLOAT>:	first hyper-parameter of beta prior that controls whether an annotator knows or guesses. Default:0.5, if --beta is set

	--beta <FLOAT>:		second hyper-parameter of beta prior that controls whether an annotator knows or guesses. Default:0.5, if --alpha is set

	--distribution:		for each items, list all labels and their probability in '[prefix.]prediction'

	--entropies:		write the entropy of each instance to a separate file '[prefix.]entropy'

	--help:			display this information

	--iterations <1-1000>:	number of iterations for each EM start. Default: 50

	--prefix <STRING>:	prefix used for output files.

	--priors <FILE>:	file with one label and weight pair per line (tab-separated). Must include all labels
		 		     in data file. Weights will be automatically normalized

	--restarts <1-1000>:	number of random restarts to perform. Default: 10

	--smoothing <0.0-1.0>:	smoothing added to fractional counts before normalization.
				Higher values mean smaller changes. Default: 0.01/|values|

	--test <FILE>:		supply a test file. Each line corresponds to one item in the CSV file,
				so the number of lines must match. If a test file is supplied,
				MACE outputs the accuracy of the predictions

	--threshold <0.0-1.0>:	only predict the label for instances whose entropy is among the top n%, ignore others.
				Thus '--threshold 0.0' will ignore all instances, '--threshold 1.0' includes all.
				This improves accuracy at the expense of coverage. Default: 1.0
```

## 2 Inputs

### Input File
The **input file** has to be a comma-separated file, where each line represents an item, and each column represents an annotator. Since version 0.3, MACE can also handle blank lines, as you might have when annotating sequential data (each word on one line, sentences separated by a blank line).

Missing annotations by an annotator on an item are represented by the empty string. Files should be formatted in UTF-8 to avoid problems with newline characters.
	
Examples:

1.: File with binary decisions:
```
0,1,,,,1,0,0
,,1,1,,0,0,1
1,0,0,1,,1,,0
```

2.: File with sequential POS annotations:
```
NOUN,,,NOUN,PRON
VERB,VERB,,VERB,

ADJ,,ADJ,,ADV
,VERB,,VERB,ADV
NOUN,,,NOUN,PRON
```	

Make sure the last line has a line break!


### Label Priors
The **prior** file is optional, and gives the a priori prevalence of the individual labels. We can supply this to MACE with `--priors priors.tsv` if we know the prior distribution, and it will take them into account. The file needs to list all labels (one per line) and tab-separated the weight, probability, or frequency (MACE automatically normalizes these).

Example:
```
NOUN	30
VERB	30
ADJ	20
ADV	10
PRON	10
```

### Control Items
If we know the correct answer for some items, we can include **control items**. This helps MACE assess annotator reliability. The file with control items needs to have the same number of lines as the input file, with the correct specified for the control items.

Example:
```
PRON






```


### Test File
If we know *all* answers and only want to get an accuray for MACE, we can supply a **test file** via `--test test.txt`. This file must have the same number of lines as the input file. MACE will output an accuracy score. This will not work when `--distribution` is set!

Example:
```
PRON
VERB

ADJ
VERB
NOUN
```


## 3 Outputs

MACE provides two standard output files:
* the most likely answer **prediction** for each item, `[prefix.]prediction`. This file has the same number of lines as the input file. Each line is the most likely answer value for the corresponding item. If you set --distribution, each line contains the distribution over answer values sorted by entropy. In the POS example from above, these files would look like this:
```
NOUN
VERB

ADJ
VERB
NOUN
```

or the full label distributions

```
NOUN 0.9997443833265887	PRON 7.140381903855615E-5	ADJ 6.140428479093134E-5	VERB 6.140428479093134E-5	ADV 6.140428479093134E-5
VERB 0.9999961943848287	NOUN 9.514037928812883E-7	ADJ 9.514037928812883E-7	PRON 9.514037928812883E-7	ADV 9.514037928812883E-7

ADJ 0.9990184050335877	ADV 2.741982824057974E-4	NOUN 2.3579889466878394E-4	VERB 2.3579889466878394E-4	PRON 2.3579889466878394E-4
VERB 0.9994950838119411	ADV 1.4104305366466138E-4	NOUN 1.2129104479807625E-4	ADJ 1.2129104479807625E-4	PRON 1.2129104479807625E-4
NOUN 0.9997443833265887	PRON 7.140381903855615E-5	ADJ 6.140428479093134E-5	VERB 6.140428479093134E-5	ADV 6.140428479093134E-5
```

* the **competence estimate** for each annotator, `[prefix.]competence`. This file has one line with tab separated values. In the POS example from above, this would be

```0.8820970950608722  0.7904155783217401		0.6598575839917008 0.8822161621354134	 0.03114062354821738```

Here, the first four annotators are fairly reliable, but the 5th one is not.


* In addition, you can output the **entropy** of each item by setting `--entropies`. This will output a file with the same number of lines as the input file, named `[prefix.]entropy`. The output looks like this:
```
0.0027237895900081095
5.657170773284981E-5

0.009138546784668605
0.005036498835041038
0.0027237895900081095
```
Higher entropy denotes more difficult items. Here, the first line after the break is the most difficult.


## 4 Examples

`$java -jar MACE.jar example.csv`
Evaluate the file example.csv and write the output to "competence" and "prediction".

`$java -jar MACE.jar --prefix out example.csv`
Evaluate the file example.csv and write the output to "out.competence" and "out.prediction".

`$java -jar MACE.jar --prefix out --distribution example.csv`
Evaluate the file example.csv and write the output to "out.competence" and "out.prediction". For each item, show the distribution over answer values sorted by entropy.

`$java -jar MACE.jar --test example.key example.csv`
Evaluate the file example.csv against the true answers in example.key. 
Write the output to "competence" and "prediction" and print the accuracy to STDOUT (acc=0.8)

`$java -jar MACE.jar --threshold 0.9 example.csv`
Evaluate the file example.csv. Return predictions only for the 90% of items the model is most confident in (acc=0.84). 
Write the output to "competence" and "prediction". The latter will have blank lines for ignored items. 

`$java -jar MACE.jar --threshold 0.9 example.csv`
Evaluate the file example.csv. Return predictions only for the top 90% of items the model is most confident in. 
Write the output to "competence" and "prediction". The latter will have blank lines for ignored items. 
Compute the accuracy of only the predicted items and write to STDOUT.


## 5 References

To cite MACE in publications, please refer to:
* *Dirk Hovy, Taylor Berg-Kirkpatrick, Ashish Vaswani, and Eduard Hovy* (2013): **Learning Whom to Trust With MACE**. In: Proceedings of NAACL-HLT. [[PDF]](http://www.aclweb.org/anthology/N13-1132)

```bib
@inproceedings{hovy2013learning,
  title={Learning whom to trust with MACE},
  author={Hovy, Dirk and Berg-Kirkpatrick, Taylor and Vaswani, Ashish and Hovy, Eduard},
  booktitle={Proceedings of the 2013 Conference of the North American Chapter of the Association for Computational Linguistics: Human Language Technologies},
  pages={1120--1130},
  year={2013}
}
```

There is an additional paper that compares MACE with some other models:
* *Silviu Paun, Bob Carpenter, Jon Chamberlain, Dirk Hovy, Udo Kruschwitz, and Massimo Poesio* (2018): **Comparing Bayesian Models of Annotation**. In: Transactions of the Association for Computational Linguistics (TACL). [[PDF]](http://www.dirkhovy.com/portfolio/papers/download/bma_draft.pdf)

```bib
@article{paun2018comparing,
  title={Comparing Bayesian Models of Annotation},
  author={Paun, Silviu and Carpenter, Bob and Chamberlain, Jon and Hovy, Dirk and Kruschwitz, Udo and Poesio, Massimo},
  journal={Transactions of the Association for Computational Linguistics},
  volume={6},
  pages={571--585},
  year={2018},
  publisher={MIT Press}
}
```
