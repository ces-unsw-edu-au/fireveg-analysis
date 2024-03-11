# Jupyter Lab

These are the steps I followed to configure a Jupyter Lab environment.

## create and activate python environment

### with venv

```sh
conda deactivate
mkdir -p $HOME/venv
python3 -m venv $HOME/venv/jptr
source $HOME/venv/jptr/bin/activate
```

Check python version
```sh
python --version
```

Update and install modules
```sh
pip install --upgrade pip
#/usr/local/opt/python@3.9/bin/python3.9 -m pip install --upgrade pip
pip3 install jupyterlab

```


### Conda
Create a new environment with conda:

```sh
conda create --name jptr
```

Activate the environment and install R (with RPostgreSQL package) and jupyter lab:

```{bash}
conda activate jptr
conda install -c conda-forge r-rpostgresql r-readxl devtools
conda install -c conda-forge jupyterlab
```

## Activate the right R kernel...

Within the environment

```{r}
#R --vanilla
install.packages("IRkernel")
IRkernel::installspec()
```

## Install additional libraries

### In Python

Install python libraries with pip

```{bash}
pip install openpyxl psycopg2-binary
pip install pandas SQLAlchemy
pip install pybtex
#pip install postgis
```
### In R

```{bash}
install.packages("remotes")
remotes::install_github("AtlasOfLivingAustralia/galah")
```

## Start the jupyter lab interface:

```sh
cd ~/proyectos/fireveg/fireveg-analysis
if [ -e $HOME/venv/jptr ]
then 
    source $HOME/venv/jptr/bin/activate
elif [ -e $HOME/proyectos/venv/ ]
then 
    source $HOME/proyectos/venv/jupyterlab/bin/activate
else 
    conda activate jptr
fi 
jupyter-lab
```

## Install R packages

```{r}
install.packages("ozmaps")
#install.packages("remotes")
remotes::install_github("AtlasOfLivingAustralia/galah")
```


### V.PhyloMaker

From this repo: https://github.com/jinyizju/V.PhyloMaker

```{r}
install.packages("devtools")
devtools::install_github("jinyizju/V.PhyloMaker")
install.packages("BiocManager")
BiocManager::install("ggtree")
install.packages("ggnewscale")
```


### Treemapify

```{r}
install.packages("treemapify")
## devtools::install_github("wilkox/treemapify")
```

### rWCVP

This is a companion R package for the World Checklist of Vascular Plants
Citation: https://doi.org/10.1111/nph.18919

```{r}
install.packages("rWCVP")
remotes::install_github('matildabrown/rWCVPdata')
```

## Database connection

We store basic database credentials in a hidden file in the `secrets` folder, but password information need to be stored in a `.pgpass` file in the home directory to avoid exposing data in the jupyter notebook.