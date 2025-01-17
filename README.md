# Computer Vision Repeatability v02

This repository contains the code, data, and analysis scripts for the project titled **"Data-Centric AI Using Experimental Training Data."** The study focuses on the sensitivity analysis of errors in training dataset labeling, specifically examining potato stem box center distance and stem count, and their effects on training an object detection model.

## Table of Contents
- [Overview](#overview)
- [Repository Structure](#repository-structure)
- [Usage](#usage)
- [Experiments](#experiments)
- [Contact](#contact)

---

## Overview
The project aims to analyze the impact of labeling errors on the performance of object detection models. It includes:
- A dataset of potato plant images with bounding box annotations for each plant stem.
- Experiments assessing the repeatability of bounding box placements by different subjects on live and computer images.
- Sensitivity analyses using YOLOv5 to evaluate how variations in bounding box accuracy and quantity affect detection performance.

---

## Repository Structure
The repository is organized as follows:

- `Exp0-repeatability/`: Contains data and scripts related to the repeatability experiment, where subjects placed bounding boxes on live and computer images.
- `Exp1-data-dist/`: Includes analyses focusing on the distribution of data and its impact on model training.
- `Exp2-data-stems/`: Encompasses experiments related to the number of stems annotated and their influence on detection performance.
- `coco128/`: A subset of the COCO dataset used for benchmarking and comparison purposes.
- `colab-notebooks/`: Jupyter notebooks designed for execution in Google Colab, facilitating reproducibility and ease of use.
- `stems-joe/`: Contains additional stem images and annotations provided by contributor Joe Mhango.
- `yaml-files/`: Configuration files in YAML format used for setting up experiments and model parameters.
- `__master_trainer.ipynb`: Jupyter Notebook serving as the primary script for training the object detection models.
- `new_master.ipynb`: An updated version of the master training script with refined methodologies.
- `objectives.pptx`: Presentation outlining the objectives and goals of the project.
- `.gitattributes`, `.gitignore`: Standard Git configuration files.
- `README.md`: This document providing an overview and guidance for the repository.

---

## Usage
Data Preparation
Place your dataset in the appropriate experiment folder (Exp0-repeatability/, Exp1-data-dist/, or Exp2-data-stems/).
Use the preprocessing scripts provided in each experiment folder to prepare the data for training.
Model Training
Execute the __master_trainer.ipynb or new_master.ipynb notebooks to train the object detection models.
Adjust the parameters in the YAML configuration files as needed to suit your experimental setup.
Evaluation
After training, use the evaluation scripts within each experiment folder to assess model performance.
Results, including metrics and visualisations, will be saved in the respective experiment directories.

## Experiments
**Exp0: Repeatability**
Investigates the consistency of bounding box placements by different labellers on live versus computer images.
**Exp1: Data Distribution**
Analyses how the placemnet of bounding boxes affects the performance and robustness of the object detection model.
**Exp2: Data Stems**
Explores the impact of the number of annotated stems on model accuracy.

Contact
For any questions or collaboration inquiries, please contact:

Georgina Anna Wager
Data Scientist at Harper Adams University
GitHub Profile
Twitter: @wager_georgina

