# DTD_sequence_mining


## Repository structure
This is the repository for the article "Exploring temporal dynamics in digital trace data: mining user-sequences for communication research" and contains the following elements:
  
- **Code ✔**: contains Python and R code used to wrangle, analyze, and visualize the data described in our manuscript, including six approaches: 
    - (1) sequence analysis,
    - (2) event history analysis,
    - (3) hidden Markov models,
    - (4) network analysis,
    - (5) process mining models,
    - (6) language-based models.
- **Figures ✔**: includes visualizations generated from our analyses. 
- **Data ❓**: Due to the sensitive nature of our dataset, we are unable to provide direct access to the data used in this study. We include a detailed description in the manuscript and encourage researchers to apply our framework and code to your own datasets.

## Studying digital trace data as user-sequences 

In this study, we propose a new **research framework** enabled by **the exploding supply of digital trace data (DTD)**, alongside **various approaches to analyzing them**. 
- We define our framework as an analytical scheme that *chronologically* orders user interactions with the media environment into one dimension and employs computational approaches to explore the inter-activity dependence that considers individual activities in the contexts of other activities.
- Specifically, it represents the evolving user activities on digital platforms as user-sequences, which provide detailed information about user activity with high temporal resolution (see the Figure below for an example of the proposed individual-level representation). 

![image](https://github.com/user-attachments/assets/46528fc6-97bd-422e-be22-12125771b36c)

## Installation

To install the required packages, run the following command:

```bash
pip install -r requirements.txt
```

## Usage

To use the code for your analyses, navigate to the `code` folder and launch Jupyter Notebook:

```bash
cd code
jupyter notebook
```

## Contributing 🤗

Contributions to this project are more than welcome! Please submit any pull requests or issues! 🙏


## Reference

More information about our method can be found in the following article:

If you use any of the provided material for your work, please cite us as follows:

## License
![image](https://user-images.githubusercontent.com/60612969/135886472-567c603e-8001-43e3-a808-f020ba14814d.png)

This work is licensed under a [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-nc-sa/4.0/). 

## Contact information
If you have any questions or suggestions, do not hesitate to contact us 😊
