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
   *Note: Some notebook outputs were cleared for privacy protection of the user data.*
## Studying digital trace data as user-sequences 

In this study, we argue for an analytical scheme that *chronologically* orders user interactions with the media environment into one dimension and employs computational approaches to explore the inter-activity dependence that considers individual activities in the contexts of other activities.
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

```bibtex
@article{Fan2026,
   author = {Yangliu Fan and Jakob Ohme and Lion Wedel},
   doi = {10.1080/19312458.2026.2664873},
   issn = {1931-2458},
   journal = {Communication Methods and Measures},
   month = {5},
   pages = {1-28},
   title = {Exploring temporal dynamics in digital trace data: mining user-sequences for communication research},
   url = {https://www.tandfonline.com/doi/full/10.1080/19312458.2026.2664873},
   year = {2026}
}
```

## License
![image](https://user-images.githubusercontent.com/60612969/135886472-567c603e-8001-43e3-a808-f020ba14814d.png)

This work is licensed under a [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-nc-sa/4.0/). 

## Contact information
If you have any questions or suggestions, do not hesitate to [contact us](mailto:yangliu.fan@weizenbaum-institut.de) 😊
