---
title: 'regtools' # Any ideas for the title? 
tags:
  - R
  - epidemiology
  - registers 
  - health data
authors:
  - name: 
      given-names: Alejandra
      surname: Martinez Sanchez
    orcid: 0009-0002-8282-6529
    corresponding: true
    affiliation: 1
  - name:
      given-names: Johanne Hagen
      surname: Pettersen
    orcid: 0009-0004-4546-9037
    affiliation: 3
  - name: Helga Ask 
    orcid: 0000-0003-0149-5319
    affiliation: 1
  - name: Alexandra Havdahl 
    orcid: 0000-0002-9268-0423
    affiliation: "1, 4"
  - name: 
      given-names: Laurie John
      surname: Hannigan 
    orcid: 0000-0003-3123-5411
    affiliation: "1, 2"
affiliations:
 - name: PsychGen Centre for Genetic Epidemiology and Mental Health, Norwegian Institute of Public Health
   index: 1
   ror: 046nvst19
 - name: Psychiatric Genetic Epidemiology Group, Research Department, Lovisenberg Diaconal Hospital
   index: 2
   ror: 03ym7ve89
 - name:  Center for Precision Psychiatry, University of Oslo
   index: 3
   ror: 01xtthb56
 - name: PROMENTA Research Center, Department of Psychology, University of Oslo
   index: 4
   ror: 01xtthb56
date: 16 September 2025
bibliography: paper.bib
---

# Summary
<!-- Summary describing the high-level functionality and purpose of the software for a diverse, non-specialist audience -->
The use of health and administrative registers opens the door to new research opportunities, particularly among the Nordic countries, as they provide already collected high-quality data with a large degree of coverage across time. Additionally, the use of already existing registers circumvents the data collection process, thus making research more cost and time effective [@Thygesen_2008]. Moreover, sample size and granularity makes registers widely used in descriptive epidemiological research. For instance, in the field of psychiatric epidemiology, comprehensive health registers can enable both researchers to pragmatically describe the patterns of disease in the population and public-health officials to efficiently allocate health resources. Although individual-based registers are rich sources of information, pre-processing and analyzing these large datasets can be cumbersome and represent a time-consuming challenge to researchers with limited programming experience.

The `regtools` R package is an open-source toolkit designed to aid researchers working with individual-level registry data in Norway prepare and analyze it in the context of descriptive epidemiology. It is designed to facilitate the reproducible use of Norwegian health and sociodemographic registry data, such as income and education information. The package includes functions to validate, filter and link health (diagnostic) and sociodemographic data. With the aim of encouraging reproducibility and transparency, each function creates a log that document the functions' internal data processing, warnings/errors and corresponding outputs. Finally, considering the extensive of use of registers in epidemiology research, `regtools` includes functions intended to help users compute common descriptive epidemiology statistics, such as prevalence and incidence rates, and their visualization.

# Statement of need
<!-- Statement of need section that clearly illustrates the research purpose of the software and places it in the context of related work -->
Currently, epidemiological research in the Nordic countries heavily relies on the use of registry data sources, such as primary and secondary health care registers. Population-based health registers in the Nordic countries have the added advantage of including personal identification numbers, which facilitate the linkage with other data sources and long-term following of individuals. Understandably, research that capitalizes on the characteristics and strengths of this type of data is increasingly more common in the Nordic countries. In the case of Norway, population-based health registers, such as the Norwegian Patient Registry (NPR), are now widely used in a large variety of research projects [@Bakken_2020]. As of 2025, there have been more than 1000 research papers published based on data from the NPR spanning from biomedical cohort studies to public health research [@NorwegianInstituteofPublicHealth_2024]. Whether they are utilized as stand-alone data sources or as auxiliary information, it is clear that population-based health registers represent an important resource for current medical and epidemiological research. Similarly, microdata from national statistical institutes (NSIs) is widely used as auxiliary information in epidemiological research. In the case of Norway, Statistics Norway (SSB) provides sociodemographic individual-level data from various topics, such as social welfare, education and income. However, it is often the case that sociodemographic and health registers are not designed with research or statistical purposes in mind.

Considering the wide range of researchers using individual-level registers in Norway, it is unsurprising that there are differences in the way researchers pre-process and prepare their data for analysis. In addition, in most research fields, access to individual-level data or microdata is regulated by strict confidentiality laws which makes training or tutorials hard to access and standardize. Similarly, the use of proprietary software to manipulate and analyze the data further hinders efforts to increase reproducibility and transparency across research projects working with the same microdata [@Mathur_2023]. In this context, we have identified the need for an open-source toolkit to aid researchers working with Norwegian individual-level registry data to manipulate and analyze it in a transparent and reproducible way.

Although projects like `phenotools` [@Hannigan_2021] and the set of packages `csverse` [@White_2025] have showcased the usefulness of using open-source R packages to aid general researchers working with Norwegian survey and register data, the `regtools` package is the first to focus on larger individual-level data with descriptive epidemiological analyses in mind. As an example of the likely use-case of the package, `regtools` has been successfully used to perform analyses included in the Thematic Issue of Norwegian Public Health Report [@MartinezSanchez_2025] and can potentially be used to more efficiently update similar descriptive epidemiological studies based on Norwegian registers [@Suren_2013; @Suren_2019b]. Furthermore, the workflow and functions presented in the package can contribute to current efforts to improve the field and tools used in psychiatric epidemiology to pragmatically describe the occurrence of mental health disorders [@McGrath_2018].

One of the most common challenges of working with population-based registers is how to efficiently manipulate very large datasets into smaller and tidier datasets. The `regtools` package includes reading and filtering functions that support parquet files, thus seamlessly enabling users to efficiently work with larger-than-memory files without requiring deeper knowledge on the inner workings of parquet objects. Furthermore, the logs created by each function can help researchers to better keep track and document any manipulation or processing done on their datasets. There are also some specific challenges related to Norwegian registry data that are also addressed in the helper functions of `regtools`, such as harmonizing municipality codes and getting population counts from SSB's open data.

As mentioned before, due to confidentiality concerns there is a lack of information on how to efficiently work with individual-level registry data for epidemiological research. The functions in this package are intended to serve as a loose framework to guide researchers working with similar data and research questions. The package includes a series of vignettes explaining the main functions, and real-life examples in descriptive epidemiology that can be addressed by using the `regtools` package. The vignettes and possibility of creating synthetic individual-level datasets (`synthetic_data()`) also allow research-groups to use the package as low-risk training material for new members.

# Acknowledgements

<!-- Acknowledgment of any financial support, grant numbers? -->
