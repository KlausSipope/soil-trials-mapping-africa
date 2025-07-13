# Soil Trials Mapping Africa 🌍

An interactive Shiny web application for visualizing and analyzing soil trial data across African countries. Built for agronomic research, policy support, and decision-making, this tool allows users to explore soil indicators (e.g., pH, nutrients, texture) by location, depth, crop, and trial type.

---

## 🎯 Purpose

This project supports the analysis and visualization of soil quality across different trials and countries (Mozambique, Tanzania, and Uganda). It aims to facilitate:

- Field-level soil indicator tracking
- Cross-country comparisons of trial results
- Data-driven decision-making for soil health management

---

## 🌐 Live Demo

👉 [Launch the App](https://ksipope.shinyapps.io/map_all_countries/)

---

## 🛠️ Technologies Used

- **R + Shiny**: Interactive web app development
- **Leaflet**: Dynamic mapping
- **sf**: Geospatial data handling
- **rnaturalearth**: Country boundary shapefiles
- **dplyr / readxl / writexl**: Data manipulation and cleaning

---

## 🧪 Features

✅ Filter by:
- Country: Mozambique, Tanzania, Uganda
- Soil depth: Top soil / Subsoil (based on data)
- Trial type: I-FOT, E-FOT, Satellite, FOT, FRS
- Crop: Maize, Beans, Rice, etc.

✅ Color-coded maps by:
- Soil pH
- Macro and micro nutrients (P, K, Ca, Mg, etc.)
- Soil texture (%Sand, %Clay, %Silt)
- Carbon, Nitrogen, CEC, and more

✅ Click markers to view:
- Trial location
- Trial type
- Crop
- Measured value of selected soil variable (e.g., pH)

✅ Dynamic legend generation based on variable

---

## 📂 Project Structure

```text
soil-trials-mapping-africa/
├── app.R                # Shiny app code
├── preprocessing.R      # Script to merge and clean country datasets
├── general_dataset.xlsx # Harmonized dataset used in the app
├── data/                # (Optional) Raw Excel files
├── www/                 # (Optional) Assets (e.g., CSS or images)
└── README.md            # This file
```

---

## 📁 Data Access

⚠️ **Note**: This project was developed during my time at IFDC. The datasets used in the application (soil trial data from Mozambique, Tanzania, and Uganda) are the property of IFDC and are not publicly available through this repository.

If you're interested in accessing the data or exploring collaboration opportunities, please contact IFDC directly.

The code is provided here for educational and demonstration purposes only.

---

## 📘 MIT License

Copyright (c) 2025 Klaus Sipope

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software **excluding any datasets**, without restriction...

...

This license applies to the source code only and does **not grant any rights to accompanying datasets**.
