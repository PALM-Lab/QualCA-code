# _QualCA_ – Qualitative Coding App

**_QualCA_** (pronounced _quokka_, like the small Australian marsupial) is a lightweight in-browser _R Shiny_ app for extracting and coding texts for qualitative analysis.

This repository contains the R Shiny code that is deployed at https://palm-lab.github.io/QualCA. You can run the ShinyApp locally within R / RStudio, making changes to the underlying code as needed. One advantage of running the app on your local device is that a temporary codebook file is automatically saved as a back-up for any unexpected crashes.

# How to use the app

Open the app in R Studio by opening the code file (`app.R`). A **Run App** button should appear in the top right of the code editor.

1. Organize your **corpus** as a CSV file, such that the each **document** is a different row and the **document text** is contained in one column.
2. Upload your **corpus** CSV to **_QualCA_** using the button on the left-side menu. A menu will appear below to select the CSV column that contains the to-be-coded text, in case the corpus contains mutliple columns.
3. If you are returning to the **_QualCA_** app, you can upload your previously saved codebook CSV using the button on the left-side menu to continue your analysis.
4. You can scroll through the documents by pressing the 'previous' or 'next' buttons in the _Document Viewer_ pane of the app. You can also type in a numeric value into the _Document #_ bar to navigate to that document.
5. To add an extract to the codebook, highlight the text in the document using your cursor and press the 'Add Selected Text as Extract' button in the _Codebook_ pane. When clicked, the highlighted text will appear in the _Extract_ column, and will be highlighted in blue in the _Document Viewer_. 
6. You can then add a Code or Theme to an extract by double clicking on the relevant cell within the codebook table, and typing the new Code.
7. A _Counter_ pane in the top-right of the app shows how many extracts are associated with each Code. You can edit a Code by double clicking on it in the _Counter_, and typing in the new Code. This will change the Code for all extracts associated with the old Code.
8. To save your progress, you can click the _Download Codebook_ button in the left-side menu. The Codebook is saved as a CSV file, which you can upload to **_QualCA_** on your next visit.

# Acknowledgements

The **_QualCA_** app is [hosted on Github Pages at https://palm-lab.github.io/QualCA](https://palm-lab.github.io/QualCA) using the [shinylive](https://posit-dev.github.io/r-shinylive/) package, and makes use of this [helpful StackOverflow answer by user GGamba](https://stackoverflow.com/questions/42274461/can-shiny-recognise-text-selection-with-mouse-highlighted-text). 

This app was created for an ongoing research project with Carly Stagg, Natasha van Antwerpen and Ella Moeck, who brought knowledge on how to conduct qualitative analysis, shared what would be desirable features, and tested earlier versions of the app.

[Clinton Hadinata](https://github.com/hadinata) provided debugging help and useful advice on how to handle overlapping intervals.

William Ngiam created this app while employed as a Lecturer in the School of Psychology at the University of Adelaide.