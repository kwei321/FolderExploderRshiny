FolderExploderRshiny 
A utility to explode folder with Rshiny as GUI

### 1.Motivation:
Most of the time, files generated by other software will be in a relatively deep directory. For example, the pdf file you need is inside a folder within a folder on the desktop. Extracting those file without using any programming language is rather time consuming, especially when there are many directories of subdirectory.

I get the inspiration for creating this Rshiny app from my previous internship: I need to parse hundreds of html files, but all the html files are rather deep in the directories; thus I created R scripts to copy all html files from their subdirectories to a single folder for easy manipulation. I called this "folder explosion"

This Rshiny application serve as GUI for performing such tasks, with the option of:

1. Specifying the depth of explosion needed. (0 for simple copy and paste, 1 for copying files and folders from 1 level down from the root directory and so on)

2. Specifying resursive option. When resursive option is chosen, all individual files inside a directory will be exploded to the destination folder.

3. Allowing user to specifying file name of interest, with three options connected with AND condition. The three options are "Start with", "contains", "end with".

For example, start with "Accounting", contains "July", "csv" will explode files with name starting with the word "Accounting", AND contains the word "July", with csv as file extension.


### 2. Instruction to use the app:
	2.1. Install R and Rstudio
	2.2. Start the application
	 - Option 1: Download the repository and run the app using Rstudio
	 - Option 2: install shiny package in R and use the function runGithub:

```r
if (!require('shiny')) install.packages("shiny")
shiny::runGitHub(repo = "FolderExploderRshiny", username = "kwei321")
```
	 
	2.3. Enter the source folder and destination folder. The destination folder must be created manually first. Select the depth of explosion or the resursive option. When recursive option is selected, all individual files inside the source folder will be copied to the destination folder.  
	2.4. If needed, use the advanced option to extract filename with certain pattern
	2.5. Hit the blue Explode Folder button. 
	2.6. Check you destination folder for the exploded file and folder
	
	
### 3. Additional note
	3.1. This application will not change anything in the source folder location. 
	3.2. If there are duplicated files or folder after explosion, a numerical identifier will be added to the file name (similar to how Windows treat duplicates.
	3.3. This app is created using Windows. It is not tested on Linux system. 

### 4. Examples: 
**4.1 Example 1**

**Source Folder Structure**

	│  dummyfile
	│  screenshot.pdf
	│
	├─dummy1
	│  │  dummy1 textfile.txt
	│  │  dummy1 textfile2.txt
	│  │  New Text Document.txt
	│  │
	│  └─New folder
	│          Accounting August.txt
	│          Accounting July.txt
	│
	└─dummy2
	    │  dummy2 important.xlsx
	    │  dummy2 Rshiny.html
	    │  New Text Document.txt
	    │
	    └─New folder
		    important dummy.xlsx
		    New Text Document.txt
		    Rshiny.html
	    
**Destination Folder Structure after exploding by depth of 1**

	│  dummy1 textfile.txt
	│  dummy1 textfile2.txt
	│  dummy2 important.xlsx
	│  dummy2 Rshiny.html
	│  New Text Document (1).txt
	│  New Text Document.txt
	│
	├─New folder
	│      Accounting August.txt
	│      Accounting July.txt
	│
	└─New folder (1)
		important dummy.xlsx
		New Text Document.txt
		Rshiny.html


**4.2 Example 2**

**Source folder structure same as above**
	
**Destination Folder Structure after exploding by depth of 2**

    Accounting August.txt
    Accounting July.txt
    important dummy.xlsx
    New Text Document.txt
    Rshiny.html

No subfolders exist


**4.3 Example 3**

**Source folder structure same as above**

**Destination Folder Structure after exploding by depth of 2, and subseting file contains the word "accounting" and ends with ".txt"**

    Accounting August.txt
    Accounting July.txt

No subfolders exist


