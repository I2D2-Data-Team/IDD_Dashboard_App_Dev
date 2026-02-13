---
title: "stylng"
output: html_document
---

# IDD_Dashboard_Demographic
Iowa Data Drive dashboard for demographic indicators

## CSS

### Some tips how to target specific elements

Shiny allows to modify some default options using `tags$head` command. It includes changing of styling using `tags$style` under the `tags$head` command. Those changes will be applied to the whole project.

`tags$style` allows to specif CSS for different elements using element **class** or **id**.  

- To refer to class you need to use period (`.`) followed by name of that class.  
- While to refer to an element by ID you need to use pound symbol (`#`) followed by id name.  

Below is an example when I tried to change outline of all plots to blue using element class:

```         
  /* Target the default class for all plotOutput elements */
  .shiny-plot-output {
    border: 2px solid #007bff; /* Example: blue solid border */
    border-radius: 5px;       /* Optional: rounded corners */
    box-shadow: 3px 3px 5px #aaa; /* Optional: subtle shadow */
    padding: 10px;            /* Optional: space inside the border */
  }
```

Eaxmple below shows how I can set up color for drop-down selectors using element id:

```
  /* Use the ID to target only the desired input */
  #my-special-selector .selectize-input {
    font-size: 11px;
    line-height: 1.1;
    background-color: #17a2b8; /* Slate blue */
    border: 2px solid #243f69; /* Darker blue border */
  }
  #my-special-selector .selectize-dropdown {
    font-size: 11px;
    line-height: 1.1;
    background-color: #f8d7da;
    border: 2px solid #243f69;
  }
  #my-special-selector .control-label {
    font-size: 12px;
    font-weight: lighter;
    line-height: 1.0;
  }
  
```
  
Here I am trying to create a container that will be of the same size
  
```
  .flex-container {
    display: flex;
    justify-content: space-around; /* Distribute items with space around them */
    align-items: center; /* Vertically center items */
    gap: 20px; /* Add a gap between the elements */
  }
  .flex-item {
    flex: 1; /* Allow items to grow and take up equal space */
  }
```
