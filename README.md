<h1 align="center">
Grupo Bimbo Inventory Demand
</h1>

<h4 align="center">
Building a machine learning model to accurately forecast inventory demand based on historical sales data
</h4>





### :bookmark_tabs: Description

Planning a celebration is a balancing act of preparing just enough food to go around without being stuck eating the same leftovers for the next week. The key is anticipating how many guests will come. Grupo Bimbo must weigh similar considerations as it strives to meet daily consumer demand for fresh bakery products on the shelves of over 1 million stores along its 45,000 routes across Mexico.

Currently, daily inventory calculations are performed by direct delivery sales employees who must single-handedly predict the forces of supply, demand, and hunger based on their personal experiences with each store. With some breads carrying a one week shelf life, the acceptable margin for error is small.

In this competition, Grupo Bimbo invites Kagglers to develop a model to accurately forecast inventory demand based on historical sales data. Doing so will make sure consumers of its over 100 bakery products aren’t staring at empty shelves, while also reducing the amount spent on refunds to store owners with surplus product unfit for sale.



### :dart: Goal
In summary, the goal is to build a machine learning model to forecast the demand of a product for a given week, at a particular store. 



### :exclamation: ​Instructions

1. Datasets: 

   - train.csv — the training set (it is found at [problem's page on Kaggle](https://www.kaggle.com/c/grupo-bimbo-inventory-demand/data));
   - test.csv — the test set (it is found at [problem's page on Kaggle](https://www.kaggle.com/c/grupo-bimbo-inventory-demand/data));
   - cliente_tabla.csv — client names (can be joined with train/test on Cliente_ID);
   - producto_tabla.csv — product names (can be joined with train/test on Producto_ID);
   - town_state.csv — town and state (can be joined with train/test on Agencia_ID);
   - train_sample — a sample from train.csv with 1kk records and set.seed = 123.
2. EDA: it contains the 'ExploratoryDataAnalysis', in which we investigate the dataset in order to know it, generating plots and correlations.
3. Modelling_and_Evaluation: it contains how the model was build, its transformation, algorithm ML and metrics on Kaggle.



### Useful links

- [Problem's page on Kaggle](https://www.kaggle.com/c/grupo-bimbo-inventory-demand/overview) 

- [Big Data Analytics with R and Microsoft Azure Machine Learning](https://www.datascienceacademy.com.br/course/analise-de-dados-com-r) (this repository is a project for data science course from Data Science Academy)

