# xmucdaDB

An Introduction to Categorical Data Analysis, 2nd Edition by Alan Agresti, Wiley (2007). 书上的数据。


## 安装

``` r
# install.packages("devtools")
devtools::install_github("jinzhen-lin/xmucdaDB")
```

## 使用

- 使用`data(package = "xmucdaDB")`查看包中包含的数据及其介绍。


- 使用`data("dataname")`来导入相应的数据。


- 使用`find_data_by_title()`函数来根据数据集描述的标题（即`data(package = "xmucdaDB")`结果中的数据介绍）来查找数据集。


- 使用`find_data_by_var()`函数根据数据中包含的变量名来查找数据集。

- 关于以上两个函数的使用，可通过`?find_data_by_title`或`?find_data_by_var`查看帮助。
