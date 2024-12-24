# Instructions 说明
### Main Part 主要部分

Please check the `Rmd` file or `html` file for the main part of my answers. All Part I answers and Part II codes and natural langugage answers are there. All other stuff are just for supplement. (In case you can't find something.)

请在压缩包主目录的 `Rmd` 文件或 `html` 文件中查看回答的主要部分。所有 Part I 的答案以及 Part II 的代码部分和自然语言答案部分都在其中。本压缩包中其他所有部分仅仅是这部分的补充。（以防您找不到输出结果或者什么的。）

### results 结果

I have put finished project directory in the `r_home_exam` directory, check it for everything(literally unchanged except for removing the `.git` directory). Codes are in `Part_1` and `Part_2`, while results are in `output`, e.g. plots, tables, etc.

我把已经跑完代码的项目目录放在了 `r_home_exam` 目录中，我所做的一切都在那里（除了把`.git`目录删除了）。 `Part_1` 和 `Part_2` 里放着代码，`output` 里面放着跑出来的结果，比如图，表格等等。

### Dependencies 依赖

Please install the following packages to replicate my home exam code:
要复现我的代码，请确保下面的包已经安装：

```
tidyverse, ggplot2, rpart, rpart.plot, dplyr, randomForest, tibble, tools，ROCR
```

### Replicate 复现

To replicate my results of Part II, just change directory to my `Replication` directory in my zip archive, `setwd("/path/to/Replication")` to set working directory to my `Replication` directory and `soucre(main.R)`. Then please check `output/` directory for the replicated results.

要想复现我 Part II 的结果，请打开 zip 压缩包中的 `Replication` 目录，确保 R 的工作路径为 `Replication` 目录，然后运行 `source(main.R)`。等待运行完成后，就请在 `output` 目录中查看复现的结果。
