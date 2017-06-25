# DM-Project1
1st Project for the Data Mining Course at THU

Gao Tong

Chen Yazheng

## 需要的R语言包

首先需要安装JDK，运行命令`R CMD javareconf`。随后安装以下包：

- XML
- tm
- SnowballC
- plyr
- wordcloud
- qdap
- ggplot2


## 运行方法

为确保可以检查逐步运行的结果，首先进入`script`目录，通过命令

```bash
$ r
```

进入R语言界面，随后通过

```R
source("runProj.R")
```

运行编辑好的运行脚本。中间过程的结果会随时保存。在脚本运行完毕后，可通过

```R
ls()
```

检查每步的运行结果。

