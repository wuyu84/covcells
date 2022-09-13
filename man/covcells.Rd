\name{Make some cells for figures}
\alias{cellsmake}
\title{covcells}
\usage{
函数使用方法
cellsmake(data)

data为输入数据框，其格式要求：
包括至少7个变量：管辖区，街道，采样，报告，姓名，社会面，职业。均为文本格式。
管辖区变量长度为2，包括深圳11个区划，取2个字，比如罗湖、福田等等。
街道为深圳市街道，不含‘街道’字样
采样为采样日期
报告为阳性报告日期
社会面包括：社会面、隔离管控 两个分类
职业为文本格式。

具体可使用str(data_test)查看包内自带数据框样式.

输出会在当前目录下生成 一个 命名为 待做图元素库.pos 的文件，将其导入wps流程图即可。

注意:一次最多只能制作20个元素,如果超过需要切分数据,分多次运行.
}
\description{
深圳市COV个案关系图 元素批量制作包:
包括一个函数 cellsmake
}
\example{
调用包内自带数据框data_test
cellsmake(data_test)
}
