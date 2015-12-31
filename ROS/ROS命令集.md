# ROS命令
## roscore
包含内容：主机（master，为ROS提供命名服务）+系统输出（rosout）+参数服务器（parameter server）
作用：开启ROS主机，运行ROS系统输出节点以及开启参数服务器。

## rosnode
节点有关命令
- `rosnode list` 显示当前运行的全部节点
- `rosnode info [node_name]` 显示该节点的信息（包括发布的话题，订阅的话题，参与的服务等信息）
- `rosnode ping [node_name]` 检查某个节点是否畅通

## rosrun
`rosrun [package_name] [node_name]`运行某个包里的某个节点，不需要知道节点的绝对路径
*`rosrun [package_name] [node_name] __name:=new_name` 将某个节点重命名为 new_name*

**重要节点补充：**
- `rosrun rqt_graph rqt_graph` 显示当前运行的节点和话题
- `rosrun rqt_plot rqt_plot` 显示话题发布的数据的滚动时间图
- `rosrun rqt_console rqt_console` 显示节点输出
- `rosrun rqt_logger_level rqt_logger_level` 允许改变显示的节点输出等级（DEBUG, WARN, INFO, ERROR）

## rostopic
话题有关命令
- `rostopic bw [topic]` 展示话题使用的频率范围
- `rostopic echo [topic]` 打印消息到屏幕上
- `rostopic hz [topic]` 展示话题的发布频率
- `rostopic list [topic]` 打印活跃的话题信息
- `rostopic pub [topic] [msg_type] [args]` 手动发布数据到话题上
- `rostopic type [topic]` 打印话题的类型

## rosmsg
消息有关命令
- `rosmsg show [message type]` 显示消息信息

## rossrv
服务有关命令
- `rossrv show [service type]` 显示服务信息

## rosservice
服务有关命令
- `rosservice list` 打印活跃的服务的信息
- `rosservice call [service] [args]` 使用提供的参数调用服务
- `rosservice type [service]` 打印服务类型
- `rosservice find` 通过服务类型查找服务
- `rosservice uri` 打印服务ROSRPC uri数据

## rosparam
`rosparam`允许你在数据服务器上对数据进行存储和操作
- `rosparam set [param_name]` 设置参数
- `rosparam get [param_name]` 得到参数
- `rosparam load [file_name] [namespace]` 从文件中装载参数
- `rosparam dump [file_name] [namespace]` 将参数保存至文件中
- `rosparam delete` 删除参数
- `rosparam list` 参数名列表

## roslaunch
将节点作为一个launch文件中定义的方式启动
- `roslaunch [package] [filename.launch]` 运行某个包下的launch文件

## roscp
从一个包里复制一个文件到另一个包中
- `roscp [package_name] [file_to_copy_path] [copy_path]`
