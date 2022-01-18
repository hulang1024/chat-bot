# chat-bot
聊天机器程序。

### 准备
#### 0. 安装 [Racket](https://www.racket-lang.org/)
Windows 安装 DrRacket。   
Linux 安装 Racket。

#### 1. 安装 [MCL](https://docs.mirai.mamoe.net/mcl/)
0. chat-bot的收发消息默认实现基于[mirai](https://docs.mirai.mamoe.net/)。
1. MCL安装 [mirai-api-http](https://docs.mirai.mamoe.net/mirai-api-http/)插件。
2. 配置 mcl的`config/MiraiApiHttp/setting.yml`，模板见[setting.yml](_mcl/config/net.mamoe.mirai-api-http/settings.yml)。


### 配置
在`config.rkt`中修改变量：
- `bot-id` 机器人qq号。 
- `bot-nickname` 机器人qq昵称。
- `admin-ids` 管理员id。 
- `mirai-ws-server-config` 为mirai websocket服务器配置。

在`db/config.rkt`中配置数据库：
- `db-path` SQLite数据库文件路径。
#### 更多配置
- [Lisp程序执行服务](eval-service/lisp/config.rkt)
- [摸鱼](func-lib/tools/moyu/config.rkt)

### 运行

先运行MCL，接着运行chat-bot：  
在DrRacket中运行 `main.rkt`。  
或命令行：`racket main.rkt`。
